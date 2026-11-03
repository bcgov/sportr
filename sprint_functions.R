library(data.table)
library(ggplot2)
library(patchwork)


plot_portfolio_draws <- function(
    climate, spp_list, bgc, 
    covariance, 
    ss_preds = NULL,
    use_returns = FALSE,
    use_returns_sd = FALSE,
    n_draws = 50L,
    n_show = 9L,
    n_frontier = 20L,
    seed = NULL,
    max_attempts = 5L * n_draws,
    setup_name = NULL,
    output_dir = NULL,
    portfolio_fn = NULL
) {
  stopifnot(
    n_show == 9L,
    n_draws >= n_show,
    n_frontier >= 2L
  )
  
  if (!is.null(seed)) set.seed(seed)
  
  if (is.null(setup_name)) {
    setup_name <- paste0(bgc, "_return_cov_", use_returns,
                         "_return_sd_", use_returns_sd)
  }
  stopifnot(length(setup_name) == 1L, !is.na(setup_name), nzchar(setup_name))
  if (!use_returns && is.null(covariance)) {
    stop("Supply covariance or set use_returns = TRUE.")
  }
  export_names <- c("selected_weights_by_draw.csv", "selected_weight_summary.csv",
                    "mean_selected_portfolios.csv", "failed_draws.csv", "settings.rds")
  if (!is.null(output_dir) && any(file.exists(file.path(output_dir, export_names)))) {
    stop("Output files already exist; choose a new output_dir to preserve earlier results.")
  }
  
  get_selected <- function(ef, draw_id, attempt_id) {
    direct_types <- c("MaxSharpe", "90%")
    types <- c(direct_types, "MaxReturn", "MinVariance")
    rows <- attr(ef, "selection_rows")
    if (is.null(rows) || !all(direct_types %in% names(rows))) {
      stop("Direct selection metadata missing: use the updated optimise_portfolio() in full mode.")
    }
    # Extract endpoints from the complete per-draw frontier, before removing
    # the extra direct solutions. Exact ties use the first matching row.
    if (!nrow(ef) || !all(c("return", "frontier_sd") %in% names(ef)) ||
        !is.numeric(ef$return) || !is.numeric(ef$frontier_sd) ||
        any(!is.finite(ef$return)) || any(!is.finite(ef$frontier_sd))) {
      stop("Finite return and risk values are required to select frontier endpoints.")
    }
    indices <- c(unname(rows[direct_types]),
                 which.max(ef$return), which.min(ef$frontier_sd))
    if (anyNA(indices) || any(indices < 1 | indices > nrow(ef)) ||
        any(indices != as.integer(indices))) stop("Invalid direct selection row indices.")
    selected <- copy(as.data.table(ef))[indices]
    for (sp in setdiff(spp_list, names(selected))) set(selected, j = sp, value = 0)
    W <- as.matrix(selected[, ..spp_list])
    if (!is.numeric(W) || any(!is.finite(W)) || any(W < -1e-6) ||
        any(abs(rowSums(W)-1) > 1e-5)) stop("Invalid selected portfolio weights.")
    W[W < 0] <- 0
    W <- W/rowSums(W)
    # Explicit columns avoid confusing individual portfolio performance with
    # performance of the averaged weights, which must be evaluated separately.
    selected <- as.data.table(W)
    setnames(selected, spp_list)
    selected[, `:=`(draw = draw_id, attempt_id = attempt_id, PortfolioType = types)]
    selected[]
  }
  
  get_grid <- function(ef, draw_id) {
    # Remove the two extra direct solutions from the updated optimiser,
    # leaving the regularly spaced return-target grid.
    selection_rows <- attr(ef, "selection_rows")
    
    extra_rows <- if (is.null(selection_rows)) {
      integer(0)
    } else {
      unname(selection_rows[
        intersect(c("MaxSharpe", "90%"), names(selection_rows))
      ])
    }
    
    grid <- copy(as.data.table(ef))
    
    if (length(extra_rows)) {
      grid <- grid[-unique(extra_rows)]
    }
    
    if (nrow(grid) != n_frontier) {
      stop(
        "Draw ", draw_id, " has ", nrow(grid),
        " grid rows; expected ", n_frontier,
        ". Use full-frontier mode."
      )
    }
    
    # Align from minimum variance to maximum return.
    setorderv(grid, "return")
    
    # Species excluded by the optimiser have zero allocation.
    for (sp in setdiff(spp_list, names(grid))) {
      set(grid, j = sp, value = 0)
    }
    
    if (!all(vapply(grid[, ..spp_list], is.numeric, logical(1)))) {
      stop("Non-numeric species weights in draw ", draw_id)
    }
    
    W <- as.matrix(grid[, ..spp_list])
    
    if (any(!is.finite(W)) ||
        any(W < -1e-6) ||
        any(abs(rowSums(W) - 1) > 1e-5)) {
      stop("Invalid allocation weights in draw ", draw_id)
    }
    
    if (any(!is.finite(grid$return)) ||
        max(grid$return) <= 0 ||
        any(!is.finite(grid$frontier_sd)) ||
        any(grid$frontier_sd < 0)) {
      stop("Invalid return or risk values in draw ", draw_id)
    }
    
    # Correct negligible numerical roundoff.
    W[W < 0] <- 0
    W <- W / rowSums(W)
    
    for (j in seq_along(spp_list)) {
      set(grid, j = spp_list[j], value = W[, j])
    }
    
    grid[, `:=`(
      draw = draw_id,
      position = seq_len(.N),
      relative_return = return / max(return)
    )]
    
    grid[]
  }
  
  stopifnot(
    length(max_attempts) == 1L,
    is.finite(max_attempts),
    max_attempts >= n_draws,
    max_attempts == as.integer(max_attempts)
  )
  
  frontiers <- vector("list", n_draws)
  grids <- vector("list", n_draws)
  selected_draws <- vector("list", n_draws)
  failure_log <- list()
  
  successful <- 0L
  attempt <- 0L
  
  while (successful < n_draws && attempt < max_attempts) {
    attempt <- attempt + 1L
    draw_id <- successful + 1L
    
    draw_result <- tryCatch(
      {
        if(is.null(portfolio_fn)){
          ef <- indiv_portfolio(
            climate = climate,
            spp_list = spp_list,
            bgc = bgc,
            covariance = covariance,
            return_cov = use_returns,
            use_returns_sd = use_returns_sd
          )
        } else {
          ef <- portfolio_fn(
            climate = climate,
            ss_preds = ss_preds,
            spp_list = spp_list,
            bgc = bgc,
            covariance = covariance,
            return_cov = use_returns,
            use_returns_sd = use_returns_sd
          )
        }
        
        
        selected <- get_selected(ef, draw_id, attempt)
        grid <- get_grid(ef, draw_id)
        grid[, attempt_id := attempt]
        
        list(ok = TRUE, frontier = ef, grid = grid, selected = selected)
      },
      error = function(e) {
        list(ok = FALSE, message = conditionMessage(e))
      }
    )
    
    if (!draw_result$ok) {
      failure_log[[length(failure_log) + 1L]] <- data.table(
        attempt = attempt,
        message = draw_result$message
      )
      
      message(
        "Skipping attempt ", attempt, ": ",
        draw_result$message
      )
      
      next
    }
    
    successful <- successful + 1L
    frontiers[[successful]] <- draw_result$frontier
    grids[[successful]] <- draw_result$grid
    selected_draws[[successful]] <- draw_result$selected
    
    if (successful %% 10L == 0L || successful == n_draws) {
      message(
        "Completed ", successful, "/", n_draws,
        " successful draws in ", attempt, " attempts"
      )
    }
  }
  
  failures <- if (length(failure_log)) {
    rbindlist(failure_log)
  } else {
    data.table(attempt = integer(), message = character())
  }
  
  if (successful < n_draws) {
    stop(
      "Obtained only ", successful, " successful draws after ",
      attempt, " attempts. Last error: ",
      if (nrow(failures)) tail(failures$message, 1L) else "Unknown"
    )
  }
  
  if (nrow(failures)) {
    warning(
      nrow(failures), " failed attempts were excluded. ",
      "The figure averages successful draws only; inspect $failures.",
      call. = FALSE
    )
  }
  
  all_grids <- rbindlist(
    grids,
    use.names = TRUE,
    fill = TRUE
  )
  
  # Average matching frontier positions, not matching risk values.
  average_columns <- c(
    spp_list, "return", "frontier_sd", "relative_return"
  )
  
  average <- all_grids[
    ,
    lapply(.SD, mean),
    by = position,
    .SDcols = average_columns
  ]
  
  setorder(average, position)
  
  # Average each per-draw selection, including maximum-return and minimum-risk
  # endpoints; do not select these portfolios from the averaged frontier.
  selected_wide <- rbindlist(selected_draws, use.names = TRUE)
  selected_weights <- melt(
    selected_wide,
    id.vars = c("draw", "attempt_id", "PortfolioType"),
    measure.vars = spp_list,
    variable.name = "species", value.name = "weight",
    variable.factor = FALSE
  )
  selected_weights[, `:=`(
    setup = setup_name, bgc = ..bgc,
    use_returns = ..use_returns, use_returns_sd = ..use_returns_sd
  )]
  group_columns <- c("setup", "bgc", "use_returns", "use_returns_sd", "PortfolioType", "species")
  selected_summary <- selected_weights[, .(
    n_draws = .N,
    mean_weight = mean(weight),
    sd_weight = sd(weight),
    median_weight = median(weight),
    q025_weight = unname(quantile(weight, .025)),
    q975_weight = unname(quantile(weight, .975))
  ), by = group_columns]
  # Quantiles describe between-draw variation; they are not confidence intervals
  # for mean weights. Zeros for omitted species are included in every statistic.
  mean_portfolios <- dcast(
    selected_summary,
    setup + bgc + use_returns + use_returns_sd + PortfolioType ~ species,
    value.var = "mean_weight"
  )
  setcolorder(mean_portfolios, c(setdiff(group_columns, "species"), spp_list))
  if (any(abs(rowSums(as.matrix(mean_portfolios[, ..spp_list]))-1)>1e-8)) {
    stop("Mean selected portfolio weights do not sum to one.")
  }
  settings <- list(
    setup = setup_name, bgc = bgc, species = spp_list,
    use_returns = use_returns, use_returns_sd = use_returns_sd,
    n_draws = n_draws, attempts = attempt, failed_attempts = nrow(failures),
    seed = seed, n_frontier = n_frontier,
    portfolio_types = c("MaxSharpe", "90%", "MaxReturn", "MinVariance"),
    summary = "Mean per-draw MaxSharpe, 90%, MaxReturn and MinVariance weights across successful draws",
    endpoint_selection = "MaxReturn: highest return; MinVariance: lowest frontier_sd in each full frontier. Exact ties use the first row.",
    quantiles = "2.5% and 97.5% empirical weight quantiles across successful draws; not mean confidence intervals"
  )
  
  panel <- function(grid, title, average_panel = FALSE) {
    long <- melt(
      grid,
      id.vars = c("position", "frontier_sd"),
      measure.vars = spp_list,
      variable.name = "species",
      value.name = "weight"
    )
    
    long[, species := factor(species, levels = spp_list)]
    
    ggplot(
      long,
      aes(
        x = frontier_sd,
        y = weight,
        fill = species,
        group = species
      )
    ) +
      geom_area(
        stat = "identity",
        position = "stack",
        colour = "grey50",
        linewidth = 0.1
      ) +
      makeColScale(spp_list, fread("./new_species_colours.csv")) +
      geom_line(
        data = grid,
        aes(
          x = frontier_sd,
          y = relative_return
        ),
        inherit.aes = FALSE,
        colour = "black",
        linetype = "dashed",
        linewidth = 0.6
      ) +
      scale_x_reverse(
        expand = expansion(mult = 0.02)
      ) +
      scale_y_continuous(
        breaks = c(0, 0.5, 1),
        labels = c("0", "0.5", "1"),
        expand = expansion(mult = c(0, 0.02))
      ) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(
        title = title,
        x = if (average_panel) {
          "Mean quadratic risk at each frontier position (decreasing →)"
        } else {
          "Quadratic risk (decreasing →)"
        },
        y = "Allocation proportion",
        fill = "Species"
      ) +
      theme_minimal(base_size = 10) +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom"
      )
  }
  
  individual_panels <- lapply(seq_len(n_show), function(i) {
    panel(
      grids[[i]],
      title = paste("SI draw", i)
    )
  })
  
  mean_panel <- panel(
    average,
    title = paste("Mean allocation across", n_draws, "SI draws"),
    average_panel = TRUE
  )
  
  figure <- wrap_plots(
    c(individual_panels, list(mean_panel)),
    design = "
ABC
DEF
GHI
JJJ
",
    heights = c(1, 1, 1, 1.15),
    guides = "collect"
  ) +
    plot_annotation(
      title = paste(
        "Portfolio sensitivity to Site Index draws:",
        bgc
      ),
      caption = paste(
        "Dashed line: return relative to each draw's maximum.",
        "Bottom panel averages allocations, risk and relative return",
        "at matching frontier positions."
      )
    )
  
  figure <- figure & theme(legend.position = "bottom")
  
  if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    fwrite(selected_weights, file.path(output_dir, "selected_weights_by_draw.csv"))
    fwrite(selected_summary, file.path(output_dir, "selected_weight_summary.csv"))
    fwrite(mean_portfolios, file.path(output_dir, "mean_selected_portfolios.csv"))
    fwrite(failures, file.path(output_dir, "failed_draws.csv"))
    saveRDS(settings, file.path(output_dir, "settings.rds"))
  }
  
  list(
    plot = figure,
    average = average,
    aligned_draws = all_grids,
    frontiers = frontiers,
    failures = failures,
    attempts = attempt,
    selected_weights = selected_weights,
    selected_summary = selected_summary,
    mean_portfolios = mean_portfolios,
    settings = settings
  )
}



indiv_portfolio <- function(climate, spp_list, bgc, covariance = NULL, return_cov = FALSE, use_returns_sd = FALSE) {
  conditions <- climate[
    , .(species = spp_list), by = .(PERIOD, DD5_an, CMD_an)
  ]
  setnames(conditions, old = c("DD5_an", "CMD_an"), new = c("DD5", "CMD"))
  conditions[, `:=`(
    bgc = bgc,
    moisture = 4,
    nutrient = "C"
  )]
  
  si <- draw_si_sigmoid(
    fit_sigmoid,
    newdata = conditions,
    n_draws = 1,
    type = "mean"
  )
  trajectory <- copy(conditions)
  trajectory[, SI := as.numeric(si[1, ])]
  temp <- dcast(trajectory, PERIOD ~ species, value.var = "SI")
  setorder(temp, PERIOD)
  temp[, PERIOD := NULL]
  if(return_cov) {
    covariance <- cov(temp)
  } else if (use_returns_sd) {
    sd_vec <- apply(
      temp,
      2,
      sd
    )
    covariance <- diag(sd_vec) %*%
      covariance %*%
      diag(sd_vec)
    colnames(covariance) <- rownames(covariance) <- names(temp)
  }
  
  boundDat <- data.table(Spp = names(temp))
  boundDat[,`:=`(minWt = 0, maxWt = 1)]
  ef <- optimise_portfolio(temp, covariance, boundDat, minTot = 0.01, mode = "full") 
  return(ef)
}

indiv_portfolio_prob_loss <- function(climate, ss_preds, spp_list, bgc, covariance = NULL, return_cov = FALSE, use_returns_sd = FALSE) {
  estab_success <- data.table(spp = spp_list, success = 100)
  estab_success[spp %in% c("Sx","Pl","Bl"), success := 100]
  
  conditions <- climate[
    , .(species = spp_list), by = .(PERIOD, DD5_an, CMD_an)
  ]
  setnames(conditions, old = c("DD5_an", "CMD_an"), new = c("DD5", "CMD"))
  conditions[, `:=`(
    bgc = bgc,
    moisture = 4,
    nutrient = "C"
  )]
  
  si <- draw_si_sigmoid(
    fit_sigmoid,
    newdata = conditions,
    n_draws = 1,
    type = "mean"
  )
  trajectory <- copy(conditions)
  trajectory[, SI := as.numeric(si[1, ])]
  temp <- dcast(trajectory, PERIOD ~ species, value.var = "SI")
  setorder(temp, PERIOD)
  feas_prob <- data.table(suit = c(1,2,3,4), gamma_shp = c(2,2,2,2), gamma_rate = c(2,1.8,0.7,0.2), ProbEvent = c(90,85,70,40))
  
  for(tree in spp_list){
    ss_run <- temp[,.(PERIOD,get(tree))]
    setnames(ss_run, c("Year", "SI"))
    ss_run[,Year := as.integer(Year)]
    suit_spp <- suit_table[spp == tree,]
    ss_preds[suit_spp, suit := i.newfeas, on = c(SS_NoSpace = "ss_nospace")]
    ss_temp <- ss_preds[,.(suit = mean(suit, na.rm = T)), by = .(Year)]
    setnafill(ss_temp, fill = 4, cols = "suit")
    ss_temp[,Year := as.integer(Year) + 14]
    annual_suit <- ss_temp[
      order(as.integer(Year)),
      {
        interpolated <- approx(
          x = as.integer(Year),
          y = suit,
          xout = 2015:2100,
          method = "linear"
        )
        .(Year = interpolated$x, suit = interpolated$y)
      }
    ]
    setnafill(annual_suit, type = "locf")
    annual_suit[,suit := round(suit)]
    
    annual_suit[feas_prob, `:=`(gamma_rate = i.gamma_rate, gamma_shp = i.gamma_shp, ProbEvent = i.ProbEvent), on = "suit"]
    annual_suit[ss_run, SI := i.SI, on = "Year"]
    setorder(annual_suit, Year)
    annual_suit <- annual_suit[,.(SI = SI/50,  gamma_shp, gamma_rate, ProbEvent)]
    Returns <- simGrowthCpp(DF = annual_suit, estab = as.integer(estab_success[spp == tree, success]), chance_ruin = 79)$returns
    tmpR <- c(0,Returns)
    assets <- Returns - tmpR[-length(tmpR)]
    # temp <- data.table(Spp = tree, 
    #                    Year = 1:length(Returns), 
    #                    It = run, 
    #                    Returns = Returns)
    # sim_ls[[count]] <- temp
    # count <- count + 1
    if(tree == spp_list[1]){
      tree_ass <- data.table(Year = 1:length(assets))
    }
    set(tree_ass, j = tree, value = assets)
  }
  returns <- tree_ass
  
  returns[, Year := NULL]
  if(return_cov) {
    covariance <- cov(returns)
  } else if (use_returns_sd) {
    sd_vec <- apply(
      returns,
      2,
      sd
    )
    covariance <- diag(sd_vec) %*%
      covariance %*%
      diag(sd_vec)
    colnames(covariance) <- rownames(covariance) <- names(temp)
  }
  
  boundDat <- data.table(Spp = names(returns))
  boundDat[,`:=`(minWt = 0, maxWt = 1)]
  ef <- optimise_portfolio(returns, covariance, boundDat, minTot = 0.01, mode = "full") 
  return(ef)
}

simulate_portfolio <- function(climate, ss_preds, spp_list, bgc, portfolio_weights, feas_prob) {
  estab_success <- data.table(spp = spp_list, success = 100)
  estab_success[spp %in% c("Sx","Pl","Bl"), success := 100]
  
  conditions <- climate[
    , .(species = spp_list), by = .(PERIOD, DD5_an, CMD_an)
  ]
  setnames(conditions, old = c("DD5_an", "CMD_an"), new = c("DD5", "CMD"))
  conditions[, `:=`(
    bgc = bgc,
    moisture = 4,
    nutrient = "C"
  )]
  
  si <- draw_si_sigmoid(
    fit_sigmoid,
    newdata = conditions,
    n_draws = 1,
    type = "mean"
  )
  trajectory <- copy(conditions)
  trajectory[, SI := as.numeric(si[1, ])]
  temp <- dcast(trajectory, PERIOD ~ species, value.var = "SI")
  setorder(temp, PERIOD)
  #feas_prob <- data.table(suit = c(1,2,3,4), gamma_shp = c(2,2,2,2), gamma_rate = c(3,2,1,0.3), ProbEvent = c(95,90,80,60))
  
  return_vec <- vector("numeric", length(spp_list))
  nTree_vec <- vector("numeric", length(spp_list))
  
  count = 1
  for(tree in spp_list){
    ss_run <- temp[,.(PERIOD,get(tree))]
    setnames(ss_run, c("Year", "SI"))
    ss_run[,Year := as.integer(Year)]
    suit_spp <- suit_table[spp == tree,]
    ss_preds[suit_spp, suit := i.newfeas, on = c(SS_NoSpace = "ss_nospace")]
    ss_temp <- ss_preds[,.(suit = mean(suit, na.rm = T)), by = .(Year)]
    setnafill(ss_temp, fill = 4, cols = "suit")
    ss_temp[,Year := as.integer(Year) + 14]
    annual_suit <- ss_temp[
      order(as.integer(Year)),
      {
        interpolated <- approx(
          x = as.integer(Year),
          y = suit,
          xout = 2015:2100,
          method = "linear"
        )
        .(Year = interpolated$x, suit = interpolated$y)
      }
    ]
    setnafill(annual_suit, type = "locf")
    annual_suit[,suit := round(suit)]
    
    annual_suit[feas_prob, `:=`(gamma_rate = i.gamma_rate, gamma_shp = i.gamma_shp, ProbEvent = i.ProbEvent), on = "suit"]
    annual_suit[ss_run, SI := i.SI, on = "Year"]
    setorder(annual_suit, Year)
    annual_suit <- annual_suit[,.(SI = SI/50,  gamma_shp, gamma_rate, ProbEvent)]
    ret_ls <- simGrowthCpp(DF = annual_suit, estab = as.integer(estab_success[spp == tree, success]), chance_ruin = 79)
    
    return_vec[count] <- ret_ls$returns[length(ret_ls$returns)]
    nTree_vec[count] <- ret_ls$nTrees
    count = count + 1
  }
  
  names(return_vec) <- spp_list
  names(nTree_vec) <- spp_list
  
  out_ls <- list()
  
  for(ptype in unique(port_weights$PortfolioType)) {
    temp <- port_weights[PortfolioType == ptype,]
    portfolio_weights <- temp$mean_weight
    names(portfolio_weights) <- temp$species
    
    return_temp <- return_vec[names(portfolio_weights)]
    nTree_temp <- nTree_vec[names(portfolio_weights)]
    
    tot_trees <- portfolio_weights %*% nTree_temp
    tot_return <- portfolio_weights %*% return_temp
    out_ls[[ptype]] <- list(return = tot_return, trees = tot_trees)
  }
  return(rbindlist(out_ls, idcol = "PortfolioType"))
}


plot_indiv_frontier <- function(ef){
  ef_long <- melt(ef, id.vars = c("frontier_sd","return","sharpe"))
  spp_clrs <- get_species_colours(unique(ef_long$variable))
  RetCurve <- stats::approx(ef$return,ef$frontier_sd,xout = 0.9*max(ef$return))
  ret90 <- RetCurve$y
  colScale <- makeColScale(unique(ef_long$variable), ccissr::TreeCols)
  
  ggplot(ef_long, aes(x = frontier_sd, y = value)) +
    geom_area(aes(fill = variable, group = variable), size = 0.00001, col = "grey50", stat = "identity")+
    colScale +
    geom_line(data = ef, aes(x = frontier_sd, y = return/max(return)), colour = "black", linetype = "F1",linewidth = .75)+
    scale_x_reverse() +
    geom_vline(data = ef[sharpe == max(sharpe),], aes(xintercept = frontier_sd,colour = "MaxSharpe"), 
               linetype = "twodash", size = .75) +
    geom_vline(xintercept = ret90, linetype = "twodash", size = .75) +
    xlab("Max Return --> Minimized Risk")+
    ylab("Portfolio Ratio")+
    guides(fill=guide_legend("Species"))
  
}

# Bray-Curtis comparison of plot_portfolio_draws() selected allocations.
# install.packages(c("data.table", "vegan", "ggplot2"))
# Input: combined selected_summary tables (level="mean") or combined
# selected_weights tables (level="draw"). Use distinct setup names for setups.
# For draw plots, pass mean_data=combined selected_summary tables. Mean portfolios
# are included as additional observations in the SAME NMDS fit (not centroids).
# Hulls use individual draws only and describe their extent, not confidence regions.
# NMDS uses untransformed proportions: no square root or Wisconsin scaling.
# Fewer than six distinct profiles: return an exact distance heatmap instead.
# Docs: https://vegandevs.github.io/vegan/reference/metaMDS.html

portfolio_nmds <- function(data, level=c("mean", "draw"),
                           seed=123, trymax=100, mean_data=NULL,
                           show_hulls=TRUE, hull_by_portfolio=FALSE) {
  level <- match.arg(level)
  for (pkg in c("data.table","vegan","ggplot2")) {
    if (!requireNamespace(pkg, quietly=TRUE)) stop("Install package: ",pkg)
  }
  d <- data.table::copy(data.table::as.data.table(data))
  value_col <- if (level=="mean") "mean_weight" else "weight"
  required <- c("setup","PortfolioType","species",value_col)
  if (level=="draw") required <- c(required,"draw")
  if (!all(required %in% names(d)) || !nrow(d))
    stop("Missing columns: ",paste(setdiff(required,names(d)),collapse=", "))
  # Include available metadata to keep sites and covariance settings distinct.
  id_cols <- c("setup",intersect(c("bgc","use_returns","use_returns_sd"),names(d)),"PortfolioType")
  group_cols <- id_cols
  d[, point_type := if(level=="mean") "Mean portfolio" else "Individual draw"]
  if (!is.null(mean_data)) {
    if (level!="draw") stop("mean_data is only used with level='draw'.")
    m <- data.table::copy(data.table::as.data.table(mean_data))
    needed <- c(group_cols,"species","mean_weight")
    if (!all(needed %in% names(m)) || !nrow(m))
      stop("mean_data needs mean_weight, species and the same setup/site/type metadata as data.")
    if (anyNA(m[,c(group_cols,"species"),with=FALSE])) stop("Missing identifiers in mean_data.")
    raw_groups <- unique(d[,group_cols,with=FALSE])
    mean_groups <- unique(m[,group_cols,with=FALSE])
    if (nrow(merge(raw_groups,mean_groups,by=group_cols))!=nrow(raw_groups) ||
        nrow(mean_groups)!=nrow(raw_groups))
      stop("mean_data must contain exactly the setup/site/portfolio groups present in the draw data.")
    m[, `:=`(weight=mean_weight, draw="mean", point_type="Mean portfolio")]
    d[, draw := as.character(draw)]
    keep <- c(group_cols,"species","weight","draw","point_type")
    d <- data.table::rbindlist(list(d[,keep,with=FALSE],m[,keep,with=FALSE]),use.names=TRUE)
  }
  if (level=="draw") id_cols <- c(id_cols,"draw")
  id_cols <- c(id_cols,"point_type")
  if (anyNA(d[, c(id_cols,"species"), with=FALSE])) stop("Missing identifiers/species.")
  if (!is.numeric(d[[value_col]]) || any(!is.finite(d[[value_col]])) || any(d[[value_col]] < -1e-10))
    stop("Weights must be finite nonnegative numbers.")
  if (anyDuplicated(d[, c(id_cols,"species"),with=FALSE]))
    stop("Duplicate species rows within a portfolio. Give each setup/run a distinct setup name.")
  d[, weight_value := pmax(get(value_col),0)]
  d[, observation_id := .GRP, by=id_cols]
  metadata <- unique(d[, c("observation_id",id_cols),with=FALSE])
  data.table::setorder(metadata,observation_id)
  wide <- data.table::dcast(d,observation_id ~ species,value.var="weight_value",fill=0)
  data.table::setorder(wide,observation_id)
  W <- as.matrix(wide[,setdiff(names(wide),"observation_id"),with=FALSE])
  if (any(abs(rowSums(W)-1)>1e-6)) stop("Each portfolio must sum to 1; check that all species were supplied.")
  W <- W/rowSums(W)
  rownames(W) <- paste0("p",metadata$observation_id)
  metadata[, label := do.call(paste,c(.SD,sep=" | ")), .SDcols=id_cols]
  # Missing species columns across setups are assumed to mean zero allocation.
  D <- vegan::vegdist(W,method="bray")
  M <- as.matrix(D)
  rownames(M) <- colnames(M) <- metadata$label
  distance_plot <- NULL
  if (nrow(W)<=40) {
    tiles <- data.table::as.data.table(as.table(M))
    data.table::setnames(tiles,c("portfolio_x","portfolio_y","bray"))
    distance_plot <- ggplot2::ggplot(tiles,ggplot2::aes(portfolio_x,portfolio_y,fill=bray)) +
      ggplot2::geom_tile() + ggplot2::coord_equal() +
      ggplot2::scale_fill_viridis_c(limits=c(0,1),name="Bray-Curtis") +
      ggplot2::labs(x=NULL,y=NULL,title="Pairwise portfolio allocation distances") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45,hjust=1),panel.grid=ggplot2::element_blank())
    if (nrow(W)<=12) distance_plot <- distance_plot +
      ggplot2::geom_text(ggplot2::aes(label=sprintf("%.2f",bray)),colour="white",size=3)
  }
  n_unique <- nrow(unique(as.data.frame(round(W,12))))
  if (nrow(W)<6 || n_unique<6) {
    warning("Fewer than six distinct allocation profiles: 2D NMDS would be poorly determined. Use the returned distance matrix/heatmap.",call.=FALSE)
    return(list(plot=distance_plot, distance_plot=distance_plot, fit=NULL,
                scores=NULL, draw_scores=NULL, mean_scores=NULL, hulls=NULL, distance=D,distance_matrix=M,
                weights=W,metadata=metadata,method="Bray-Curtis distances; NMDS not fitted"))
  }
  set.seed(seed)
  # Passing a distance object prevents ecological abundance transformations.
  fit <- vegan::metaMDS(D,k=2,engine="monoMDS",try=20,trymax=trymax,
                        trace=FALSE,autotransform=FALSE,wascores=FALSE)
  coords <- as.data.frame(vegan::scores(fit,display="sites"))
  coords <- coords[rownames(W),,drop=FALSE]
  scores <- data.table::copy(metadata)
  scores[, `:=`(NMDS1=coords$NMDS1,NMDS2=coords$NMDS2)]
  draw_scores <- scores[point_type=="Individual draw"]
  mean_scores <- scores[point_type=="Mean portfolio"]
  hulls <- data.table::copy(draw_scores[0])
  hull_groups <- if(hull_by_portfolio) c("setup","PortfolioType") else "setup"
  if (show_hulls && nrow(draw_scores)) {
    hulls <- draw_scores[, {
      xy <- unique(.SD[,.(NMDS1,NMDS2)])
      if (nrow(xy)<3 || qr(cbind(1,as.matrix(xy)))$rank<3) .SD[0] else
        .SD[chull(NMDS1,NMDS2)]
    }, by=hull_groups]
    hulls[, hull_id := .GRP, by=hull_groups]
  }
  p <- ggplot2::ggplot(scores,ggplot2::aes(NMDS1,NMDS2,colour=setup,shape=PortfolioType)) +
    ggplot2::coord_equal() + ggplot2::theme_minimal(base_size=12) +
    ggplot2::labs(title=if(level=="mean") "Mean portfolio allocations" else "Individual-draw portfolio allocations",
                  subtitle=paste0("Bray-Curtis NMDS | stress = ",formatC(fit$stress,digits=3,format="f"),
                                  " | ",nrow(W)," portfolios"),
                  colour="Setup",shape="Portfolio type",x="NMDS1",y="NMDS2")
  # Draw polygons first so they do not obscure the portfolio points.
  if (nrow(hulls)) p <- p + ggplot2::geom_polygon(
    data=hulls,ggplot2::aes(NMDS1,NMDS2,group=hull_id,fill=setup,colour=setup),
    inherit.aes=FALSE,alpha=.12,linewidth=.4,show.legend=FALSE)
  if (nrow(draw_scores)) p <- p + ggplot2::geom_point(data=draw_scores,size=1.8,alpha=.35)
  if (nrow(mean_scores)) p <- p + ggplot2::geom_point(data=mean_scores,size=4.5,alpha=1)
  if (level=="mean") {
    p <- p + ggplot2::geom_text(ggplot2::aes(label=label),vjust=-.8,size=3,show.legend=FALSE,check_overlap=TRUE)
  } else {
    p <- p + ggplot2::labs(caption=paste(
      "Small points: individual draws.",
      if(nrow(mean_scores)) "Large points: supplied mean-weight portfolios." else "",
      if(show_hulls) "\nHulls show draw extent, not confidence regions." else ""))
  }
  if (fit$converged==0) warning("The best NMDS solution was not repeated; increase trymax and inspect stability.",call.=FALSE)
  list(plot=p,distance_plot=distance_plot,fit=fit,scores=scores,
       draw_scores=draw_scores,mean_scores=mean_scores,hulls=hulls,
       distance=D,distance_matrix=M,weights=W,metadata=metadata,method="NMDS")
}


subsample_portfolio_draws <- function(
    data, n_per_setup = 30L, seed = 123
) {
  d <- data.table::copy(data.table::as.data.table(data))
  
  stopifnot(
    n_per_setup >= 1,
    n_per_setup == as.integer(n_per_setup)
  )
  
  # Keep sites and modelling settings separate if present.
  group_cols <- c(
    "setup",
    intersect(c("bgc", "use_returns", "use_returns_sd"), names(d))
  )
  
  # One entry per draw, regardless of species or portfolio type.
  draw_ids <- unique(
    d[, c(group_cols, "draw"), with = FALSE]
  )
  
  set.seed(seed)
  
  selected_ids <- draw_ids[
    ,
    .(draw = draw[
      sample.int(.N, size = min(.N, n_per_setup))
    ]),
    by = group_cols
  ]
  
  # Retain every species and portfolio type from selected draws.
  merge(
    d,
    selected_ids,
    by = c(group_cols, "draw"),
    all = FALSE,
    sort = FALSE
  )
}
