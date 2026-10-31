# Practical input-accuracy experiment for tree-species portfolios.
# Usage (after loading your REAL returns and sigma2):
# source("PracticalToleranceAnalysis.R")
# result <- run_practical_tolerance(returns, sigma2)
# Quick pilot: result <- run_practical_tolerance(returns, sigma2, n_rep = 20)
# Requires data.table, nloptr and Matrix, plus optimisation.R beside this script.
# Sourcing defines functions only: it does not overwrite inputs or run the grid.
#
# Defaults: Gaussian species-mean error SD = growth_error * abs(mean growth).
# Covariance error combines mean-one lognormal species-SD multipliers (CV =
# covariance_error) and additive correlation errors (SD = covariance_error).
# Use covariance_mode = "sd_only" or "correlation_only" to separate these.
# Correlations are projected with nearPD; realised error and repair are recorded.
# These assumptions describe errors in estimates, not process variability.
#
# Uses optimise_portfolio() and its helpers from optimisation.R unchanged.
# One frontier per draw: highest reported sharpe, lowest reported frontier_sd,
# and row nearest 90% of THAT frontier's maximum reported return. Ties use the
# first row. The 90% selection is approximate on the optimiser's finite grid.
# Its target changes per draw; it is not a fixed baseline growth constraint.
# Species removed by the optimiser receive zero weight for baseline comparison.
# Selection retains the original variance-based sharpe score; evaluation below
# uses sqrt(w' S w) for risk. minTot defaults to the original 0.01 removal rule.
# All performance is evaluated under the SAME baseline mean and covariance.
# If sigma2 is a correlation matrix, risk is an index in those units, not growth SD.
# Use a covariance in growth units when interpreting risk as growth uncertainty.

.pt_optimisation_file <- local({
  source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(source_file)) "optimisation.R" else
    file.path(dirname(normalizePath(source_file, mustWork = FALSE)), "optimisation.R")
})

pt_select_frontier <- function(X, S, optimiser, minTot = .01, return_fraction = .90) {
  types <- c("MaxSharpe", "90%", "MinVariance")
  failed <- function(msg) setNames(lapply(types, function(type)
    list(status = "solver_failure", message = msg)), types)
  tryCatch({
    spp <- colnames(X)
    bounds <- data.table::data.table(Spp = spp, minWt = 0, maxWt = 1)
    ef <- as.data.frame(optimiser(data.table::as.data.table(X), S, bounds, minTot))
    if (!nrow(ef) || !all(c("return", "frontier_sd", "sharpe") %in% names(ef)))
      stop("Optimiser returned an empty or incomplete frontier")
    if (any(!is.finite(ef$return)) || any(!is.finite(ef$frontier_sd)))
      stop("Optimiser returned non-finite frontier returns or risk")
    present <- intersect(spp, names(ef))
    if (!length(present)) stop("Optimiser returned no recognised species weights")
    target <- return_fraction * max(ef$return)
    # Keep the optimiser's definitions; do not recompute its Sharpe ranking.
    sharpe <- ef$sharpe
    sharpe[!is.finite(sharpe)] <- NA_real_
    indices <- c(MaxSharpe = if (all(is.na(sharpe))) NA_integer_ else which.max(sharpe),
                 "90%" = which.min(abs(ef$return - target)),
                 MinVariance = which.min(ef$frontier_sd))
    setNames(lapply(types, function(type) tryCatch({
      j <- indices[[type]]
      if (is.na(j)) stop("No finite Sharpe score")
      w <- setNames(numeric(length(spp)), spp)
      w[present] <- as.numeric(ef[j, present, drop = TRUE])
      if (any(!is.finite(w)) || min(w) < -1e-6 || abs(sum(w)-1) > 1e-5)
        stop("Selected frontier row has invalid weights")
      w <- pmax(w, 0); w <- w/sum(w)
      estimated_return <- sum(w * colMeans(X))
      if (abs(estimated_return - ef$return[j]) > 1e-4 * max(1, abs(ef$return[j])))
        stop("Selected frontier weights do not achieve the reported return")
      list(status = "ok", message = "", weights = w, frontier_row = j,
           estimated_return = estimated_return, frontier_return = ef$return[j],
           frontier_max_return = max(ef$return), frontier_sharpe = ef$sharpe[j],
           target_return = if (type == "90%") target else NA_real_,
           target_gap = if (type == "90%") estimated_return-target else NA_real_)
    }, error = function(e) list(status = "solver_failure", message = conditionMessage(e)))), types)
  }, error = function(e) failed(conditionMessage(e)))
}

pt_covariance <- function(S, error, z_sd, z_cor, mode) {
  if (error == 0) return(list(S = S, repair = 0, relative_error = 0, correlation_rmse = 0))
  s <- sqrt(diag(S)); R <- cov2cor(S); candidate <- R
  if (mode != "sd_only") {
    lo <- lower.tri(R)
    candidate[lo] <- R[lo] + error * z_cor
    candidate[upper.tri(R)] <- t(candidate)[upper.tri(R)]
    fit <- Matrix::nearPD(candidate, corr = TRUE, eig.tol = 1e-10, posd.tol = 1e-10)
    if (!fit$converged) stop("Correlation projection did not converge")
    R <- as.matrix(fit$mat)
  }
  if (mode != "correlation_only") {
    log_sd <- sqrt(log1p(error^2))
    s <- s * exp(log_sd * z_sd - log_sd^2 / 2)
  }
  result <- R * outer(s, s)
  dimnames(result) <- dimnames(S)
  chol(result)
  list(S = result, repair = norm(R - candidate, "F"),
       relative_error = norm(result - S, "F") / norm(S, "F"),
       correlation_rmse = sqrt(mean((R[lower.tri(R)] - cov2cor(S)[lower.tri(R)])^2)))
}

pt_wilson <- function(k, n, confidence = .95) {
  z <- qnorm((1 + confidence) / 2); p <- k / n
  center <- (p + z^2 / (2*n)) / (1 + z^2/n)
  half <- z * sqrt(p*(1-p)/n + z^2/(4*n^2)) / (1 + z^2/n)
  c(lower = max(0, center-half), upper = min(1, center+half))
}

# Can also analyse an existing result$runs or read.csv(".../run_results.csv")
# without rerunning optimisation. Limits apply only to tested grid points.
summarise_bray_tolerance <- function(runs, bray_tolerance = .10,
                                    required_success = .90, confidence = .95,
                                    output_dir = NULL) {
  stopifnot(length(bray_tolerance) == 1, is.finite(bray_tolerance),
            bray_tolerance >= 0, bray_tolerance <= 1,
            length(required_success) == 1, required_success > 0, required_success < 1,
            length(confidence) == 1, confidence > 0, confidence < 1)
  runs <- as.data.frame(runs)
  key <- c("PortfolioType", "GrowthError", "CovarianceError")
  if (!all(c(key, "Replicate", "status", "bray") %in% names(runs)) || !nrow(runs))
    stop("Supply run_results.csv or result$runs, including replicate IDs, status and Bray-Curtis distances.")
  if (anyNA(runs[c(key, "Replicate", "status")]) || anyDuplicated(runs[c(key,"Replicate")]))
    stop("Missing IDs/status or duplicate portfolio/cell/replicate keys.")
  if (any(!is.finite(runs$GrowthError)) || any(!is.finite(runs$CovarianceError)) ||
      any(runs$GrowthError < 0) || any(runs$CovarianceError < 0)) stop("Invalid error levels.")
  valid <- runs$status == "ok" & is.finite(runs$bray)
  if (any(runs$bray[valid] < -1e-10 | runs$bray[valid] > 1+1e-10)) stop("Invalid Bray-Curtis distance.")
  runs$bray_pass <- valid & !is.na(runs$bray) & runs$bray <= bray_tolerance + 1e-10
  types <- unique(runs$PortfolioType)
  growth_levels <- sort(unique(runs$GrowthError)); covariance_levels <- sort(unique(runs$CovarianceError))
  if (!0 %in% growth_levels || !0 %in% covariance_levels) stop("Both axes must include zero.")
  groups <- split(seq_len(nrow(runs)), interaction(runs[key], drop = TRUE))
  cells <- do.call(rbind, lapply(groups, function(i) {
    x <- runs[i, ]; good <- valid[i]; k <- sum(x$bray_pass); n <- nrow(x)
    ci <- pt_wilson(k, n, confidence)
    # This experiment reuses its deterministic baseline at the origin.
    if (x$GrowthError[1] == 0 && x$CovarianceError[1] == 0 &&
        all(good) && all(abs(x$bray) <= 1e-10)) ci <- c(lower=1,upper=1)
    data.frame(x[1,key], n=n, valid=sum(good), unavailable=sum(!good),
      bray_tolerance=bray_tolerance, required_success=required_success, confidence=confidence,
      within_rate=k/n, within_lower=ci[1], within_upper=ci[2],
      exceedances=sum(good & !x$bray_pass),
      exceed_rate_valid=if (any(good)) sum(good & !x$bray_pass)/sum(good) else NA_real_,
      not_within_or_unavailable_rate=1-k/n,
      estimated_meets_tolerance=k/n >= required_success,
      supported_meets_tolerance=ci[1] >= required_success)
  }))
  rownames(cells) <- NULL
  if (nrow(cells) != length(types)*length(growth_levels)*length(covariance_levels))
    stop("A complete rectangular grid per portfolio type is required for frontier calculation.")
  # All lower-error cells must pass: do not select isolated high-error successes.
  limit <- function(type, fixed, axis, column) {
    levels <- if (axis == "GrowthError") growth_levels else covariance_levels
    ok <- vapply(levels, function(level) {
      ge <- if (axis == "GrowthError") level else fixed
      ce <- if (axis == "GrowthError") fixed else level
      z <- cells[cells$PortfolioType == type & cells$GrowthError <= ge & cells$CovarianceError <= ce, ]
      all(z[[column]])
    }, logical(1))
    if (any(ok)) max(levels[ok]) else NA_real_
  }
  growth_frontier <- do.call(rbind, lapply(types, function(type) do.call(rbind, lapply(covariance_levels, function(ce)
    data.frame(PortfolioType=type, CovarianceErrorUpTo=ce,
      GrowthErrorUpTo_estimate=limit(type,ce,"GrowthError","estimated_meets_tolerance"),
      GrowthErrorUpTo_supported=limit(type,ce,"GrowthError","supported_meets_tolerance"))))))
  covariance_frontier <- do.call(rbind, lapply(types, function(type) do.call(rbind, lapply(growth_levels, function(ge)
    data.frame(PortfolioType=type, GrowthErrorUpTo=ge,
      CovarianceErrorUpTo_estimate=limit(type,ge,"CovarianceError","estimated_meets_tolerance"),
      CovarianceErrorUpTo_supported=limit(type,ge,"CovarianceError","supported_meets_tolerance"))))))
  axis_limits <- merge(growth_frontier[growth_frontier$CovarianceErrorUpTo==0,
    c("PortfolioType","GrowthErrorUpTo_estimate","GrowthErrorUpTo_supported")],
    covariance_frontier[covariance_frontier$GrowthErrorUpTo==0,
    c("PortfolioType","CovarianceErrorUpTo_estimate","CovarianceErrorUpTo_supported")], by="PortfolioType")
  if (!is.null(output_dir)) {
    filenames <- c("bray_cell_summary.csv", "bray_tolerance_frontier.csv", "bray_covariance_frontier.csv", "bray_axis_limits.csv", "bray_README.txt")
    if (any(file.exists(file.path(output_dir,filenames)))) stop("Bray outputs already exist; choose a new output directory.")
    dir.create(output_dir, recursive=TRUE, showWarnings=FALSE)
    tables <- list(cells, growth_frontier, covariance_frontier, axis_limits)
    for (j in seq_along(tables)) write.csv(tables[[j]],file.path(output_dir,filenames[j]),row.names=FALSE)
    writeLines(c(
      paste("Bray-Curtis tolerance:",bray_tolerance,"; required within-tolerance probability:",required_success),
      paste("Pointwise Wilson confidence level:",confidence),
      "Within-rate uses all attempts. Failed/missing portfolios count as not within tolerance, not measured exceedances.",
      "exceed_rate_valid describes only successfully measured portfolios; unavailable counts are separate.",
      "Frontiers require every tested cell in the rectangle from zero to pass. No interpolation is performed.",
      "Axis limits hold the OTHER input error at zero; their two maxima are not necessarily jointly acceptable.",
      "NA means no qualifying rectangle. A limit at the largest tested level is a lower bound on tolerance; expand the grid.",
      "Supported limits use pointwise lower confidence bounds, not simultaneous coverage over the grid.",
      "GrowthError is species-mean error SD relative to its configured scale; CovarianceError retains the source run's mode and units.",
      "This allocation criterion is separate from growth/risk performance criteria."
    ),file.path(output_dir,"bray_README.txt"))
  }
  invisible(list(cell_summary=cells, tolerance_frontier=growth_frontier,
                 covariance_frontier=covariance_frontier, axis_limits=axis_limits))
}

run_practical_tolerance <- function(
    returns, sigma2, growth_levels = seq(0, .20, .02),
    covariance_levels = seq(0, .20, .02), n_rep = 300, seed = 20261030,
    growth_loss_tolerance = .05, risk_increase_tolerance = .10,
    required_success = .95, confidence = .95, return_fraction = .90,
    covariance_mode = c("both", "sd_only", "correlation_only"),
    growth_error_scale = NULL, minTot = .01, optimisation_file = .pt_optimisation_file,
    output_dir = file.path(getwd(), paste0("PracticalTolerance_", format(Sys.time(), "%Y%m%d_%H%M%S"))),
    bray_tolerance = .10) {
  stopifnot(length(bray_tolerance)==1, is.finite(bray_tolerance), bray_tolerance>=0, bray_tolerance<=1)
  covariance_mode <- match.arg(covariance_mode)
  if (!all(vapply(c("data.table", "nloptr", "Matrix"), requireNamespace, logical(1), quietly = TRUE)))
    stop("Install packages data.table, nloptr and Matrix first.")
  # The original source uses unqualified data.table and nloptr functions.
  suppressPackageStartupMessages(library(data.table))
  suppressPackageStartupMessages(library(nloptr))
  if (!file.exists(optimisation_file)) stop("Cannot find optimisation.R; supply optimisation_file explicitly.")
  optimiser_env <- new.env(parent = globalenv())
  sys.source(optimisation_file, envir = optimiser_env)
  if (!exists("optimise_portfolio", envir = optimiser_env, inherits = FALSE))
    stop("optimisation.R does not define optimise_portfolio()")
  optimiser <- optimiser_env$optimise_portfolio
  X <- as.matrix(returns); S <- as.matrix(sigma2)
  if (!is.numeric(X) || ncol(X) < 2 || nrow(X) < 1 || any(!is.finite(X)))
    stop("returns must contain finite numeric species columns only.")
  spp <- colnames(X)
  if (is.null(spp) || anyDuplicated(spp) || any(!nzchar(spp))) stop("Unique species names required.")
  if (!is.numeric(S) || any(!is.finite(S)) || nrow(S) != length(spp) || ncol(S) != length(spp) ||
      is.null(rownames(S)) || is.null(colnames(S)) || anyDuplicated(rownames(S)) ||
      anyDuplicated(colnames(S)) || !setequal(rownames(S), spp) || !setequal(colnames(S), spp))
    stop("sigma2 must be finite, square, and named for the species in returns.")
  S <- S[spp, spp, drop = FALSE]
  if (max(abs(S-t(S))) > 1e-10 * max(abs(S))) stop("Baseline sigma2 is asymmetric.")
  S <- (S+t(S))/2
  if (min(eigen(S, symmetric = TRUE, only.values = TRUE)$values) <= 0)
    stop("Baseline sigma2 must be positive definite; resolve its singularity explicitly.")
  mu <- colMeans(X)
  if (is.null(growth_error_scale)) growth_error_scale <- abs(mu)
  if (!is.null(names(growth_error_scale))) {
    if (anyDuplicated(names(growth_error_scale)) || !setequal(names(growth_error_scale), spp))
      stop("Named growth_error_scale must match species.")
    growth_error_scale <- growth_error_scale[spp]
  }
  if (length(growth_error_scale) != length(spp) || any(!is.finite(growth_error_scale)) || any(growth_error_scale < 0))
    stop("growth_error_scale must supply a nonnegative finite scale per species.")
  if (any(growth_error_scale == 0)) warning("Some species receive no growth error; consider a meaningful absolute scale.")
  for (lev in list(growth_levels, covariance_levels)) {
    if (!length(lev) || any(!is.finite(lev)) || any(lev < 0) || !0 %in% lev || anyDuplicated(lev))
      stop("Grid levels must be unique nonnegative numbers and include zero.")
  }
  growth_levels <- sort(growth_levels); covariance_levels <- sort(covariance_levels)
  stopifnot(length(n_rep) == 1, is.finite(n_rep), n_rep >= 2, n_rep == as.integer(n_rep),
            length(seed) == 1, is.finite(seed),
            length(growth_loss_tolerance) == 1, is.finite(growth_loss_tolerance), growth_loss_tolerance >= 0,
            length(risk_increase_tolerance) == 1, is.finite(risk_increase_tolerance), risk_increase_tolerance >= 0,
            length(required_success) == 1, required_success > 0, required_success < 1,
            length(confidence) == 1, confidence > 0, confidence < 1,
            length(return_fraction) == 1, return_fraction == .90,
            length(minTot) == 1, is.finite(minTot), minTot >= 0, minTot < 1)
  types <- c("MaxSharpe", "90%", "MinVariance")
  baselines <- pt_select_frontier(X, S, optimiser, minTot, return_fraction)
  if (any(vapply(baselines, function(b) b$status != "ok", logical(1))))
    stop("Baseline optimisation failed: ", paste(vapply(baselines, function(b) b$message, character(1)), collapse = "; "))
  baseline <- do.call(rbind, lapply(types, function(type) {
    w <- baselines[[type]]$weights
    data.frame(PortfolioType = type, growth = sum(w*mu), risk = sqrt(drop(t(w)%*%S%*%w)))
  }))
  if (any(baseline$growth <= 0) || any(baseline$risk <= 0))
    stop("Relative performance tolerances require positive baseline growth and risk.")
  if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE, no.. = TRUE)))
    stop("output_dir is not empty; choose a new directory to preserve previous runs.")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) stop("Could not create output_dir.")
  settings <- as.list(environment())[c("growth_levels", "covariance_levels", "n_rep", "seed",
    "growth_loss_tolerance", "risk_increase_tolerance", "required_success", "confidence",
    "return_fraction", "minTot", "optimisation_file", "covariance_mode", "growth_error_scale", "bray_tolerance")]
  settings$optimiser_md5 <- unname(tools::md5sum(optimisation_file))
  settings$selection <- "Original frontier: max sharpe, min frontier_sd, closest to 90% of per-run max return"
  saveRDS(list(returns = X, covariance = S, settings = settings, baselines = baselines),
          file.path(output_dir, "inputs_and_settings.rds"))
  write.csv(baseline, file.path(output_dir, "baseline_performance.csv"), row.names = FALSE)
  capture.output(sessionInfo(), file = file.path(output_dir, "sessionInfo.txt"))
  # Common random draws across grid levels allow paired interaction comparisons.
  # Replicates remain independent. The baseline is fixed, not posterior uncertainty.
  set.seed(seed)
  grid <- expand.grid(GrowthError = growth_levels, CovarianceError = covariance_levels)
  rows <- vector("list", n_rep*nrow(grid)*length(types)); counter <- 0L
  for (replicate in seq_len(n_rep)) {
    z_growth <- rnorm(length(spp)); z_sd <- rnorm(length(spp)); z_cor <- rnorm(choose(length(spp), 2))
    for (g in seq_len(nrow(grid))) {
      ge <- grid$GrowthError[g]; ce <- grid$CovarianceError[g]
      estimated_X <- sweep(X, 2, ge * growth_error_scale * z_growth, "+")
      cv <- tryCatch(pt_covariance(S, ce, z_sd, z_cor, covariance_mode), error = identity)
      fits <- if (inherits(cv, "error")) setNames(lapply(types, function(type)
        list(status = "covariance_failure", message = conditionMessage(cv))), types) else
        if (ge == 0 && ce == 0) baselines else
          pt_select_frontier(estimated_X, cv$S, optimiser, minTot, return_fraction)
      for (type in types) {
        counter <- counter + 1L
        fit <- fits[[type]]
        row <- data.frame(Replicate = replicate, PortfolioType = type, GrowthError = ge,
          CovarianceError = ce, status = fit$status, message = fit$message,
          growth = NA_real_, risk = NA_real_, growth_loss = NA_real_, risk_increase = NA_real_,
          bray = NA_real_, pass = FALSE,
          frontier_row = NA_integer_, estimated_return = NA_real_, frontier_return = NA_real_,
          frontier_max_return = NA_real_, frontier_sharpe = NA_real_,
          target_return = NA_real_, target_gap = NA_real_,
          covariance_repair = if (inherits(cv, "error")) NA_real_ else cv$repair,
          covariance_relative_error = if (inherits(cv, "error")) NA_real_ else cv$relative_error,
          correlation_rmse = if (inherits(cv, "error")) NA_real_ else cv$correlation_rmse)
        weights <- rep(NA_real_, length(spp))
        if (fit$status == "ok") {
          weights <- fit$weights; b <- baseline[baseline$PortfolioType == type, ]
          row$growth <- sum(weights * mu)
          row$risk <- sqrt(drop(t(weights) %*% S %*% weights))
          row$growth_loss <- (b$growth - row$growth) / b$growth
          row$risk_increase <- (row$risk - b$risk) / b$risk
          row$bray <- sum(abs(weights - baselines[[type]]$weights)) / 2
          for (field in c("frontier_row", "estimated_return", "frontier_return", "frontier_max_return",
                          "frontier_sharpe", "target_return", "target_gap")) row[[field]] <- fit[[field]]
          row$pass <- row$growth_loss <= growth_loss_tolerance + 1e-10 &&
                      row$risk_increase <= risk_increase_tolerance + 1e-10
        }
        for (s in seq_along(spp)) row[[paste0("weight_", spp[s])]] <- weights[s]
        rows[[counter]] <- row
      }
    }
    if (replicate %% 10 == 0 || replicate == n_rep) message("Completed ", replicate, "/", n_rep, " replicates")
  }
  runs <- do.call(rbind, rows)
  runs$bray_pass <- runs$status == "ok" & is.finite(runs$bray) &
                    !is.na(runs$bray) & runs$bray <= bray_tolerance + 1e-10
  write.csv(runs, file.path(output_dir, "run_results.csv"), row.names = FALSE)
  bray_results <- summarise_bray_tolerance(runs, bray_tolerance, required_success, confidence, output_dir)
  safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  safe_q90 <- function(x) if (all(is.na(x))) NA_real_ else unname(quantile(x, .9, na.rm = TRUE))
  groups <- split(seq_len(nrow(runs)), interaction(runs$PortfolioType, runs$GrowthError, runs$CovarianceError))
  summary <- do.call(rbind, lapply(groups, function(i) {
    x <- runs[i, ]; k <- sum(x$pass); ci <- pt_wilson(k, nrow(x), confidence)
    if (x$GrowthError[1] == 0 && x$CovarianceError[1] == 0) ci <- c(lower = 1, upper = 1)
    # Interaction in failure probability: joint - growth-only - covariance-only + zero.
    control <- function(ge, ce) {
      z <- runs[runs$PortfolioType == x$PortfolioType[1] & runs$GrowthError == ge & runs$CovarianceError == ce, ]
      as.numeric(!z$pass[match(x$Replicate, z$Replicate)])
    }
    interaction <- as.numeric(!x$pass) - control(x$GrowthError[1], 0) - control(0, x$CovarianceError[1]) + control(0,0)
    data.frame(x[1, c("PortfolioType", "GrowthError", "CovarianceError")], n = nrow(x),
      valid = sum(x$status == "ok"), failures = sum(x$status != "ok"), success_rate = k/nrow(x),
      success_lower = ci[1], success_upper = ci[2],
      estimated_meets_tolerance = k/nrow(x) >= required_success,
      supported_meets_tolerance = ci[1] >= required_success,
      mean_bray = safe_mean(x$bray), q90_growth_loss = safe_q90(x$growth_loss),
      q90_risk_increase = safe_q90(x$risk_increase),
      mean_covariance_repair = safe_mean(x$covariance_repair),
      mean_realised_covariance_error = safe_mean(x$covariance_relative_error),
      failure_interaction = mean(interaction), interaction_se = sd(interaction)/sqrt(nrow(x)))
  }))
  rownames(summary) <- NULL
  # Largest tested rectangular region from zero whose cells all meet the criterion.
  # This avoids selecting an isolated favourable high-noise cell. It does not imply
  # success at untested levels, nor simultaneous statistical coverage across cells.
  frontier <- do.call(rbind, lapply(types, function(type) do.call(rbind, lapply(covariance_levels, function(ce) {
    max_level <- function(column) {
      ok <- vapply(growth_levels, function(ge) {
        z <- summary[summary$PortfolioType == type & summary$GrowthError <= ge & summary$CovarianceError <= ce, ]
        all(z[[column]])
      }, logical(1))
      if (any(ok)) max(growth_levels[ok]) else NA_real_
    }
    data.frame(PortfolioType = type, CovarianceErrorUpTo = ce,
      GrowthErrorUpTo_estimate = max_level("estimated_meets_tolerance"),
      GrowthErrorUpTo_supported = max_level("supported_meets_tolerance"))
  }))))
  write.csv(summary, file.path(output_dir, "cell_summary.csv"), row.names = FALSE)
  write.csv(frontier, file.path(output_dir, "tolerance_frontier.csv"), row.names = FALSE)
  writeLines(c(
    "Start with tolerance_frontier.csv: bounds are largest tested rectangles from zero.",
    "NA means no qualifying rectangle; a bound at the grid maximum means expand the grid.",
    "Supported uses pointwise Wilson lower bounds, not simultaneous coverage or a precise threshold CI.",
    "Zero-noise results reuse the deterministic baseline; their probability is exactly 1 conditional on that baseline.",
    "Failed runs count as unsuccessful; metric quantiles exclude unavailable values.",
    "Positive failure_interaction means joint errors fail more often than the sum of individual effects.",
    "Interaction SE uses paired replicate contrasts. Grid cells are not independent.",
    "CovarianceError means SD-multiplier CV and/or pre-projection correlation-error SD, depending on mode.",
    "Realised covariance errors and repairs are reported separately. Units differ from growth-error fractions.",
    "Success concerns growth/risk relative to matching baseline portfolios, using sqrt(w' S w) for risk.",
    "Optimisation uses optimisation.R unchanged, including its species removal and variance-based Sharpe score.",
    "90% selects the nearest frontier row to 0.9 times that run's maximum reported return; ties select the first row.",
    "target_return changes per run; target_gap is achieved estimated return minus that target (grid approximation).",
    "The original optimiser does not expose convergence codes; returned weights and achieved returns are checked.",
    "Results assume the baseline is correct and the specified error distribution is appropriate.",
    "Use independent validation simulations at a chosen boundary before making an accuracy requirement."
  ), file.path(output_dir, "README.txt"))
  if (length(growth_levels) > 1 && length(covariance_levels) > 1) {
    grDevices::pdf(file.path(output_dir, "tolerance_heatmaps.pdf"), width = 13, height = 5)
    tryCatch({
      for (metric in c("success_rate", "success_lower", "mean_bray")) {
        par(mfrow = c(1,3), mar = c(4.5,4.5,3,1), oma = c(0,0,3,0))
        for (type in types) {
          z <- summary[summary$PortfolioType == type, ]
          mat <- matrix(NA_real_, length(growth_levels), length(covariance_levels))
          mat[cbind(match(z$GrowthError, growth_levels), match(z$CovarianceError, covariance_levels))] <- z[[metric]]
          image(growth_levels, covariance_levels, mat, col = hcl.colors(100, "Viridis"), zlim = c(0,1),
                xlab = "Growth error (relative SD)", ylab = "Covariance error level", main = type)
          text(z$GrowthError, z$CovarianceError, ifelse(is.na(z[[metric]]), "NA", sprintf("%.2f", z[[metric]])), cex=.55)
        }
        mtext(paste(metric, "| Common scale: 0 (purple) to 1 (yellow)"), outer=TRUE, line=1)
      }
    }, finally = grDevices::dev.off())
  }
  message("Saved results to ", normalizePath(output_dir))
  invisible(list(runs = runs, summary = summary, tolerance_frontier = frontier,
                 bray = bray_results,
                 baseline = baseline, settings = settings, output_dir = output_dir))
}
