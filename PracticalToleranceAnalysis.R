# Practical input-accuracy experiment for tree-species portfolios.
# Usage (after loading your REAL returns and sigma2):
# source("PracticalToleranceAnalysis.R")
# result <- run_practical_tolerance(returns, sigma2)
# Quick pilot: result <- run_practical_tolerance(returns, sigma2, n_rep = 20)
# Requires quadprog and Matrix. install.packages("quadprog") if needed.
# Sourcing defines functions only: it does not overwrite inputs or run the grid.
#
# Defaults: Gaussian species-mean error SD = growth_error * abs(mean growth).
# Covariance error combines mean-one lognormal species-SD multipliers (CV =
# covariance_error) and additive correlation errors (SD = covariance_error).
# Use covariance_mode = "sd_only" or "correlation_only" to separate these.
# Correlations are projected with nearPD; realised error and repair are recorded.
# These assumptions describe errors in estimates, not process variability.
#
# Optimisation: long-only, fully invested, no minimum allocation or species-removal
# heuristic. MinVariance and FixedGrowth are exact convex QPs. MaxSharpe maximises
# mean growth / SD with zero reference return. These definitions differ from the
# old optimiser's (return - minimum-frontier-return) / variance score.
# FixedGrowth uses 90% of the baseline maximum species mean, unchanged in all runs.
# All performance is evaluated under the SAME baseline mean and covariance.
# If sigma2 is a correlation matrix, risk is an index in those units, not growth SD.
# Use a covariance in growth units when interpreting risk as growth uncertainty.

pt_solve <- function(mu, S, type, target) {
  n <- length(mu)
  if (type == "FixedGrowth" && max(mu) < target - 1e-10 * max(1, abs(target)))
    return(list(status = "infeasible_target", message = "Target exceeds maximum estimated growth"))
  if (type == "MaxSharpe" && max(mu) <= 0)
    return(list(status = "undefined_sharpe", message = "No positive expected growth"))
  tryCatch({
    # Scaling improves numerical conditioning without changing the optimum.
    D <- 2 * S / max(diag(S))
    if (type == "MaxSharpe") {
      A <- cbind(mu / max(abs(mu)), diag(n))
      b <- c(1, rep(0, n))
    } else {
      A <- cbind(rep(1, n), diag(n)); b <- c(1, rep(0, n))
      if (type == "FixedGrowth") {
        scale <- max(abs(mu), abs(target), .Machine$double.eps)
        A <- cbind(A, mu / scale); b <- c(b, target / scale)
      }
    }
    w <- quadprog::solve.QP(D, rep(0, n), A, b, meq = 1)$solution
    if (any(!is.finite(w)) || min(w) < -1e-7 || sum(w) <= 0)
      stop("Invalid optimiser weights")
    w <- pmax(w, 0); w <- w / sum(w)
    if (type == "FixedGrowth" && sum(w * mu) < target - 1e-7 * max(1, abs(target)))
      stop("Returned weights violate growth target")
    list(status = "ok", message = "", weights = w)
  }, error = function(e) list(status = "solver_failure", message = conditionMessage(e)))
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

run_practical_tolerance <- function(
    returns, sigma2, growth_levels = seq(0, .20, .02),
    covariance_levels = seq(0, .20, .02), n_rep = 300, seed = 20261030,
    growth_loss_tolerance = .05, risk_increase_tolerance = .10,
    required_success = .95, confidence = .95, fixed_target_fraction = .90,
    covariance_mode = c("both", "sd_only", "correlation_only"),
    growth_error_scale = NULL,
    output_dir = file.path(getwd(), paste0("PracticalTolerance_", format(Sys.time(), "%Y%m%d_%H%M%S")))) {
  covariance_mode <- match.arg(covariance_mode)
  if (!requireNamespace("quadprog", quietly = TRUE) || !requireNamespace("Matrix", quietly = TRUE))
    stop("Install packages quadprog and Matrix first.")
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
    stop("Baseline sigma2 must be positive definite for QP optimisation; resolve its singularity explicitly.")
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
            length(fixed_target_fraction) == 1, fixed_target_fraction > 0, fixed_target_fraction <= 1)
  target <- fixed_target_fraction * max(mu)
  types <- c("MaxSharpe", "FixedGrowth", "MinVariance")
  baselines <- setNames(lapply(types, function(type) pt_solve(mu, S, type, target)), types)
  if (any(vapply(baselines, function(b) b$status != "ok", logical(1)))) stop("Baseline optimisation failed.")
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
    "fixed_target_fraction", "target", "covariance_mode", "growth_error_scale")]
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
      estimated_mu <- mu + ge * growth_error_scale * z_growth
      cv <- tryCatch(pt_covariance(S, ce, z_sd, z_cor, covariance_mode), error = identity)
      for (type in types) {
        counter <- counter + 1L
        fit <- if (inherits(cv, "error")) list(status = "covariance_failure", message = conditionMessage(cv)) else
          if (ge == 0 && ce == 0) baselines[[type]] else pt_solve(estimated_mu, cv$S, type, target)
        row <- data.frame(Replicate = replicate, PortfolioType = type, GrowthError = ge,
          CovarianceError = ce, status = fit$status, message = fit$message,
          growth = NA_real_, risk = NA_real_, growth_loss = NA_real_, risk_increase = NA_real_,
          bray = NA_real_, target_shortfall = NA_real_, pass = FALSE,
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
          row$target_shortfall <- if (type == "FixedGrowth") max(0, target-row$growth) else NA_real_
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
  write.csv(runs, file.path(output_dir, "run_results.csv"), row.names = FALSE)
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
      valid = sum(x$status == "ok"), infeasible = sum(x$status == "infeasible_target"),
      failures = sum(!x$status %in% c("ok", "infeasible_target")), success_rate = k/nrow(x),
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
    "Failure and infeasible runs count as unsuccessful; metric quantiles exclude unavailable values.",
    "Positive failure_interaction means joint errors fail more often than the sum of individual effects.",
    "Interaction SE uses paired replicate contrasts. Grid cells are not independent.",
    "CovarianceError means SD-multiplier CV and/or pre-projection correlation-error SD, depending on mode.",
    "Realised covariance errors and repairs are reported separately. Units differ from growth-error fractions.",
    "Success concerns growth/risk relative to matching baseline portfolios; fixed-target shortfall is also recorded.",
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
                 baseline = baseline, settings = settings, output_dir = output_dir))
}
