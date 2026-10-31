## Portfolio optimisation: shared direct solutions for selected and full modes.
## Requires data.table and nloptr. Existing positional arguments are retained.
## frontier_sd retains its historical meaning: VARIANCE (not standard deviation).
## sharpe retains (return - minimum-variance return) / variance.
##
## fast <- optimise_portfolio(returns, sigma2, boundDat, .01, mode = "selected")
## full <- optimise_portfolio(returns, sigma2, boundDat, .01, mode = "full")
## extract_portfolios(fast) and extract_portfolios(full) give the SAME two rows.
## Full mode adds the shared direct solutions to its plotting grid. It therefore
## has up to n_frontier + 2 rows; callers must not assume exactly 20 rows.
## Selection metadata are attributes so all original columns remain numeric.

optimise_portfolio <- function(returns, cov_matrix, boundDat, minTot,
                               mode = c("full", "selected"), n_frontier = 20L) {
  mode <- match.arg(mode)
  X <- as.matrix(returns); S <- as.matrix(cov_matrix)
  spp <- colnames(S)
  if (!is.numeric(X) || any(!is.finite(X)) || !is.numeric(S) || any(!is.finite(S)) ||
      is.null(spp) || anyDuplicated(spp) || nrow(S) != ncol(S) ||
      !identical(rownames(S), spp) || !setequal(colnames(X), spp))
    stop("Finite numeric returns and a square covariance with matching unique species names are required.")
  if (max(abs(S-t(S))) > 1e-10 * max(1,abs(S))) stop("Covariance must be symmetric.")
  S <- (S+t(S))/2
  if (min(eigen(S, symmetric=TRUE, only.values=TRUE)$values) <= 0)
    stop("Covariance must be positive definite.")
  if (length(minTot)!=1 || !is.finite(minTot) || minTot<0 || minTot>=1)
    stop("minTot must be in [0,1).")
  if (length(n_frontier)!=1 || !is.finite(n_frontier) || n_frontier<2 || n_frontier!=as.integer(n_frontier))
    stop("n_frontier must be an integer >= 2.")
  mu <- colMeans(X[,spp,drop=FALSE])
  bounds <- as.data.frame(boundDat)
  if (!all(c("Spp","minWt","maxWt") %in% names(bounds)) || anyDuplicated(bounds$Spp) ||
      !all(spp %in% bounds$Spp)) stop("Unique bounds required for every species.")
  bounds <- bounds[match(spp,bounds$Spp),,drop=FALSE]
  .check_bounds(bounds)
  # Retain the original low-return-end species-removal rule. Clamp its +0.5
  # probe to the feasible return range for narrow frontiers.
  repeat {
    minfit <- .minimum_variance(mu,S,bounds)
    low <- sum(mu*minfit$par)
    high_w <- .extreme_weights(mu,bounds,TRUE)
    high <- sum(mu*high_w)
    if (length(mu)==1 || minTot==0) break
    probe <- .efficient_return(mu,S,min(low+.5,high),bounds)
    if (!any(probe$par < minTot)) break
    keep <- probe$par > minTot
    if (!any(keep)) stop("minTot removal would remove every species.")
    mu <- mu[keep]; S <- S[keep,keep,drop=FALSE]
    bounds <- bounds[keep,,drop=FALSE]
    .check_bounds(bounds)
  }
  target90 <- .9*high
  feasible_low <- sum(mu*.extreme_weights(mu,bounds,FALSE))
  tolerance <- 1e-8*max(1,abs(high),abs(feasible_low))
  if (target90 < feasible_low-tolerance || target90 > high+tolerance)
    stop("90% of maximum return is outside the feasible return range.")
  maxfit <- .maximum_sharpe(mu,S,bounds,minfit$par,low)
  fit90 <- .efficient_return(mu,S,target90,bounds)
  fits <- list(maxfit,fit90)
  labels <- c("MaxSharpe","90%")
  if (mode=="full") {
    targets <- seq(low,high,length.out=n_frontier)
    fits <- c(fits,lapply(targets,function(target) .efficient_return(mu,S,target,bounds)))
  }
  W <- do.call(rbind,lapply(fits,function(fit) fit$par))
  result <- data.table::as.data.table(W)
  data.table::setnames(result,names(mu))
  variance <- rowSums((W %*% S)*W)
  growth <- drop(W %*% mu)
  result[, frontier_sd := variance]
  result[, `return` := growth]
  result[, sharpe := (growth-low)/variance]
  # Keep explicit selections: never infer them from the maximum of just two rows.
  selection_rows <- c(MaxSharpe=1L, "90%"=2L)
  if (mode=="full") {
    order_rows <- order(growth,seq_along(growth))
    result <- result[order_rows]
    selection_rows <- setNames(match(c(1L,2L),order_rows),labels)
    selection_rows <- c(selection_rows, MinVariance=which.min(result$frontier_sd))
  }
  attr(result,"selection_rows") <- selection_rows
  attr(result,"maximum_return") <- high
  attr(result,"target90") <- target90
  attr(result,"minimum_variance_return") <- low
  attr(result,"optimisation_mode") <- mode
  result
}

extract_portfolios <- function(frontier, types=c("MaxSharpe","90%")) {
  rows <- attr(frontier,"selection_rows")
  if (is.null(rows) || !all(types %in% names(rows))) stop("Requested selection metadata are unavailable.")
  result <- data.table::copy(frontier[unname(rows[types]),])
  result[, PortfolioType := types]
  result
}

.check_bounds <- function(bounds) {
  if (any(!is.finite(bounds$minWt)) || any(!is.finite(bounds$maxWt)) ||
      any(bounds$minWt<0) || any(bounds$maxWt>1) || any(bounds$minWt>bounds$maxWt) ||
      sum(bounds$minWt)>1+1e-10 || sum(bounds$maxWt)<1-1e-10)
    stop("Infeasible long-only bounds.")
}

.extreme_weights <- function(mu,bounds,maximum=TRUE) {
  w <- bounds$minWt; left <- 1-sum(w)
  for (i in order(mu,decreasing=maximum)) {
    add <- min(left,bounds$maxWt[i]-w[i]); w[i] <- w[i]+add; left <- left-add
  }
  if (left>1e-8) stop("Bounds cannot form a fully invested portfolio.")
  w
}

.checked_slsqp <- function(start,fn,gr,bounds,heq,heqjac) {
  fit <- nloptr::slsqp(x0=start, fn=fn, gr=gr, lower=bounds$minWt, upper=bounds$maxWt,
    heq=heq, heqjac=heqjac, control=list(xtol_rel=1e-10,maxeval=2000))
  if (fit$convergence<1 || fit$convergence>4 || any(!is.finite(fit$par)) ||
      max(abs(heq(fit$par)))>1e-6 || any(fit$par<bounds$minWt-1e-7) ||
      any(fit$par>bounds$maxWt+1e-7))
    stop("Optimiser failed convergence or feasibility checks: ",fit$message)
  fit
}

.minimum_variance <- function(mu,S,bounds) {
  start <- (.extreme_weights(mu,bounds,TRUE)+.extreme_weights(mu,bounds,FALSE))/2
  if (length(mu)==1) return(list(par=1,value=S[1,1]))
  scale <- max(diag(S)); A <- S/scale
  fit <- .checked_slsqp(start,function(w) drop(t(w)%*%A%*%w),function(w) 2*drop(A%*%w),
    bounds,function(w) sum(w)-1,function(w) matrix(1,1,length(w)))
  fit$value <- .portfolio_volatility(fit$par,mu,S)
  fit
}

.maximum_sharpe <- function(mu,S,bounds,start,reference_return) {
  excess <- mu-reference_return
  if (max(abs(excess))<1e-12) return(list(par=start,value=.portfolio_volatility(start,mu,S)))
  a <- excess/max(abs(excess)); A <- S/max(diag(S))
  fn <- function(w) -sum(a*w)/drop(t(w)%*%A%*%w)
  gr <- function(w) {
    v <- drop(t(w)%*%A%*%w); r <- sum(a*w)
    -(a*v-2*drop(A%*%w)*r)/v^2
  }
  fit <- .checked_slsqp(start,fn,gr,bounds,function(w) sum(w)-1,function(w) matrix(1,1,length(w)))
  fit$value <- .portfolio_volatility(fit$par,mu,S)
  fit
}

# Retain these helper signatures for existing callers.
.set_target <- function(mean_returns,cov_matrix) {
  bounds <- data.frame(minWt=rep(0,length(mean_returns)),maxWt=rep(1,length(mean_returns)))
  fit <- .minimum_variance(mean_returns,cov_matrix,bounds)
  seq(sum(fit$par*mean_returns),max(mean_returns),length.out=20)
}

.efficient_return <- function(mean_returns,cov_matrix,target_ret,bounds) {
  bounds <- as.data.frame(bounds)
  low <- .extreme_weights(mean_returns,bounds,FALSE); high <- .extreme_weights(mean_returns,bounds,TRUE)
  rlow <- sum(low*mean_returns); rhigh <- sum(high*mean_returns)
  tol <- 1e-8*max(1,abs(rlow),abs(rhigh))
  if (target_ret<rlow-tol || target_ret>rhigh+tol) stop("Infeasible target return.")
  if (abs(rhigh-rlow)<=1e-12*max(1,abs(rhigh))) return(.minimum_variance(mean_returns,cov_matrix,bounds))
  mix <- min(1,max(0,(target_ret-rlow)/(rhigh-rlow)))
  start <- (1-mix)*low+mix*high
  A <- cov_matrix/max(diag(cov_matrix)); scale <- max(abs(mean_returns),abs(target_ret),1)
  fit <- .checked_slsqp(start,function(w) drop(t(w)%*%A%*%w),function(w) 2*drop(A%*%w),bounds,
    function(w) c(sum(w)-1,(sum(w*mean_returns)-target_ret)/scale),
    function(w) rbind(rep(1,length(w)),mean_returns/scale))
  fit$value <- .portfolio_volatility(fit$par,mean_returns,cov_matrix)
  fit
}

.portfolio_volatility <- function(weights,mean_returns,cov_matrix) drop(t(weights)%*%cov_matrix%*%weights)
.portfolio_return <- function(weights,mean_returns) sum(weights*mean_returns)
