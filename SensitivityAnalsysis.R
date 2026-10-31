### Sensitivity analysis

library(data.table)
library(ccissr)
library(cmdstanr)
library(climr)
library(nloptr)
require(magrittr)
require(ggplot2)

source("./optimisation.R")

###fully synthetic data
# spp_list <- c("Cw", "Pw", "Hw", "Py", "Fd", "Lw")
# boundDat <- data.table(Spp = names(returns))
# boundDat[,`:=`(minWt = 0, maxWt = 1)]
# 
# returns <- data.table(Cw = rep(1,80), Pw = rep(20,80), Hw = rep(40,80),
#                       Py = rep(60,80), Fd = rep(80,80), Lw = rep(100,80))
# returns <- data.table(Cw = rep(100,80), Pw = rep(50,80), Hw = rep(50,80),
#                       Py = rep(50,80), Fd = rep(50,80), Lw = rep(50,80))
# #returns <- returns[,lapply(.SD, function(x) x + rnorm(length(x), 2))]
# sigma <- matrix(data = rep(0, ncol(returns)^2), nrow = ncol(returns))
# diag(sigma) <- 1
# colnames(sigma) <- spp_list
# rownames(sigma) <- spp_list
# 
# sigma["Cw","Hw"] <- 0.2
# sigma["Hw","Cw"] <- 0.2
# 
# sigma["Cw","Fd"] <- -0.2
# sigma["Fd","Cw"] <- -0.2
# 
# ef <- optimise_portfolio(returns, sigma, boundDat, minTot = 0.01) 
# efAll <- melt(ef, id.vars = c("frontier_sd","return","sharpe"))
# RetCurve <- stats::approx(ef$return,ef$frontier_sd,xout = 90)
# ret90 <- RetCurve$y
# ggplot(efAll, aes(x = frontier_sd, y = value)) +
#   geom_area(aes(fill = variable, group = variable), size = 0.00001, col = "grey50", stat = "identity")+
#   geom_line(data = ef, aes(x = frontier_sd, y = return/50), colour = "black", linetype = "F1",linewidth = .75)+
#   scale_x_reverse() +
#   geom_vline(data = ef[sharpe == max(sharpe),], aes(xintercept = frontier_sd,colour = "MaxSharpe"), 
#              linetype = "twodash", size = .75) +
#   geom_vline(xintercept = ret90, linetype = "twodash", size = .75) +
#   xlab("Max Return --> Minimized Risk")+
#   ylab("Portfolio Ratio")+
#   guides(fill=guide_legend("Species"))
# 
# maxSharpe <- ef[sharpe == max(sharpe),]
# maxSharpe[,PortfolioType := "MaxSharpe"]
# ret90 <- ef[which.min(abs(return - 0.9*(max(return)))),]
# ret90[,PortfolioType := "90%"]
# minvar <- ef[which.min(frontier_sd),]
# minvar[,PortfolioType := "MinVariance"]
# dat <- rbind(maxSharpe, ret90, minvar)

###Baseline plus noise################################

##plot baseline portfolio
sigma2 <- fread("SensitivityCovariance_SBSmc2.csv") |> as.matrix()
rownames(sigma2) <- colnames(sigma2)
returns <- fread("SensitivityReturns_SBSmc2.csv")
boundDat <- data.table(Spp = names(returns))
boundDat[,`:=`(minWt = 0, maxWt = 1)]
ef <- optimise_portfolio(returns, sigma2, boundDat, minTot = 0.01) 
ef_long <- melt(ef, id.vars = c("frontier_sd","return","sharpe"))

RetCurve <- stats::approx(ef$return,ef$frontier_sd,xout = 0.9*max(ef$return))
ret90 <- RetCurve$y
ggplot(ef_long, aes(x = frontier_sd, y = value)) +
  geom_area(aes(fill = variable, group = variable), size = 0.00001, col = "grey50", stat = "identity")+
  geom_line(data = ef, aes(x = frontier_sd, y = return/max(return)), colour = "black", linetype = "F1",linewidth = .75)+
  scale_x_reverse() +
  geom_vline(data = ef[sharpe == max(sharpe),], aes(xintercept = frontier_sd,colour = "MaxSharpe"), 
             linetype = "twodash", size = .75) +
  geom_vline(xintercept = ret90, linetype = "twodash", size = .75) +
  xlab("Max Return --> Minimized Risk")+
  ylab("Portfolio Ratio")+
  guides(fill=guide_legend("Species"))



#######################Run Sensivity Analysis######################
source("PracticalToleranceAnalysis_whm.R")
result <- run_practical_tolerance(
  returns, sigma2,
  
  growth_levels = c(
    0, seq(0.02, 0.4, length.out = 10)
  ),
  
  covariance_levels = c(
    0, seq(0.02, 0.2, length.out = 10)
  ),
  
  covariance_mode = "correlation_only",
  n_rep = 50,
  seed = 20261031,
  
  growth_loss_tolerance = .10,
  risk_increase_tolerance = .10,
  required_success = .90,
  confidence = .95
)

result$bray$axis_limits
result$summary


###OLD CODE####
# Add Gaussian noise once per unique covariance entry, then project to a
# positive-definite matrix by flooring eigenvalues. CovNoise/20 remains the
# pre-projection entrywise SD; projection changes the realised noise distribution.
# Variances are perturbed too. This is a covariance, not a correlation, model.
# validate_covariance <- function(covariance) {
#   covariance <- as.matrix(covariance)
#   if (!is.numeric(covariance) || nrow(covariance) == 0L ||
#       nrow(covariance) != ncol(covariance) || any(!is.finite(covariance))) {
#     stop("Covariance must be a finite numeric square matrix.")
#   }
#   scale <- max(abs(covariance), .Machine$double.eps)
#   if (max(abs(covariance - t(covariance))) > 1e-10 * scale) {
#     stop("Baseline covariance is not symmetric; check sigma2.")
#   }
#   covariance <- (covariance + t(covariance)) / 2
#   if (min(eigen(covariance, symmetric = TRUE, only.values = TRUE)$values) < -1e-10 * scale) {
#     stop("Baseline covariance is not positive semidefinite; check sigma2.")
#   }
#   covariance
# }
# 
# perturb_covariance <- function(covariance, noise_sd) {
#   covariance <- validate_covariance(covariance)
#   if (length(noise_sd) != 1L || !is.finite(noise_sd) || noise_sd < 0) {
#     stop("noise_sd must be a single finite nonnegative number.")
#   }
#   if (noise_sd == 0) {
#     attr(covariance, "repair_frobenius") <- 0
#     return(covariance)
#   }
#   noise <- matrix(0, nrow(covariance), ncol(covariance))
#   lower <- lower.tri(noise, diag = TRUE)
#   noise[lower] <- rnorm(sum(lower), mean = 0, sd = noise_sd)
#   noise[upper.tri(noise)] <- t(noise)[upper.tri(noise)]
#   candidate <- covariance + noise
#   eig <- eigen(candidate, symmetric = TRUE)
#   # A small positive floor also supports optimisers requiring invertibility.
#   eigen_floor <- 1e-10 * max(abs(eig$values), .Machine$double.eps)
#   adjusted <- pmax(eig$values, eigen_floor)
#   result <- tcrossprod(sweep(eig$vectors, 2, sqrt(adjusted), `*`))
#   dimnames(result) <- dimnames(covariance)
#   if (min(eigen(result, symmetric = TRUE, only.values = TRUE)$values) <= 0) {
#     stop("Covariance repair failed numerically.")
#   }
#   attr(result, "repair_frobenius") <- norm(result - candidate, type = "F")
#   result
# }
# 
# # sigma2 is supplied by the calling analysis, as in the original script.
# # Check names/order before any optimisation so weights match covariance entries.
# sigma2 <- validate_covariance(sigma2)
# if (is.null(rownames(sigma2)) || is.null(colnames(sigma2)) ||
#     anyDuplicated(rownames(sigma2)) || anyDuplicated(colnames(sigma2)) ||
#     !setequal(rownames(sigma2), names(returns)) ||
#     !setequal(colnames(sigma2), names(returns))) {
#   stop("sigma2 must have row and column names matching the species in returns.")
# }
# sigma2 <- sigma2[names(returns), names(returns), drop = FALSE]
# 
# portree <- function(returns, covariance) {
#   boundDat <- data.table(Spp = names(returns))
#   boundDat[,`:=`(minWt = 0, maxWt = 1)]
#   ef <- optimise_portfolio(returns, covariance, boundDat, minTot = 0.01) 
#   
#   maxSharpe <- ef[sharpe == max(sharpe),]
#   maxSharpe[,PortfolioType := "MaxSharpe"]
#   ret90 <- ef[which.min(abs(return - 0.9*(max(return)))),]
#   ret90[,PortfolioType := "90%"]
#   minvar <- ef[which.min(frontier_sd),]
#   minvar[,PortfolioType := "MinVariance"]
#   dat <- rbind(maxSharpe, ret90, minvar)
#   return(dat)
# }
# 
# basecase <- portree(returns, sigma2)
# 
# combinations <- expand.grid(ReturnNoise = 0:10, CovNoise = 0:10)
# port_ls <- list()
# 
# for(it in 1:10){
#   cat(".")
#   for(ReturnNoise in 1:10){
#     for(CovNoise in 1:10) {
#       ret_new <- returns[,lapply(.SD, function(x) x + rnorm(nrow(returns), 0, ReturnNoise/2))]
#       cov_new <- perturb_covariance(sigma2, noise_sd = CovNoise/20)
#       res <- portree(ret_new, cov_new)
#       res[,RetNoise := ReturnNoise]
#       res[,CovNoise := CovNoise]
#       # Size of the PSD repair, separate from the requested Gaussian perturbation.
#       res[,CovRepairFrobenius := attr(cov_new, "repair_frobenius")]
#       port_ls[[paste0(ReturnNoise,"-",CovNoise, "-", it)]] <- res
#     }
#   }
# }
# 
# res_all <- rbindlist(port_ls, fill = TRUE, idcol = "iteration")
# res_all[is.na(res_all)] <- 0
# basecase <- basecase |> mutate(RetNoise = 0, CovNoise = 0, CovRepairFrobenius = 0, iteration = "base") |> dplyr::select(iteration, everything())
# res_all <- rbind(basecase, res_all, use.names = T)
# fwrite(res_all, "Sensitivity_Results.csv")
# 
