library(data.table)
library(ccissr)
library(cmdstanr)
library(climr)

BGCmodel <- readRDS("../Common_Files/WNA_BGCv13/BGCmodel_WNA_V4.2gini.rds")

current_unit <- "ICHxw/101" ##should lookup on map so this is automatic
location <- data.table(lat =  49.07449,	lon = -117.62451, elev =	468, id = 1)

current_unit <- "ICHdw1/101"
location <- data.table(lat =  49.10443,	lon = -117.78877, elev =	1088, id = 1)

clim_dat <- get_clim(location)
clim_si <- clim_dat[,.(id,GCM,SSP,RUN,PERIOD, CMD_an, DD5_an)]
clim_si[,run_id := interaction(id,GCM,SSP,RUN)]
clim_si <- na.omit(clim_si)
#clim_periods <- get_clim_periods(location)

bgc_ss <- prep_data(clim_dat, BGCmodel, copy(ccissr::S1), copy(ccissr::E1))
bgc_ss <- unique(bgc_ss)

feas_prob <- data.table(Feas = c(1,2,3,4), PropLoss = c(0.1,0.5,2,5), "NoMort" = c(95,85,60,40))

change_prob <- data.table(FeasDiff = c(-3,-2,-1,0,1,2,3), PropLoss = c(0,0,0,0,1,1,1))

tree_ls <- c("Fd","Pl","At","Lw", "Cw", "Py", "Dr", "Hw", "Pw", "Sx")
boundDat <- data.table(Spp = tree_ls)
boundDat[,`:=`(minWt = 0, maxWt = 1)]

##SIBayes models
source("../SIBayes/draw_si_sigmoid.R")
run_dir <- "../SIBayes/si_fits_quadratic_sigmoid/run_5fb023e0188b/"

fit_sigmoid <- list(
  fit      = readRDS(file.path(run_dir, "fit.rds")),
  prepared = readRDS(file.path(run_dir, "prepared.rds"))
)

suit_table <- copy(ccissr::S1)
bgc_ss <- na.omit(bgc_ss)
run_ls <- unique(bgc_ss$run_id) |> as.character()
sim_ls <- list()
portfolio_ls <- list()
return_ls <- list()
count <- 1
for(run in run_ls){
  ss_run_orig <- bgc_ss[run_id == run,]
  ss_run_orig[,PERIOD := as.integer(PERIOD)]
  setorder(ss_run_orig, PERIOD)
  clim <- clim_si[run_id == run,]
  setorder(clim, PERIOD)
  cat(".")
  
  for(tree in tree_ls){
    
    conditioning <- data.table(species = tree, bgc = "ICHdw1", DD5 = clim$DD5_an, CMD = clim$CMD_an, moisture = 4, nutrient = "C")
    si <- draw_si_sigmoid(
      fit_sigmoid,
      newdata = conditioning,
      n_draws = 50
    )
    si_mean <- colMeans(si)
    si_exp <- data.table(PERIOD = 2015:2099, SI = si_mean)
    suit_spp <- suit_table[spp == tree,]
    ss_run <- copy(ss_run_orig)
    ss_run[si_exp, SI := i.SI, on = "PERIOD"]
    ss_run[suit_spp, Feas := i.newfeas, on = c(SS_NoSpace = "ss_nospace")]
    setnafill(ss_run, type = "const", fill = 4, cols = "Feas")
    ss_sum <- ss_run[,.(SI = mean(SI), Feas = round(mean(Feas))), by = .(PERIOD)]
    ss_sum[,FeasRoll := frollmean(Feas, n = 3)]
    ss_sum[,FeasDiff := c(NA,diff(Feas))]
    ss_sum <- ss_sum[-c(1:3),]
    ss_sum[Feas > 4, Feas := 4]
    ss_sum[feas_prob, `:=`(Prop_Feas = i.PropLoss, NoMort = i.NoMort), on = "Feas"]
    #ss_sum[change_prob, Prop_Change := i.PropLoss, on = "FeasDiff"]
    ss_sum <- ss_sum[,.(SI = SI/50, Prop_Feas, FeasDiff, NoMort)]
    Returns <- simGrowthCpp(DF = ss_sum)
    tmpR <- c(0,Returns)
    assets <- Returns - tmpR[-length(tmpR)]
    temp <- data.table(Spp = tree, 
                       Year = 1:length(Returns), 
                       It = run, 
                       Returns = Returns)
    sim_ls[[count]] <- temp
    count <- count + 1
    if(tree == tree_ls[1]){
      tree_ass <- data.table(Year = 1:length(assets))
    }
    set(tree_ass, j = tree, value = assets)
  }
  returns <- tree_ass
  returns[,Year := NULL]
  ###only include species with mean return > 1 in portfolio
  use <- colnames(returns)[colMeans(returns) > stats::quantile(colMeans(returns),0.25)] ###should probably be higher
  if(length(use) > 1){
    returns <- returns[,..use]
    #print(use)
    sigma2 <- cor(returns) ###to create cov mat from returns
    sigma2 <- sigma[use,use]
    #print(colnames(sigma2))
    ef <- optimise_portfolio(returns, sigma2, boundDat, minTot = 0.01) 
    setnames(ef,old = c("frontier_sd","return","sharpe"),
             new = c("Sd","RealRet","Sharpe"))
    ef[,Return := 1:20]
    max_sd <- ef[20,RealRet]
    min_sd <- ef[1,RealRet]
    max_sharpe <- ef[which.min(Sharpe),RealRet]
    return_ls[[run]] <- data.table(MaxSD = max_sd, MinSD = min_sd, Sharpe = max_sharpe)
    
    eff_front2 <- ef
    eff_front2[,RealRet := RealRet/max(RealRet)]
    eff_front2[,SiteNo := run]
    portfolio_ls[[run]] <- melt(eff_front2,id.vars = c("SiteNo", "Return"),variable.name = "Spp")
  }
}

sim_res <- rbindlist(sim_ls, idcol = "runid")
ret_res <- rbindlist(return_ls, idcol = "runid")
efAll <- rbindlist(portfolio_ls, idcol = "runid")
port3 <- list(Simulation = sim_res, Portfolio = efAll, 
            Return_Values = ret_res, run_list = run_ls)

plot_ef(portfolio = port3$Portfolio, run_list = port3$run_list, current_unit = current_unit, returnValue = 0.9)
ggsave("Rossland_Portfolio_ICHdw1.png", height = 5, width = 7)

plot_ef(portfolio = port2$Portfolio, run_list = port2$run_list, current_unit = current_unit, returnValue = 0.9)
ggsave("Rossland_Portfolio_ICHxw.png", height = 5, width = 7)

ggplot(sim_res[It == "1.ACCESS-ESM1-5.ssp245.r2i1p1f1",], aes(x = Year, y = Returns, col = Spp, group = interaction(It, Spp))) +
  geom_line()

corrplot(temp, method = 'square', type="lower", order="alpha", diag = FALSE,tl.col="black", tl.srt=45,  col=brewer.pal(n=8, name="RdYlBu"))

x <- seq(0,5, by = 0.001)
png("ProbDistributions.png", width = 6, height = 5, units = "in", res = 300)
plot(x, dgamma(x, 1.5, scale = 0.1), type = "l", col = "forestgreen", lwd = 2, xaxt = "n", yaxt = "n", xlab = "Severity of Loss", ylab = "Probability")
lines(x, dgamma(x, 1.5, scale = 0.3), type = "l", lwd = 2, col = "orange")
lines(x, dgamma(x, 1.5, scale = 0.7), type = "l", lwd = 2, col = "firebrick")
dev.off()

get_clim <- function(location, years = 2015:2099){
  clim_dat <- downscale(location, gcms = list_gcms(), gcm_ssp_years = years, 
                              ssp = "ssp245", max_run = 3L, vars = list_vars())
  ccissr::addVars(clim_dat)
  clim_dat <- clim_dat[!is.nan(CMD.total),]
  return(clim_dat)
}
