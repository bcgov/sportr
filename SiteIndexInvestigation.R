##site index investigation
library(cmdstanr)
library(climr)
library(data.table)
library(ggplot2)

##SIBayes models
source("../SIBayes/draw_si_sigmoid.R")
run_dir <- "../SIBayes/si_fits_quadratic_sigmoid/run_5fb023e0188b/"

fit_sigmoid <- list(
  fit      = readRDS(file.path(run_dir, "fit.rds")),
  prepared = readRDS(file.path(run_dir, "prepared.rds"))
)

current_unit <- "ICHdw1/101"
location <- data.table(lat =  49.10443,	lon = -117.78877, elev =	1088, id = 1)

current_unit <- "SBSmc2/01"
location <- data.table(lat =  54.7797,	lon = -126.91428, elev =	896, id = 1)

current_unit <- "IDFxh1/101"
location <- data.table(lat =  50.23796,	lon = -119.22145, elev =	484, id = 1)

get_clim <- function(location, years = 2015:2099){
  clim_dat <- downscale(location, gcms = list_gcms(), gcm_ssp_years = years, 
                        ssp = "ssp245", max_run = 3L, vars = list_vars())
  ccissr::addVars(clim_dat)
  clim_dat <- clim_dat[!is.nan(CMD.total),]
  return(clim_dat)
}

clim_dat <- get_clim(location, years = 2015:2100)
clim_si <- clim_dat[,.(id,GCM,SSP,RUN,PERIOD, CMD_an, DD5_an)]
clim_si[,run_id := interaction(id,GCM,SSP,RUN)]
clim_si <- na.omit(clim_si)

clim_temp <- clim_si[run_id == "1.MPI-ESM1-2-HR.ssp245.r1i1p1f1",]

spp <- c("Fd", "Pl", "Lw", "Py", "At", "Cw", "Hw")

conditions <- clim_temp[
  , .(species = spp), by = .(PERIOD, DD5_an, CMD_an)
]
setnames(conditions, old = c("DD5_an", "CMD_an"), new = c("DD5", "CMD"))
conditions[, `:=`(
  bgc = "ICHdw1",
  moisture = 4,
  nutrient = "C"
)]

si <- draw_si_sigmoid(
  fit_sigmoid,
  newdata = conditions,
  n_draws = 100,
  type = "mean"
)

si <- colMeans(si)
trajectory <- copy(conditions)
#trajectory[, SI := as.numeric(si[1, ])]
trajectory[, SI := as.numeric(si)]

setorder(trajectory, species, PERIOD)
#t2 <- trajectory[,.(SI_stand = SI - mean(SI[1:10]), PERIOD = PERIOD), by = species]

temp <- dcast(trajectory, PERIOD ~ species, value.var = "SI")
setorder(temp, PERIOD)
temp[, PERIOD := NULL]
covmat <- cor(temp)

suit <- copy(ccissr::S1)
suit <- suit[spp %in% c("Fd", "Pl", "Lw", "Py", "At", "Cw", "Hw"),]
suit <- unique(suit)
smat <- dcast(suit, ss_nospace ~ spp, value.var = "newfeas", fun.aggregate = mean)
num_cols <- names(smat)[sapply(smat, is.numeric)]
setnafill(smat, fill = 5, cols = num_cols)
smat[,ss_nospace := NULL]

# R <- cor(
#   smat,
#   method = "spearman"
# )
library(psych)

R <- polychoric(
  smat
)$rho
sd_vec <- apply(
  smat,
  2,
  sd
)
sigma2 <- diag(sd_vec) %*%
  R %*%
  diag(sd_vec)


rownames(sigma2) <- names(smat)
colnames(sigma2) <- names(smat)

trajectory[,PERIOD := as.numeric(PERIOD)]
ggplot(trajectory, aes(x = PERIOD, y = SI, group = species, col = species)) +
  geom_line() +
  geom_smooth()

ggplot(t2, aes(x = PERIOD, y = SI_stand, group = species, col = species)) +
  geom_line() +
  geom_smooth()

## SI based portfolio
boundDat <- data.table(Spp = names(temp))
boundDat[,`:=`(minWt = 0, maxWt = 1)]
ef <- optimise_portfolio(temp, sigma2, boundDat, minTot = 0.01) 
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


#### Annual BGC and SIBEC (looks suspect) #############################
vars_needed <- c("CMD_sm", "DDsub0_sp", "DD5_sp", "Eref_sm", "Eref_sp", "EXT", 
                 "MWMT", "NFFD_sm", "NFFD_sp", "PAS_an", "PAS_sp", "SHM", "Tave_sm", "Tave_sp", "Tmax_sm", "Tmax_sp", "Tmin_an", "Tmin_at", "Tmin_sm", 
                 "Tmin_sp", "Tmin_wt","CMI_an", "PPT_05","PPT_06","PPT_07","PPT_08","PPT_09","PPT_at","PPT_wt","CMD_07","CMD_an"
)

clim <- downscale(location, gcm_ssp_years = 2015:2099, ssps = "ssp245", 
                  gcms = "MPI-ESM1-2-HR", run_nm = "r1i1p1f1", vars = vars_needed)
clim <- na.omit(clim)
ccissr::addVars(clim)

temp <- predict(BGCmodel, clim)
bgc_preds <- data.table(Year = clim$PERIOD, BGC = temp$predictions)
setkey(bgc_preds, "BGC")
eda <- copy(ccissr::E1)
eda <- eda[is.na(SpecialCode) & Edatopic == "C4",]
setkey(eda, "BGC")
eda <- unique(eda)

ss_preds <- eda[,.(BGC,SS_NoSpace)][bgc_preds]

spp <- c("Fd", "Pl", "Lw", "Py")
sibec <- copy(ccissr::SIBEC)
sibec <- sibec[TreeSpp %in% spp,]
sibec[SS_NoSpace == "IDFww/01", SS_NoSpace := "IDFww/101"]
sibec[SS_NoSpace == "CWHds1/01", SS_NoSpace := "CWHds1/101"]
si_preds <- sibec[ss_preds, on = "SS_NoSpace"]
setorder(si_preds, TreeSpp, Year)
si_preds <- na.omit(si_preds)
si_preds <- si_preds[,.(SI = mean(MeanPlotSiteIndex)), by = .(Year, TreeSpp)]
si_preds[,Year := as.numeric(Year)]

setnames(trajectory, old = c("PERIOD","species"), new = c("Year", "TreeSpp"))
trajectory[,Model := "Bayesian"]
si_preds[,Model := "SIBEC"]

allSI <- rbind(si_preds, trajectory[,.(Year,TreeSpp,SI,Model)])

ggplot(allSI, aes(x = Year, y = SI, group = TreeSpp, col = TreeSpp)) +
  geom_line() +
  geom_smooth() +
  facet_wrap(~Model)
