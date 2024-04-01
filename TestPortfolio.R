##non-functionised portfolio testing script (likely won't run without tinkering)

library(terra)
library(data.table)
library(climr)
library(ccissdev)
library(sportr)
library(pool)
library(foreach)
library(scales)
library(nloptr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggthemes)


source("optimisation.R")

##make covar based on feasibility

fwrite(simga_full,"Feasibility_CovMat.csv", row.names = TRUE)

current_unit <- "IDFdk3/01"

vars_needed <- c("DD5", "DD_0_at", "DD_0_wt", "DD5_05", "DD5_06", "DD5_07", 
                 "DD5_08", "DD5_09", "PPT05", "PPT06", "PPT07", "PPT08", "PPT09", 
                 "CMD", "PPT_at", "PPT_wt", "CMD05", "CMD06", "CMD07", "CMD08", 
                 "CMD09", "SHM", "AHM", "NFFD", "PAS", "CMI", "Tmax07")
location <- data.table(lat = 52.23336,	lon = -122.53276, elev =	981, id = 1)
clim_dat <- climr_downscale(location, gcm_models = list_gcm(), gcm_ts_years = 2015:2080, 
                            ssp = "ssp245", max_run = 3L, return_normal = FALSE, vars = vars_needed)
addVars(clim_dat)

load("../Common_Files/BGCModel_Extratrees_Balanced.Rdata")
xvars <- BGCmodel[["forest"]][["independent.variable.names"]]
pred_bgc <- predict(BGCmodel, data = clim_dat[,..xvars])
bgc_dat <- clim_dat[,.(id, GCM, SSP, RUN, PERIOD)]
bgc_dat[,BGC_Pred := pred_bgc$predictions]
bgc_dat[,row_key := 1:nrow(bgc_dat)]
setkey(bgc_dat, "BGC_Pred")

suit_table <- copy(S1)

eda_table <- copy(E1)
eda <- "C4"
eda_sub <- eda_table[Edatopic == eda,]
eda_sub <- eda_sub[is.na(SpecialCode),] 
eda_sub <- eda_sub[,.(BGC,SS_NoSpace)]

setkey(eda_sub, "BGC")

bgc_ss <- eda_sub[bgc_dat, allow.cartesian = T]
bgc_ss[,run_id := interaction(id,GCM,SSP,RUN)]
bgc_ss <- bgc_ss[,.(run_id,PERIOD,BGC,SS_NoSpace)]
run_ls <- unique(bgc_ss$run_id)
#tree_ls <- unique(suit_table[ss_nospace %in% current_unit & feasible %in% c(1,2,3),spp]) ##fix

feas_prob <- data.table(Feas = c(1,2,3,4), PropLoss = c(0.1,0.5,2,5), "NoMort" = c(95,85,60,40))
change_prob <- data.table(FeasDiff = c(-3,-2,-1,0,1,2,3), PropLoss = c(0,0,0,0,1,1,1))

tree_ls <- c("Fd","Pl","Sx","At","Lw","Bl")
boundDat <- data.table(Spp = tree_ls)
boundDat[,`:=`(minWt = 0, maxWt = 1)]

sim_ls <- list()
portfolio_ls <- list()
count <- 1
for(run in run_ls){
  ss_run_orig <- bgc_ss[run_id == run,]
  setorder(ss_run_orig, PERIOD)
  cat(".")
  
  for(tree in tree_ls){
    si_spp <- SIBEC[TreeSpp == tree,]
    suit_spp <- suit_table[spp == tree,]
    ss_run <- copy(ss_run_orig)
    ss_run[si_spp, SI := i.MeanPlotSiteIndex, on = "SS_NoSpace"]
    setnafill(ss_run, type = "locf",cols = "SI")
    setnafill(ss_run, type = "const", fill = 15, cols = "SI")
    ss_run[suit_spp, Feas := i.newfeas, on = c(SS_NoSpace = "ss_nospace")]
    setnafill(ss_run, type = "const", fill = 4, cols = "Feas")
    ss_sum <- ss_run[,.(SI = mean(SI), Feas = round(mean(Feas))), by = .(PERIOD)]
    #ss_sum[SI > 20 & Feas == 4, Feas := 2] ##hack for dealing with missing
    ss_sum[,FeasRoll := frollmean(Feas, n = 3)]
    ss_sum[,FeasDiff := c(NA,diff(Feas))]
    ss_sum <- ss_sum[-c(1:3),]
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
    #sigma2 <- cor(returns) ###to create cov mat from returns
    sigma2 <- simga_full[use,use]
    #print(colnames(sigma2))
    ef <- optimise_portfolio(returns, sigma2, boundDat, minTot = 0.1) 
    setnames(ef,old = c("frontier_sd","return","sharpe"),
             new = c("Sd","RealRet","Sharpe"))
    ef[,Return := 1:20]
    
    eff_front2 <- ef
    eff_front2[,RealRet := RealRet/max(RealRet)]
    eff_front2[,SiteNo := run]
    portfolio_ls[[run]] <- melt(eff_front2,id.vars = c("SiteNo", "Return"),variable.name = "Spp")
  }
}

sim_res <- rbindlist(sim_ls)

ggplot(sim_res, aes(x = Year, y = Returns, col = Spp, group = interaction(It, Spp))) +
  geom_line()

efAll <- rbindlist(portfolio_ls)

##process for plotting
returnValue <- 0.9
efAll <- dcast(efAll,Return ~ Spp, fun.aggregate = function(x){sum(x)/(length(run_ls))})
efAll <- na.omit(efAll)
#efAll$RealRet <- efAll$RealRet/max(efAll$RealRet) ##standardise return
RetCurve <- stats::approx(efAll$RealRet,efAll$Sd,xout = 0.9)
ret90 <- RetCurve$y
maxSharpe <- efAll[Sharpe == max(Sharpe),!c("Return","Sharpe")]
maxSPos <- maxSharpe$Sd
maxSharpe <- t(maxSharpe) %>% as.data.frame() %>%
  mutate(Spp = rownames(.)) %>% set_colnames(c("Sharpe_Opt","Spp"))
ret90Props <- efAll[which.min(abs(RealRet - returnValue)),-c("Return","Sharpe")]
ret90Props <- t(ret90Props) %>% as.data.frame() %>%
  mutate(Spp = rownames(.)) %>% set_colnames(c("Set_Return","Spp"))
maxSharpe$SSCurrent <- current_unit
#maxSharpe$Unit <- selectBGC
maxSharpe$Set_Return <- ret90Props$Set_Return
maxSharpe$Set_Return[maxSharpe$Spp == "Sd"] <- ret90
setDT(maxSharpe)
efAll <- efAll[,-c("Return","Sharpe")]
efAll <- melt(efAll, id.vars = "Sd")
efAll$Unit <- current_unit
efAll <- efAll[is.finite(Sd),]

colScale <- makeColScale(tree_ls, ccissdev::TreeCols)
ef_plot(efAll, maxSharpe, colScale)



##############################################################

# Trees <- unique(suit_table[ss_nospace %in% unique(sspreds$SS_NoSpace) & feasible %in% c(1,2,3),spp])
# treeList <- Trees
# boundDat <- data.table(Spp = Trees)
# boundDat[,`:=`(minWt = 0, maxWt = 1)]
# 
# SuitTable <- suit_table
# setnames(SuitTable,old = c("feasible","spp","ss_nospace"),new = c("Suitability","Spp","SS_NoSpace"),skip_absent = T)
# 
# 
# SSPredOrig[,allOverlap := NULL]
# setnames(SSPredOrig, old = c("BGC","SiteRef"), new = c("MergedBGC","SiteNo"))
# SSPredOrig <- SSPredOrig[,.(MergedBGC,SS_NoSpace,SSratio,SSprob,SS.pred,FuturePeriod,SiteNo)]
# 
# SSPredFull <- edatopicSubset(SSPredOrig,E1,pos = "Zonal")
# nSpp <- length(treeList)
# SSList <- unique(SSPredFull$SS_NoSpace)
# selectBGC = SSList[1] 
# SSPredAll <- SSPredFull
# SiteList <- unique(SSPredAll$SiteNo)
# SSPredAll <- SSPredAll[SiteNo %in% SiteList & !is.na(SSprob),]
# 
# SL <- SiteList
# numTimes <- as.integer(25/length(SL))
# SL <- rep(SL, each = numTimes)
# returnValue <- 0.9
# minAccept <- 0.1
# port_results <- run_portfolio(SL,SSPredAll,SIBEC,SuitTable,
#                               Trees,timePeriods,selectBGC,SuitProb,returnValue,
#                               minAccept,boundDat)
# 
# 
# 


# siteLoc <- data.table(lon = c(-127.74817), lat = c(55.45829), elev = c(312), id = 1)
# FutScn <- "ssp245"
# period_end <- 2060
# point_dat <- data.table(lon = c(-127.74817), lat = c(55.45829), elev = c(312), id = 1)
# clim <- climr_downscale(point_dat, which_normal = "normal_composite", gcm_models = list_gcm()[c(1,5,7,10:12)],
#                         ssp = "ssp245", gcm_ts_years = 2020:2060, max_run = 4L, return_normal = FALSE, 
#                         vars = c("CMD","Tmin_sp","Tmax_sm"))

# pool <- dbPool(
#   drv = RPostgres::Postgres(),
#   dbname = Sys.getenv("BCGOV_DB"),
#   host = Sys.getenv("BCGOV_HOST"),
#   port = 5432, 
#   user = Sys.getenv("BCGOV_USR"),
#   password = Sys.getenv("BCGOV_PWD")
# )
# 
# site_ref_sql <- paste0("
#     WITH pts3005 AS (
#       ", paste0("SELECT st_transform(st_pointfromtext('POINT(", Long, " ", Lat, ")', 4326), 3005) geom", collapse = "\n UNION ALL \n") ,"
#     )
#     
#     SELECT cast(hgrid.siteno as text) site_no
#     FROM pts3005 pts 
#     LEFT JOIN hex_grid hgrid
#     ON ST_Intersects(pts.geom, hgrid.geom)
#   ")
# 
# 
# timePeriods <- TimePeriods <- c(1961,1991,2021,2041,2061)
# 
# gcm_weight <- data.table(gcm = c("ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5", "CNRM-ESM2-1", "EC-Earth3", 
#                                  "GFDL-ESM4", "GISS-E2-1-G", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6", 
#                                  "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"),
#                          weight = c(1,0,0,1,1,1,1,0,0,1,1,1,0))
# 
# rcp_weight <- data.table(rcp = c("ssp126","ssp245","ssp370","ssp585"), 
#                          weight = c(0.8,1,0.8,0))
# 
# all_weight <- as.data.table(expand.grid(gcm = gcm_weight$gcm,rcp = rcp_weight$rcp))
# all_weight[gcm_weight,wgcm := i.weight, on = "gcm"]
# all_weight[rcp_weight,wrcp := i.weight, on = "rcp"]
# all_weight[,weight := wgcm*wrcp]
# 
# ##run cciss
# siteids <- c(6487982,6484391,6484900,6485410,6485920)
# bgcDat <- dbGetCCISS(pool,siteids,avg = T, all_weight)
# sspreds <- edatopicOverlap(bgcDat,E1, E1_Phase, onlyRegular = TRUE) ##reuse from uData$sspreds
# SSPredOrig <- sspreds
