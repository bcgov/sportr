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

source("prep_data.R")
source("run_portfolio.R")
source("plot.R")
source("optimisation.R")

pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("BCGOV_DB"),
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

timePeriods <- TimePeriods <- c(1961,1991,2021,2041,2061)

gcm_weight <- data.table(gcm = c("ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5", "CNRM-ESM2-1", "EC-Earth3", 
                                 "GFDL-ESM4", "GISS-E2-1-G", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6", 
                                 "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"),
                         weight = c(1,0,0,1,1,1,1,0,0,1,1,1,0))

rcp_weight <- data.table(rcp = c("ssp126","ssp245","ssp370","ssp585"), 
                         weight = c(0.8,1,0.8,0))

all_weight <- as.data.table(expand.grid(gcm = gcm_weight$gcm,rcp = rcp_weight$rcp))
all_weight[gcm_weight,wgcm := i.weight, on = "gcm"]
all_weight[rcp_weight,wrcp := i.weight, on = "rcp"]
all_weight[,weight := wgcm*wrcp]

##run cciss
siteids <- c(6487982,6484391,6484900,6485410,6485920)
bgcDat <- dbGetCCISS(pool,siteids,avg = T, all_weight)
sspreds <- edatopicOverlap(bgcDat,E1, E1_Phase, onlyRegular = TRUE) ##reuse from uData$sspreds
SSPredOrig <- sspreds

suit_table <- copy(S1)

Trees <- unique(suit_table[ss_nospace %in% unique(sspreds$SS_NoSpace) & feasible %in% c(1,2,3),spp])
treeList <- Trees
boundDat <- data.table(Spp = Trees)
boundDat[,`:=`(minWt = 0, maxWt = 1)]
SuitProb <- data.frame("Suit" = c(1,2,3,4), "ProbDead" = c(0.1,0.5,1,4), "NoMort" = c(95,85,75,50))
SuitTable <- suit_table
setnames(SuitTable,old = c("feasible","spp","ss_nospace"),new = c("Suitability","Spp","SS_NoSpace"),skip_absent = T)


SSPredOrig[,allOverlap := NULL]
setnames(SSPredOrig, old = c("BGC","SiteRef"), new = c("MergedBGC","SiteNo"))
SSPredOrig <- SSPredOrig[,.(MergedBGC,SS_NoSpace,SSratio,SSprob,SS.pred,FuturePeriod,SiteNo)]

SSPredFull <- edatopicSubset(SSPredOrig,E1,pos = "Zonal")
nSpp <- length(treeList)
SSList <- unique(SSPredFull$SS_NoSpace)
selectBGC = SSList[1] 
SSPredAll <- SSPredFull
SiteList <- unique(SSPredAll$SiteNo)
SSPredAll <- SSPredAll[SiteNo %in% SiteList & !is.na(SSprob),]

SL <- SiteList
numTimes <- as.integer(25/length(SL))
SL <- rep(SL, each = numTimes)
returnValue <- 0.9
minAccept <- 0.1
port_results <- run_portfolio(SL,SSPredAll,SIBEC,SuitTable,
                              Trees,timePeriods,selectBGC,SuitProb,returnValue,
                              minAccept,boundDat)

colScale <- makeColScale(Trees, ccissdev::TreeCols)
ef_plot(port_results$raw, port_results$summary, colScale)



# siteLoc <- data.table(lon = c(-127.74817), lat = c(55.45829), elev = c(312), id = 1)
# FutScn <- "ssp245"
# period_end <- 2060
# point_dat <- data.table(lon = c(-127.74817), lat = c(55.45829), elev = c(312), id = 1)
# clim <- climr_downscale(point_dat, which_normal = "normal_composite", gcm_models = list_gcm()[c(1,5,7,10:12)],
#                         ssp = "ssp245", gcm_ts_years = 2020:2060, max_run = 4L, return_normal = FALSE, 
#                         vars = c("CMD","Tmin_sp","Tmax_sm"))
