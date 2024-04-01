
addVarsOld <- function(dat){ ##this function modifies everything inplace, so no need for return value
  dat[,`:=`(PPT_MJ = PPT05+PPT06,
            PPT_JAS = PPT07+PPT08+PPT09,
            PPT.dormant = PPT_at+PPT_wt)]
  dat[,`:=`(CMD.def = 500-PPT.dormant)]
  dat[CMD.def < 0, CMD.def := 0]
  dat[,`:=`(CMDMax = CMD07,
            CMD.total = CMD.def + CMD)]
  dat[,`:=`(CMD.grow = CMD05+CMD06+CMD07+CMD08+CMD09,
            DD5.grow = DD5_05+DD5_06+DD5_07+DD5_08+DD5_09,
            #DDgood = DD5 - DD18,
            #DDnew = (DD5_05+DD5_06+DD5_07+DD5_08)-(DD18_05+DD18_06+DD18_07+DD18_08),
            TmaxJuly = Tmax07)]
}

addVars <- function(dat) {
  dat[, PPT_MJ := PPT05 + PPT06]
  dat[, PPT_JAS := PPT07 + PPT08 + PPT09]
  dat[, PPT.dormant := PPT_at + PPT_wt]
  dat[, CMD.def := pmax(0, 500 - PPT.dormant)]
  dat[, CMDMax := CMD07]   ## TODO: THIS IS NOT NECESSARILY CMD MAX
  dat[, CMD.total := CMD.def + CMD]
  dat[, DD_delayed := pmax(0, ((DD_0_at + DD_0_wt)*0.0238) - 1.8386)]
}


#' Clean Portfolio Data

#' @param SSPredAll cleanData parameter.
#' @param SIBEC cleanData parameter.
#' @param SuitTable cleanData parameter.
#' @param SNum cleanData parameter.
#' @param Trees cleanData parameter.
#' @param timePer cleanData parameter.
#' @param selectBGC cleanData parameter.
#' @details What the function does
#' @return What the function returns
#' @import data.table
#' @import foreach
#' @export
cleanData <- function(SSPredAll,SIBEC,SuitTable,SNum,Trees,timePer,selectBGC){
  
  # Declare binding for checks
  if (FALSE) {
    SiteNo <- 
      TreeSpp <- 
      FuturePeriod <- 
      SS_NoSpace <- 
      SS.pred <- 
      SSprob <- 
      Year <- 
      SS <- 
      Suitability <- 
      i.Suitability <- 
      MeanPlotSiteIndex <- 
      Spp <- 
      MeanSuit <- 
      MeanSI <- 
      NULL
  }
  
  SSPred <- SSPredAll[SiteNo == SNum,] ###subset
  
  ##Merge SIBEC data
  SIBEC <- SIBEC[TreeSpp %in% Trees,]
  SSPred <- SSPred[SSPred$FuturePeriod %in% timePer,]
  SSPred <- SSPred[,list(FuturePeriod, SS_NoSpace,SS.pred, SSprob)]
  SSPred <- SIBEC[SSPred, on = c(SS_NoSpace = "SS.pred")]
  setnames(SSPred, old = c("SS_NoSpace","i.SS_NoSpace"), new = c("SS.pred","SS_NoSpace"))
  
  ###Add rows for species with missing SI - mostly US units here
  add <- foreach(Year = unique(SSPred$FuturePeriod), .combine = rbind) %do%{
    byYear <- SSPred[SSPred$FuturePeriod == Year,]
    foreach(SS = unique(byYear$SS.pred), .combine = rbind) %do%{
      bySS <- byYear[byYear$SS.pred == SS,]
      missing <- Trees[!Trees %in% bySS$TreeSpp]
      new <- bySS[rep(1,length(missing)),]
      new$TreeSpp <- missing
      new
    }
  }
  if(!is.null(add)){
    if(nrow(add) > 0){
      add$MeanPlotSiteIndex <- 5 ##Set missing SI
      SSPred <- rbind(SSPred, add)
    }
  } 
  
  
  SSPred <- SSPred[!is.na(SSPred$TreeSpp),]
  setnames(SSPred,old = "TreeSpp", new = "Spp")
  
  ##Add suitability
  SSPred[SuitTable, Suitability := i.Suitability, on = c(SS.pred = "SS_NoSpace", "Spp")]
  SSPred$Suitability[is.na(SSPred$Suitability)] <- 5
  
  temp <- SIBEC[SS_NoSpace == selectBGC,]
  if(nrow(temp) == 0){
    return(NULL)
  }
  
  ##Summarise data- average SI and Suit weighted by SSProb
  SS.sum <- SSPred[,list(MeanSI = sum(MeanPlotSiteIndex*(SSprob/sum(SSprob))),
                         MeanSuit = round(sum(Suitability*(SSprob/sum(SSprob))), digits = 0)),
                   by = list(Spp,FuturePeriod)]
  
  SS.sum <- SS.sum[FuturePeriod %in% timePer,]
  ###not sure what we were doing here?
  SS.sum[MeanSuit == 4, MeanSI := 5]
  SS.sum[MeanSuit == 5, MeanSI := 5]
  SS.sum[MeanSuit == 5, MeanSuit := 4]
  SS.sum[MeanSI == 0, MeanSI := 5]
  SS.sum <- unique(SS.sum)
  setorder(SS.sum,Spp,FuturePeriod)
  return(SS.sum)
}

#' Portfolio Subset
#' @param SSPredOrig A parameter
#' @param eda A parameter
#' @param pos A parameter
#' @export
edatopicSubset <- function(SSPredOrig, eda, pos = "Zonal"){
  
  # Declare binding for checks
  if (FALSE) {
    Edatopic <- SS_NoSpace <- BGC_analysis <- NULL
  }
  SSPredOrig <- as.data.table(SSPredOrig)
  if(pos == "Zonal"){
    SSPredFull <- SSPredOrig[grep("01",SS_NoSpace),]
  }else{
    edaSub <- eda[Edatopic == pos,]
    SSPredFull <- SSPredOrig[SS_NoSpace %in% edaSub$SS_NoSpace,]
  }
  SSPredFull[,BGC_analysis := gsub("/.*","", SS_NoSpace)]
  SSPredFull <- SSPredFull[,c("MergedBGC", "SS.pred", "SSprob", "SS_NoSpace", 
                              "FuturePeriod", "SiteNo","BGC_analysis")]
  return(SSPredFull)
}
