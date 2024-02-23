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
