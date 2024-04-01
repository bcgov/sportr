#' Run species Portfolio
#' @param SiteList A parameter
#' @param climVar A parameter
#' @param SSPredAll A parameter
#' @param SIBEC A parameter
#' @param SuitTable A parameter
#' @param Trees A parameter
#' @param TimePeriods A parameter
#' @param selectBGC A parameter
#' @param SuitProb A parameter
#' @param returnValue A parameter
#' @param sppLimits A parameter
#' @param minAccept A parameter
#' @param boundDat A parameter
#' @param ProbPest A parameter
#' @import foreach
#' @import scales
#' @import magrittr
#' @importFrom dplyr mutate 
#' @importFrom stats approx quantile
#' @export

run_portfolio <- function(SiteList,SSPredAll,SIBEC,SuitTable,Trees,
                          TimePeriods,selectBGC,SuitProb,returnValue,
                          minAccept,boundDat){
  
  cat("in portfolio")
  nSpp <- length(Trees)
  treeList <- Trees
  ss_sum_save <- data.table()
  simOut <- data.table()
  it = 0
  allSitesSpp <- foreach(SNum = SiteList, .combine = rbind) %do% {
    it = it+1
    ##simulate climate
    SS.sum <- cleanData(SSPredAll,SIBEC,SuitTable,SNum, Trees, 
                        timePer = TimePeriods,selectBGC = selectBGC)
    ss_sum_save <- rbind(ss_sum_save,SS.sum, fill = T)
    if(any(is.na(SS.sum$MeanSuit))){
      warning("Missing Suitability in unit ",
              BGC,", sitenumber ",SNum," for ",
              SS.sum$Spp[is.na(SS.sum$MeanSuit)], ": They will be filled with suit = 4")
      SS.sum$MeanSuit[is.na(SS.sum$MeanSuit)] <- 4
    }
    SS.sum[,FuturePeriod := as.numeric(FuturePeriod)]
    if(length(TimePeriods) == 1){
      temp <- SS.sum
      temp$FuturePeriod <- SS.sum$FuturePeriod[1]+85
      SS.sum <- rbind(SS.sum, temp)
    }
    
    if(!is.null(SS.sum)){
      cat(".")
      output <- data.table("year" = seq(2000,2100,1))
      
      for (k in 1:nSpp){ ##for each tree
        DatSpp <- SS.sum[Spp == treeList[k],]
        dat <- data.table("Period" = rescale(as.numeric(DatSpp$FuturePeriod), 
                                             to = c(2000,2085)), 
                          "SIBEC" = DatSpp$MeanSI/50, "Suit" = DatSpp$MeanSuit)
        
        dat <- merge(dat, SuitProb, by = "Suit")
        s <- stats::approx(dat$Period, dat$SIBEC, n = 101) ##Smooth SI
        p <- stats::approx(dat$Period, dat$ProbDead, n = 101) ###Smooth Prob Dead
        m <- stats::approx(dat$Period, dat$NoMort, n = 101) ##Smooth No Mort
        r <- stats::approx(dat$Period, dat$Suit, n = 101)
        ##r <- stats::approx(dat$Period, dat$RuinSeverity, n = 101)
        
        ###data frame of annual data
        annualDat <- data.table("Growth" = s[["y"]], "MeanDead" = p[["y"]], "NoMort" = m[["y"]], "Suit" = r[["y"]]) ##create working data
        Returns <- SimGrowth_Regular(DF = annualDat, climLoss = 0.005)
        tmpR <- c(0,Returns)
        assets <- Returns - tmpR[-length(tmpR)]
        temp <- data.frame(Spp = treeList[k], 
                           Year = 1:101, It = it, Returns = Returns)
        simOut <- rbind(simOut,temp,fill = T)
        output <- cbind(output, assets)
      } ## for each tree species
      
      colnames(output) <- c("Year", treeList)
      
      ####Portfolio#######################################
      returns <- output
      returns[,Year := NULL]
      ###only include species with mean return > 1 in portfolio
      use <- colnames(returns)[colMeans(returns) > stats::quantile(colMeans(returns),0.25)] ###should probably be higher
      use <- use[use %in% colnames(covMat)]
      if(length(use) > 1){
        returns <- returns[,..use]
        #print(use)
        #sigma2 <- cor(returns) ###to create cov mat from returns
        sigma2 <- covMat[use,use]
        #print(colnames(sigma2))
        ef <- optimise_portfolio(returns, sigma2, boundDat,minAccept) 
        setnames(ef,old = c("frontier_sd","return","sharpe"),
                 new = c("Sd","RealRet","Sharpe"))
        ef[,Return := 1:20]
        
        eff_front2 <- ef
        eff_front2[,RealRet := RealRet/max(RealRet)]
        eff_front2[,SiteNo := SNum]
        melt(eff_front2,id.vars = c("SiteNo", "Return"),variable.name = "Spp")
      }else NULL
    }else{NULL}
  }
  
  ##preprocess for plotting
  BGC <- selectBGC
  efAll <- allSitesSpp
  efAll <- dcast(efAll,Return ~ Spp, fun.aggregate = function(x){sum(x)/(length(SiteList))})
  efAll <- na.omit(efAll)
  #efAll$RealRet <- efAll$RealRet/max(efAll$RealRet) ##standardise return
  RetCurve <- stats::approx(efAll$RealRet,efAll$Sd,xout = returnValue)
  ret90 <- RetCurve$y
  maxSharpe <- efAll[Sharpe == max(Sharpe),!c("Return","Sharpe")]
  maxSPos <- maxSharpe$Sd
  maxSharpe <- t(maxSharpe) %>% as.data.frame() %>%
    mutate(Spp = rownames(.)) %>% set_colnames(c("Sharpe_Opt","Spp"))
  ret90Props <- efAll[which.min(abs(RealRet - returnValue)),-c("Return","Sharpe")]
  ret90Props <- t(ret90Props) %>% as.data.frame() %>%
    mutate(Spp = rownames(.)) %>% set_colnames(c("Set_Return","Spp"))
  maxSharpe$SSCurrent <- selectBGC
  maxSharpe$Unit <- selectBGC
  maxSharpe$Set_Return <- ret90Props$Set_Return
  maxSharpe$Set_Return[maxSharpe$Spp == "Sd"] <- ret90
  setDT(maxSharpe)
  efAll <- efAll[,-c("Return","Sharpe")]
  efAll <- melt(efAll, id.vars = "Sd")
  efAll$Unit <- BGC
  efAll <- efAll[is.finite(Sd),]
  #cat("return from port fun")
  return(list(raw = efAll,summary = maxSharpe,ssdat = ss_sum_save,simulated = simOut))
}
