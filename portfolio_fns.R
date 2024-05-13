
make_covmat <- function(feas, trees_req){
  tempfeas <- feas
  tempfeas <- tempfeas[spp != "",]
  tempfeas <- tempfeas[!newfeas >3]
  # tempfeas$newfeas2 <- ifelse(tempfeas$newfeas %in% 3, 1, 
  #                         ifelse(tempfeas$newfeas %in% 2, 2, 
  #                                ifelse(tempfeas$newfeas %in% 1, 3, tempfeas$newfeas )))
  tempfeas <- dcast(tempfeas, ss_nospace ~ spp, value.var = "newfeas", fun.aggregate = mean, na.rm = T)
  tempfeas <- tempfeas[, lapply(.SD, function(x) replace(x, is.nan(x), 5))]
  tempfeas[,ss_nospace := NULL]
  tempfeas <- tempfeas[,..trees_req]
  simga_full <- cor(tempfeas, method = "kendall")#, use = "pairwise.complete.obs")
  return(simga_full)
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


get_clim <- function(location, years = 2023:2099){
  #vars_needed <- c("PPT05", "PPT06", "PPT07", "PPT08", "PPT09",
                  # "CMD", "PPT_at", "PPT_wt", "CMD07")
  #vars_needed <- c(vars_needed, vars) %>% unique
  clim_dat <- climr_downscale(location, gcm_models = list_gcm(), gcm_ts_years = years, 
                              ssp = "ssp245", max_run = 3L, return_normal = FALSE, vars = climr::list_variables())
  addVars(clim_dat)
  clim_dat <- clim_dat[!is.nan(CMD.total),]

  return(clim_dat)
}

prep_data <- function(clim_dat, BGCmodel, suit_table, eda_table, eda = "C4"){
  xvars <- BGCmodel[["forest"]][["independent.variable.names"]]
  pred_bgc <- predict(BGCmodel, data = clim_dat[,..xvars])
  bgc_dat <- clim_dat[,.(id, GCM, SSP, RUN, PERIOD)]
  bgc_dat[,BGC_Pred := pred_bgc$predictions]
  bgc_dat[,row_key := 1:nrow(bgc_dat)]
  setkey(bgc_dat, "BGC_Pred")
  
  eda_sub <- eda_table[Edatopic == eda,]
  eda_sub <- eda_sub[is.na(SpecialCode),] 
  eda_sub <- eda_sub[,.(BGC,SS_NoSpace)]
  
  setkey(eda_sub, "BGC")
  
  bgc_ss <- eda_sub[bgc_dat, allow.cartesian = T]
  bgc_ss[,run_id := interaction(id,GCM,SSP,RUN)]
  bgc_ss <- bgc_ss[,.(run_id,PERIOD,BGC,SS_NoSpace)]
  return(bgc_ss)
}


# tree = "Sx"  
# run="1.ACCESS-ESM1-5.ssp245.r10i1p1f1"
# suit_table = feas
run_portfolio <- function(bgc_ss, SIBEC, si_default = 5, suit_table, tree_ls, feas_prob, sigma = NULL){
  sim_ls <- list()
  portfolio_ls <- list()
  count <- 1
  run_ls <- unique(bgc_ss$run_id)
  for(run in run_ls){
    ss_run_orig <- bgc_ss[run_id == run,]
    setorder(ss_run_orig, PERIOD)
    cat(".")

    for(tree in tree_ls){
      si_spp <- SIBEC[TreeSpp == tree,]
      suit_spp <- suit_table[spp == tree,]
      ss_run <- copy(ss_run_orig)
      ss_run[si_spp, SI := i.MeanPlotSiteIndex, on = "SS_NoSpace"]
      #setnafill(ss_run, type = "locf",cols = "SI")
      setnafill(ss_run, type = "const", fill = si_default, cols = "SI")
      ss_run[suit_spp, Feas := i.newfeas, on = c(SS_NoSpace = "ss_nospace")]
      setnafill(ss_run, type = "const", fill = 4, cols = "Feas")
      ss_run[is.na(ss_run)] <- "unknown"
      ss_sum <- ss_run[,.(SI = mean(SI), Feas = round(mean(Feas))), by = .(PERIOD)]
      ss_sum[,FeasRoll := frollmean(Feas, n = 3)]
      ss_sum[,FeasDiff := c(NA,diff(Feas))]
      #ss_sum <- ss_sum[-c(1:3),]
      ss_sum[feas_prob, `:=`(Prop_Feas = i.PropLoss, NoMort = i.NoMort), on = "Feas"]
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
    use <- colnames(returns)[colMeans(returns) > stats::quantile(colMeans(returns),0.1)] ###removes species with very low returns should probably be higher
    if(length(use) > 1){
      returns <- returns[,..use]
      #print(use)
      if(is.null(sigma)){
        sigma2 <- cor(returns) ###to create cov mat from returns
      }else{
        sigma2 <- sigma[use,use]
      }
      
      #print(colnames(sigma2))
      ef <- optimise_portfolio(returns, sigma2, boundDat, minTot = 0.01) 
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
  efAll <- rbindlist(portfolio_ls)
  return(list(Simulation = sim_res, Portfolio = efAll, run_list = run_ls))
}

makeColScale <- function(Trees, TreeCols){
  cols <- TreeCols
  myPal <- cols$HexColour
  names(myPal) <- cols$TreeCode
  myColours <- data.table(TreeCode = Trees)
  myColours <- cols[myColours, on = "TreeCode"]
  myColours <- myColours[!is.na(HexColour),]
  pal <- myColours$HexColour
  names(pal) <- myColours$TreeCode
  colScale <- scale_fill_manual(name = "variable", values = pal)
  return(colScale)
}

get_portfolio <- function(portfolio_raw, run_list, 
                          what = c("Sharpe","Return","MaxSd","MinSd"), return_value = 0.9){
  raw2 <- dcast(portfolio_raw, SiteNo + Return ~ Spp)
  if(what == "Sharpe"){
    res <- raw2[raw2[,.I[Sharpe == max(Sharpe)], by=SiteNo]$V1]
    res[,c("Return","RealRet","Sd","Sharpe") := NULL]
    setnafill(res, type = "const", fill = 0, cols = 2:ncol(res))
    return(res)
  }else if(what == "Return"){
    raw2[,RetDiff := abs(return_value - RealRet)]
    res <- raw2[raw2[,.I[RetDiff == min(RetDiff)], by=SiteNo]$V1]
    res[,c("Return","RealRet","Sd","Sharpe","RetDiff") := NULL]
    setnafill(res, type = "const", fill = 0, cols = 2:ncol(res))
    return(res)
  }else if(what == "MaxSd"){
    res <- raw2[raw2[,.I[Sd == max(Sd)], by=SiteNo]$V1]
    res[,c("Return","RealRet","Sd","Sharpe") := NULL]
    setnafill(res, type = "const", fill = 0, cols = 2:ncol(res))
    return(res)
  }else if(what == "MinSd"){
    res <- raw2[raw2[,.I[Sd == min(Sd)], by=SiteNo]$V1]
    res[,c("Return","RealRet","Sd","Sharpe") := NULL]
    setnafill(res, type = "const", fill = 0, cols = 2:ncol(res))
    return(res)
  }else{
    stop("Unknown what. Please try again")
  }
}
#portfolio = portfolio1$Portfolio; run_list = run_ls; current_unit = current_unit; returnValue = 0.9

plot_ef <- function(portfolio, run_list, current_unit, returnValue = 0.9){
  portfolio <- portfolio[!is.infinite(value),]
  efAll <- dcast(portfolio,Return ~ Spp, fun.aggregate = function(x){sum(x)/(length(run_list))})
  efAll <- na.omit(efAll)
  RetCurve <- stats::approx(efAll$RealRet,efAll$Sd,xout = returnValue)
  ret90 <- RetCurve$y
  maxSharpe <- efAll[Sharpe == max(Sharpe),!c("Return","Sharpe")]
  maxSPos <- maxSharpe$Sd
  maxSharpe <- t(maxSharpe) %>% as.data.frame() %>%
    mutate(Spp = rownames(.)) %>% set_colnames(c("Sharpe_Opt","Spp"))
  ret90Props <- efAll[which.min(abs(RealRet - returnValue)),-c("Return","Sharpe")]
  ret90Props <- t(ret90Props) %>% as.data.frame() %>%
    mutate(Spp = rownames(.)) %>% set_colnames(c("Set_Return","Spp"))
  ret100Props <- efAll[which.min(abs(RealRet - 1)),-c("Return","Sharpe")]
  ret100Props <- t(ret100Props) %>% as.data.frame() %>%
    mutate(Spp = rownames(.)) %>% set_colnames(c("Max_Return","Spp"))
  maxSharpe$SSCurrent <- current_unit
  maxSharpe$Set_Return <- ret90Props$Set_Return
  maxSharpe$Set_Return[maxSharpe$Spp == "Sd"] <- ret90
  setDT(maxSharpe)
  efAll <- efAll[,-c("Return","Sharpe")]
  efAll <- melt(efAll, id.vars = "Sd")
  efAll$Unit <- current_unit
  efAll <- efAll[is.finite(Sd),]
  
  colScale <- makeColScale(unique(efAll[variable != "RealRet",variable]), ccissdev::TreeCols)
  efAll$variable <- factor(efAll$variable, levels = sort(unique(as.character(efAll$variable))))
  
  ggplot(efAll[efAll$variable != "RealRet",],aes(x = Sd, y = value,group = variable))+
    geom_area(aes(fill = variable), size = 0.00001, col = "grey50", stat = "identity")+
    colScale +
    geom_vline(data = maxSharpe[maxSharpe$Spp == "Sd",], aes(xintercept = Sharpe_Opt,colour = "blue"), 
               linetype = "twodash", size = .75)+
    geom_vline(data = maxSharpe[maxSharpe$Spp == "Sd",], aes(xintercept = Set_Return,colour = "grey52"),
               linetype = "dashed", size = .75)+
    geom_line(data = efAll[efAll$variable == "RealRet",], 
              aes(x = Sd, y = value,colour = "black"),linetype = "F1",linewidth = .75)+
    scale_colour_identity(name = "", guide = 'legend', labels = c("Return","MaxSharpe","SetReturn"))+
    scale_x_reverse() +
    xlab("Max Return --> Minimized Risk")+
    ylab("Portfolio Ratio")+
    guides(fill=guide_legend("Species"))+
    theme_few()#+
    #facet_wrap(.~Unit, scales = "free_x")
}

port_ratios <- function(portfolio, run_list, current_unit, returnValue = 0.9){
  efAll <- dcast(portfolio,Return ~ Spp, fun.aggregate = function(x){sum(x)/(length(run_list))})
  efAll <- na.omit(efAll)
  RetCurve <- stats::approx(efAll$RealRet,efAll$Sd,xout = returnValue)
  ret90 <- RetCurve$y
  RetCurve2 <- stats::approx(efAll$RealRet,efAll$Sd,xout = 1)
  ret100 <- RetCurve2$y
  maxSharpe <- efAll[Sharpe == max(Sharpe),!c("Return","Sharpe")]
  maxSPos <- maxSharpe$Sd
  maxSharpe <- t(maxSharpe) %>% as.data.frame() %>%
    mutate(Spp = rownames(.)) %>% set_colnames(c("Sharpe_Opt","Spp"))
  ret90Props <- efAll[which.min(abs(RealRet - returnValue)),-c("Return","Sharpe")]
  ret90Props <- t(ret90Props) %>% as.data.frame() %>%
    mutate(Spp = rownames(.)) %>% set_colnames(c("Set_Return","Spp"))
  ret100Props <- efAll[which.min(abs(RealRet - 1)),-c("Return","Sharpe")]
  ret100Props <- t(ret100Props) %>% as.data.frame() %>%
    mutate(Spp = rownames(.)) %>% set_colnames(c("Max_Return","Spp"))
  maxSharpe$SSCurrent <- current_unit
  maxSharpe$Set_Return <- ret90Props$Set_Return
  maxSharpe$Max_Return <- ret100Props$Max_Return
  maxSharpe$Sharpe_Opt <- as.numeric(maxSharpe$Sharpe_Opt)
  maxSharpe <- maxSharpe %>% 
      filter(!Spp %in% c("Sd", "RealRet")) %>% 
      mutate_if(is.numeric, round, 2)
  setDT(maxSharpe)
 setcolorder(maxSharpe, c("SSCurrent", "Spp", "Max_Return", "Set_Return", "Sharpe_Opt"))
 setorder(maxSharpe, -Max_Return)
  return(maxSharpe)
}

