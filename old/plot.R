#' Plot Efficient Frontier
#' @param efAll A parameter
#' @param intDat A parameter
#' @param colScale A parameter
#' @import ggplot2
#' @import ggthemes
#' @export
ef_plot <- function(efAll,intDat,colScale){
  
  # Declare binding for checks
  if (FALSE) {
    Sd <-
      value <-
      variable <-
      Sharpe_Opt <-
      Set_Return <-
      NULL
  }
  
  # efAll <- outAll$GraphDat
  # intDat <- outAll$MaxS
  efAll$variable <- factor(efAll$variable, levels = sort(unique(as.character(efAll$variable))))
  ggplot(efAll[efAll$variable != "RealRet",],aes(x = Sd, y = value,group = variable))+
    geom_area(aes(fill = variable), size = 0.00001, col = "grey50", stat = "identity")+
    colScale +
    geom_vline(data = intDat[intDat$Spp == "Sd",], aes(xintercept = Sharpe_Opt,colour = "blue"), 
               linetype = "twodash", size = .75)+
    geom_vline(data = intDat[intDat$Spp == "Sd",], aes(xintercept = Set_Return,colour = "grey52"),
               linetype = "dashed", size = .75)+
    geom_line(data = efAll[efAll$variable == "RealRet",], 
              aes(x = Sd, y = value,colour = "black"),linetype = "F1",linewidth = .75)+
    scale_colour_identity(name = "", guide = 'legend', labels = c("Return","MaxSharpe","SetReturn"))+
    scale_x_reverse() +
    xlab("Max Return --> Minimized Risk")+
    ylab("Portfolio Ratio")+
    guides(fill=guide_legend("Species"))+
    theme_few()+
    facet_wrap(.~Unit, scales = "free_x")
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
