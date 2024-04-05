##########Script for creation and evaluation of a correlation matrix from the Species Suitability matrix


.libPaths("E:/R packages351")
require(corpcor)
require(tseries)
require(tcltk)
require(pcaPP)
require(reshape2)
require(corrplot)
require(PerformanceAnalytics)
rm(list=ls())
wd=tk_choose.dir(); setwd(wd)

#treeSuit <- read.csv("TreeSuitabilitybySS_Reversed2.csv") # species by siteseries suitability matrix with n/As replaced with zeros and the suitability ranks reversed
treeSuitraw <- read.csv ("TreeSppSuit_v10.10.csv")
treeSuit$Suitability <- ifelse(treeSuit$Suitability %in% c('3'), 1, 
                           ifelse(treeSuit$Suitability %in% c('2'), 2, 
                             ifelse(treeSuit$Suitability %in% c('1'), 3,treeSuit$Suitability )))
treeSuitraw$All <- paste(treeSuitraw$Unit, treeSuitraw$Spp)
treeSuitremove <- treeSuitraw[duplicated(treeSuitraw$All),]# remove duplicate entries 
treeSuitnew <-treeSuitraw[!duplicated(treeSuitraw$All),]
treeSuit <- treeSuitraw[,-c(5)]
treeSuitMatrix <- dcast(treeSuit, Unit ~ Spp, mean)
treeSuitMatrix[is.na(treeSuitMatrix)] <- 0

write.csv(treeSuitMatrix, "TreeSuitability Matrix.csv")
##create correlation matrix
corr.spearman <- cor(treeSuitMatrix[,-1], method=c("spearman"))
##Graphical output1
corrplot(corr.spearman,type = "lower", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = .5, is.corr = TRUE)
##Graphical output correlations limited species
my_data <- corr.spearman[, c("Bl","Cw","Pl", "Lw", "Sx", "La")]
chart.Correlation(my_data, histogram = TRUE, pch = 19)
##Graphical heatmap
col <- colorRampPalette(c("darkblue", "white", "darkorange"))(10)
heatmap(x = corr.spearman, col = col, symm = TRUE)
write.csv(corr.spearman, "Spearman Correlation Matrix.csv")


###---------Testing alternate methods to remove effect of shared zero suitabilities------------------------
treeSuitMatrix <- read.csv("TreeSuitability Matrix.csv")
treeSuitMatrix <- treeSuitMatrix[,-1]

###########taken from stackoverflow but not necessarily what looking for.
#Getting the variable names from the data frame
av_variables<-variable.names(data.1)

#Creating a huge data frame for all possible combinations
corr_combinations <- as.data.frame(matrix(1,0,length(av_variables)))
for (i in 1:length(av_variables)){
  corr_combinations.i <- t(combn(av_variables,i))
  corr_combinations.new <- as.data.frame(matrix(1,length(corr_combinations.i[,1]),length(av_variables)))
  corr_combinations.new[,1:i] <- corr_combinations.i
  corr_combinations <- rbind(corr_combinations,corr_combinations.new)
  
  #How many combinations for 0:2 variables?
  comb_par_var<-choose(20, k=0:2)
  ##211
  
  #A new column to recieve the values
  corr_combinations$cor <- 0
  
  
  #Getting the correlations and assigning to the empty column
  for (i in (length(av_variables)+1):(length(av_variables)+ sum(comb_par_var) +1)){
    print(i/length(corr_combinations[,1]))
    corr_combinations$cor[i] <- max(as.dist(abs(cor(data.1[,as.character(corr_combinations[i,which(corr_combinations[i,]!=0&corr_combinations[i,]!=1)])]))))
    # combinations$cor[i] <- max(as.dist(abs(cor(data.0[,as.character(combinations[i,combinations[i,]!=0&combinations[i,]!=1])]))))
  }
  
  #Keeping only the rows with the combinations of 2 variables
  corr_combinations[1:(length(av_variables)+ sum(comb_par_var) +2),21]
  corr_combinations<-corr_combinations[1:212,]
  corr_combinations<-corr_combinations[21:210,]
  
  #Keeping only the columns var1, var2 and cor
  corr_combinations<-corr_combinations[,c(1,2,21)]
  
  #Ordering to keep only the pairs with correlation >0.95, 
  #which was my purpose the whole time
  corr_combinations <- corr_combinations[order(corr_combinations$cor),]
  corr_combinations<-corr_combinations[corr_combinations$cor >0.95, ] 
}




corr.test <- cor.shrink(treeSuitMatrix[,-1],lambda = 0)
corrplot(corr.test,type = "lower", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = .5, is.corr = TRUE)



corrplot(corr.spearman,type = "lower", tl.col = "black", tl.srt = 45, tl.cex = .5, is.corr = TRUE)

corrplot.mixed(corr.spearman, is.corr = TRUE, shade.col=NA, tl.col="black", tl.srt=45, tl.cex = .5,lower.col = "black", number.cex = .5)


##Creates spearman correlation and covarience matrices -- needs to be numeric


col <- colorRampPalette(c("darkblue", "white", "darkorange"))(20)
M <- cor(mtcars[1:7])
heatmap(x = M, col = col, symm = TRUE)

write.csv(corr.spearman, "Spearman Correlation Matrix.csv")



#_________________________________________________________________________________________________

#treeSuit$Suitability <- ifelse(treeSuit$Suitability %in% c('3'), 30, 
#                             ifelse(treeSuit$Suitability %in% c('2'), 70, 
#                             ifelse(treeSuit$Suitability %in% c('1'), 100,treeSuit$Suitability )))

###alternate cov or corr calculations - not used
covar.spearman <- cov(treeSuitMatrix[,-1], method=c("spearman"))
write.csv(covar.spearman, "Spearman Covariance Matrix.csv")
corr.paramet <- cor(treeSuitMatrix[,-1])
corr.kendall <- cor.fk(treeSuitMatrix[,-1], y=NULL) ##computes Kendall Correlation Matrix (non-parametric)
write.csv(corr.kendall, "Kendall Correlation Matrix.csv")
covar.pca <- covPCAproj(treeSuit[,-1]) ##computes robus covariance matrix (using principle components analysis)
covar.pca <- as.matrix(covar.pca)
write.csv(covar.pca, "PCA Covariance Matrix.csv")
#row.names(treeSuit) <- treeSuit$Row.Labels

write.csv(corr.paramet, "Parametric Correlation Matrix.csv")
cov.paramet <- cov(treeSuit[,-1])
write.csv(cov.paramet, "Parametric Covariance Matrix.csv")


############ Correlation Plot


#sigma <- read.csv("CovarianceMatrix_Full.csv")
sigma <- corr.spearman

#sigma2 <- round(matrix(runif(225, -100,100), 15))
#corrplot(sigma2, is.corr = FALSE, method = "square")

#corrplot(abs(sigma),order = "AOE", col = col3(200), cl.lim = c(-1, 1))


#corrplot.mixed(sigma,order =  "alphabet", shade.col=NA, tl.col="black", tl.srt=45, lower.col = "black", number.cex = .7)

