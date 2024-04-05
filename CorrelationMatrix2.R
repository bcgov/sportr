##########Script for creation and evaluation of a correlation matrix from the Species Suitability matrix
require(corpcor)
require(tseries)
require(tcltk)
require(pcaPP)
require(reshape2)
require(corrplot)
require(PerformanceAnalytics)
library(foreach)
require(labdsv)

#treeSuit <- read.csv("TreeSuitabilitybySS_Reversed2.csv") # species by siteseries suitability matrix with n/As replaced with zeros and the suitability ranks reversed
feas$newfeas2 <- ifelse(feas$newfeas %in% 3, 1, 
                           ifelse(feas$newfeas %in% 2, 2, 
                             ifelse(feas$newfeas %in% 1, 3, feas$newfeas )))
# treeSuitraw$All <- paste(treeSuitraw$Unit, treeSuitraw$Spp)
# treeSuitremove <- treeSuitraw[duplicated(treeSuitraw$All),]# remove duplicate entries 
# treeSuitnew <-treeSuitraw[!duplicated(treeSuitraw$All),]
#treeSuit <- treeSuitraw[,-c(5)]
# select <- c("Ba","Bl","Cw", "Fd", "Hw", "Lw", "Pl", "Ss", "Sx", "Yc")# select tree species to run in correlation
# feas2 <- feas[feas$spp %in% select,]
treeSuitMatrix <- dcast(feas, ss_nospace ~ spp, value.var = "newfeas2",mean)
treeSuitMatrix[is.na(treeSuitMatrix)] <- 0
treeSuitMatrix <- treeSuitMatrix[,-1] %>% as.matrix
####Kiri###################
# 
nonZero <- apply(treeSuitMatrix, 2, FUN = function(x){return(length(x[x != 0]))}) ###number of non-zero entries
 treeSuitMatrix <- treeSuitMatrix[,nonZero > 40] ###remove species with X or less entries
# len <- length(treeSuitMatrix)

###calculate pairwise correlations
out <- foreach(Spp1 = as.character(colnames(treeSuitMatrix)), .combine = rbind) %do% {
  #pos = grep(Spp1, colnames(treeSuitMatrix))
  names <- as.character(colnames(treeSuitMatrix))
  foreach(Spp2 = names, .combine = rbind) %do% {
    sub <- treeSuitMatrix[,c(Spp1,Spp2)]
    sub <- sub[rowSums(sub) != 0,] ### remove where both species are non-existant
    #sub[sub == 0] <- 5 ###replace 0 with 5
    correl <- cor(sub[,1],sub[,2], method="kendall")
    out <- data.frame(Spp1 = Spp1, Spp2 = Spp2, Value = correl)
    out
  }
}
out2 <- as.data.frame(out)
mat <- matrify(out2)
mat <- dcast(out, Spp1 ~ Spp2, value.var = "Value") ###convert to matrix
rownames(mat) <- mat$Spp1
mat <- mat[,-1]
mat <- as.matrix(mat)
png("PairwiseCorr.png", width = 6, height = 6, units = "in", res = 300)
corrplot(mat,type = "upper", order = "original", tl.col = "black", tl.srt = 45, tl.cex = .5, is.corr = FALSE)
dev.off()
#########End Kiri################

write.csv(mat, "TreeSuitability Matrix.csv")

####OLD

##create correlation matrix
corr.spearman <- cor(treeSuitMatrix[,c("Ba","Bl","Cw", "Fd", "Hw", "Lw", "Pl", "Ss", "Sx", "Yc")], method=c("spearman"))
##Graphical output1
corrplot(corr.spearman,type = "lower", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = .5, is.corr = TRUE)
##Graphical output correlations limited species
my_data <- corr.spearman[, c("Ba", "Bg", "Bl","Cw","Pl", "Lw", "Sx", "Yc")]
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

