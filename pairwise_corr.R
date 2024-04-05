### pairwise correlation matrix where double negatives are removed

pairwise_corr <- function(feas, req_trees=NULL){
feas$newfeas2 <- ifelse(feas$newfeas %in% 3, 1, 
                        ifelse(feas$newfeas %in% 2, 2, 
                               ifelse(feas$newfeas %in% 1, 3, feas$newfeas )))
# treeSuitraw$All <- paste(treeSuitraw$Unit, treeSuitraw$Spp)
# treeSuitremove <- treeSuitraw[duplicated(treeSuitraw$All),]# remove duplicate entries 
# treeSuitnew <-treeSuitraw[!duplicated(treeSuitraw$All),]
#treeSuit <- treeSuitraw[,-c(5)]
# select <- c("Ba","Bl","Cw", "Fd", "Hw", "Lw", "Pl", "Ss", "Sx", "Yc")# select tree species to run in correlation
feas <- feas[feas$spp %in% req_trees]
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

return(mat)

}
