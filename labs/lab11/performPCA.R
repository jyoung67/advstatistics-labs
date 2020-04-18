performPCA <- function()
{
  source('~/GitHub/advstatistics-labs/labs/lab11/getColorVector.R')
  source('~/GitHub/advstatistics-labs/labs/lab11/getPValues.R')
  inFileName <- "http://afodor.github.io/classes/stats2015/prePostPhylum.txt"
  
  myT <-read.table(inFileName,header=TRUE,sep="\t")
  myTData<- data.matrix(myT[,5:10])
  myPCOA <- princomp(myTData)
  
  matSubAvg <- matrix(nrow = nrow(myTData), ncol = ncol(myTData))
  for(i in 1:ncol(matSubAvg))
  {
    matSubAvg[,i] <- mean(myTData[,i]) - myTData[,i]
  }
  
  cageColors <- getColorVector(myT$cage)
  genotypeColors <- getColorVector(myT$genotype)
  timeColors <- getColorVector(myT$time)
  
  plot(myPCOA$scores[,1:2], col=cageColors, main = "PCA:  Cage Type", xlab="PCA1", ylab="PCA2")
  plot(myPCOA$scores[,1:2], col=genotypeColors, main = "PCA:  Genotype", xlab="PCA1", ylab="PCA2")
  plot(myPCOA$scores[,1:2], col=timeColors, main = "PCA:  Time", xlab="PCA1", ylab="PCA2")
  
  getPValues <- getPValues(myPCOA, myT)
  
 
  abline(a = 0, b = eigen(cov(matSubAvg))$vectors[2,1]/eigen(cov(matSubAvg))$vectors[1,1])
#  abline(a = 0, b = eigen(cov(matSubAvg))$vectors[2,2]/eigen(cov(matSubAvg))$vectors[1,2])
#  abline(a = 0, b = eigen(cov(matSubAvg))$vectors[2,3]/eigen(cov(matSubAvg))$vectors[1,3])
#  abline(a = 0, b = eigen(cov(matSubAvg))$vectors[2,4]/eigen(cov(matSubAvg))$vectors[1,4])
#  abline(a = 0, b = eigen(cov(matSubAvg))$vectors[2,5]/eigen(cov(matSubAvg))$vectors[1,5])
#  abline(a = 0, b = eigen(cov(matSubAvg))$vectors[2,6]/eigen(cov(matSubAvg))$vectors[1,6])
  
  
  
  return(list(rawData=myT, pcoaData=myPCOA, avgMat=matSubAvg, cageColors=cageColors, genotypeColors=genotypeColors,timeColors=timeColors))
}