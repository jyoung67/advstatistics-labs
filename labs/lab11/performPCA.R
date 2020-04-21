performPCA <- function()
{
  source('~/GitHub/advstatistics-labs/labs/lab11/getColorVector.R')
  source('~/GitHub/advstatistics-labs/labs/lab11/getPValues.R')
  inFileName <- "http://afodor.github.io/classes/stats2015/prePostPhylum.txt"
  
  # Read/transform data
  myT <-read.table(inFileName,header=TRUE,sep="\t")
  myTData<- data.matrix(myT[,5:10])
  myPCOA <- princomp(myTData)
  matSubAvg <- matrix(nrow = nrow(myTData), ncol = ncol(myTData))
  for(i in 1:ncol(matSubAvg))
  {
    matSubAvg[,i] <- mean(myTData[,i]) - myTData[,i]
  }
  
  # Generate color vectors
  cageColors <- getColorVector(as.vector(myT$cage))
  genotypeColors <- getColorVector(myT$genotype)
  timeColors <- getColorVector(myT$time)
  shapes <- ifelse(myT$time == "PRE", 0, 1)
  # Generate plots
  plot(myPCOA$scores[,1:2], col=cageColors, main = "PCA:  Cage", xlab="PCA1", ylab="PCA2", pch=shapes)
  legend(0, 2, legend=c("PRE", "POST"), col=c("blue", "red"), pch=c(0,1), cex=0.7,title="Time Period", text.font=7)
  #abline(a = 0, b = eigen(cov(matSubAvg))$vectors[2,2]/eigen(cov(matSubAvg))$vectors[1,2], col="black")
  plot(myPCOA$scores[,1:2], col=genotypeColors, main = "PCA:  Genotype ('WT'/'10-/-)'", xlab="PCA1", ylab="PCA2")
  abline(a = 0, b = eigen(cov(matSubAvg))$vectors[2,2]/eigen(cov(matSubAvg))$vectors[1,2], col="black")
  plot(myPCOA$scores[,1:2], col=timeColors, main = "PCA:  Time ('pre'/'post')", xlab="PCA1", ylab="PCA2")
  abline(a = 0, b = eigen(cov(matSubAvg))$vectors[2,1]/eigen(cov(matSubAvg))$vectors[1,1], col="green")
  
  # Calculate/display p-values
  getPValues(myPCOA, myT)
  
  return(list(rawData=myT, pcoaData=myPCOA, avgMat=matSubAvg, cageColors=cageColors, genotypeColors=genotypeColors,timeColors=timeColors))
}