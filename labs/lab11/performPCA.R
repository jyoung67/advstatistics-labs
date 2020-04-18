performPCA <- function()
{
  source('~/GitHub/advstatistics-labs/labs/lab11/getColorVector.R')
  
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
  
  plot(myPCOA$scores[,1:2], col=cageColors, main = "PCA:  Cage Type")
  plot(myPCOA$scores[,1:2], col=genotypeColors, main = "PCA:  Genotype")
  plot(myPCOA$scores[,1:2], col=timeColors, main = "PCA:  Time")
  
  
  
  return(list(rawData=myT, pcoaData=myPCOA, avgMat=matSubAvg, cageColors=cageColors, genotypeColors=genotypeColors,timeColors=timeColors))
}