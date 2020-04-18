performPCA <- function()
{
  
  inFileName <- "http://afodor.github.io/classes/stats2015/prePostPhylum.txt"
  
  myT <-read.table(inFileName,header=TRUE,sep="\t")
  myTData<- data.matrix(myT[,5:10])
  myPCOA <- princomp(myTData)
  
  matSubAvg <- matrix(nrow = nrow(myTData), ncol = ncol(myTData))
  for(i in 1:ncol(matSubAvg))
  {
    matSubAvg[,i] <- mean(myTData[,i]) - myTData[,i]
  }
  
  return(list(rawData=myT, pcoaData=myPCOA, avgMat=matSubAvg))
}

# colours <- c("black", "blue", "red", "brown", "yellow", "green", "cyan", "gray", "purple", "pink", "orange")