generateNormalizedData <- function()
{
  myT<-read.table("/Users/young/Documents/GitHub/advstatistics-labs/labs/lab10/nc101_scaff_dataCounts.txt",sep="\t",header=TRUE,row.names=1)
  
  # remove rare genes
  myT <- myT[ apply( myT,1, median)> 5,]
  
  myTNorm <- myT
  for ( i in 1:ncol(myT))
  {
    colSum = sum(myT[,i])
    myTNorm[,i] =myTNorm[,i]/colSum
  }
  return (myTNorm)
}