getNormalizedData <- function()
{
  myT<-read.table("/Users/young/Documents/GitHub/advstatistics-labs/labs/lab10/nc101_scaff_dataCounts.txt",sep="\t",header=TRUE,row.names=1)
  myT <- myT[ apply( myT,1, median)> 5,]
  myTNorm <- myT
  myMat <- matrix(nrow=nrow(myT), ncol=ncol(myT))
  
  #for ( i in 1:ncol(myT))
  #{
    #colSum = sum(myT[,i])
    #myTNorm[,i] =myTNorm[,i]/colSum
  #}
  for ( i in 1:ncol(myT))
  {
    colSum = sum(myT[,i])
    myMat[,i] =myTNorm[,i]/colSum
  }
  
  threshold <- 0.05
  
  
  
  return (myMat)
}