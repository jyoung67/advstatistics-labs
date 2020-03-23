generateNormalizedData <- function()
{
  myT<-read.table("/Users/young/Documents/GitHub/advstatistics-labs/labs/lab08/nc101_scaff_dataCounts.txt",sep="\t",header=TRUE,row.names=1)
  myT <- myT[ apply( myT,1, median)> 5,]
  myTNorm <- myT
  
  for ( i in 1:ncol(myT))
  {
    colSum = sum(myT[,i])
    myTNorm[,i] =myTNorm[,i]/colSum
  }
  raw_pvalues_2_12 <- vector(mode = "numeric", length = nrow(myTNorm))
  raw_pvalues_2_20 <- vector(mode = "numeric", length = nrow(myTNorm))
  raw_pvalues_12_20 <- vector(mode = "numeric", length = nrow(myTNorm))
  
  
  for ( i in 1:nrow(myTNorm))
  {
    raw_pvalues_2_12[i] <- t.test(myTNorm[i,1:3], myTNorm[i,4:6])$p.value
  }
  for ( i in 1:nrow(myTNorm))
  {
    raw_pvalues_2_20[i] <- t.test(myTNorm[i,1:3], myTNorm[i,7:11])$p.value
  }
  for ( i in 1:nrow(myTNorm))
  {
    raw_pvalues_12_20[i] <- t.test(myTNorm[i,4:6], myTNorm[i,7:11])$p.value
  }
  
  return (data.frame(raw_pvalues_2_12, raw_pvalues_2_20, raw_pvalues_12_20))
}