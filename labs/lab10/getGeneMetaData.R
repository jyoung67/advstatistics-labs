getGeneMetaData <- function(indexes, modelNames)
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
  
  for (i in 1:length(indexes))
  {
    cat("Gene info for model type", modelNames[i], "for index", indexes[i], "====>", attr(myTNorm[indexes[i],], "row.names"), "\n")
  }
}