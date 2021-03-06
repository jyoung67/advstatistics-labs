onewayanova <- function(threshold=0.05)
{
  myT<-read.table("/Users/young/Documents/GitHub/advstatistics-labs/labs/lab10/nc101_scaff_dataCounts.txt",sep="\t",header=TRUE,row.names=1)
  myT <- myT[ apply( myT,1, median)> 5,]
  myTNorm <- myT
  numRows <- nrow(myT)
  numCols <- ncol(myT)
  myMat <- matrix(nrow=numRows, ncol=numCols)
  rawPvalues <- vector(mode = "numeric", length = numRows)
  
  for (i in 1:numCols)
  {
    colSum = sum(myT[,i])
    myMat[,i] =myTNorm[,i]/colSum
  }
   for ( i in 1:numRows)
  {
    D2 <- myMat[i, 1:3]; W12 <- myMat[i, 4:6]; W18 <- myMat[i, 7:11]
    data <- c(D2, W12, W18)
    period <- c(rep("day 2", length(D2)), rep("week 12", length(W12)), rep("week 18", length(W18)))
    myLm <- lm(data ~ factor(period), x=TRUE)
    rawPvalues[i] <- anova(myLm)$"Pr(>F)"[1]
   }
  cat("# of significant genes at BH FDR-corrected 0.05 threshold:", sum(p.adjust(rawPvalues, method =  "BH") < threshold), "\n")
  
  hist(rawPvalues, breaks = 50, main = "Raw P- values (three-parameter model)", xlab = "p-values")
  return (list(pValues=rawPvalues, sourceData=myMat))
}