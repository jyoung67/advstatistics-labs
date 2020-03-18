createDataset <- function()
{
  numRows = 3000
  numCols = 20
  mat <- matrix(nrow = numRows, ncol=numCols)
  
  for( i in 1: numCols)
  {
    for( j in 1:numRows)
    {
      aMean = j /10
      aMean = max( aMean,5)
      aVar = aMean+ 5* aMean 
      aVal = round( max( rnorm(1,mean=aMean,sd=sqrt(aVar)), 1))
      mat[j, i] = aVal
    }
  }
  pValues <- vector(mode="numeric", length = numRows)
  
  for(i in 1:numRows)
  {
    pValues[i] <- t.test(mat[i, 1:10], mat[i, 11:20])$p.value
  }
  
  threshold <- 0.05
  default_result <- sum(pValues <= threshold)
  Bonferroni_result <- sum(pValues <= threshold/numRows)
  BHFDR_result <- sum(((numRows*pValues)/order(pValues)) <= threshold)

  print(paste("Default threshold:  # of significant values:", default_result))
  print(paste("Bonferroni adjusted threshold:  # of significant values:", Bonferroni_result))
  print(paste("BH FDR adjusted threshold:  # of significant values:", BHFDR_result))
  
  return (pValues)
}