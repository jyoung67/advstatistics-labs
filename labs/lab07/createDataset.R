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
  
  # Test only
  #pValues <- c(0.0001, 0.0004, 0.0019, 0.0095, 0.0201, 0.0278, 0.0298, 
              # 0.0344, 0.0459, 0.3240, 0.4262, 0.5719, 0.6528, 0.7590, 1.000)
  #numRows <- length(pValues)
  
  threshold <- 0.05
  default_result <- sum(pValues <= threshold)
  # equivalent to:  p.adjust(pValues, method =  "bonferroni")
  Bonferroni_result <- sum(pValues <= threshold/numRows)
 
  # equivalent to:  p.adjust(pValues, method = "BH")
  adjustedPValues <- ((numRows*pValues)/rank(pValues))
  BHFDR_result <- sum(adjustedPValues <= threshold)

  print(paste("Default threshold:  # of significant values:", default_result))
  print(paste("Bonferroni adjusted threshold:  # of significant values:", Bonferroni_result))
  print(paste("BH FDR adjusted threshold:  # of significant values:", BHFDR_result))
  
  return (data.frame(raw=pValues, adjusted=adjustedPValues))
}