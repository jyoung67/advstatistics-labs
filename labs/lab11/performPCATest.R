performPCATest <- function()
{
  source('https://raw.githubusercontent.com/jyoung67/advstatistics-labs/master/labs/lab11/performPCA.R')
  
  # Invoke custom R function from the above source
  result <- performPCA()
  
  # Get principle results from source script for comparing with manually calculated values
  sourcePrincipleComponents <- result$pcoaData$scores
  
  # Trim for only numerical columns
  myMat <- data.matrix(result$rawData[,5:10])
  
  matSubAvg <- matrix(nrow = nrow(myMat), ncol = ncol(myMat))
  
  # Populate matrix for column means subtracted from column values
  for(i in 1:ncol(matSubAvg))
    {   
      matSubAvg[,i] <- myMat[,i] - mean(myMat[,i])
  }
  
  # Generate covariance matrix
  covMetaData <- cov(matSubAvg)
  
  # Manually generate principal components matrix 
  testPrincipleComponents <- matSubAvg %*% eigen(covMetaData)$vectors
  
  # Calculate differences between sorucePrincipleComponents and testPrincipleComponents
  #  The expected result is zero differences
  differences <- sum(sourcePrincipleComponents == matSubAvg %*% eigen(covMetaData)$vector)
  
  cat("\nThere were ", differences, "between sourcePrincipleComponents and testPrincipleComponents.")
  
  return(list(eigenVectors=eigen(covMetaData)$vectors, 
              eigenValues=eigen(covMetaData)$values, 
              principleComponents=testPrincipleComponents,
              pcoaSummary = summary(result$pcoaData)))
}