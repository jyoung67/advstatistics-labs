performPCATest <- function()
{
  source('https://raw.githubusercontent.com/jyoung67/advstatistics-labs/master/labs/lab11/performPCA.R')
  
  # Invoke custom R function from the above source
  result <- performPCA()
  
  # Get principle results from source script for comparing with manually calculated values
  sourcePrincipleComponents <- round(result$pcoaData$scores, digits = 4)
  
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
  testPrincipleComponents <- round(matSubAvg %*% eigen(covMetaData)$vectors, digits = 4)
  
  # Calculate differences between sorucePrincipleComponents and testPrincipleComponents
  #  The expected result is zero differences
  differences <-  sum(!(-1*sourcePrincipleComponents == testPrincipleComponents || sourcePrincipleComponents == testPrincipleComponents ))
  
  cat("\nThere were ", differences, "difference(s) between sourcePrincipleComponents and testPrincipleComponents.")
  
  return(list(eigenVectors=eigen(covMetaData)$vectors, 
              eigenValues=eigen(covMetaData)$values, 
              principleComponents=testPrincipleComponents,
              sourcePrincipleComponents=sourcePrincipleComponents,
              pcoaSummary = summary(result$pcoaData)))
}