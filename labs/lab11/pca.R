pca <- function()
{
  library(faraway)
  data(gala)
  myMat <- matrix(ncol = ncol(gala), nrow = nrow(gala))
  for (i in 1:ncol(myMat)) 
  { myMat[,i] <- gala[,i] - mean(gala[,i])
    # myMat[,i] <- mean(gala[,i]) - gala[,i] 
  }
  
  
  return (myMat)
  
}