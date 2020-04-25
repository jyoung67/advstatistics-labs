performPCA <- function()
{
  source('~/GitHub/advstatistics-labs/labs/lab12/getColorVector.R')
  inFileName <- "http://afodor.github.io/classes/stats2015/prePostPhylum.txt"
  
  # Read/transform data
  myT <-read.table(inFileName,header=TRUE,sep="\t")
  myT <- myT[myT$time == "POST",]
  myTData <- myT[,5:10]
  
  # Get principal components
  myPCOA <- princomp(myTData)
  
  # Generate color vectors
  cageColors <- getColorVector(as.vector(myT$cage))
  uniqueCageColumns <- as.vector(unique(myT$cage))
  uniqueCageColors <- unique(cageColors)
  for(ii in 1:length(uniqueCageColors))
  {
  cat("column|color: ", uniqueCageColumns[ii], "|", uniqueCageColors[ii], "\n" )
  }
  
  # Generate plot
  plot(myPCOA$scores[,1:2], col=cageColors, main = "PCA:  Cage", xlab="PCA1", ylab="PCA2")
  
  return (myPCOA)
}