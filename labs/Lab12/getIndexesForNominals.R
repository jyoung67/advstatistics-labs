getIndexesForNominals <- function(inputVector)
{
  uniqueLabelVector <- unique(inputVector)
  indexVector <- vector(mode = "numeric", length = length(inputVector))
  for(i in 1:length(uniqueLabelVector))
  {
    indexVector[which(inputVector == uniqueLabelVector[i])] <- i
  }
  
  trimmedUniqueLabelVector <- sapply(uniqueLabelVector, gsub, pattern = "Cage", replacement ="C")
  return(list(indexVector=indexVector, uniqueLabelVector=trimmedUniqueLabelVector))
}