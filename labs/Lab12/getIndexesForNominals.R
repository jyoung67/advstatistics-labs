getIndexesForNominals <- function(inputVector)
{
  uniqueLabelVector <- unique(inputVector)
  indexVector <- vector(mode = "numeric", length = length(inputVector))
  for(i in 1:length(uniqueLabelVector))
  {
    indexVector[which(inputVector == uniqueLabelVector[i])] <- i
  }
  
  return(list(indexVector=indexVector, uniqueLabelVector=uniqueLabelVector))
}