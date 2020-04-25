getColorVector <- function(input)
{
  colours_12 <- c("blue", "red", "black", "tan", "yellow", "green", "orange", "cyan", "gray", "purple", "pink", "brown")
  uniqueValues <- as.vector(unique(input))
  result <- vector(mode = "character", length = length(input))
  
  for(i in 1:length(uniqueValues))
  {
    result[which(input == uniqueValues[i])] <- colours_12[i]
  }
  
  return (result)
}