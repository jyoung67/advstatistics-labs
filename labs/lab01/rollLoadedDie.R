rollLoadedDie <- function(numOfRolls)
{
  rolls<- sample(1:6, numOfRolls, replace=TRUE, prob=c(.1,.1,.1,.1,.1,.5))
  hist(rolls, breaks = seq(.5,7,1), freq=F, main=paste(as.character(length(rolls)),"Die Rolls", collapse = " "), xlab = "Die Values")
  return (rolls)
}