rollLoadedDie <- function(numOfRolls)
{
  rolls<- sample(1:6, numOfRolls, replace=TRUE, prob=c(.1,.1,.1,.1,.1,.5))
  return (rolls)
}