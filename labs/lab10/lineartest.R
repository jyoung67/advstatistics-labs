lineartest <- function()
{
  source('~/GitHub/advstatistics-labs/labs/lab10/linearanova.R')
  periods <- c(rep(2, 3), rep(86, 3), rep(128, 5))
  
  ls <- linearanova()
  # Obtain minimum index
  minIndex <- order(ls$pValues)[1]
  cat("Minimum index:", minIndex)
  # Retrieve row with minimum index
  myData <- ls$sourceData[minIndex,]
  # Generate box plot
  boxplot(myData~periods,xlab="days", ylab="relative abundance", main="Most Sig Gene Values for Two-Parameter Model",   frame.plot=TRUE, axes = FALSE, xlim=c(0,128), at= c(2,86, 128), boxwex = 5)
  axis(1, at =c(2,86,128), labels = c(2, 86, 128))
  axis(2)
  
  # Generate linear model
  model <- lm(myData ~ periods, x = TRUE)
  # Add regression line to box plot
  abline(model)
  
  return (model)
 
}