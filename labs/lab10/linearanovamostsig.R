linearanovamostsig <- function()
{
  source('~/GitHub/advstatistics-labs/labs/lab10/linearanova.R')
  period <- c(rep(2, 3), rep(86, 3), rep(128, 5))
  ls <- linearanova()
  minIndex <- order(ls$pValues)[1]
  cat("Minimum index:", minIndex)
  boxplot(ls$sourceData[minIndex,]~period, log = "y", names=c("2 days","86 days","128 days"), xlab="", ylab="relative abundance", main="Most Significant Gene Values for Reduced Model")
}