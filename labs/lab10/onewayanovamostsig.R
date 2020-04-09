onewayanovamostsig <- function()
{
  source('~/GitHub/advstatistics-labs/labs/lab10/onewayanova.R')
  period <- c(rep("day 2", 3), rep("week 12", 3), rep("week 18", 5))
  ls <- onewayanova()
  minIndex <- order(ls$pValues)[1]
  cat("Minimum index:", minIndex)
  boxplot(ls$sourceData[minIndex,]~period, log = "y",xlab="", ylab="relative abundance", main="Most Significant Gene Values for Full Model")
}