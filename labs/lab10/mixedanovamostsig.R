mixedanovamostsig <- function()
{
  source('~/GitHub/advstatistics-labs/labs/lab10/mixedanova.R')
  period <- c(rep("day 2", 3), rep("week 12", 3), rep("week 18", 5))
  ls <- mixedanova()
  minIndex <- order(ls$pValues)[1]
  cat("Minimum index:", minIndex)
  boxplot(ls$sourceData[minIndex,]~period, log = "y",xlab="", ylab="relative abundance", main="Most Significant Gene Values for Model Comparisons")
}