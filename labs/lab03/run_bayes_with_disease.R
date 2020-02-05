run_bayes_with_disease <- function()
{
  result <- bayes_with_disease(maxEmitLength=40, iterations=1000)
  plot(1:40, result$powerValues, main = "Patient with Disease", xlab = "num of tests", ylab = "power value")
  abline(h = 0.94, v = 26)
  text(30, 0.6, "# of tests = 26")
  text(7, .97, "power = 0.95")
}

# write(result, "/Users/young/Documents/GitHub/advstatistics-labs/labs/lab03/result.txt", ncolumns = 1)
# dataFrm <- read.csv(file = "/Users/young/Documents/GitHub/advstatistics-labs/labs/lab03/result.txt", header = FALSE)
# vecData <- dataFrm[,1]