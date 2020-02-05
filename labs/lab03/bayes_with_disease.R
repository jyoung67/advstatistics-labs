bayes_with_disease <- function(maxEmitLength, iterations) 
{
  # Example to run method
  # result <- bayes_with_disease(maxEmitLength=50, iterations=10000)
  meanPostValues <- vector(mode = "numeric", length = maxEmitLength)
  powerValues <- vector(mode = "numeric", length = maxEmitLength)
  df <- data.frame(c(0.91, 0.09), c(0.16, 0.84))
  prior <- c(0.001, 0.999)
  # With Disease
  
  for(i in 1:maxEmitLength)
  {
    currentRun <- vector(mode = "numeric", length = iterations)
    for(j in 1:iterations)
    {
      emits <- replicate(i, sample(c(1,2), 1, replace = TRUE, prob= c(df[1,1], df[1,2])))
      result <- run_bayes_sim(prior, df, emits)
     
      currentRun[j] <- result[length(result[,1]), 1]
    }
    meanPostValues[i] <- mean(currentRun)
    powerValues[i] <- length(currentRun[currentRun >= 0.9999])/iterations
  }
  
  
  return(data.frame(meanPostValues, powerValues))
}


# write(result, "/Users/young/Documents/GitHub/advstatistics-labs/labs/lab03/result.txt", ncolumns = 1)
# dataFrm <- read.csv(file = "/Users/young/Documents/GitHub/advstatistics-labs/labs/lab03/result.txt", header = FALSE)
# vecData <- dataFrm[,1]
