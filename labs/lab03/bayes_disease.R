bayes_disease <- function(emitLength, sampleLength, iterations) 
{
  # Example to run method
  # result <- bayes_disease(emitLength=75, sampleLength=10, iterations=10000)
  
  meanNumOfSteps <- vector(mode = "numeric", length = iterations)
  df <- data.frame(c(0.91, 0.09), c(0.16, 0.84))
  prior <- c(0.001, 0.999)
  # With Disease
  currentRun <- vector(mode = "numeric", length = sampleLength)
  for(i in 1:iterations)
  {
    for(j in 1:sampleLength)
    {
      emits <- replicate(emitLength, sample(c(1,2), 1, replace = TRUE, prob= c(df[1,1], df[1,2])))
      result <- run_bayes_sim(prior, df, emits)
      currentRun[j] <- which(result[,1] >= 0.9999)[1]
    }
    meanNumOfSteps[i] <- mean(currentRun)
  }
  
  return(meanNumOfSteps)
}


# write(result, "/Users/young/Documents/GitHub/advstatistics-labs/labs/lab03/result.txt", ncolumns = 1)
# dataFrm <- read.csv(file = "/Users/young/Documents/GitHub/advstatistics-labs/labs/lab03/result.txt", header = FALSE)
# vecData <- dataFrm[,1]
