bayes_without_disease_part_2 <- function(maxEmitLength, iterations) 
{
  # Example Function Call
  # result <- bayes_without_disease_part_2(maxEmitLength=40, iterations=1000)
  
  meanPostValues <- vector(mode = "numeric", length = maxEmitLength)
  powerValues <- vector(mode = "numeric", length = maxEmitLength)
  df <- data.frame(c(0.96, 0.04), c(0.05, 0.95))
  prior <- c(0.001, 0.999)
  
  for(i in 1:maxEmitLength)
  {
    currentRun <- vector(mode = "numeric", length = iterations)
    for(j in 1:iterations)
    {
      emits <- replicate(i, sample(c(1,2), 1, replace = TRUE, prob= c(df[1,2], df[2,2])))
      result <- run_bayes_sim(prior, df, emits)
      currentRun[j] <- result[length(result[,2]), 2]
    }
    meanPostValues[i] <- mean(currentRun)
    powerValues[i] <- sum(currentRun >= 0.9999)/iterations
  }
  
  plot(1:length(powerValues), powerValues, main = "Patient without Disease", xlab = "num of tests", ylab = "power value", xlim=c(1, maxEmitLength),ylim = c(0,1))
  abline(h = 0.95)
  text(10, .91, "power = 0.95")
  
  # write.csv(powerValues, row.names = F, file = "/Users/young/Documents/GitHub/advstatistics-labs/labs/lab03/power_values_without_disease_part_2.csv")
  # write.csv(meanPostValues, row.names = F, file = "/Users/young/Documents/GitHub/advstatistics-labs/labs/lab03/mean_posterior_values_without_disease_part_2.csv")
  
  return(data.frame(meanPostValues, powerValues))
}