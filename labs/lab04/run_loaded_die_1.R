run_loaded_die_1 <- function(maxEmitLength, iterations) 
{
  # Example Function Call
  # result <- run_loaded_die_1(maxEmitLength=40, iterations=1000)
  
  meanPostValues <- vector(mode = "numeric", length = maxEmitLength)
  powerValues <- vector(mode = "numeric", length = maxEmitLength)
  df <- data.frame(c(0.1,0.1,0.1,0.1,0.1,0.5), c(1/6,1/6,1/6,1/6,1/6,1/6))
  prior <- c(0.01, 0.99)
  
  for(i in 1:maxEmitLength)
  {
    currentRun <- vector(mode = "numeric", length = iterations)
    for(j in 1:iterations)
    {
      emits <- replicate(i, sample(c(1,2,3,4,5,6), 1, replace = TRUE, prob= c(df[1,1], df[1,2], df[1,3], df[1,4],df[1,5], df[1,6])))
      result <- run_bayes_sim(prior, df, emits)
      currentRun[j] <- result[length(result[,1]), 1]
    }
    meanPostValues[i] <- mean(currentRun)
    powerValues[i] <- sum(currentRun >= 0.9999)/iterations
  }
  
  plot(1:length(powerValues), powerValues, main = "Patient with Disease", xlab = "num of tests", ylab = "power value", xlim=c(1, maxEmitLength),ylim = c(0,1))
  abline(h = 0.95)
  text(7, .98, "power = 0.95")
  
  # write.csv(powerValues, row.names = F, file = "/Users/young/Documents/GitHub/advstatistics-labs/labs/lab03/power_values_part_1.csv")
  # write.csv(meanPostValues, row.names = F, file = "/Users/young/Documents/GitHub/advstatistics-labs/labs/lab03/mean_posterior_values_part_1.csv")
  
  return(data.frame(meanPostValues, powerValues))
}