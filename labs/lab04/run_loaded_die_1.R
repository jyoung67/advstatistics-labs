run_loaded_die_1 <- function(maxEmitLength, iterations) 
{
  # Example Function Call
  # result <- run_loaded_die_1(maxEmitLength=120, iterations=1000)
  # plot number of times rolls vs mean power values
  # plot(1:length(result$powerValues), result$powerValues, main = "Loaded Die", xlab = "num of rolls", ylab = "power value", xlim=c(1, 120),ylim = c(0,1))
  # abline(h = 0.95)
  # text(30, 0.992, "power = 0.95", cex = 0.7)
  
  meanPostValues <- vector(mode = "numeric", length = maxEmitLength)
  powerValues <- vector(mode = "numeric", length = maxEmitLength)
  df <- data.frame(c(0.1,0.1,0.1,0.1,0.1,0.5), c(1/6,1/6,1/6,1/6,1/6,1/6))
  prior <- c(0.01, 0.99)
  
  for(i in 1:maxEmitLength)
  {
    currentRun <- vector(mode = "numeric", length = iterations)
    for(j in 1:iterations)
    {
      emits <- replicate(i, sample(c(1,2,3,4,5,6), 1, replace = TRUE, prob= c(df[1,1], df[2,1], df[3,1], df[4,1],df[5,1], df[6,1])))
      result <- run_bayes_sim(prior, df, emits)
      currentRun[j] <- result[length(result[,1]), 1]
    }
    meanPostValues[i] <- mean(currentRun)
    powerValues[i] <- sum(currentRun >= 0.9999)/iterations
  }
  
  return(data.frame(meanPostValues, powerValues))
}