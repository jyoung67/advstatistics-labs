generateData <- function()
{
  probs <- seq(.01, 1, .01) 
  len <- length(probs)
  numOfWins <- 0:7
  
  
  result <- matrix(nrow=len, ncol=length(numOfWins))
  
  for(i in 1:len)
  {
    result[i, 1:7] <- dnbinom(0:6,3,p=probs[i])
    result[i, 8] = 1-sum(result[i, 1:7])
  }
  
  expected_values_interim <- sweep(result, MARGIN=2, c(1.45, 1.72, 2.24, 2.76, 3.55, 4.60, 5.65, 6.75), `*`)
  expectedValues <- apply(expected_values_interim,1,sum)
  
  
  plot(probs, expectedValues, main = "Expected Tournament Payoff", xlab = "probability of failure", ylab = "expected payoff ($)")
  abline(h=3.75, col="red")
  text(0.1, 3.8, "$3.75")
  
  return(expectedValues)
}