runMetropolisTest <- function(iterations, initialValue, numHeads, numTails)
{
  # Example function call
  # runMetropolisTest(iterations = 10000, initialValue=0.5, numHeads=14, numTails=24)
  # runMetropolisTest(iterations = 1000000, initialValue=0.5, numHeads=583, numTails=417)
  target <- function(x, a, b){ifelse((x >= 0) & (x <= 1), (dexp(x, rate=5)/0.9932621) * dbinom(a,a+b,prob = x), 0)}
  
  xvect <- vector(length = iterations, mode = "numeric")
  xvect[1] = initialValue
    for(i in 2:iterations)
    {
      current_x = xvect[i-1]
      proposed_x = current_x + rnorm(n = 1,mean = 0, sd=0.01)
      ratio = target(proposed_x, numHeads, numTails)/target(current_x, numHeads, numTails)
      if(runif(1) < ratio)
      {
        xvect[i] = proposed_x
      }
      else
      {
        xvect[i] = current_x
      }
    }
  
  myHist <- hist(xvect, breaks = 200, plot=FALSE)
  plot(myHist$mids, myHist$counts/iterations, main = paste("num of heads =", numHeads, "/", "num of tails =", numTails))
  expSum <- sum(target(myHist$mids, numHeads, numTails))
  lines(myHist$mids, target(myHist$mids, numHeads, numTails)/expSum, col="red")
  dbetasum <- sum(dbeta(myHist$mids, 40+numHeads, 40+numTails))
  lines(myHist$mids, dbeta(myHist$mids, 40+numHeads, 40+numTails)/dbetasum, col="blue")
}