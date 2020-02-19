runMetropolisTest <- function(iterations, initialValue, numSuccess, totalNum)
{
  # result <- runMetropolisTest(iterations = 10000, initialValue=0.5, numSuccess=14, totalNum=24)
  # target <- function(x, a, b){ifelse((x >= 0) & (x <= 1), (dexp(x, rate=5)/0.9932621) * dbinom(a,b,prob = x), 0)}
  target <- function(x, a, b){ifelse((x >= 0) & (x <= 1), dbeta(x, 10, 10) * dbinom(a,b,prob = x), 0)}
  
  xvect <- vector(length = iterations, mode = "numeric")
  xvect[1] = initialValue
    for(i in 2:iterations)
    {
      current_x = xvect[i-1]
      proposed_x = current_x + rnorm(1,0, sd=0.01)
      ratio = target(proposed_x, numSuccess, totalNum)/target(current_x, numSuccess, totalNum)
      if(runif(1) < ratio)
      {
        xvect[i] = proposed_x
      }
      else
      {
        xvect[i] = current_x
      }
    }
  return (xvect)
}

result <- runMetropolisTest(iterations = 500000, initialValue=0.5, numSuccess=14, totalNum=24)
myHist <- hist(result, breaks = 200, plot=FALSE)
plot(myHist$mids, myHist$counts/length(result))
dbetasum <- sum(dbeta(myHist$mids, 10+14, 10+10))
lines(myHist$mids, dbeta(myHist$mids, 10+14, 10+10)/dbetasum, col="red")