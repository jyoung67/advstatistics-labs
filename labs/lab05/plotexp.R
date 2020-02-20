plotexp <- function()
{
  target <- function(x){dexp(x, rate=5)/.9932621}
  x <- seq(0,1,.01)
  myHist <- hist(x, breaks = 200, plot=FALSE)
  plot(myHist$mids, myHist$counts/length(x))
}