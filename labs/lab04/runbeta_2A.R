runbeta_2A <- function()
{
  
  s <- seq(0,1,0.001)
  plot(s, dbeta(s,1,1), col="blue", ylim=c(0,3.0), cex=0.5)
  lines(s, dbeta(s,6,6), col="red", lty = 2)
  legend(.01, 3, legend=c("dbeta(1,1)", "dbeta(6,6)"), col=c("blue", "red"), lty=1:2, cex=0.8)
  
}