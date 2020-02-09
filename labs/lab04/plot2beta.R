plot2beta <- function(dbeta1, dbeta2, lpos = "topleft")
{
  s <- seq(0,1,0.001)
  ymax = (dbeta2[1])/(dbeta2[1]+dbeta2[2])
  plot(s, dbeta(s,dbeta1[1],dbeta1[2]), col="blue", ylim=c(0,dbeta(ymax, dbeta2[1], dbeta2[2])), cex=0.2, xlab = "probs" ,ylab = "dbeta(...)")
  lines(s, dbeta(s,dbeta2[1],dbeta2[2]), col="red", lty = 2)
  ldbeta1 = paste("dbeta(", dbeta1[1], ",",dbeta1[2], ")", sep ="")
  ldbeta2 = paste("dbeta(", dbeta2[1], ",",dbeta2[2], ")", sep ="")
  legend(lpos, legend=c(ldbeta1, ldbeta2), col=c("blue", "red"), lty=1:2, cex=0.7)

}