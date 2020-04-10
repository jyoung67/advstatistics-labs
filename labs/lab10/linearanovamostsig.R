linearanovamostsig <- function()
{
  source('~/GitHub/advstatistics-labs/labs/lab10/linearanova.R')
  library("ggplot2")
  
  periods <- c(rep(2, 3), rep(86, 3), rep(128, 5))
  ls <- linearanova()
  minIndex <- order(ls$pValues)[1]
  myData <- ls$sourceData[minIndex,]
  myLm <- lm( myData ~ periods, x=TRUE)
  dat <- data.frame(
    x = periods,
    y = myData 
  )
  cat("Minimum pvalue index:",  minIndex)
  coefs <- coef(lm(y ~ x, data = dat))
  bp <- ggplot(dat, aes(x, y, group = x)) + geom_boxplot()
  bp + geom_abline(intercept = coefs[1], slope = coefs[2]) + 
  scale_x_continuous(breaks=c(2,86,128), labels=c(2, 86, 128), limits=c(0,150)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  ggtitle("Most Sig Gene Values for Reduced Model") +
  xlab("Days") + 
  ylab("relative abundance")
}