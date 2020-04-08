RModels <- function()
{
  AA <- c(4.3, 2.3, 4.5, 5.6, 4.2, 3.9, 2.8); Aa <- c(2.7, 2.3, 1.9, 1.3, 1.2, 1.8, 2.1); aa <- c(1.6, 0.9, 1.1, 1.2, 2.1, 0.5, 0.9)
  myData <- c(AA, Aa, aa)
  genotypes <- c(rep(0, length(AA)), rep(1, length(Aa)), rep(2, length(aa)))
  myLm <- lm(myData ~ genotypes, x = TRUE)
  
  p <- function(x){myLm$coefficients[2]*x + myLm$coefficients[1]}
  idx <- seq(0,2, .01)
  plot(genotypes, myData)
  abline(a = myLm$coefficients[1], b = myLm$coefficients[2])
  abline(myLm)
  
  
 
  
  print("Linear Model Summary-------->")
  print(summary(myLm))
  
  print("ANOVA Summary-------->")
  summary(anova(myLm))
  
  return(myLm)
}