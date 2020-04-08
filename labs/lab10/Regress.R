Regress <- function()
{
  AA <- c(4.3, 2.3, 4.5, 5.6, 4.2, 3.9, 2.8); Aa <- c(2.7, 2.3, 1.9, 1.3, 1.2, 1.8, 2.1); aa <- c(1.6, 0.9, 1.1, 1.2, 2.1, 0.5, 0.9)
  myData <- c(AA, Aa, aa)
  genotypes <- c(rep("AA", length(AA)), rep("Aa", length(Aa)), rep("aa", length(aa)))
  myLm <- lm(myData ~ factor(genotypes), x = TRUE)
  
  myData_Recreated <- myLm$coefficients[1]*myLm$x[,1] + myLm$coefficients[2]*myLm$x[,2] + myLm$coefficients[3]*myLm$x[,3] + myLm$residuals
  print("Input vectors recreated with matrix and coefficients from model:")
  print("myLm$coefficients[1]*myLm$x[,1] + myLm$coefficients[2]*myLm$x[,2] + myLm$coefficients[3]*myLm$x[,3] + myLm$residuals")
  cat(myData_Recreated, "\n\n")
  
  print("Linear Model Summary-------->")
  print(summary(myLm))

  print("ANOVA Summary-------->")
  summary(anova(myLm))
  
  return(list(lModel = myLm, inputData = myData))
}