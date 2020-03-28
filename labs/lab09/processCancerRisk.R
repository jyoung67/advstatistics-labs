processCancerRisk <- function()
{
  setwd("/Users/young/Documents/GitHub/advstatistics-labs/labs/lab09/")
  
  df <- read.table("cancerRisk.txt", header = TRUE, sep="\t")
  # plot( log10(df$CumulativeCellDivisions),log10(df$Lifetime_cancer_risk),  xaxt = "n",  yaxt = "n")
  plot( log10(df$CumulativeCellDivisions),log10(df$Lifetime_cancer_risk),  axes = FALSE, xlim = c(5,13), ylim = c(-5, 0), xlab="Total stem cell divisions", ylab = "Lifetime risk")
  
  axis(1, at = c(5, 7, 9, 11, 13), labels = c("10^5", "10^7", "10^9", "10^11", "10^13"))
  axis(2, at = c(-5, -3, -1), labels = c("10^-5", "10^-3", "10^-1"))
  
  lmodel <- lm(log10(Lifetime_cancer_risk)~log10(CumulativeCellDivisions), data = df)
  
  abline(lmodel)
  
  summary(lmodel)
  
  
}