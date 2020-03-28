processCancerRisk <- function()
{
  setwd("/Users/young/Documents/GitHub/advstatistics-labs/labs/lab09/")
  
  df <- read.table("cancerRisk.txt", header = TRUE, sep="\t")
  cancerTypes <- gsub("_", " ",as.character(df$Cancer_type))
  print(paste("length", length(cancerTypes)))
  lbls <- vector(mode = "character", length = length(cancerTypes))
  nums <- 1:length(cancerTypes)
  for (i in 1:length(cancerTypes)) 
  { 
    lbls[i] <- paste(i, ") ", cancerTypes[i], sep = "")
  }
  for (val in lbls)
  {
    print(val)
  }

# Plot data
  plot( log10(df$CumulativeCellDivisions),log10(df$Lifetime_cancer_risk),  axes = FALSE, xlim = c(5,13), ylim = c(-5, 0), xlab="Total stem cell divisions", ylab = "Lifetime risk", pch=20, cex=0.8)
  axis(1, at = c(5, 7, 9, 11, 13), labels = c("10^5", "10^7", "10^9", "10^11", "10^13"))
  axis(2, at = c(-5, -3, -1), labels = c("10^-5", "10^-3", "10^-1"))
  lmodel <- lm(log10(Lifetime_cancer_risk)~log10(CumulativeCellDivisions), data = df)
  text(log10(Lifetime_cancer_risk)~log10(CumulativeCellDivisions), labels=nums,data=df, font=8)  
  abline(lmodel, col="red")
  plot(lmodel)
  summary(lmodel)
}