generateNormalizedData <- function()
{
  myT<-read.table("/Users/young/Documents/GitHub/advstatistics-labs/labs/lab10/nc101_scaff_dataCounts.txt",sep="\t",header=TRUE,row.names=1)
  myT <- myT[ apply( myT,1, median)> 5,]
  myTNorm <- myT
  
  for ( i in 1:ncol(myT))
  {
    colSum = sum(myT[,i])
    myTNorm[,i] =myTNorm[,i]/colSum
  }
  
  threshold <- 0.05
  
  getSigGeneCount_Raw <- function(p_2_12, p_2_20, p_12_20, threshold)
  {
    print(paste("Raw Pvalues_2_12:", sum(p_2_12 < threshold)))
    print(paste("Raw Pvalues_2_20:", sum(p_2_20 < threshold)))
    print(paste("Raw Pvalues_12_20:", sum(p_12_20 < threshold)))
  }
  getSigGeneCount_Bonf <- function(p_2_12, p_2_20, p_12_20, threshold)
  {
    print(paste("BONF Pvalues_2_12:", sum(p.adjust(p_2_12, method =  "bonferroni") < threshold)))
    print(paste("BONF Pvalues_2_20:", sum(p.adjust(p_2_20, method =  "bonferroni") < threshold)))
    print(paste("BONF Pvalues_12_20:", sum(p.adjust(p_12_20, method =  "bonferroni") < threshold)))
  }
  
  getSigGeneCount_Bh <- function(p_2_12, p_2_20, p_12_20, threshold)
  {
    print(paste("BH Pvalues_2_12:", sum(p.adjust(p_2_12, method =  "BH") < threshold)))
    print(paste("BH Pvalues_2_20:", sum(p.adjust(p_2_20, method =  "BH") < threshold)))
    print(paste("BH Pvalues_12_20:", sum(p.adjust(p_12_20, method =  "BH") < threshold)))
  }
  
  raw_pvalues_2_12 <- vector(mode = "numeric", length = nrow(myTNorm))
  raw_pvalues_2_20 <- vector(mode = "numeric", length = nrow(myTNorm))
  raw_pvalues_12_20 <- vector(mode = "numeric", length = nrow(myTNorm))
  
  # Calculate raw p-values for day 2/week 12
  for ( i in 1:nrow(myTNorm))
  {
    raw_pvalues_2_12[i] <- t.test(myTNorm[i,1:3], myTNorm[i,4:6])$p.value
  }
  # Calculate raw p-values for day 2/week 20
  for ( i in 1:nrow(myTNorm))
  {
    raw_pvalues_2_20[i] <- t.test(myTNorm[i,1:3], myTNorm[i,7:11])$p.value
  }
  # Calculate raw p-values for week 12/week 20
  for ( i in 1:nrow(myTNorm))
  {
    raw_pvalues_12_20[i] <- t.test(myTNorm[i,4:6], myTNorm[i,7:11])$p.value
  }
  
  # Calculate significant gene counts
  getSigGeneCount_Raw(raw_pvalues_2_12, raw_pvalues_2_20, raw_pvalues_12_20, threshold)
  getSigGeneCount_Bonf(raw_pvalues_2_12, raw_pvalues_2_20, raw_pvalues_12_20, threshold)
  getSigGeneCount_Bh(raw_pvalues_2_12, raw_pvalues_2_20, raw_pvalues_12_20, threshold)
  
  # Plot histograms
  hist(raw_pvalues_2_12, breaks = 50, xlab = "p-values", main = "P-values (Day 2/Week 12", ylim = c(0,200))
  hist(raw_pvalues_2_20, breaks = 50, xlab = "p-values", main = "P-values (Day 2/Week 20")
  hist(raw_pvalues_12_20, breaks = 50, xlab = "p-values", main = "P-values (Week 12/Week 20", ylim=c(0,500))
  
  return (data.frame(raw_pvalues_2_12, raw_pvalues_2_20, raw_pvalues_12_20))
}