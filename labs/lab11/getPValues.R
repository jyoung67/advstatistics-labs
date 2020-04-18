getPValues <- function(pcaoData, rawData)
{
  myLmPCA1Cage <- lm(pcaoData$scores[,1]~factor(rawData$cage), x=TRUE)
  pvaluePCA1Cage <- anova(myLmPCA1Cage)$"Pr(>F)"[1]
  myLmPCA2Cage <- lm(pcaoData$scores[,2]~factor(rawData$cage), x=TRUE)
  pvaluePCA2Cage <- anova(myLmPCA2Cage)$"Pr(>F)"[1]
  
  # result$pcoaData$scores[,1][which(result$rawData$genotype=="10-/-")]
  # result$pcoaData$scores[,1][which(result$rawData$genotype=="WT")]
  cat("Pvalue for PCA1.cage is: ", pvaluePCA1Cage, "\n")
  cat("Pvalue for PCA2.cage is: ", pvaluePCA2Cage, "\n")
}