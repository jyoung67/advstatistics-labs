getPValues <- function(pcaoData, rawDataInput)
{
  # Perform one-way ANOVA
  myLmPCA1Cage <- lm(pcaoData$scores[,1]~factor(rawDataInput$cage), x=TRUE)
  pvaluePCA1Cage <- anova(myLmPCA1Cage)$"Pr(>F)"[1]
  myLmPCA2Cage <- lm(pcaoData$scores[,2]~factor(rawDataInput$cage), x=TRUE)
  pvaluePCA2Cage <- anova(myLmPCA2Cage)$"Pr(>F)"[1]
  
  # Get pvalues for genetypes ('WT' or '10-/-')
  genotypes <- as.vector(unique(rawDataInput$genotype))
  genotypePCA1_1 <- pcaoData$scores[,1][which(rawDataInput$genotype == genotypes[1])]
  genotypePCA1_2 <- pcaoData$scores[,1][which(rawDataInput$genotype == genotypes[2])]
  genotypePCA2_1 <- pcaoData$scores[,2][which(rawDataInput$genotype == genotypes[1])]
  genotypePCA2_2 <- pcaoData$scores[,2][which(rawDataInput$genotype == genotypes[2])]
  pValueGenotypePCA1 <- t.test(genotypePCA1_1, genotypePCA1_2)$p.value
  pValueGenotypePCA2 <- t.test(genotypePCA2_1, genotypePCA2_2)$p.value
  
  # Get pvalues for time types ('PRE' or 'POST')
  timeTypes <- as.vector(unique(rawDataInput$time))
  timeTypePCA1_1 <- pcaoData$scores[,1][which(rawDataInput$time == timeTypes[1])]
  timeTypePCA1_2 <- pcaoData$scores[,1][which(rawDataInput$time == timeTypes[2])]
  timeTypePCA2_1 <- pcaoData$scores[,2][which(rawDataInput$time == timeTypes[1])]
  timeTypePCA2_2 <- pcaoData$scores[,2][which(rawDataInput$time == timeTypes[2])]
  pValuetimeTypePCA1 <- t.test(timeTypePCA1_1, timeTypePCA1_2)$p.value
  pValuetimeTypePCA2 <- t.test(timeTypePCA2_1, timeTypePCA2_2)$p.value
 
  # Display calculated p-values for cage types
  cat("Pvalue for PCA1.cage is: ", pvaluePCA1Cage, "\n")
  cat("Pvalue for PCA2.cage is: ", pvaluePCA2Cage, "\n\n")
  
  # Display calculated p-values for genotypes
  cat("Pvalue for PCA1.genotype is: ", pValueGenotypePCA1, "\n")
  cat("Pvalue for PCA2.genotype is: ", pValueGenotypePCA2, "\n\n")
  
  # Display calculated p-values for time types
  cat("Pvalue for PCA1.time is: ", pValuetimeTypePCA1, "\n")
  cat("Pvalue for PCA2.time is: ", pValuetimeTypePCA2, "\n")
}