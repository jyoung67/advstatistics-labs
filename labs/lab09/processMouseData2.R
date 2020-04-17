processMouseData2 <- function()
{
  setwd("/Users/young/Documents/GitHub/advstatistics-labs/labs/lab09/")
  # Retrieve data
  bmiData <- read.table("BMI_Data.txt", header = TRUE, sep="\t")
  caseData <- read.table("caseControlData.txt", header = TRUE, sep="\t")
  caseData$bmi <- -1
  
  # Map BMI value to study Id
  for (i in 1:nrow(caseData))
  {
    key <- strsplit(levels(caseData$sample)[i], "_")[[1]][1]
    idx <- which(bmiData$studyid == key)
    caseData$bmi[i] <- bmiData$bmi[idx]
  }
  
  # Remove rows with value = 'NA'
  caseData <- caseData[!is.na(caseData$bmi),]
  
  numCols <- ncol(caseData)
  numRows <- nrow(caseData)
  threshold <- 0.10
  pValues <- vector(mode = "numeric", length = (numCols - 2))
  
  # Retrieve pvalues
  for(i in 2:(numCols-1))
  {
    myLinearModel <- lm(caseData$bmi~caseData[,i])
    pValues[i-1] <- anova(myLinearModel)$"Pr(>F)"[1]
  }
  
  # Default method
  default_result <- sum(pValues <= threshold)
  # Bonferroni method
  Bonferroni_result <- sum(pValues <= threshold/numRows)
  # BH/FDR method
  sortedPValues <- sort(pValues)
  vals <- sortedPValues <= (rank(sortedPValues)/length(sortedPValues)) * threshold
  BHFDR_result <- ifelse(length(which(vals == TRUE)) == 0 , 0, max(which(vals == TRUE) ) )
  
  print(paste("Default threshold:  # of significant values:", default_result))
  print(paste("Bonferroni adjusted threshold:  # of significant values:", Bonferroni_result))
  print(paste("BH FDR adjusted threshold:  # of significant values:", BHFDR_result))
  
  # Generate histogram of pvalues
  hist(pValues, xlab="p-value", main = "Linear Model P-Values", breaks=50)
  
  return (pValues)
}