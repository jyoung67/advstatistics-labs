processMouseData <- function()
{
  setwd("/Users/young/Documents/GitHub/advstatistics-labs/labs/lab09/")
  bmiData <- read.table("BMI_Data.txt", header = TRUE, sep="\t")
  # nrow = 71
  caseData <- read.table("caseControlData.txt", header = TRUE, sep="\t")
  caseData$bmi <- -1
  for (i in 1:nrow(caseData))
  {
    key <- strsplit(levels(caseData$sample)[i], "_")[[1]][1]
    idx <- which(bmiData$studyid == key)
    caseData$bmi[i] <- bmiData$bmi[idx]
  }
  
  # Remove rows with value = 'NA'
  caseData <- caseData[!is.na(caseData$bmi),]
  
  caseData <- 
  
  return (caseData)
  
}