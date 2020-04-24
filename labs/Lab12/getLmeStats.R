getLmeStats <- function()
{
  library("nlme")
  source('~/GitHub/advstatistics-labs/labs/Lab12/getIndexesForNominals.R')
  threshold <- 0.10
  
  # Calculates intraclass correlation coefficient and p-value
  getStatValues <- function(lmeModel)
  {
    varcor <- sapply(VarCorr(lmeModel)[,2], as.double)
    icc <- varcor[1]^2/(varcor[1]^2+ varcor[2]^2)
    pvalue <- unclass(summary(lmeModel))$tTable[2,5]
    cat("ICC: ", icc, " | ", "p-value: ", pvalue, "\n")
    return (data.frame(icc=icc, pvalue=pvalue))
  }
  
  # Read/transform data
  inFileName <- "http://afodor.github.io/classes/stats2015/prePostPhylum.txt"
  rawData <-read.table(inFileName,header=TRUE,sep="\t")
  myT <- rawData[rawData$time == "POST",]
  cages <- as.vector(myT$cage)
  cageIndexes <- getIndexesForNominals(cages)
  genotypes <- as.vector(myT$genotype)
  
  TenericutesDF <- data.frame(bugData=as.vector(myT$Tenericutes), cage=cages, genotype=genotypes)
  VerrucomicrobiaDF <- data.frame(bugData=as.vector(myT$Verrucomicrobia), cage=cages, genotype=genotypes)
  BacteroidetesDF <- data.frame(bugData=as.vector(myT$Bacteroidetes), cage=cages, genotype=genotypes)
  ActinobacteriaDF <- data.frame(bugData=as.vector(myT$Actinobacteria), cage=cages, genotype=genotypes)
  FirmicutesDF <- data.frame(bugData=as.vector(myT$Firmicutes), cage=cages, genotype=genotypes)
  ProteobacteriaDF <- data.frame(bugData=as.vector(myT$Proteobacteria), cage=cages, genotype=genotypes)
  
  Tenericutes.mixed <- lme( bugData ~ genotype, method= "REML", random = ~1 | cage, data = TenericutesDF)
  Verrucomicrobia.mixed <- lme( bugData ~ genotype, method= "REML", random = ~1 | cage, data = VerrucomicrobiaDF)
  Bacteroidetes.mixed <- lme( bugData ~ genotype, method= "REML", random = ~1 | cage, data = BacteroidetesDF)
  Actinobacteria.mixed <- lme( bugData ~ genotype, method= "REML", random = ~1 | cage, data = ActinobacteriaDF)
  Firmicutes.mixed <- lme( bugData ~ genotype, method= "REML", random = ~1 | cage, data = FirmicutesDF)
  Proteobacteria.mixed <- lme( bugData ~ genotype, method= "REML", random = ~1 | cage, data = ProteobacteriaDF)
  
  TResult <- getStatValues(Tenericutes.mixed)
  VResult <- getStatValues(Verrucomicrobia.mixed)
  BResult <- getStatValues(Bacteroidetes.mixed)
  AResult <- getStatValues(Actinobacteria.mixed)
  FResult <- getStatValues(Firmicutes.mixed)
  PResult <- getStatValues(Proteobacteria.mixed)
  
  
  cat("Number of significant p-values at 0.10 threshold: ", sum(p.adjust(c(TResult, VResult, BResult, AResult, FResult, PResult), method =  "BH") < threshold))
}