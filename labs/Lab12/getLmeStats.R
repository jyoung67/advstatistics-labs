getLmeStats <- function()
{
  source('~/GitHub/advstatistics-labs/labs/Lab12/getIndexesForNominals.R')
  
  # Calculates intraclass correlation coefficient
  getStatValues <- function(lmeModel)
  {
    varcor <- sapply(VarCorr(lmeModel)[,2], as.double)
    icc <- varcor[1]^2/(varcor[1]^2+ varcor[2]^2)
    pvalue <- unclass(summary(lmeModel))$tTable[2,5]
    return (data.frame(icc=icc, pvalue=pvalue))
  }
  
  # Read/transform data
  inFileName <- "http://afodor.github.io/classes/stats2015/prePostPhylum.txt"
  rawData <-read.table(inFileName,header=TRUE,sep="\t")
  myT <- rawData[rawData$time == "POST",]
  cages <- as.vector(myT$cage)
  cageIndexes <- getIndexesForNominals(cages)
  genotypes <- genotype=as.vector(myT$genotype)
  
  TenericutesDF <- data.frame(bugData=as.vector(myT$Tenericutes), cages=cages, genotypes=genotypes)
  VerrucomicrobiaDF <- data.frame(bugData=as.vector(myT$Verrucomicrobia), cages=cages, genotypes=genotypes)
  BacteroidetesDF <- data.frame(bugData=as.vector(myT$Bacteroidetes), cages=cages, genotypes=genotypes)
  ActinobacteriaDF <- data.frame(bugData=as.vector(myT$Actinobacteria), cages=cages, genotypes=genotypes)
  FirmicutesDF <- data.frame(bugData=as.vector(myT$Firmicutes), cages=cages, genotypes=genotypes)
  ProteobacteriaDF <- data.frame(bugData=as.vector(myT$Proteobacteria), cages=cages, genotypes=genotypes)
  
  Tenericutes.mixed <- lme( bugData ~ genotypes, method= "REML", random = ~1 | cages, data = TenericutesDF)
  Verrucomicrobia.mixed <- lme( bugData ~ genotypes, method= "REML", random = ~1 | cages, data = Verrucomicrobia)
  Bacteroidetes.mixed <- lme( bugData ~ genotypes, method= "REML", random = ~1 | cages, data = Bacteroidetes)
  Actinobacteria.mixed <- lme( bugData ~ genotypes, method= "REML", random = ~1 | cages, data = Actinobacteria)
  Firmicutes.mixed <- lme( bugData ~ genotypes, method= "REML", random = ~1 | cages, data = Firmicutes)
  Proteobacteria.mixed <- lme( bugData ~ genotypes, method= "REML", random = ~1 | cages, data = Proteobacteria)
  pValuesMixed[index] <- anova(M.mixed)$"p-value"[2]
  
  
  
  
 
 
  
 
  
  
}