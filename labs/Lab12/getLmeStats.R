getLmeStats <- function()
{
  library("nlme")
  threshold <- 0.10
  
  # Calculate intraclass correlation coefficient & p-value for input lme model
  getStatValues <- function(lmeModel, name)
  {
    varcor <- sapply(VarCorr(lmeModel)[,2], as.double)
    icc <- varcor[1]^2/(varcor[1]^2+ varcor[2]^2)
    pvalue <- unclass(summary(lmeModel))$tTable[2,5]
    cat(name, " intraclass corr coef: ", icc, " | ", "raw p-value: ", pvalue, "\n")
    return (data.frame(icc=icc, pvalue=pvalue))
  }
  
  # Read/transform data
  inFileName <- "http://afodor.github.io/classes/stats2015/prePostPhylum.txt"
  rawData <-read.table(inFileName,header=TRUE,sep="\t")
  myT <- rawData[rawData$time == "POST",]
  cages <- as.vector(myT$cage)
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
  
  TResult <- getStatValues(Tenericutes.mixed, "Tenericutes")
  VResult <- getStatValues(Verrucomicrobia.mixed, "Verrucomicrobia")
  BResult <- getStatValues(Bacteroidetes.mixed, "Bacteroidetes")
  AResult <- getStatValues(Actinobacteria.mixed, "Actinobacteria")
  FResult <- getStatValues(Firmicutes.mixed, "Firmicutes")
  PResult <- getStatValues(Proteobacteria.mixed, "Proteobacteria")
  
  pvalues <- c(TResult$pvalue, VResult$pvalue, BResult$pvalue, AResult$pvalue, FResult$pvalue, PResult$pvalue)
  
  cat("\nNumber of significant BH-FDR adjusted p-values at ", threshold, " threshold: ", sum(p.adjust(pvalues, method =  "BH") < threshold))
  
  cat("\n\nAdjusted p-values at 10% FDR:\n", p.adjust(pvalues, method =  "BH"))
  
  return (pvalues)
}