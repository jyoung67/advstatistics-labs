createMixedLinearModels <- function()
{
  source('~/GitHub/advstatistics-labs/labs/Lab12/getIndexesForNominals.R')
  
  # Read/transform data
  inFileName <- "http://afodor.github.io/classes/stats2015/prePostPhylum.txt"
  rawData <-read.table(inFileName,header=TRUE,sep="\t")
  myT <- rawData[rawData$time == "POST",]
  Tenericutes <- as.vector(myT$Tenericutes)
  Verrucomicrobia <- as.vector(myT$Verrucomicrobia)
  Bacteroidetes <- as.vector(myT$Bacteroidetes)
  Actinobacteria <- as.vector(myT$Actinobacteria)
  Firmicutes <- as.vector(myT$Firmicutes)
  Proteobacteria <- as.vector(myT$Proteobacteria)
  cage <- as.vector(myT$cage)
  genotype <-as.vector(myT$genotype)
  myList <- list(Tenericutes=Tenericutes, Verrucomicrobia=Verrucomicrobia, Bacteroidetes=Bacteroidetes,
                        Actinobacteria=Actinobacteria, Firmicutes=Firmicutes, Proteobacteria=Proteobacteria,
                        cage=cage, genotype=genotype)
  
  op <- par(mfrow = c(3,2))
  plotIndexes <- getIndexesForNominals(cage)
  tmarksAt <- seq(1,11, by=1)
  plot(plotIndexes$indexVector, Tenericutes, main="Tenericutes", ylab = "abundance", xaxt="none", xlab="")
  axis(side=1, at=tmarksAt, labels=FALSE)
  text(tmarksAt, par("usr")[3] - 0.75, labels = plotIndexes$uniqueLabelVector, srt = 270, pos = 1, xpd = TRUE, cex =.8)
  
  
  # plot(plotIndexes$indexVector, Verrucomicrobia, main="Verrucomicrobia", ylab = "abundance", xlab = "cage")
  # axis(1, at=1:11, labels=1:11)
  # 
  # plot(plotIndexes$indexVector, Bacteroidetes, main="Bacteroidetes", ylab = "abundance", xlab = "cage")
  # axis(1, at=1:11, labels=1:11)
  # 
  # plot(plotIndexes$indexVector, Actinobacteria, main="Actinobacteria", ylab = "abundance", xlab = "cage")
  # axis(1, at=1:11, labels=1:11)
  # 
  # plot(plotIndexes$indexVector, Firmicutes, main="Firmicutes", ylab = "abundance", xlab = "cage")
  # axis(1, at=1:11, labels=1:11)
  # 
  # plot(plotIndexes$indexVector, Proteobacteria, main="Proteobacteria", ylab = "abundance", xlab = "cage")
  # axis(1, at=1:11, labels=1:11)
  
  par(op)
  
  
  return(myList)
}