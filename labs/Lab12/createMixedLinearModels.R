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
  cageIndexes <- getIndexesForNominals(cage)
  myList <- list(Tenericutes=Tenericutes, Verrucomicrobia=Verrucomicrobia, Bacteroidetes=Bacteroidetes,
                        Actinobacteria=Actinobacteria, Firmicutes=Firmicutes, Proteobacteria=Proteobacteria,
                        cage=cage, genotype=genotype, cageIndexes=cageIndexes$indexVector)
  op <- par(mfrow = c(3,2))
  xlabText <- "cage ref #"
  ylabText <- "abundance"
 
  boxplot(Tenericutes~cageIndexes$indexVector,xlab = xlabText, ylab = ylabText, main = "Tenericutes by Cage")
  stripchart(Tenericutes~cageIndexes$indexVector, vertical=TRUE, pch=21, add=TRUE)
  
  boxplot(Verrucomicrobia~cageIndexes$indexVector,xlab = xlabText, ylab = ylabText, main = "Verrucomicrobia by Cage")
  stripchart(Verrucomicrobia~cageIndexes$indexVector, vertical=TRUE, pch=21, add=TRUE)

  boxplot(Bacteroidetes~cageIndexes$indexVector,xlab = xlabText, ylab = ylabText, main = "Bacteroidetes by Cage")
  stripchart(Bacteroidetes~cageIndexes$indexVector, vertical=TRUE, pch=21, add=TRUE)

  boxplot(Actinobacteria~cageIndexes$indexVector,xlab = xlabText, ylab = ylabText, main = "Actinobacteria by Cage")
  stripchart(Actinobacteria~cageIndexes$indexVector, vertical=TRUE, pch=21, add=TRUE)

  boxplot(Firmicutes~cageIndexes$indexVector,xlab = xlabText, ylab = ylabText, main = "Firmicutes by Cage")
  stripchart(Firmicutes~cageIndexes$indexVector, vertical=TRUE, pch=21, add=TRUE)

  boxplot(Proteobacteria~cageIndexes$indexVector,xlab = xlabText, ylab = ylabText, main = "Proteobacteria by Cage")
  stripchart(Proteobacteria~cageIndexes$indexVector, vertical=TRUE, pch=21, add=TRUE)

par(op)

  
  return(myList)
}