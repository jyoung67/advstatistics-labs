generateBoxPlots <- function()
{
  source('~/GitHub/advstatistics-labs/labs/Lab12/getIndexesForNominals.R')
  
  # Read/transform data
  inFileName <- "http://afodor.github.io/classes/stats2015/prePostPhylum.txt"
  rawData <-read.table(inFileName,header=TRUE,sep="\t")
  myT <- rawData[rawData$time == "POST",]
  cages <- as.vector(myT$cage)
  cageIndexes <- getIndexesForNominals(cages)
  
  bugData <- data.frame(Tenericutes=as.vector(myT$Tenericutes), 
                        Verrucomicrobia=as.vector(myT$Verrucomicrobia), 
                        Bacteroidetes=as.vector(myT$Bacteroidetes),
                        Actinobacteria=as.vector(myT$Actinobacteria), 
                        Firmicutes=as.vector(myT$Firmicutes), 
                        Proteobacteria=as.vector(myT$Proteobacteria))
  
  nominalData <- data.frame(cage=as.vector(myT$cage), 
                            genotype=as.vector(myT$genotype), 
                            cageIndexes=cageIndexes$indexVector)
  op <- par(mfrow = c(3,2))
  xlabText <- "cage ref #"
  ylabText <- "abundance"
  
  columnNames <- colnames(bugData)
  xComponent <- nominalData$cageIndexes
  
  for (i in 1:length(columnNames))
  {
    boxplot(bugData[[i]]~xComponent,xlab = "", ylab = ylabText, main = paste(columnNames[i], "by Cage"), names=cageIndexes$uniqueLabelVector, las = 2)
    stripchart(bugData[[i]]~xComponent, vertical=TRUE, pch=21, add=TRUE)
  }
  par(op)
  
  return(list(bugData=bugData, nominalData=nominalData))
}