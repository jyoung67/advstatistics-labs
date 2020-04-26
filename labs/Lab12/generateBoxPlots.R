generateBoxPlots <- function()
{
  # Read/transform data
  inFileName <- "http://afodor.github.io/classes/stats2015/prePostPhylum.txt"
  rawData <-read.table(inFileName,header=TRUE,sep="\t")
  myT <- rawData[rawData$time == "POST",]
  cages <- as.vector(myT$cage)
  
  # Create data frame for explanatory variables
  bugData <- data.frame(Tenericutes=as.vector(myT$Tenericutes), 
                        Verrucomicrobia=as.vector(myT$Verrucomicrobia), 
                        Bacteroidetes=as.vector(myT$Bacteroidetes),
                        Actinobacteria=as.vector(myT$Actinobacteria), 
                        Firmicutes=as.vector(myT$Firmicutes), 
                        Proteobacteria=as.vector(myT$Proteobacteria))
  
  # Trim cage label text
  trimmedUniqueLabelVector <- sapply(myT$cage, gsub, pattern = "Cage", replacement ="C")
  
  # Create data frame for nominal variables
  nominalData <- data.frame(cage=as.vector(trimmedUniqueLabelVector),genotype=as.vector(myT$genotype))
  
  # Set plot parameters
  op <- par(mfrow = c(3,2))
  ylabText <- "relative abundance"
  columnNames <- colnames(bugData)
  xComponent <- nominalData$cage
  
  # Generate box plots
  for (i in 1:length(columnNames))
  {
    boxplot(bugData[[i]]~xComponent,xlab = "", ylab = ylabText, main = paste(columnNames[i], "by Cage"), las = 2)
    abline(h=mean((bugData[[i]])), col="blue", lty=3)
    stripchart(bugData[[i]]~xComponent, vertical=TRUE, pch=21,cex=0.8,col="red", add=TRUE)
  }
  par(op)
  
  return(list(bugData=bugData, nominalData=nominalData))
}