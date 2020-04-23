SquidData_4.1 <- function()
{
  Squid <- read.table(file = "~/GitHub/advstatistics-labs/Mixed_Effects_Models_Book/ZuurDataMixedModelling/Squid.txt", header = TRUE, dec = ".") 
  Squid$fMONTH <- factor(Squid$MONTH)
  Ml <- lm(Testisweight ~ DML *fMONTH, data = Squid, x=TRUE)
  # The following line is equivalent to the preceding line
  # Ml <- lm(Testisweight ~ DML + fMONTH + DML:fMONTH, data = Squid, x=TRUE)
  op <- par(mfrow = c(2,2), mar = c(4,4,2,2))
  plot(Ml, which = c(1), col = 1, add.smooth = FALSE, caption = "")
  plot(Squid$fMONTH, Ml$residuals, xlab = "Month", ylab = "Residuals")
  plot(Squid$DML, Ml$residuals, xlab="DML", ylab = "Residuals")
  par(op)
  
  return(Ml)

}