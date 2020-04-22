gls_4.2 <- function()
{
  library(nlme)
  Squid <- read.table(file = "~/GitHub/advstatistics-labs/Mixed_Effects_Models_Book/ZuurDataMixedModelling/Squid.txt", header = TRUE, dec = ".") 
  Squid$fMONTH <- factor(Squid$MONTH)
  M.lm <- lm(Testisweight ~ DML *fMONTH, data = Squid, x=TRUE)
  vf1Fixed <- varFixed(~DML)
  M.gls1 <- gls(Testisweight ~ DML * fMONTH, weights = vf1Fixed, data = Squid)
 anova(M.lm, M.gls1)

 return (list(model1=M.lm, model2=M.gls1))
  
}