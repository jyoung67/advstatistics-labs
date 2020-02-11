roll_die_with_emits <- function()
{
  prior <- c(0.01, 0.99)
  df <- data.frame(c(0.1,0.1,0.1,0.1,0.1,0.5), c(1/6,1/6,1/6,1/6,1/6,1/6))
  emits<-c(2,3,2,6,3,5,6,2,6,6,2,6,6,2,3,6,6,6,5,6,6,5,6,6,6,6,6,4,6,3,3,3,6,6,5,6,6)
  result <- run_bayes_sim(prior, df, emits)
  plot(1:length(result[,1]), result[,1], xlim=c(1, length(result[,1])), xlab = "num of rolls", ylab="posterior prob", main = "Die Rolls")
  return (result)
}