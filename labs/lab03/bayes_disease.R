bayes_disease <- function(runs) 
{
  df <- data.frame(c(0.91, 0.09), c(0.16, 0.84))
  prior <- c(0.001, 0.999)
  # With Disease
  # emits <- replicate(runs, sample(c(1,2), 1, replace = TRUE, prob= c(df[1,1], df[1,2])))
  # Without Disease
  emits <- replicate(runs, sample(c(1,2), 1, replace = TRUE, prob= c(df[2,1], df[2,2])))
  print(emits)
  result <- run_bayes_sim(prior, df, emits)
}