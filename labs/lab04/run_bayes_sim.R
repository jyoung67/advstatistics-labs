run_bayes_sim <- function(prior, likelihoods, emits) 
{
  iterations <- length(emits)
  numerator <- matrix(data = prior, nrow = length(prior), ncol=1)
  L <- data.matrix(likelihoods)
  posterior <- matrix(ncol = length(likelihoods), nrow = iterations)
  
  for(i in 1:iterations)
    {
      numerator <- numerator *L[(emits[i]),]
      posterior[i,] <- numerator/sum(numerator)
    }

  return(posterior)
}