pcaReview <- function()
{
  x <- c(2.5, 0.5, 2.2, 1.9, 3.1, 2.3, 2.0, 1.0, 1.5, 1.1)
  x <- x - mean(x)
  y <- y - mean(y)
  plot(x,y)
  cv <- function(x,y){sum((x-mean(x)) * (y-mean(y)))/(length(x) - 1)}
  covm <- cbind(c(cv(x,x), cv(y,x)), c(cv(x,y), cv(y,y))); covm
  result <- eigen(covm)
  return (result)
  
  # sqrt(sum(result$vectors[,1]^2))
}
