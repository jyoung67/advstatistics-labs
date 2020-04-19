pcaReview <- function()
{
  x <- c(2.5, 0.5, 2.2, 1.9, 3.1, 2.3, 2.0, 1.0, 1.5, 1.1)
  y <- c(2.4, 0.7, 2.9, 2.2, 3.0, 2.7, 1.6, 1.1, 1.6, 0.9)
  x <- x - mean(x)
  y <- y - mean(y)
  plot(x,y)
  cv <- function(x,y){sum((x-mean(x)) * (y-mean(y)))/(length(x) - 1)}
  covm <- cbind(c(cv(x,x), cv(y,x)), c(cv(x,y), cv(y,y))); covm
  result <- eigen(covm)
  abline(a = 0, b = result$vectors[2,1]/result$vectors[1,1])
  abline(a = 0, b = result$vectors[2,2]/result$vectors[1,2])
  return (result)
  
  # sqrt(sum(result$vectors[,1]^2))
}
