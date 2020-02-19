runMetropolis <- function(iterations, initialValue, a, b)
{
  target <- function(x, a, b){ifelse((x >= 0) & (x <= 1), (dexp(x, rate=5)/0.9932621) * dbinom(a,b,prob = x), 0)}
  
  xvect <- vector(length = iterations, mode = "numerical")
  xvect[1] = initialValue
    for(i in 2:iterations)
    {
      current_x = xvect[i-1]
      proposed_x = current_x + rnorm(1,0, sd=0.01)
      ratio = target(proposed_x, a, b)/target(current_x, a, b)
      if(runif(1) > ratio)
      {
        x[i] = proposed
      }
      else
      {
        x[i] = current
      }
      
    }
  return (target)
}