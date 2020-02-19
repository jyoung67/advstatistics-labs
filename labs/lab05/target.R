target <- function(x)
{
  return(ifelse((x >= 0) & (x <= 1), dexp(x, rate=5)/0.9932621, 0))
}