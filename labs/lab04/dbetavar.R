dbetavar <- function(alpha, beta)
{
  num = alpha * beta
  dem = ((alpha + beta)^2)*(alpha + beta + 1)
  return (num/dem)
}

# x <- seq(1, 410, 1)
# plot(x, dbetavar(x,x))

# var_2_7 <- (dbetavar(7,7) - dbetavar(2,2))/dbetavar(2,2); var_2_7
# [1] -0.6666667

# var_401_406 <- (dbetavar(406,406) - dbetavar(401,401))/dbetavar(401,401); var_401_406
# [1] -0.01230012