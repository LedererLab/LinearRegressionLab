library(MASS) # for multivarite normal

leastsquares_test <- function(stepsize = 0.01,max_iter = 100)
{
  n <- 10
  p <- 3
  Sigma <- diag(p)
  mu <- rep(0,p)
  
  X <- mvrnorm(n, mu, Sigma, tol = 1e-6)
  y <- mvrnorm(n, 0, 1)
  
  lmfit <- lm(y ~ X - 1) # -1 removes intercept
  coeff_lm <- lmfit$coefficients
  print("Coefficients from lm package:")
  print(coeff_lm)
  
  coeff_math <- solve(t(X)%*%X)%*%t(X)%*%y
  print("Coefficients from direct computation:")
  print(coeff_math)
  
  coeff_gd <- leastsquares_gd(y,X,stepsize = stepsize)
  print("Coefficients from gradient descent function:")
  print(coeff_gd)
  
  return(1)
}
