library(MASS) # for multivarite normal

leastsquares_test <- function(stepsize = 1,scale_step = 0.8,scale_total = 0.5,iter_max = 100)
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
  fvalue_lm <- norm(y-X%*%coeff_lm,"2")
  print("Function value from lm package:")
  print(fvalue_lm)
  
  coeff_math <- solve(t(X)%*%X)%*%t(X)%*%y
  print("Coefficients from direct computation:")
  print(coeff_math)
  fvalue_math <- norm(y-X%*%coeff_math,"2")
  print("Function value from direct computation:")
  print(fvalue_math)
  
  coeff_gd <- leastsquares_gd(y,X,stepsize = 0.01)
  print("Coefficients from gradient descent function:")
  print(coeff_gd)
  fvalue_gd <- norm(y-X%*%coeff_gd,"2")
  print("Function value from gradient descent:")
  print(fvalue_gd)
  
  coeff_gd_bt <- leastsquares_gd_bt(y,X)
  print("Coefficients from gradient descent function with backtracking:")
  print(coeff_gd_bt)
  fvalue_gd_bt <- norm(y-X%*%coeff_gd_bt,"2")
  print("Function value from gradient descent function with backtracking:")
  print(fvalue_gd_bt)
  
  coeff_cd <- leastsquares_cd(y,X,stepsize = 0.01)
  print("Coefficients from coodinate descent function:")
  print(coeff_cd)
  fvalue_cd <- norm(y-X%*%coeff_cd,"2")
  print("Function value from coordinate descent function:")
  print(fvalue_cd)
  
  coeff_cd_pr <- leastsquares_cd_pr(y,X,stepsize = 0.01)
  print("Coefficients from coodinate descent function with pre-computations:")
  print(coeff_cd_pr)
  fvalue_cd_pr <- norm(y-X%*%coeff_cd_pr,"2")
  print("Function value from coordinate descent function with pre-computations:")
  print(fvalue_cd_pr)
  
  
  return(1)
}
