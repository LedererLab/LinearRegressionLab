
leastsquares_gd <- function(y,X,stepsize = 0.01,iter_max = 100) 
{
  while (iter_max>0)
  {
    iter_max <- iter_max - 1
    beta <- beta+stepsize*2*t(X)%*%(y-X%*%beta)
  }
  return(beta)  
}

