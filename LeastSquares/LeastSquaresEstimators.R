
leastsquares_gd <- function(y,X,stepsize = 0.01,iter_max = 100) 
{
  beta <- rep(0,dim(X)[2])
  while (iter_max > 0)
  {
    beta <- beta + stepsize*2*t(X)%*%(y-X%*%beta)
    iter_max <- iter_max - 1
  }
  return(beta)  
}

leastsquares_gd_bt <- function(y,X,stepsize = 1,scale_step = 0.8, scale_total = 0.5,iter_max = 100) 
{
  beta_old <- rep(0,dim(X)[2])
  beta_new <- rep(0,dim(X)[2])
  beta_test <- beta_old + stepsize*2*t(X)%*%(y-X%*%beta_old)
  while (iter_max > 0)
  {
    stepsize_current <- stepsize
    fvalue_test <- norm(y-X%*%beta_test)^2
    fvalue_old <- norm(y-X%*%beta_old)^2
    inner_old <- -norm(2*t(X)%*%(y-X%*%beta_old),"2")
    while ( fvalue_test >= fvalue_old - scale_total*scale_total*stepsize_current*inner_old)  
    {
      stepsize_current <- stepsize_current * scale_step
      beta_test <- beta_old + stepsize_current*2*t(X)%*%(y-X%*%beta_old)
      fvalue_test <- norm(y-X%*%beta_test)^2
    }  
    beta_new <- beta_test
    iter_max <- iter_max - 1
  }
  return(beta_new)  
}


