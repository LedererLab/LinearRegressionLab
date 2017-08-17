
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

leastsquares_gd_bt <- function(y,X,stepsize = 1,scale_step = 0.8,scale_total = 0.5,iter_max = 100) 
{
  beta_new <- rep(0,dim(X)[2])
  while (iter_max > 0)
  {
    beta_old <- beta_new
    
    fvalue_old <- norm(y-X%*%beta_old,"2")^2
    inner_old <- -norm(2*t(X)%*%(y-X%*%beta_old),"2")
    
    beta_new <- beta_old + stepsize*2*t(X)%*%(y-X%*%beta_old)
    fvalue_new <- norm(y-X%*%beta_new,"2")^2
    
    stepsize_current <- stepsize
    while ( fvalue_new > fvalue_old + scale_total*scale_total*stepsize_current*inner_old)  
    {
      stepsize_current <- stepsize_current * scale_step
      beta_new <- beta_old + stepsize_current*2*t(X)%*%(y-X%*%beta_old)
      fvalue_new <- norm(y-X%*%beta_new,"2")^2
    }  

    iter_max <- iter_max - 1
    
  }
  return(beta_new)  
}

leastsquares_cd <- function(y,X,stepsize = 0.01,iter_max = 100) 
{
  beta <- rep(0,dim(X)[2])
  while (iter_max > 0)
  {
    j <- 1
    while (j <= dim(X)[2])
    {  
      beta[j] <- beta[j] + stepsize*2*(t(X)%*%(y-X%*%beta))[j]
      j <- j + 1
    }
    iter_max <- iter_max - 1
  }
  return(beta)  
}


