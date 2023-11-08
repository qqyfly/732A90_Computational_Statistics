
g <- function(x,y)
{
  ret <- - x^2 - x^2 * y^2 - 2 * x * y + 2 * x + 2
  return(ret)
}

x <- seq(-3, 3, by = 0.01)
y <- seq(-3, 3, by = 0.01)

dx <- length(x)
dy <- length(y)

gx <- matrix(rep(NA, dx*dy), nrow=dx,ncol=dy)

for (i in 1:dx){
  for (j in 1:dy)
  {
    gx[i,j] <- g(x[i],y[j])
  }
}

contour(x,y,gx)