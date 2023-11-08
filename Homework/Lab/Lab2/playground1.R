
g <- function(x,y)
{
  ret <- - x^2 - x^2 * y^2 - 2 * x * y + 2 * x + 2
}
# First derivative
dg <- function(x){
  ret <- matrix(data = c(-2 * x -2 * x * y^2 + 2*x  + 2,-2 * x^2 -2 * x),
                ncol = 1,nrow = 2)
}
# Second derivative
d2g <- function(x){
  ret <- matrix(data = c(-2 * y^2,-4 * x - 2,-4 * x - 2,0),
                ncol = 2,nrow = 2)
}

newton <- function(x0, eps=0.0001){
  xt  <- x0
  xt1 <- x0 + 5

  while(abs(xt-xt1)>eps)
  {
    xt1 <- xt
    xt  <- xt1 -  solve(d2g(xt1)) %*% dg(xt1)
    print(xt) 
  }
  xt
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

#newton_result <- newton(x0 = gx,eps=0.0001)
