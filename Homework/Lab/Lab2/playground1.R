########################## [ 1 a ] #############################################
# The gradient and the Hessian matrix is as follows
# Gradient of g = (-2x - 2xy^2 + 2x + 2, -2x^2 - 2x)
# Hessian of g = (-2y^2, -4x - 2, -4x - 2, 0)

# Function g
g <- function(x, y) {
  ret <- - x^2 - x^2 * y^2 - 2 * x * y + 2 * x + 2
  return(ret)
}

# First derivative
dg <- function(x) {
  ret <- matrix(data = c(-2 * x - 2 * x * y^2 + 2 * x  + 2, -2 * x^2 - 2 * x),
                ncol = 1, nrow = 2)
  return(ret)
}
# Second derivative
d2g <- function(x) {
  ret <- matrix(data = c(-2 * y^2, -4 * x - 2, -4 * x - 2, 0),
                ncol = 2, nrow = 2)
  return(ret)
}

# Contour plot using the following code.
x <- seq(-3, 3, by = 0.01)
y <- seq(-3, 3, by = 0.01)
dx <- length(x)
dy <- length(y)
gx <- matrix(rep(NA, dx * dy), nrow = dx, ncol = dy)

for (i in 1:dx){
  for (j in 1:dy){
    gx[i, j] <- g(x[i], y[j])
  }
}
contour(x, y, gx)
################################################################################


########################## [ 1 b ] #############################################
# we can get a local maximum of g by using the following code.

newton <- function(x0, eps = 0.0001, max_step = 1000) {
  xt  <- x0
  xt1 <- x0 + 5
  step <- 1
  while (base::abs(xt - xt1) > eps) {
    if (step > max_step) {
      break
    }
    xt1 <- xt
    xt  <- xt1 -  solve(d2g(xt1)) %*% dg(xt1)
  }
  if (step > max_step) {
    return(NA)
  }
  return(xt)
}
################################################################################


########################## [ 1 c ] #############################################
# let's use (2,0),(-1,2),(0,-1) and (0,2) as initial values.
# and we get the following results.

# we use the default eps = 0.0001 and max_step here.
newton_value_1 <- newton(c(2, 0))
newton_value_2 <- newton(c(-1, 2))
newton_value_3 <- newton(c(0, -1))
newton_value_4 <- newton(c(0, 2))

# We can find the following results.

# Since our newton_value_1 to newton_value_4 are not NA, we can calculate the
# Gradient vector and Hessian matrix of g at these points listed as table below.

#  |        | Gradient | Hessian |  Type  |
#  |:---:   |:---:     |:---:    |        |
#  |(2, 0)  |          |         |        |
#  |(-1, 2) |          |         |        |
#  |(0, -1) |          |         |        |
#  |(0, 2)  |          |         |        |

# According to the table above and the contour plot, we can find that
# () are the global maximum if x,y \in [-3, 3] range.
################################################################################

########################## [ 1 d] #############################################
# The advantages of the steepest ascent algorithm is that it is easy to
# calculate without need to consider about the Second derivative of the
# function. However, the steepest ascent algorithm is not as efficient as the
# newton method. If we implement it using newton method, with hessian matrix,
# we can find the most steepest route to the maximum point from the start point.
################################################################################
