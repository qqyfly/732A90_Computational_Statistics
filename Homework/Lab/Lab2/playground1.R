########################## [ 1 a ] #############################################
# The gradient and the Hessian matrix is as follows
# Gradient of g = (-2x - 2xy^2 + 2x + 2, -2x^2 - 2x)
# Hessian of g = (-2y^2, -4x - 2, -4x - 2, 0)

# Function g
g <- function(x, y) {
  ret <- -x^2 - x^2 * y^2 - 2 * x * y + 2 * x + 2
  return(ret)
}

# First derivative
dg <- function(x, y) {
  ret <- matrix(data = c(-2 * x - 2 * x * y^2 - 2 * y  + 2, 
                         -2 * x^2 * y - 2 * x),
                ncol = 1, nrow = 2)
  return(ret)
}
# Second derivative
d2g <- function(x, y) {
  ret <- matrix(data = c(-2 * y^2 + 2, -4 * x * y - 2,
                         -4 * x * y - 2, -2 * x^2),
                ncol = 2, nrow = 2)
  return(ret)
}

# Contour plot using the following code.
n <- 3
x <- seq(-n, n, by = 0.01)
y <- seq(-n, n, by = 0.01)
dx <- length(x)
dy <- length(y)
gx <- matrix(rep(NA, dx * dy), nrow = dx, ncol = dy)

#gx <- sapply(1:length(x))
#for (i in 1:dx){
#  for (j in 1:dy){
#    gx[i, j] <- g(x[i], y[j])
#  }
#}

gx <- sapply(y, function(i) sapply(x, function(j) g(i, j)))

#sapply(1:length(x), function(i) func(x[i], y[i]))
contour(x, y, gx, nlevels = 7, col = "blue")
################################################################################


########################## [ 1 b ] #############################################
# we can get a local maximum of g by using the following code.

distance <- function(xt1, xt0) {
  ret <- t(xt1 - xt0) %*% (xt1 - xt0)
  return(ret[1][1])
}

newton <- function(x0, eps = 1e-10, max_step = 1000) {
  xt0  <- x0
  xt1 <- x0 + 5
  step <- 1
  criterion <- 100
  while (criterion > eps) {
    if (step > max_step) {
      break
    }else {
      xt1  <- xt0 - solve(d2g(xt0[1], xt0[2])) %*% dg(xt0[1], xt0[2])
      #old_criterion <- criterion 
      criterion <- distance(xt1, xt0)

      cat(criterion, "->")

      xt0 <- xt1
      step <- step + 1
    }
  }

  if (step > max_step) {
    print("The maximum step is reached.")
    return(NA)
  }
  return(xt1)
}
################################################################################


########################## [ 1 c ] #############################################
# let's use (2,0),(-1,2),(0,-1) and (0,2) as initial values.
# and we get the following results.

# we use the default eps = 0.0001 and max_step here.
newton_value_1 <- newton(matrix(c(2, 0), nrow = 2, ncol = 1))
newton_value_2 <- newton(matrix(c(-1, 2), nrow = 2, ncol = 1))
newton_value_3 <- newton(matrix(c(0, -1), nrow = 2, ncol = 1))
newton_value_4 <- newton(matrix(c(0, 2), nrow = 2, ncol = 1))

# We can find the following results.

# It seems that not every initial values will lead to converge.

# Since our newton_value_1 to newton_value_4 are not NA, we can calculate the
# Gradient vector and Hessian matrix of g at these points listed as table below.

#  |        | Gradient | Hessian           |  Type  |
#  |:---:   |:---:     |:---:              |        |
#  |(2, 0)  | -2 \\ -4 |  2 & -2 \\ -2 & 8 |        |
#  |(-1, 2) | 8 \\ -2  |  -6 & 6 \\ 6 & -2 |        |
#  |(0, -1) | 4 \\ 0   |  0 & -2 \\ -2 & 0 |        |
#  |(0, 2)  | -2 \\ 0  |  -6 & -2\\ -2 & 0 |        |

# According to the table above and the contour plot, we can find that
# point (0,1) are the global maximum if x,y \in [-3, 3] range.
################################################################################

########################## [ 1 d] #############################################
# The advantages of the steepest ascent algorithm is that it is easy to
# calculate without need to consider about the Second derivative of the
# function. However, the steepest ascent algorithm is not as efficient as the
# newton method. If we implement it using newton method, with hessian matrix,
# we can find the most steepest route to the maximum point from the start point.
################################################################################
