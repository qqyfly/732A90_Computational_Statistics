########################## INIT CODE ###########################################
rm(list = ls())

########################## [ 1 a ] #############################################
# The gradient and the Hessian matrix is as follows
# Gradient of g = (-2x - 2xy^2 - 2y + 2, -2x^2y - 2x)
# Hessian of g = (-2y^2 - 2, -4xy - 2, -4xy - 2, -2x^2)

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
  ret <- matrix(data = c(-2 * y^2 - 2, -4 * x * y - 2,
                         -4 * x * y - 2, -2 * x^2),
                ncol = 2, nrow = 2)
  return(ret)
}

# Draw contour plot
n <- 3
x <- seq(-n, n, by = 0.01)
y <- seq(-n, n, by = 0.01)
dx <- length(x)
dy <- length(y)
gx <- matrix(rep(NA, dx * dy), nrow = dx, ncol = dy)
gx <- sapply(y, function(i) sapply(x, function(j) g(i, j)))

contour(x, y, gx, nlevels = 7, col = "blue")
################################################################################


########################## [ 1 b ] #############################################
# define distance of two points
distance <- function(xt1, xt0) {
  ret <- t(xt1 - xt0) %*% (xt1 - xt0)
  return(ret[1][1])
}

# newton function with eps = 1e-8 and max_step = 1000
# x0 is the initial value
newton <- function(x0, eps = 1e-8, max_step = 1000) {
  xt0  <- x0
  for (i in 1:max_step){
    xt1  <- xt0 - solve(d2g(xt0[1], xt0[2])) %*% dg(xt0[1], xt0[2])
    criterion <- distance(xt1, xt0)
    # we get the qualified result
    if (criterion < eps) {
      cat("Converged after ", i, " steps\n")
      return(xt1)
    }
    xt0 <- xt1
  }
  # after max_step steps, we still cannot get the qualified result
  cat("Did not converge after", max_step, " steps\n")
}
################################################################################


########################## [ 1 c ] #############################################
# let's use (2,0),(-1,2),(0,-1) and (0,2) as initial values.
# and we get the following results using the newton function above.

# we use the default and max_step.
newton_value_1 <- newton(matrix(c(2, 0), nrow = 2, ncol = 1))
newton_value_2 <- newton(matrix(c(-1, 2), nrow = 2, ncol = 1))
newton_value_3 <- newton(matrix(c(0, -1), nrow = 2, ncol = 1))
newton_value_4 <- newton(matrix(c(0, 2), nrow = 2, ncol = 1))

# We find the following results.
# (1,-1),(7.022621e-12,1),(0,1),(0,1)

# It seems that we have 2 points (1,-1)(init point 1) and (0,1)(init point 2-4)

# we can calculate the Gradient vector and Hessian matrix of g at these points
# using dg and d2g function defined above. The result is as follows.

gradient_1 <- dg(1, -1)
gradient_2 <- dg(-0, 1)

hessian_1 <-  d2g(1, -1)
hessian_2 <-  d2g(0, 1)

check_type <- function(hessian_matrix) {
  eigenvalues <- eigen(hessian_matrix)$values
  if (all(eigenvalues > 0)) {
    cat("Local minimum\n")
  } else if (all(eigenvalues < 0)) {
    cat("Local maximum\n")
  } else {
    cat("Saddle point\n")
  }
}
check_type(hessian_1)
check_type(hessian_2)

#  |  Point | Gradient | Hessian             |  Type        |
#  |:---:   |:---:     |:---:                | :---:        |
#  |(1, -1) | 0 & 0    |  -4 & 2  \\ 2  & -2 | Local maximum|
#  |(0, 1)  | 0 & 0    |  -4 & -2 \\ -2 & 0  | Saddle point |

# According to the table above and the contour plot, we can find that
# point (0,1) are the global maximum if x,y \in [-3, 3] range.
################################################################################

########################## [ 1 d] #############################################
# The advantages of the steepest ascent algorithm is that it is easy to
# calculate without need to consider about the second derivative of the
# function. But the steepest ascent algorithm is not as efficient as the
# Newton method.

# Newton method, can get the answer much faster than other methods,however, it
# cannot guaranteed that g(x) will increase every single step. And we have to
# calculate the second derivative of the function. Another thing we need to
# mention is that we have to choose a proper initial value to get a convergent
# result. If we choose a bad initial value, we may get a diverge result since
# the Newton method is sensitive to the initial value.

################################################################################
