########################## INIT CODE ###########################################
rm(list = ls())

########################## [ 2 a ] #############################################
# Function g
g <- function(x, y, b0, b1) {
  ret <- sum(y * log((1 + exp(-b0 - b1 * x))^(-1)) +
               (1 - y) * log(1 -  (1 + exp(-b0 - b1 * x))^(-1)))
  return(ret)
}

# First derivative
dg <- function(x, y, b0, b1) {
  ret <- matrix(data = c(-2 * x - 2 * x * y^2 + 2 * x  + 2, -2 * x^2 - 2 * x),
                ncol = 1, nrow = 2)
  return(ret)
}

steepest_ascent <- function(x,) {

}

################################################################################


########################## [ 2 b ] #############################################
x <- c(0, 0, 0, 0.1, 0.1, 0.3, 0.3, 0.9, 0.9, 0.9)
y <- c(0, 0, 1,   0,   1,   1,   1,   0,   1,   1)
b_init <- c(-0.2, 1)


################################################################################


########################## [ 2 c ] #############################################
b_init <- c(-0.2, 1)
ret1 <- optim(par = b_init, fn = g, method = c("BFGS"),
            lower = -Inf, upper = Inf,control = list())

ret2 <- optim(par = b_init, fn = g, method = c("Nelder-Mead"),
            lower = -Inf, upper = Inf,control = list())

# According to the result, the two methods give the same result.
# compare to the result from 2b, there is no difference.

# The result of the function and the gradient of the function using the 
# parameters calculated by above methods are as follows:

#  |             | b0       | b1      |  g     | g'    |
#  |:---:        |:---:     |:---:    | :---:  |:---:  |
#  | Own method  |          |         |        |       |
#  | BFGS        |          |         |        |       |
#  | Nelder-Mead |          |         |        |       |

# Compare to the result from the output of optim function, we have the same.

#  |             | output   |
#  |:---:        |:---:     |
#  | Own method  |          |
#  | BFGS        |          |
#  | Nelder-Mead |          |

################################################################################


########################## [ 2 d ] #############################################
# We use glm to obtain the optimal parameters. code as below.

# glm(y ~ x, family = binomial(link = "logit"))



################################################################################
