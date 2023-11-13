########################## INIT CODE ###########################################
rm(list = ls())

########################## [ 2 a ] #############################################
# Function g
g <- function(beta) {
  #p <- 1 / (1 + exp(-x %*% beta))
  #ret <- sum(y * log(p) + (1 - y) * log(1 - p))
  n <- length(y)
  b0 <- beta[1]
  b1 <- beta[2]

  ret <- 0

  for(i in 1:n){
    ret <- ret + y[i] * log(1 / (1 + exp(-b0 - b1 * x[i]))) + 
               (1 - y[i]) * log(1 - 1 / (1 + exp(-b0 - b1 * x[i])))
  }
  return(ret)
}

# First derivative
dg <- function(beta) {
  n <- length(y)
  b0 <- beta[1]
  b1 <- beta[2]
  
  ret <- matrix(data = c(0, 0), ncol = 1)

  for(i in 1:n){
    tmp <- y[i] - 1 / (1 + exp(-b0 - b1 * x[i]))
    
    y[i] * log(1 / (1 + exp(-b0 - b1 * x[i]))) + 
               (1 - y[i]) * log(1 - 1 / (1 + exp(-b0 - b1 * x[i])))
  }

  tmp <- sum((y - 1 / (1 + exp(-b0 - b1 * x))))
  tmp1 <- matrix(data = x,ncol =   ret <- matrix(data = c(tmp,ncol = 1)
  return(ret)
}

steepest_ascent <- function(beta, alpha = 1, eps = 1e-10, max_step = 1000,
                            rate = 0.5) {

  n <- length(y)
  x <- cbind(1, X)
  beta <- rep(0, ncol(x))

  for(i in 1:max_step){
      p <- 1 / (1 + exp(-x %*% beta))
      gradient <- t(x) %*% (y - p)
      learning_rate <- alpha

      while (sum(y * log(p) + (1 - y) * log(1 - p)) < 
             sum(y * log(1 / 2) + (1 - y) * log(1 / 2) + 
                 learning_rate * alpha * sum(gradient^2))) {
        learning_rate <- rate * learning_rate
        p <- 1 / (1 + exp(-X %*% (theta + learning_rate * gradient)))
      }

    # Update the coefficients
    theta <- theta + learning_rate * gradient
    
    # Check for convergence
    if (max(abs(gradient)) < eps) {
      break
    }
    
  }

  return(beta)
}

################################################################################


########################## [ 2 b ] #############################################
x <- c(0, 0, 0, 0.1, 0.1, 0.3, 0.3, 0.9, 0.9, 0.9)
y <- c(0, 0, 1,   0,   1,   1,   1,   0,   1,   1)
data <- data.frame(x, y)



################################################################################


########################## [ 2 c ] #############################################
# According to the result, the two methods give the same result.
# compare to the result from 2b, there is no difference.

# set init point
beta_init <- c(-0.2, 1)

# Use optim function (BFGS) to find the optimal parameters

result_bfgs <- optim(par = beta_init, g,dg, method = 'BFGS')


# Use optim function () to find the optimal parameters
result_bfgs <- optim(par = beta_init, fn = g, x = x, y = y, method = 'NELDER-MEAD')


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

# fit <- glm(y ~ x, family = binomial(link = 'logit'), data = data)
# summary(fit)
################################################################################
