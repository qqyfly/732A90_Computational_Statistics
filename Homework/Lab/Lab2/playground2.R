########################## INIT CODE ###########################################
rm(list = ls())

x <- c(0, 0, 0, 0.1, 0.1, 0.3, 0.3, 0.9, 0.9, 0.9)
y <- c(0, 0, 1,   0,   1,   1,   1,   0,   1,   1)

########################## [ 2 a ] #############################################
p <- function(beta, x_i) {
  b0 <- beta[1]
  b1 <- beta[2]
  return(1 / (1 + exp(-b0 - b1 * x_i)))
}

# Function g
g <- function(beta) {
  n <- length(x)
  ret <- 0
  for (i in 1:n){
    p_val <- p(beta, x[i])
    ret <- ret + y[i] * log(p_val) + (1 - y[i]) * log(1 - p_val)
  }
  return(ret)
}

# First derivative
dg <- function(beta) {
  n <- length(x)

  # init ret
  ret <- matrix(data = c(0, 0), ncol = 1)
  tmp <- 0
  for (i in 1:n){
    p_val <- p(beta, x[i])
    tmp <- tmp + y[i] - p_val
  }
  ret[1, 1] <- tmp
  ret[2, 1] <- tmp * x[i]
  return(ret)
}

distance <- function(beta0, beta1) {
  ret <- t(beta1 - beta0) %*% (beta1 - beta0)
  #ret <- g(beta1) - g(beta0)
  return(ret[1][1])
}

# steepest ascent method 1
# beta_init: the initial value
# alpha_init: the learning rate
# eps: the threshold of the gradient
# max_step: the maximum number of steps
# rate: the rate of change of alpha when g not increase

steepest_ascent_1 <- function(beta_init, alpha_init = 1, eps = 1e-10,
                            max_step = 1000, rate = 0.5) {
  beta0 <- beta_init

  for (i in 1:max_step){
    alpha <- alpha_init

    beta1 <- beta0 + alpha * diag(2) %*% dg(beta0)
    criterion <- distance(beta1, beta0)
    dg0 <- dg(beta0)
    while (g(beta1) < g(beta0)) {
      alpha <-  alpha * rate
      beta1 <- beta0 + alpha * diag(2) %*% dg0
      print(alpha)
    }
    if (criterion < eps) {
      cat("Converged after ", i, " steps\n")
      return(beta1)
    }
    beta0 <- beta1
  }

  # after max_step steps, we still cannot get the qualified result
  cat("Did not converge after", max_step, " steps\n")
  return(NULL)
}


# steepest ascent method 2
# beta_init: the initial value
# alpha_init: the learning rate
# eps: the threshold of the gradient
# max_step: the maximum number of steps
# rate: the rate of change of alpha when g not increase

steepest_ascent_2 <- function(beta_init, alpha_init = 1, eps = 1e-10,
                            max_step = 1000, rate = 0.5) {
  beta0 <- beta_init
  alpha <- alpha_init
  for (i in 1:max_step){
    beta1 <- beta0 + alpha * diag(2) %*% dg(beta0)
    criterion <- distance(beta1, beta0)
    dg0 <- dg(beta0)
    while (g(beta1) < g(beta0)) {
      alpha <-  alpha * rate
      beta1 <- beta0 + alpha * diag(2) %*% dg0
    }
    if (criterion < eps) {
      cat("Converged after ", i, " steps\n")
      return(beta1)
    }
    beta0 <- beta1
  }

  # after max_step steps, we still cannot get the qualified result
  cat("Did not converge after", max_step, " steps\n")
  return(NULL)
}

################################################################################


########################## [ 2 b ] #############################################
beta_init <- c(-0.2, 1)

# First method not convergence, everytime we reset the alpha to 1
steepest_ascent_1(beta_init)

# Second method convergence, we don't reset the alpha
# result is  beta = (0.0169513,1.1952562)
steepest_ascent_2(beta_init)

################################################################################


########################## [ 2 c ] #############################################
# According to the result, the two methods give the same result.
# compare to the result from 2b, there is no difference.

# Use optim function (BFGS) to find the optimal parameters
result_bfgs <- optim(par = beta_init, g, dg, method = 'BFGS', 
                     hessian = TRUE, control = list(fnscale = -1))


# Use optim function () to find the optimal parameters
result_nelder_mead <- optim(par = beta_init, g, dg, method = 'Nelder-Mead',
                            hessian = TRUE, control = list(fnscale = -1))


# The result of the function and the gradient of the function using the 
# parameters calculated by above methods are as follows:

#  |             | b0         | b1        |  g      | g'                           |
#  |:---:        |:---:       |:---:      | :---:   |:---:                         |
#  | Own method  |0.0169513   |1.1952562  |-6.484974|(-0.01309331, -0.01178398)    |
#  | BFGS        |0.01248138  |1.19123325 |-6.485018|(-1.610715e-07, -1.449644e-07)|
#  | Nelder-Mead |-0.009423433|1.262738266|-6.484279|(0.0002044366, 0.0001839929)  |
#  | glm         |-0.00936    |1.26282    |-6.484279|(2.714971e-06, 2.443474e-06)  |

################################################################################

########################## [ 2 d ] #############################################
# We use glm to obtain the optimal parameters. code as below.
x_new <- cbind(1, x)
data <- data.frame(x = x_new, y = y)
fit <- glm(y ~ x, family = binomial(link = 'logit'), data = data)
summary(fit)
################################################################################

# Compare to the result from the output of optim function, all the results are
# near (0, 1.2)

#  |             | output                     |
#  |:---:        |:---:                       |
#  | Own method  | (0.0169513,1.1952562)      |
#  | BFGS        | (0.01248138,1.19123325)    |
#  | Nelder-Mead | (-0.009423433,1.262738266) |
#  | glm         | (-0.00936,1.26282)         |
