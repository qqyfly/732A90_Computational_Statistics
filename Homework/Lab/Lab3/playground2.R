########################## INIT CODE ###########################################
rm(list = ls())

########################## [ 2 a ] #############################################
# We have the function

#$$
# DE(\mu,\lambda) = \frac{\lambda}{2} exp(-\lambda|x-\mu|)
#$$

# Calculate the integral of the function on x to get CDF
# since \mu = 0 and \lambda = 1, we have
# \frac{1}{2} exp(-1|x|)
# since we have a absolute value, we need to split the integral into two parts
# f(x) = 1/2 * exp(-x) for x >= 0
# f(x) = 1/2 * exp(x) for x < 0

#$$
# F(x) = - \frac{1}{2}exp(-x) if x >= 0
# F(x) = \frac{1}{2}exp(x) if x < 0
#$$

# Calculate the inverse function of CDF
#$$
# F^{-1}(y) = log(2y)
# F^{-1}(y) = -log(-2y)
#$$

generate_random_var_2a <- function(n){
  u <- runif(n)
  x1 <-  log(2 * u)
  x2 <-  - log(-2 * u)
  return(c(x1, x2))
}

random_var_2a <- generate_random_var_2a(10000)
hist(random_var_2a)

# Comment on the result

########################## [ 2 b ] #############################################


# Generate 2000 random variables from the distribution using rnorm
random_var_2b2 <- rnorm(2000, 0, 1)
hist(random_var_2b2)
