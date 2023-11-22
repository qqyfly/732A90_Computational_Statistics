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
  u1 <- runif(n,0,1)
  x1 <-  log(2 * u1)
  u2 <- runif(n,-1,0)
  x2 <-  - log(-2  * u2)
  return(c(x1, x2))
}

random_var_2a <- generate_random_var_2a(10000)
hist(random_var_2a)



########################## [ 2 b ] #############################################

# Now we will write code for the rejection sampling algorithm
# since \mu = 0 and \lambda = 1, we have
# g(x) = 1/2 * exp(-x) for x >= 0
# g(x) = 1/2 * exp(x) for x < 0

# our e function is e(x) = g(x) / a
# which means e(x) = 1 / (2a) * exp(-x) for x >= 0
# e(x) = 1 / (2a) * exp(x) for x < 0
# we will use a = 0.5 here

# generate a function to show normal distribution

normal_distribution_function <- function(x){
  return(1 / sqrt(2 * pi) * exp(-x^2 / 2))
}

generate_random_var_2b <- function(n, a) {
  samples <- numeric(n)
  # Generate a sample from the proposal distribution in [-1,1]
  count <- 1
  execute_count <- 1

  while (count <= n) {

    x <- runif(1, -1, 1)
    # Calculate the acceptance probability
    u <- runif(1)

    # since g will always be 1 in the range [-1,1], so y = 1
    # f(Y) = \frac{1}{\sqrt{2*pi}} * e^{\frac{1}{2} x^2 } 
    # so we have f(Y) / e(Y) have 2 choices

    # Calculate the acceptance probability
    f_val <- normal_distribution_function(x)

    if (x > 0){
      e <- exp(-x)
      if (u <= (f_val / e)) {
        samples[count] <- x
        count <- count + 1
      }
    } else{
      e <- exp(x)
      if (u <= (f_val / e)) {
        samples[count] <- x
        count <- count + 1
      }
    }
    execute_count <- execute_count + 1
  }
  cat("Average rejection rate: ", (execute_count - count) / execute_count, "\n")
  return(samples)
}

a <- 0.5
random_var_2b1 <- generate_random_var_2b(10000, a)
hist(random_var_2b1)


# Generate 10000 random variables from the distribution using rnorm
random_var_2b2 <- rnorm(2000, 0, 1)
hist(random_var_2b2)
