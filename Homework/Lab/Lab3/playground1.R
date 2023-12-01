########################## INIT CODE ###########################################
rm(list = ls())

########################## [ 1 a ] #############################################
# Since we need to find a envelope function for the density function, so we need
# to plot the density function first. let $ x \in [-3, 3] $, and we know it's  
# triangle sharped function. We also can plot it using the code below.
# the target function g(x) = 1 in the range [-1,1], otherwise g(x) = 0
# since we set a = 0.5, so e(x) = g / a = 2

x <- seq(-2, 2, by = 0.001)
y <- rep(0, length(x))
z <- rep(0, length(x))

x1_index <- which(x > 1 | x < -1)
x2_index <- which(x >= -1 & x <= 0)
x3_index <- which(x > 0 & x <= 1)

y[x1_index] <- 0
y[x2_index] <- x[x2_index] + 1
y[x3_index] <- 1 - x[x3_index]

z0_index <- which(x > 1 | x < -1)
z1_index <- which(x >= -1 & x <= 1)

z[z1_index] <- 2
z[-z1_index] <- 0

data <- data.frame(x, y, z)

# After we get the plot, we can see that the density function is a triangle
# we will use an envelope function to cover the density function, and we can
# use function $ g(x) = 1 $ to cover the density function in the [-1,1] range. 

graph <- ggplot2::ggplot(data) +
  ggplot2::geom_line(mapping=ggplot2::aes(x=x,y=y)) + 
  ggplot2::geom_line(mapping=ggplot2::aes(x=x,y=z),color="blue") + 
  ggplot2::ggtitle("Density function of X and envelope function") +
  ggplot2::xlab("x") +
  ggplot2::ylab("y,z")


graph

# Now we will write a code for the rejection sampling algorithm
# our g function is g(x) = 1 in the range [-1,1], otherwise g(x) = 0
# our e function is e(x) = 1 / a in the range [-1,1], otherwise e(x) = 0
# we will use a = 0.5 here

generate_random_var_1a <- function(n, a) {
  samples <- numeric(n)
  # Generate a sample from the proposal distribution in [-1,1]
  count <- 1
  e <- 1 / a

  while (count <= n) {

    x <- runif(1, -1, 1)
    # Calculate the acceptance probability
    u <- runif(1)

    # since g will always be 1 in the range [-1,1], so y = 1
    # f1(Y) = 1 - Y and e(Y) = 1 / a = 2
    # f2(Y) = Y + 1 and e(Y) = 1 / a = 2
    # so we have f(Y) / e(Y) = (1 - Y) / 2 or Y+1 / e

    # Calculate the acceptance probability

    if (x > 0 && u <= (1 - x) / e) {
      samples[count] <- x
      count <- count + 1
    }

    if (x <= 0 && u <= (x + 1) / e) {
      samples[count] <- x
      count <- count + 1
    }
  }

  return(samples)
}

a <- 0.5
random_var_1a <- generate_random_var_1a(10000, a)
hist(random_var_1a,freq = FALSE, nclass=100)
hist(random_var_1a,breaks = "Sturges")
var_of_random_var_1a <- var(random_var_1a)
cat("variance of 1st method is",var_of_random_var_1a)
########################## [ 1 b ] #############################################
# To generate a random variable from the density function using Inverse CDF,
# we will need to calculate the CDF function first.

# CDF function

# in range (-inf,-1) we get CDF = 0
# in range (-1,0) we get CDF = \frac{x^2}{2} + x
# in range (0,1)  we get CDF = x - \frac{x^2}{2}
# in range (1,+inf)  we get CDF = 1

# Since -Y has a triangle  distribution on [-1,0], so we have F^{-1} in [-1,0) is x = -1 + sqrt(1-2y)
# Also we have F^{-1} in [0,1] range is x =  1 - sqrt(1-2y)

# The following is the function to generate a random variable from it

generate_random_var_1b <- function(n){
  u <- runif(n)
  x1 <-  - 1 + sqrt(1 - 2 * u)
  x2 <-  1 - sqrt(1 - 2 * u)
  return(c(x1, x2))
}

random_var_1b <- generate_random_var_1b(10000)
hist(random_var_1b)
var_of_random_var_1b <- var(random_var_1b)
cat("variance of 2nd method is",var_of_random_var_1b)
########################## [ 1 c ] #############################################
generate_random_var_1c <- function(n){
  u1_1c <- runif(n)
  u2_1c <- runif(n)
  return(u1_1c - u2_1c)
}

random_var_1c <- generate_random_var_1c(10000)
hist(random_var_1c)
var_of_random_var_1c <- var(random_var_1c)
cat("variance of 3rd method is",var_of_random_var_1c)
########################## [ 1 d ] #############################################
# Since we already plot the data, we will generate the variable of 1a to 1c

# since the 1c is the simplest way to generate the random variable, but since it 
# can not adapt to some specific distribution, for 1b , since we need to calculate 
# CDF and inverse function, in some cases, it's hard to do that, so we will use 
# 1a to generate the random variable.

