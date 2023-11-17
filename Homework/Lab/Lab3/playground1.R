########################## INIT CODE ###########################################
rm(list = ls())

########################## [ 1 a ] #############################################
# Since we need to find a envelope function for the density function, so we need
# to plot the density function first. let $ x \in [-3, 3] $, and we know it's  
# triangle sharped function. We also can plot it using the code below.

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

z[z1_index] <- 1
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

# Now we will write a code for the rejection sampling algorithm. 


########################## [ 1 b ] #############################################
# To generate a random variable from the density function using Inverse CDF,
# we will need to calculate the CDF function first.

# CDF function

# in range (-inf,-1) we get CDF = 0
# in range (-1,0) we get CDF = \frac{x^2}{2} + x
# in range (0,1)  we get CDF = x - \frac{x^2}{2}
# in range (1,+inf)  we get CDF = 1

# we have F^{-1} in (-1,0) range is x = -1 + sqrt(1+2y)
# we have F^{-1} in ( 0,1) range is x =  1 - sqrt(1-2y)

# so we can write a function to generate a random variable from it
# For x2, sqrt will generate some error because of the negative value

u <- runif(10000)
x1 <- -1 + sqrt(1 + u)
x2 <- 1 - sqrt(1-u)

random_var <- c(x1,x2)
hist(random_var)

# So the random variable is generated from the density function is random_var.
########################## [ 1 c ] #############################################

u1_1c <- runif(10000)
u2_1c <- runif(10000)

random_var_1c <- u1_1c - u2_1c
hist(random_var_1c)

########################## [ 1 d ] #############################################
# Since we already plot the data, we will generate the variable of 1a to 1c

random_var_1c_var <- var(random_var_1c)




