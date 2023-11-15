########################## INIT CODE ###########################################
rm(list = ls())

########################## [ 1 a ] #############################################
# Since we need to find a envelope function for the density function, so we need
# to plot the density function first. let $ x \in [-3, 3] $

x <- seq(-2, 2, by = 0.001)
y <- rep(0, length(x))

x1_index <- which(x > 1 | x < -1)
x2_index <- which(x >= -1 & x <= 0)
x3_index <- which(x > 0 & x <= 1)

y[x1_index] <- 0
y[x2_index] <- x[x2_index] + 1
y[x3_index] <- 1 - x[x3_index]

data <- data.frame(x, y)

ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_line() +
  ggplot2::ggtitle("Density function of X") +
  ggplot2::xlab("x") +
  ggplot2::ylab("y")

# After we get the image, we can see that the density function is a triangle


########################## [ 1 b ] #############################################
