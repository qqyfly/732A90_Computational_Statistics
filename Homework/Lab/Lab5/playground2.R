
### Init Code
rm(list = ls())
library(ggplot2)
library(boot)

################################## (2.1) #######################################
# DONE

data <- read.csv("./Homework/Lab/Lab5/prices1.csv", sep = ";")

g_2_1 <- ggplot(data, aes(x = SqFt, y = Price)) + geom_point()
g_2_1

# fit a linear regression model
fit2_1_lm <- lm(Price ~ SqFt, data = data)
summary(fit2_1_lm)

# output
#Call:
#lm(formula = Price ~ SqFt, data = data)

#Residuals:
#     Min       1Q   Median       3Q      Max 
#-1041.43   -99.12     1.99    59.26   751.43 

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 69.30663   65.13461   1.064     0.29    
#SqFt         0.60457    0.03713  16.282   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

minimal_x <- min(data$SqFt)
maximum_x <- max(data$SqFt)

x_2_1_lm <- seq(minimal_x, maximum_x, 0.1)
y_2_1_lm <- predict(fit2_1_lm, newdata = data.frame(SqFt = x_2_1_lm))

data_2_1 <- data.frame(SqFt = x_2_1_lm, Price = y_2_1_lm)

g_2_1 + geom_line(data = data_2_1, aes(x = x_2_1_lm, y = y_2_1_lm), color = "red") 

# fitting this model using a straight line is not too bad,but not perfect.
# since at the bottom of the plot, more points are here and on the top of 
# the plot, less points are here.

################################## (2.2) #######################################
# We will try a new model
# Price = b + a_{1} * SqFt + a_{2} * (SqFt −c)1_{SqFt>c}

# Fit the model using optim function
# We will use the least square method to fit the model

# define the function
# x is the vector of parameters
# x[1] is b
# x[2] is a_{1}
# x[3] is a_{2}
# x[4] is c
# y is the vector of response
# x is the vector of predictor
# n is the number of observations
# return the sum of square of residuals
f_2_2 <- function(x, y, x, n) {
    b <- x[1]
    a_1 <- x[2]
    a_2 <- x[3]
    c <- x[4]
    sum <- 0
    for (i in 1:n) {
        sum <- sum + (y[i] - b - a_1 * x[i] - a_2 * (x[i] - c) * (x[i] > c))^2
    }
    return(sum)
}

# fit the model
# we will use the optim function to find the parameters

# init the parameters
x_2_2 <- c(0, 0, 0, 0)
# init the control parameters
control <- list(fnscale = -1, maxit = 1000)
# fit the model
fit2_2 <- optim(x_2_2, f_2_2, y = data$Price, x = data$SqFt, n = length(data$Price), control = control)


################################## (2.3) #######################################

################################## (2.4) #######################################

################################## (2.5) #######################################
