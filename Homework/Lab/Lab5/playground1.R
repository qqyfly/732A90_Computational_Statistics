
### Init Code
rm(list = ls())
library(ggplot2)

### (1.1)

data <- read.csv("./Homework/Lab/Lab5/lottery.csv",sep = ";")

# Remove Month column since there is a Mo.Number column which is same as Month
data <- data[-2]

ggplot(data, aes(x = Draft_No, y = Day_of_year)) + geom_point()

# according to the plot, the pattern is not very clear.

### (1.2)
# fit the data using linear regression
fit1_2_1 <- lm(Day_of_year ~ Draft_No, data = data)
summary(fit1_2_1)

# We got
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 224.98148   10.81167  20.809  < 2e-16 ***
#Draft_No     -0.22602    0.05105  -4.427 1.26e-05 ***

# fit using loess function 
fit1_2_2 <- loess(Day_of_year ~ Draft_No, data = data, se=TRUE)
summary(fit1_2_2)

# Output:
# Number of Observations: 366 
# Equivalent Number of Parameters: 4.34 
#Residual Standard Error: 103.5 
#Trace of smoother matrix: 4.73  (exact)

#Control settings:
#  span     :  0.75 
#  degree   :  2 
#  family   :  gaussian
#  surface  :  interpolate         cell = 0.2
#  normalize:  TRUE
# parametric:  FALSE
#drop.square:  FALSE

# according to the output , we drew the following conclusion:

# TODO: add some comments here.

### (1.3)

### (1.4)

### (1.5)
