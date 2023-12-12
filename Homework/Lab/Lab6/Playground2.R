# EM algorithm

library(ggplot2)


# Question 1(DONE)

# Load data
data <- read.csv("./Homework/Lab/Lab6/censoredproc.csv", 
                sep = ";", header = TRUE)



# Since we don't use the failure row, so all the rows with cens = 1 are filtered
left_data <- data[data$cens == 2,]

layout(matrix(c(1:2), 1, 2))

# plot the data
hist(data$time, breaks = 100)

# plot the left data
hist(left_data$time, breaks = 100)

# comment:
# Accoridng to the plots , we can see that the left_data get sparser and not
# converge to 0,  as what we saw at the plot 1.

# Question 2 & 3

# Since the first one is come from an exponential distribution, and second plot 
# from a truncated exponential distribution. 

# according to the page:
# https://www.statlect.com/fundamentals-of-statistics/exponential-distribution-maximum-likelihood

# we should know the formula 


# question 4 
# and according to the code that provided by the teacher, we can copy it and then
# modify it to fit our data.

# Question 5
# Plot the density curve of the exp(ˆλ)  over plot 1

# question 6
# use se a parametric bootstrap to implement a function and then compare it with
# the data we generated, and then write some comment.

