# EM algorithm

library(ggplot2)

# Question 1(DONE)

# Load data
data <- read.csv("./Homework/Lab/Lab6/censoredproc.csv", 
                sep = ";", header = TRUE)

# Since we don't use the failure row, so all the rows with cens = 2 are filtered out
filtered_data <- data[data$cens != 2,]

layout(matrix(c(1:2), 1, 2))

# plot the data
hist(data$time, breaks = 100, main="Hist of filtered data")

# plot the filtered data
hist(filtered_data$time, breaks = 100, main="Hist of filtered data")

# TODO: comment:
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

# Function to compute the E-step
estep <- function(lambda, x, c) {
  return(lambda / c * exp(-lambda * x))
}

# Function to compute the M-step
mstep <- function(lambda, x, c) {
  return(sum(x) / sum(c * exp(-lambda * x)))
}

# EM algorithm
em_algorithm <- function(initial_lambda, observed_data, truncation_point, max_iter = 100, tol = 1e-3) {
  lambda_current <- initial_lambda
  
  for (iter in 1:max_iter) {
    # E-step
    expected_values <- estep(lambda_current, observed_data, truncation_point)
    
    # M-step
    lambda_next <- mstep(lambda_current, observed_data, expected_values)
    
    # Check for convergence
    if (abs(lambda_next - lambda_current) < tol) {
      break
    }
    
    # Update lambda for the next iteration
    lambda_current <- lambda_next
  }
  
  return(list(lambda = lambda_current, iterations = iter))
}

# Example usage
set.seed(123)
observed_data <- rexp(100, rate = 0.5)  # Simulated truncated exponential data
truncation_point <- 2

# Initial guess for lambda
initial_lambda <- 0.5

# Run EM algorithm
result <- em_algorithm(initial_lambda, observed_data, truncation_point)

# Print the result
cat("Estimated lambda:", result$lambda, "\n")
cat("Number of iterations:", result$iterations, "\n")
