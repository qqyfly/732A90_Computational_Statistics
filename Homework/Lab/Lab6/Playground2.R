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
df <- data.frame(value = data$time)

histogram <- ggplot(df, aes(x = value)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Truncated Exponential Data", x = "Value") +
  theme_minimal()

# Add density curve
density_curve <- histogram +
  stat_function(fun = function(x) truncated_exp_density(x, estimated_lambda, truncation_point), 
                color = "red", linewidth = 1) +  # Use 'linewidth' instead of 'size'
  labs(title = "Histogram with Truncated Exponential Density Curve")

# Display the plot
print(density_curve)

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
em_algorithm <- function(initial_lambda, observed_data, truncation_point, max_iter = 100, tol = 0.001) {
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

set.seed(12345)
truncation_point <- 2

# Initial guess for lambda
initial_lambda <- 100

# Run EM algorithm
result <- em_algorithm(initial_lambda, filtered_data, truncation_point)

# Print the result
cat("Estimated lambda:", result$lambda, "\n")
cat("Number of iterations:", result$iterations, "\n")

estimated_lambda <- result$lambda

truncated_exp_density <- function(x, lambda, c) {
  return(lambda * exp(-lambda * x) / (1 - exp(-lambda * c)))
}

# Add density curve
x_values <- seq(0, 5, length.out = 100)
curve(truncated_exp_density(x, estimated_lambda, truncation_point), col = "red", lwd = 2, add = FALSE, from = 0, to = 5)
# Display the plot
print(density_curve)

hist(filtered_data, breaks = 20, col = "lightblue", main = "Histogram of Truncated Exponential Data", xlab = "Value")

# Add density curve
curve(truncated_exp_density(x, estimated_lambda, truncation_point), col = "red", lwd = 2, add = TRUE, from = 0, to = 5)

# Function to calculate MLE for truncated exponential distribution
calculate_mle <- function(data, c) {
  n <- length(data)
  lambda_hat <- n / sum(data)
  return(lambda_hat)
}

# Function to simulate data, randomly censor, and estimate lambda
simulate_and_estimate <- function(observed_data, truncation_point, num_simulations) {
  results <- vector("list", length = num_simulations)

  for (i in 1:num_simulations) {
    # Record start time for each iteration
    start_time_iter <- Sys.time()

    # (a) Simulate data from exponential distribution
    simulated_data <- rexp(length(observed_data), rate = estimated_lambda)

    # (b) Randomly censor observations
    censoring_times <- runif(length(observed_data), min = 0, max = truncation_point)
    censored_data <- ifelse(simulated_data <= censoring_times, simulated_data, NA)

    # (c) Estimate lambda using EM algorithm
    em_result <- em_algorithm(initial_lambda, censored_data, truncation_point)
    lambda_em <- em_result$lambda

    # Estimate lambda using MLE based on uncensored observations
    lambda_mle <- calculate_mle(simulated_data[!is.na(censored_data)], truncation_point)

    # Record end time for each iteration
    end_time_iter <- Sys.time()

    # Store results and timing information
    results[[i]] <- list(lambda_em = lambda_em, lambda_mle = lambda_mle, 
                         time_taken = end_time_iter - start_time_iter)
  }

  return(results)
}

# Set the number of simulations
num_simulations <- 1000

# Record start time
start_time <- Sys.time()

# Perform simulations and estimates
simulation_results <- simulate_and_estimate(filtered_data, truncation_point, num_simulations)

# Record end time
end_time <- Sys.time()

# Print the results
cat("Total Time:", end_time - start_time, "\n")
