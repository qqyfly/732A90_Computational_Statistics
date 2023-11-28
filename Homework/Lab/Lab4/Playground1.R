########################## INIT CODE ###########################################
rm(list = ls())
library(ggplot2)
library(fitdistrplus)

set.seed(12345)

########################## 1a ##################################################
# DONE

fx <- function(x) {
  return(ifelse(x <= 0, 0, x^5 * exp(-x)))
}

metropolis_hastings_log_normal_1a <- function(n) {
  samples <- rep(0, n)

  # sample from a log-normal distribution
  xt <- rlnorm(1, 0, 1)
  samples[1] <- xt

  # set some initial values
  count <- 1
  accepted_count <- 0

  while (count <= n) {
    # sample a candidate from log normal distribution
    x_star <- rlnorm(1, log(xt), 1)

    # calc a ratio
    mh_ratio <- (fx(x_star) * dlnorm(xt, log(x_star), 1)) /
                (fx(xt) * dlnorm(x_star, log(xt), 1))


    # accept according to the ratio
    if (mh_ratio > 1) {
      xt_plus <- x_star
      accepted_count <- accepted_count + 1
    } else {
      u <- runif(1)
      if (u < mh_ratio) {
        xt_plus <- x_star
        accepted_count <- accepted_count + 1
      } else {
        xt_plus <- xt
      }
    }

    samples[count] <- xt_plus
    count <- count + 1
    xt <- xt_plus
  }
  return(list(random_variable = samples, acceptance_rate = accepted_count / n))
}

sample_number <- 10000
result_1a <- metropolis_hastings_log_normal_1a(sample_number)

data_set_1a <- data.frame(x = 1:sample_number, y = result_1a$random_variable)

result_1a_sample <- result_1a$random_variable

ggplot(data = data_set_1a, mapping = aes(x = x, y = y)) +
  geom_line() +
  ggtitle("x vs y") +
  xlab("x") +
  ylab("y")

# according to the result, the convergence of the chain seems not converge to 
# a fixed value.

# According to the graph, there does not have a significant burn-in period.From
# the beginning, the chain seems very stable.

# The acceptance rate is 0.445
result_1a_acceptance_rate <- result_1a$acceptance_rate
cat("acceptance rate is", result_1a_acceptance_rate, "\n")

# plot a histogram of the 1a samples
hist(result_1a_sample)
mean(result_1a_sample)
########################## 1b ##################################################
# we use chi-square distribution X^2(|x+1|) as the proposal distribution

metropolis_hastings_chi_square_1b <- function(n) {
  samples <- rep(0, n)

  # sample from a chi-square distribution
  xt <- rchisq(n = 1, df = 1)
  samples[1] <- xt

  # set some initial values
  count <- 1
  accepted_count <- 0

  while (count <= n) {
    # sample a candidate from log normal distribution
    x_star <- rchisq(n = 1, df = floor(xt + 1))

    # calc a ratio

    mh_ratio <- (fx(x_star) * dchisq(xt, df = floor(x_star + 1)) /
                (fx(xt) * dchisq(x_star, floor(xt + 1))))


    # accept according to the ratio
    if (mh_ratio > 1) {
      xt_plus <- x_star
      accepted_count <- accepted_count + 1
    } else {
      u <- runif(1)
      if (u < mh_ratio) {
        xt_plus <- x_star
        accepted_count <- accepted_count + 1
      } else {
        xt_plus <- xt
      }
    }

    samples[count] <- xt_plus
    count <- count + 1
    xt <- xt_plus
  }
  return(list(random_variable = samples, acceptance_rate = accepted_count / n))
}

sample_number <- 10000
result_1b <- metropolis_hastings_chi_square_1b(sample_number)

data_set_1b <- data.frame(x = 1:sample_number, y = result_1a$random_variable)

result_1b_sample <- result_1b$random_variable

ggplot(data = data_set_1b, mapping = aes(x = x, y = y)) +
  geom_line() +
  ggtitle("x vs y") +
  xlab("x") +
  ylab("y")

# according to the result, the convergence of the chain seems not converge to 
# a fixed value.

# According to the graph, there does not have a significant burn-in period.From
# the beginning, the chain seems very stable.

# The acceptance rate is 0.594
result_1b_acceptance_rate <- result_1b$acceptance_rate
cat("acceptance rate is", result_1b_acceptance_rate, "\n")

# plot a histogram of the 1b samples
hist(result_1b_sample)
mean(result_1b_sample)
########################## 1c ##################################################
# we use LN(X_{t},2) as the proposal distribution
metropolis_hastings_log_normal_1c <- function(n) {
  samples <- rep(0, n)

  # sample from a log-normal distribution
  xt <- rlnorm(1, 0, 2)
  samples[1] <- xt

  # set some initial values
  count <- 1
  accepted_count <- 0

  while (count <= n) {
    # sample a candidate from log normal distribution
    x_star <- rlnorm(1, log(xt), 2)

    # calc a ratio
    mh_ratio <- (fx(x_star) * dlnorm(xt, log(x_star), 2)) /
                (fx(xt) * dlnorm(x_star, log(xt), 2))


    # accept according to the ratio
    if (mh_ratio > 1) {
      xt_plus <- x_star
      accepted_count <- accepted_count + 1
    } else {
      u <- runif(1)
      if (u < mh_ratio) {
        xt_plus <- x_star
        accepted_count <- accepted_count + 1
      } else {
        xt_plus <- xt
      }
    }

    samples[count] <- xt_plus
    count <- count + 1
    xt <- xt_plus
  }
  return(list(random_variable = samples, acceptance_rate = accepted_count / n))
}

sample_number <- 10000
result_1c <- metropolis_hastings_log_normal_1c(sample_number)

data_set_1c <- data.frame(x = 1:sample_number, y = result_1c$random_variable)

result_1c_sample <- result_1c$random_variable

ggplot(data = data_set_1c, mapping = aes(x = x, y = y)) +
  geom_line() +
  ggtitle("x vs y") +
  xlab("x") +
  ylab("y")

# according to the result, the convergence of the chain seems not converge to 
# a fixed value.

# According to the graph, there does not have a significant burn-in period.From
# the beginning, the chain seems very stable.

# The acceptance rate is 0.2447
result_1c_acceptance_rate <- result_1c$acceptance_rate
cat("acceptance rate is", result_1c_acceptance_rate, "\n")

# plot a histogram of the 1a samples
hist(result_1c_sample)
mean(result_1c_sample)
########################## 1d ##################################################
# comment something here
########################## 1e ##################################################

# we will combine 3 data sets together, the E(X) is 2440223
all_samples <- c(result_1a_sample, result_1b_sample, result_1c_sample) 
EX <- sum(all_samples * fx(all_samples))
print(EX)
hist(all_samples)
########################## 1e ##################################################
fit.gamma <- fitdist(all_samples, distr = "gamma", method = "mle")
summary(fit.gamma)
plot(fit.gamma)

shape 6.0551684
rate  0.9990455