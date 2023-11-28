########################## INIT CODE ###########################################
rm(list = ls())
library(ggplot2)

########################## 1a ##################################################
# DONE

fx_1a <- function(x) {
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
    mh_ratio <- (fx_1a(x_star) * dlnorm(xt, log(x_star))) /
                (fx_1a(xt) * dlnorm(x_star, log(xt)))


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

# The acceptance rate is 0.448 
result_1a_acceptance_rate <- result_1a$acceptance_rate
cat("acceptance rate is", result_1a_acceptance_rate, "\n")

# plot a histogram of the 1a samples
hist(result_1a_sample)

########################## 1b ##################################################
# we use chi-square distribution X^2(|x+1|) as the proposal distribution

  xt <- rchisq(n=1,df = 1)
metropolis_hastings_chi_square <- function(n) {
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
    mh_ratio <- (fx_1a(x_star) * dlnorm(xt, log(x_star))) /
                (fx_1a(xt) * dlnorm(x_star, log(xt)))


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

########################## 1c ##################################################
# we use LN(X_{t},2) as the proposal distribution
metropolis_hastings_log_normal_1c <- function(n) {
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
    mh_ratio <- (fx_1a(x_star) * dlnorm(xt, log(x_star))) /
                (fx_1a(xt) * dlnorm(x_star, log(xt)))


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


# Question 1d
# TODO: comment on the plot of 1a,1b,1c and draw a conclusion

# Question 1e
# TODO: check what is the requirement

# Question 1f
# TODO: check what is the requirement

