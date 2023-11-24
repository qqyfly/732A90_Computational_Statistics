# Question 1a

fx_1a <- function(x) {
  return(x^5 * exp(-x))
}

metropolis_hastings <- function(n) {
  
  samples <- numeric(n)

  count <- 1
  execute_count <- 0
  # sample from a proposal distribution g
  # here we use log-normal distribution
  xt <- rlnorm(1, 0, 1)

  while(count <= n) {
    # sample a candidate from a proposal distribution g
    x_star <- rlnorm(1, 0, 1)
    
    execute_count <- execute_count + 1

    # TODO: check it 
    mh_ratio <- f(x_star) / f(xt)

    xt_plus <- xt

    # TODO: check it  
    if (mh_ratio < 1) {
        xt_plus <- x_star
        accept_count <- accept_count + 1
    }

    samples[count] <- xt_plus
    count <- count + 1
    x_t <- xt_plus
  }
  
  
  return(list(random_variable=samples, acceptance_rate= n / execute_count )
}

sample_number <- 1
result_1a <- metropolis_hastings(sample_number)
result_1a_sample <- result_1a$random_variable
result_1a_acceptance_rate <- result_1a$acceptance_rate

data <- data.frame(x=1:sample_number,y=result_1a)
plot_1a <- ggplot2::ggplot() +
  ggplot2::geom_line(mapping=ggplot2::aes(x=x,y=y)) + 
  ggplot2::ggtitle("x vs y") +
  ggplot2::xlab("x") +
  ggplot2::ylab("y")

# TODO: comment on the plot

# TODO: check burn–in period meaning

# get the acceptance rate
cat("acceptance rate is", result_1a_acceptance_rate)

# plot a histogram of the 1a samples
hist(result_1a_sample)

# Question 1b

# Question 1c

# Question 1d

# Question 1e

# Question 1f

