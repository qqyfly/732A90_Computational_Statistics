# Question 1a

fx_1a <- function(x) {
  return(x^5 * exp(-x))
}

metropolis_hastings_log_normal <- function(n) {
  
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

    if (mh_ratio > 1) {
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
result_1a <- metropolis_hastings_log_normal(sample_number)
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
# we use chi-square distribution X^2(|x+1|) as the proposal distribution


metropolis_hastings_chi_square <- function(n) {
  
  samples <- numeric(n)

  count <- 1
  execute_count <- 0

  # sample from a proposal distribution g
  # here we use log-normal distribution
  # TODO: find how to define df, since we don't know the Xt now,and in the prev 
  # sample we use rlnorm(1, 0, 1) to generate xt directly

  xt <- rchisq(n=1,df = 1)

  while(count <= n) {
    # sample a candidate from a proposal distribution g
    # TODO: reason as above

    x_star <- rchisq(n=1,df = 1)
    
    execute_count <- execute_count + 1

    # TODO: check it 
    mh_ratio <- f(x_star) / f(xt)

    xt_plus <- xt

    if (mh_ratio > 1) {
        xt_plus <- x_star
        accept_count <- accept_count + 1
    }

    samples[count] <- xt_plus
    count <- count + 1
    x_t <- xt_plus
  }  
  return(list(random_variable=samples, acceptance_rate= n / execute_count )
}

# Question 1c
# same as 1b , we will use another proposal distribution like what we see on 1b
# TODO: add code here

# Question 1d
# TODO: comment on the plot of 1a,1b,1c and draw a conclusion

# Question 1e
# TODO: check what is the requirement

# Question 1f
# TODO: check what is the requirement

