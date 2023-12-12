rm(list = ls())
library(ggplot2)
library(boot)

################################## (2.1) #######################################
data <- read.csv("./Homework/Lab/Lab5/prices1.csv", sep = ";")

g_2_1 <- ggplot(data, aes(x = SqFt, y = Price)) + geom_point()

# fit a linear regression model
fit2_1_lm <- lm(Price ~ SqFt, data = data)
summary(fit2_1_lm)

minimal_x <- min(data$SqFt)
maximum_x <- max(data$SqFt)

x_2_1_lm <- seq(minimal_x, maximum_x, 0.1)
y_2_1_lm <- predict(fit2_1_lm, newdata = data.frame(SqFt = x_2_1_lm))

data_2_1 <- data.frame(SqFt = x_2_1_lm, Price = y_2_1_lm)

g_2_1 + geom_line(data = data_2_1, aes(x = x_2_1_lm, y = y_2_1_lm), color = "red") 

################################## (2.2) #######################################
rss_fun <- function(par,SqFt,Price) {

  a1 <- par[1]
  a2 <- par[2]
  b <- par[3]
  c <- par[4]
  
  predicted <- ifelse(SqFt > c, b + a1 * SqFt + a2 * (SqFt - c), b + a1 * SqFt)
  #predicted <- b + a1 * SqFt + a2 * (SqFt - c)
  
  rss <- sum((Price - predicted)^2)
  return(rss)
}

c_func <- function(c, , Price) {
  # Init
  init_pars <- c(2, 0.1, 50, c)

  # Optimize using optim()
  result <- optim(par = init_pars, rss_fun, SqFt = SqFt, Price = Price)

  opt_pars <- result$par
  rss <- result$value

  return(list(c = c, params = opt_pars, rss = rss))
}


c <- 150
results <- c_func(c, SqFt = data$SqFt, Price = data$Price)
results



################################## (2.3) #######################################

stat1 <- function(data,vn){
    data = as.data.frame(data[vn,])
    res <- c_func(c, SqFt = data$SqFt, Price = data$Price)
    return(res$param[4])
}

set.seed(12345)
res1 = boot(data, stat1, R=1000)

print(boot.ci(res1)) 

# plot
plot(res1)

################################## (2.4) #######################################

# Jackknife Function
jackknife <- function(data, fun) {
  n <- length(data)
  indices <- 1:n
  estimates <- numeric(n)
  
  for (i in 1:n) {
    jackknife_sample <- data[-i]
    estimates[i] <- fun(jackknife_sample)
  }
  
  bias_correction <- (n - 1) * mean(estimates) - fun(data)
  
  jackknife_var <- ((n - 1) / n) * sum((estimates - mean(estimates) - bias_correction)^2)
  
  return(list(estimates = estimates, jackknife_var = jackknife_var))
}

# not working here
jackknife_results <- jackknife(c, function(c_val) {
  c_func(c_val, SqFt, Price)$params[1]
})

# Print Jackknife Results
cat("Jackknife Estimates of c:", jackknife_results$estimates, "\n")
cat("Jackknife Variance of c:", jackknife_results$jackknife_var, "\n")
