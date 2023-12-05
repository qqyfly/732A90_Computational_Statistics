
### Init Code
rm(list = ls())
library(ggplot2)
library(boot)
set.seed(12345)


################################## (1.1) #######################################
# DONE

data <- read.csv("./Homework/Lab/Lab5/lottery.csv", sep = ";")

# Remove Month column since there is a Mo.Number column which is same as Month
data <- data[-2]

g_1_1 <- ggplot(data, aes(x = Day_of_year, y = Draft_No)) + geom_point()
g_1_1

# according to the plot, the pattern is not very clear.

################################## (1.2) #######################################
# DONE

minimal_x <- min(data$Day_of_year)
maximum_x <- max(data$Day_of_year)

x_1_2 <- seq(minimal_x, maximum_x, 0.1)

# fit the data using linear regression
fit1_2_lm <- lm(Draft_No ~ Day_of_year, data = data)
summary(fit1_2_lm)
y_1_2_lm <- predict(fit1_2_lm, newdata = data.frame(Day_of_year = x_1_2))

# fit using loess function
fit1_2_loess <- loess( Draft_No ~ Day_of_year, data = data, se = TRUE)
summary(fit1_2_loess)
y_1_2_loess <- predict(fit1_2_loess, newdata = data.frame(Day_of_year = x_1_2))


# plot 2 predicted lines
data_1_2 <- data.frame(Day_of_year = x_1_2, y_lm = y_1_2_lm, y_loess = y_1_2_loess)

g_1_1 + geom_line(data = data_1_2, aes(x = Day_of_year, y = y_lm,colour="lm")) +
geom_line(data = data_1_2, aes(x = Day_of_year, y = y_loess, colour = "loess")) + 
scale_color_manual(name = "method", values = c("lm" = "blue", "loess" = "red"))



# according to the 2 curves, is seems that all the points are dived to 2 groups.
# since it's a linear regression, not a classification, we can guess that the'
# points is randomly distributed.

# according to the  plot , we can not get the conclusion that which model is 
# better since only [Day of year] is used.

################################## (1.3) #######################################

fun_lm_s <- function(formula,data,indices){
  d <- data[indices, ]
  lm_mod <- lm(formula, data = d)
  lm_val <- predict(lm_mod,
                    newdata = data.frame(Day_of_year = d$Day_of_year))
  s_lm <- sum(abs(lm_val - mean(d$Draft_No)))
  return(s_lm)
}

fun_loess_s <- function(formula,data,indices){
  d <- data[indices, ]
  loess_mod <- loess(formula, data = d, se = TRUE)
  loess_val <- predict(loess_mod,
                    newdata = data.frame(Day_of_year = d$Day_of_year))
  s_loess <- sum(abs(loess_val - mean(d$Draft_No)))
  return(s_loess)
}

bootstrap_sample_number <- 2000

# bootstrap for linear regression
boot_samples_lm <- boot(data, statistic = fun_lm_s, R = bootstrap_sample_number, formula=Draft_No  ~ Day_of_year,parallel = "multicore")

boot_samples_loess <- boot(data, statistic = fun_loess_s, R = bootstrap_sample_number, formula=Draft_No  ~ Day_of_year,parallel = "multicore")

# calculate mean of bootstrap sample statistics
mean_boot_samples_lm <- mean(boot_samples_lm$t)
#cat("mean of bootstrap sample statistics(lm) = ", mean_boot_samples_lm, "\n")
#mean of bootstrap sample statistics(lm) =  7646.339 

mean_boot_samples_loess <- mean(boot_samples_loess$t)
# cat("mean of bootstrap sample statistics(loess) = ", mean_boot_samples_loess, "\n")
# mean of bootstrap sample statistics(loess) =  8518.325 

par(mfrow = c(1,2))

# Plot the histogram of bootstrap sample statistics(lm)
hist(boot_samples_lm$t, main = "Bootstrap S Distribution(lm)", xlab = "S")

# Plot the histogram of bootstrap sample statistics(loess)
hist(boot_samples_loess$t, main = "Bootstrap S Distribution(loess)", xlab = "S")


# since 2 plots almost match normal distribution, but it is not symmetry, so  we  
# also can say that it is not randomly distributed.

# calculate the p-value

pvalue_lm = mean(abs(boot_samples_lm$t) > abs(boot_samples_lm$t0))
pvalue_loess = mean(abs(boot_samples_loess$t) > abs(boot_samples_loess$t0))
cat("p-value for linear regression = ", pvalue_lm, "\n")
cat("p-value for loess regression = ", pvalue_loess, "\n")
# p-value for linear regression =  0.509 
# p-value for loess regression =  0.568 

################################## (1.4) #######################################


################################## (1.5a) ######################################


################################## (1.5b) ######################################


################################## (1.5c) ######################################
