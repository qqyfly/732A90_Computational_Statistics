########################## INIT CODE ###########################################
rm(list = ls())

########################## 2a ##################################################
# DONE

w  <- 1.999

# Draw the boundary of the ellipse
# a range of x1-values, where the term below the root is non-negative 
xv <- seq(-1, 1, by=0.01) * 1/sqrt(1-w^2/4)
plot(xv, xv, type="n", xlab=expression(x[1]), ylab=expression(x[2]), las=1)

# ellipse
lines(xv, -(w/2)*xv-sqrt(1-(1-w^2/4)*xv^2), lwd=2, col=8)
lines(xv, -(w/2)*xv+sqrt(1-(1-w^2/4)*xv^2), lwd=2, col=8)

########################## 2b ##################################################
# DONE
# Since w = 1.999, and  x_{1}^2 + 1.999*x_{1}*x_{2} + x_{2}^2 < 1
# Using the quadratic formula, we apply this on
# x_{1}^2 + 1.999*x_{1}*x_{2} + x_{2}^2 - 1 = 0

# we have
# The conditional distribution for x_{2} given x_{1} is a uniform distribution
# on the interval
# ( -0.9995 * x_{1} - \sqrt{1-0.00099975 * x_{1}^2} , -0.9995 * x_{1} + \sqrt{1-0.00099975 * x_{1}^2})
# The conditional distribution for x_{1} given x_{2} is a uniform distribution
# on the interval
# ( -0.9995 * x_{2} - \sqrt{1-0.00099975 * x_{2}^2} , -0.9995 * x_{2} + \sqrt{1-0.00099975 * x_{2}^2})


########################## 2c ##################################################
# DONE

gibbs_sampling_2c <- function(n, x0) {
    samples <- matrix(nrow = n, ncol = 2)
    x1 <- x0[1]
    x2 <- x0[2]

    count <- 1
    while (count <= n) {
        # Sample x1 from the conditional distribution for x1 given x2
        x1 <- runif(1, -0.9995 * x2 - sqrt(1-0.00099975 * x2^2), -0.9995 * x2 + sqrt(1-0.00099975 * x2^2))
        
        # Sample x2 from the conditional distribution for x2 given x1
        x2 <- runif(1, -0.9995 * x1 - sqrt(1-0.00099975 * x1^2), -0.9995 * x1 + sqrt(1-0.00099975 * x1^2))

        samples[count,] <- c(x1, x2)

        # Calculate p(x1 > 0)
        prob_value <- sum(samples[1:count, 1] > 0) /
              length(samples[1:count, 1])

        cat("The probability that x1 > 0 is", prob_value, "\n")

        count <- count + 1
    }

    return(samples)
}

x0 <- c(0, 0)
n <- 1000
gibbs_sampling_result_2c <- gibbs_sampling_2c(n, x0)

# plot the sample point on the eclipse
points(gibbs_sampling_result_2c[,1], gibbs_sampling_result_2c[,2], col=2, pch=20, cex=0.5)

# comment on the result ( about the true result)

########################## 2d ##################################################
# As we can see from the 2 plots, the ellipse for w = 1.999 is more flat than 
# the ellipse for w = 1.8.

########################## 2e ##################################################
# We need to transform the variable X and convert to 
# U = (U1, U2) = (X_{1} - X{2}, X{1} + X{2})
# And since U is still a uniform distribution, we can use the same method as in 2c

# We have 
# U1 = X_{2} - X_{2}
# U2 = X_{1} + X_{2}
# so we have
# X_{1} = (U1 + U2) / 2
# X_{2} = (U2 - U1) / 2

# We replace this into the original function, and after some simplification, we have
# (2+w) * u_{2}^2 + (2 - w) * u_{1}^2 - 4 = 0

# calculate the boundary of the ellipse, we have 
# (-sqrt((4 -  (2-w) * u_{1}^2) / (2 + w)), sqrt((4 -  (2-w) * u_{1}^2) / (2 + w)))
# another one is
# (-sqrt((4 -  (2+w) * u_{2}^2) / (2 - w)), sqrt((4 -  (2+w) * u_{2}^2) / (2 - w)))

# The boundary of the ellipse is:
w  <- 1.999
xv <- seq(-1, 1, by=0.01) * 1/sqrt(1-w^2/4)

#plot(xv, xv, type="n", xlab=expression(x[1]), ylab=expression(x[2]), las=1)


# substitute w = 1.999, we have

# The conditional distribution for u_{2} given u_{1} is a uniform distribution
# on the interval
# (-\sqrt{\frac{4-0.001 * u_{1}^2}{3.999}}, \sqrt{\frac{4-0.001 * u_{1}^2}{3.999}})
# The conditional distribution for u_{1} given u_{2} is a uniform distribution
# on the interval
# (-\sqrt{\frac{4-3.999 * u_{2}^2}{0.001}}, \sqrt{\frac{4-3.999 * u_{2}^2}{0.001}})



# ellipse
#(-1 * sqrt{(4-0.001 * u1^2}{3.999}}, \sqrt{\frac{4-0.001 * u_{1}^2}{3.999}})
#lines(xv, -(w/2)*xv-sqrt(1-(1-w^2/4)*xv^2), lwd=2, col=8)
#lines(xv, -(w/2)*xv+sqrt(1-(1-w^2/4)*xv^2), lwd=2, col=8)


gibbs_sampling_2e <- function(n, u0) {
    samples <- matrix(nrow = n, ncol = 2)
    u1 <- u0[1]
    u2 <- u0[2]

    count <- 1
    while (count <= n) {
        # Sample x1 from the conditional distribution for x1 given x2
        u1_new <- runif(1,
                        -1 * sqrt((4 - 3.999 * u2^2) / 0.001),
                        sqrt((4 - 3.999 * u2^2) / 0.001)
                        )
        
        # Sample x2 from the conditional distribution for x2 given x1
        u2_new <- runif(1,
                        -1 * sqrt((4 - 0.001 * u1^2)) / 3.999,
                        sqrt((4 - 0.001 * u1^2) / 3.999)
                        )

        samples[count,] <- c(u1_new, u2_new)


        u1 <- u1_new
        u2 <- u2_new

        count <- count + 1
    }

    return(samples)
}

u0 <- c(0, 0)
n <- 1000
gibbs_sampling_result_2e <- gibbs_sampling_2e(n, u0)

# Calculate p(x1 > 0) = p((u1 + u2) / 2 > 0)
prob_value <- sum((gibbs_sampling_result_2e[1:count, 1] + gibbs_sampling_result_2e[1:count, 2])/2 > 0) /
              length(gibbs_sampling_result_2e[1:count, 1])

cat("The probability that x1 > 0 is", prob_value, "\n")

# plot the sample point on the eclipse
points(gibbs_sampling_result_2e[,1], gibbs_sampling_result_2e[,2], col=2, pch=20, cex=0.5)

# TODO: compare the result with the result from 2c
