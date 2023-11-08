library(ggplot2)

# Create a data frame
df <- data.frame(
  x = c(1:10),
  y = c(1:10)
)

# Create a plot
g <- ggplot(df, aes(x = x, y = y)) + geom_point()
plot(g)
