library(broom)

# Extract data set from csv file
data_set <- read.csv("question2-data.csv")
data_set

# Assign x and y vectors to respective columns
x <- data_set$Age.X..Years.
y <- data_set$Average.peak.EEG.Frequency.Y..Hertz.

# Fir a simple linear regression model
simple_reg <- lm(y ~ x)

# Model parameters
tidy(simple_reg)

# Overall model summary statistics
glance(simple_reg)

# Predictions and residuals
augment(simple_reg)

# a) Construct a scatter plot for the data.
plot(
  x,
  y,
  main="Age (Years) vs Average Peak EEG Frequency (Hz)",
  xlab="Age (Years)",
  ylab="Average Peak EEG Frequency (Hertz)",
  pch=18,
  col="blue"
  )

# b) Find the least squares prediction equation using the calculation formulas.
x_bar <- mean(x)
y_bar <- mean(y)
sum_x_squared <- sum(x^2)
sum_y_squared <- sum(y^2)
sum_xy <- sum(x * y)
n <- length(x)

SSxy <- sum_xy - n * x_bar * y_bar
SSxx <- sum_x_squared - n * x_bar^2
SSyy <- sum_y_squared - n * y_bar^2

beta_hat_1 <- SSxy / SSxx
beta_hat_0 <- y_bar - beta_hat_1 * x_bar
sprintf("B0 = %0.4f", beta_hat_0)
sprintf("B1 = %0.4f", beta_hat_1)

least_squares_model <- function(x_val, B0=beta_hat_0, B1=beta_hat_1) {
  return (B0 + B1 * x_val)
}

# c) Calculate SSE, S^2 and S for the least squares line. Interpret the value of S.
y_hat <- least_squares_model(x)
SSE <- SSyy - beta_hat_1 * SSxy
SSE
s_squared <- SSE / (n - 2)
s <- sqrt(s_squared)
sprintf(
  paste0(
  "Assuming that the regression residuals are approximately normally distributed, ",
  "approximately 95 percent of the observed average peak EEG frequencies are expected to ",
  "lie within approximately %0.3f (Hz) of their corresponding values ",
  "predicted by the least-squares regression line."),
  s * 2
  )