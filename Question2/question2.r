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

