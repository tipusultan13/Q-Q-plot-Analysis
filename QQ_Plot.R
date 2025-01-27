##################################

# Author: Tipu Sultan
# Supervisor: Inka Dorothee Roesel

##################################
# Set Directory
setwd("/Users/tipusultan/Documents/GitHub/Covid-19-Representing-a-Global-Pandemic-in-Data")

#Load Libraries
library(ggplot2)
library(qqplotr)
library(gridExtra)

# Generate random normal data
smp <- data.frame(norm = rnorm(100))

# Simultenious Method
QQ_ks <- ggplot(data = smp, mapping = aes(sample = norm)) +
  stat_qq_band(bandType = "ks", conf = 0.95) + 
  stat_qq_line() + # Theoretical line
  stat_qq_point() + # Sample points
  labs(
    title = "Normal Q-Q Plot with Simultenious Confidence Bands",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# Display the plot
print(QQ_ks)

# Pointwise Method
QQ_pointwise <- ggplot(data = smp, mapping = aes(sample = norm)) +
  stat_qq_band(bandType = "pointwise", conf = 0.95) + 
  stat_qq_line() + # Theoretical line
  stat_qq_point() + # Sample points
  labs(
    title = "Normal Q-Q Plot with Pointwise Confidence Bands",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# Display the plot
print(QQ_pointwise)

# Bootstrap Method
QQ_boot <- ggplot(data = smp, mapping = aes(sample = norm)) +
  stat_qq_band(bandType = "boot", conf = 0.95) + 
  stat_qq_line() + # Theoretical line
  stat_qq_point() + # Sample points
  labs(
    title = "Normal Q-Q Plot with Bootstrap Confidence Bands",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# Display the plot
print(QQ_boot)

# Tail-Sensitive Method
QQ_ts <- ggplot(data = smp, mapping = aes(sample = norm)) +
  stat_qq_band(bandType = "ts", conf = 0.95) + 
  stat_qq_line() + # Theoretical line
  stat_qq_point() + # Sample points
  labs(
    title = "Normal Q-Q Plot with Tail-Sensitive Confidence Bands",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# Display the plot
print(QQ_ts)

grid.arrange(QQ_ks, QQ_pointwise, QQ_boot, QQ_ts, ncol = 2)



# Right Skewed data with sample size 100
################################################

RightSkewed <- rexp(100)
RightSkewed = data.frame(norm = RightSkewed)

# Simultenious Method
QQ_ks <- ggplot(data = RightSkewed, mapping = aes(sample = norm)) +
  stat_qq_band(bandType = "ks", conf = 0.95) + 
  stat_qq_line() + # Theoretical line
  stat_qq_point() + # Sample points
  labs(
    title = "Normal Q-Q Plot with Simultenious Confidence Bands",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# Display the plot
print(QQ_ks)

# Pointwise Method
QQ_pointwise <- ggplot(data = RightSkewed, mapping = aes(sample = norm)) +
  stat_qq_band(bandType = "pointwise", conf = 0.95) + 
  stat_qq_line() + # Theoretical line
  stat_qq_point() + # Sample points
  labs(
    title = "Normal Q-Q Plot with Pointwise Confidence Bands",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# Display the plot
print(QQ_pointwise)

# Bootstrap Method
QQ_boot <- ggplot(data = RightSkewed, mapping = aes(sample = norm)) +
  stat_qq_band(bandType = "boot", conf = 0.95) + 
  stat_qq_line() + # Theoretical line
  stat_qq_point() + # Sample points
  labs(
    title = "Normal Q-Q Plot with Bootstrap Confidence Bands",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# Display the plot
print(QQ_boot)

# Tail-Sensitive Method
QQ_ts <- ggplot(data = RightSkewed, mapping = aes(sample = norm)) +
  stat_qq_band(bandType = "ts", conf = 0.95) + 
  stat_qq_line() + # Theoretical line
  stat_qq_point() + # Sample points
  labs(
    title = "Normal Q-Q Plot with Tail-Sensitive Confidence Bands",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# Display the plot
print(QQ_ts)

grid.arrange(QQ_ks, QQ_pointwise, QQ_boot, QQ_ts, ncol = 2)

# Normal data with 50% noise for sample size 100
################################################

# Generate 100 samples from a normal distribution
normal_data <- rnorm(100, mean = 0, sd = 1) # Mean = 0, SD = 1

# Generate 50 samples from a uniform distribution
uniform_noise <- runif(50, min = 2, max = 4) 

# Add the uniform noise to the first 50 values of the normal dataset
noise_50 <- normal_data
noise_50[1:50] <- noise_50[1:50] + uniform_noise
noise_50 = data.frame(norm = noise_50)

# Simultenious Method
QQ_ks <- ggplot(data = noise_50, mapping = aes(sample = norm)) +
  stat_qq_band(bandType = "ks", conf = 0.95) + 
  stat_qq_line() + # Theoretical line
  stat_qq_point() + # Sample points
  labs(
    title = "Normal Q-Q Plot with Simultenious Confidence Bands",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# Display the plot
print(QQ_ks)

# Pointwise Method
QQ_pointwise <- ggplot(data = noise_50, mapping = aes(sample = norm)) +
  stat_qq_band(bandType = "pointwise", conf = 0.95) + 
  stat_qq_line() + # Theoretical line
  stat_qq_point() + # Sample points
  labs(
    title = "Normal Q-Q Plot with Pointwise Confidence Bands",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# Display the plot
print(QQ_pointwise)

# Bootstrap Method
QQ_boot <- ggplot(data = noise_50, mapping = aes(sample = norm)) +
  stat_qq_band(bandType = "boot", conf = 0.95) + 
  stat_qq_line() + # Theoretical line
  stat_qq_point() + # Sample points
  labs(
    title = "Normal Q-Q Plot with Bootstrap Confidence Bands",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# Display the plot
print(QQ_boot)

# Tail-Sensitive Method
QQ_ts <- ggplot(data = noise_50, mapping = aes(sample = norm)) +
  stat_qq_band(bandType = "ts", conf = 0.95) + 
  stat_qq_line() + # Theoretical line
  stat_qq_point() + # Sample points
  labs(
    title = "Normal Q-Q Plot with Tail-Sensitive Confidence Bands",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# Display the plot
print(QQ_ts)

grid.arrange(QQ_ks, QQ_pointwise, QQ_boot, QQ_ts, ncol = 2)

# Normal data with 50% noise for sample size 1000
################################################

# Generate 1000 samples from a normal distribution
normal_data <- rnorm(1000, mean = 0, sd = 1) # Mean = 0, SD = 1

# Generate 500 samples from a uniform distribution
uniform_noise <- runif(500, min = 2, max = 4) 

# Add the uniform noise to the first 500 values of the normal dataset
noise_50 <- normal_data
noise_50[1:500] <- noise_50[1:500] + uniform_noise
noise_50 = data.frame(norm = noise_50)

# Simultenious Method (ks), # Pointwise Method (pointwise)
# Bootstrap Method (boot), Tail-Sensitive Method (ts) 


# Simultenious Method
QQ_ks <- ggplot(data = noise_50, mapping = aes(sample = norm)) +
  stat_qq_band(bandType = "ks", conf = 0.95) + 
  stat_qq_line() + # Theoretical line
  stat_qq_point() + # Sample points
  labs(
    title = "Normal Q-Q Plot with Simultenious Confidence Bands",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# Display the plot
print(QQ_ks)

# Pointwise Method
QQ_pointwise <- ggplot(data = noise_50, mapping = aes(sample = norm)) +
  stat_qq_band(bandType = "pointwise", conf = 0.95) + 
  stat_qq_line() + # Theoretical line
  stat_qq_point() + # Sample points
  labs(
    title = "Normal Q-Q Plot with Pointwise Confidence Bands",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# Display the plot
print(QQ_pointwise)

# Bootstrap Method
QQ_boot <- ggplot(data = noise_50, mapping = aes(sample = norm)) +
  stat_qq_band(bandType = "boot", conf = 0.95) + 
  stat_qq_line() + # Theoretical line
  stat_qq_point() + # Sample points
  labs(
    title = "Normal Q-Q Plot with Bootstrap Confidence Bands",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# Display the plot
print(QQ_boot)

# Tail-Sensitive Method
QQ_ts <- ggplot(data = noise_50, mapping = aes(sample = norm)) +
  stat_qq_band(bandType = "ts", conf = 0.95) + 
  stat_qq_line() + # Theoretical line
  stat_qq_point() + # Sample points
  labs(
    title = "Normal Q-Q Plot with Tail-Sensitive Confidence Bands",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# Display the plot
print(QQ_ts)

grid.arrange(QQ_ks, QQ_pointwise, QQ_boot, QQ_ts, ncol = 2)



