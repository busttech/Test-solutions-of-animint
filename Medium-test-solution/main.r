# Load necessary libraries
library(ggplot2)
library(animint2)
library(dplyr)

# Coin flip simulation setup
faces <- c("Head", "Tail", "Stand")
prob <- c(0.45, 0.45, 0.1)
n_tosses <- 100

# Simulate coin flips
flips <- sample(faces, size = n_tosses, replace = TRUE, prob = prob)
flip_data <- data.frame(
  toss = seq_len(n_tosses),
  result = flips
)

# Calculate cumulative frequencies
flip_data <- flip_data %>%
  group_by(result) %>%
  mutate(freq = row_number()) %>%
  ungroup() %>%
  mutate(cumulative = cumsum(result == "Head") / toss)

# Convert result to a factor
flip_data$result <- factor(flip_data$result, levels = faces)

# Bar plot: Frequency of each face of the coin (with clickSelects for interaction)
bar_plot <- ggplot(flip_data, aes(x = result, fill = result)) +
  geom_bar(stat = "count", clickSelects = "result") + # Use clickSelects as a parameter
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Coin Flip Frequencies", x = "Face", y = "Count")

# Line plot: Cumulative frequency over tosses (animated over toss)
line_plot <- ggplot(flip_data, aes(x = toss, y = cumulative, color = result)) +
  geom_line(size = 1) +
  geom_point(size = 2, showSelected = "toss") + # Use showSelected as a parameter
  labs(title = "Cumulative Frequency Over Tosses", x = "Toss Number", y = "Cumulative Frequency")

# List of ggplots for animation
plots <- list(
  frequency = bar_plot,
  cumulative = line_plot,
  time = list(variable = "toss", ms = 200) # Define animation interval
)

# Save animation to directory
animint2dir(plots, out.dir = "coin_flip_animation")
