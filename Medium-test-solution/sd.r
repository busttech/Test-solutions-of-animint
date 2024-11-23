library(ggplot2)
library(animint2)
library(dplyr)

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

# Bar plot: Frequency of each face of the coin (animated over toss)
bar_plot <- ggplot(flip_data, aes(x = result, fill = result, time = toss)) +
  geom_bar(stat = "count") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Coin Flip Frequencies", x = "Face", y = "Count")

# Line plot: Cumulative frequency over tosses (animated over toss)
line_plot <- ggplot(flip_data, aes(x = toss, y = cumulative, color = result, time = toss)) +
  geom_line(size = 1) +
  labs(title = "Cumulative Frequency Over Tosses", x = "Toss Number", y = "Cumulative Frequency") +
  geom_point(size = 2)

# List of ggplots for animation
plots <- list(
  frequency = bar_plot,
  cumulative = line_plot,
  time = list(variable = "toss", ms = 200) # Define animation interval
)

# Create the animation and save it to a directory
animint2dir(plots, out.dir = "coin_flip_animation")