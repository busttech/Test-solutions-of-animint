# Load necessary libraries
library(ggplot2)  # For creating visualizations
library(animint2) # For adding interactivity and animations to plots
library(dplyr)    # For data manipulation

# Step 1: Set up coin flip simulation
faces <- c("Head", "Tail", "Stand")  # Possible outcomes of the coin flip
prob <- c(0.45, 0.45, 0.1)           # Probabilities for each outcome
n_tosses <- 100                      # Number of coin tosses to simulate

# Step 2: Simulate coin flips and store the results in a data frame
flips <- sample(faces, size = n_tosses, replace = TRUE, prob = prob)  # Simulate outcomes
flip_data <- data.frame(
  toss = seq_len(n_tosses),  # Toss number (1, 2, ..., n_tosses)
  result = flips             # Outcome of each toss
)

# Step 3: Calculate cumulative statistics
flip_data <- flip_data %>%
  group_by(result) %>%                       # Group by the outcome (result)
  mutate(freq = row_number()) %>%            # Add a running count for each outcome
  ungroup() %>%                              # Remove grouping
  mutate(cumulative = cumsum(result == "Head") / toss)  # Calculate cumulative proportion of "Head"

# Step 4: Convert result column to a factor with defined levels
flip_data$result <- factor(flip_data$result, levels = faces)  # Ensures consistent order in plots

# Step 5: Create a bar plot for the frequency of outcomes
bar_plot <- ggplot(flip_data, aes(x = result, fill = result)) +
  geom_bar(stat = "count", clickSelects = "toss") +          # Interactive: bar updates with selected toss
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) + # Add bar labels
  labs(title = "Coin Flip Frequencies (Interactive)", 
       x = "Face", 
       y = "Count")  # Title and axis labels

# Step 6: Create a line plot for cumulative frequency over tosses
line_plot <- ggplot(flip_data, aes(x = toss, y = cumulative, color = result, group = result)) +
  geom_line(size = 1) +                                       # Add lines for cumulative frequency
  geom_point(size = 2, showSelected = "toss") +               # Highlight points for selected toss
  geom_tallrect(aes(xmin = toss - 0.5, xmax = toss + 0.5),    # Add a tall rectangle for selection
                alpha = 0.2, clickSelects = "toss") +         # Make the rectangle interactive
  labs(title = "Cumulative Frequency Over Tosses (Interactive)", 
       x = "Toss Number", 
       y = "Cumulative Frequency")  # Title and axis labels

# Step 7: Create a scatter plot for toss number vs cumulative frequency
scatter_plot <- ggplot(flip_data, aes(x = toss, y = cumulative, color = result)) +
  geom_point(size = 2, clickSelects = "toss") +               # Points are interactive with toss selection
  labs(title = "Scatterplot: Toss vs Cumulative Frequency", 
       x = "Toss Number", 
       y = "Cumulative Frequency")  # Title and axis labels

# Step 8: Combine all plots into a list for animation
plots <- list(
  frequency = bar_plot,         # Bar plot of frequencies
  cumulative = line_plot,       # Line plot of cumulative frequencies
  scatter = scatter_plot,       # Scatter plot of toss vs cumulative frequency
  time = list(variable = "toss", ms = 200)  # Define animation frame variable and interval (200 ms)
)

# Step 9: Save the animation to a directory
animint2dir(plots, out.dir = "coin_flip_animation")  # Save the interactive animation to a folder


