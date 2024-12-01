# Load necessary libraries 
library(ggplot2)
library(animint2)
library(dplyr)
library(tidyr)  # Load tidyr for pivot_longer

# Step 1: Set up coin flip simulation
faces <- c("Head", "Tail", "Stand")
prob <- c(0.45, 0.45, 0.1)
n_tosses <- 100

# Step 2: Simulate coin flips and store the results in a data frame
flips <- sample(faces, size = n_tosses, replace = TRUE, prob = prob)
flip_data <- data.frame(
  toss = seq_len(n_tosses),
  result = flips
)

# Step 3: Calculate cumulative counts for each face
flip_data <- flip_data %>% 
  mutate(HEAD = cumsum(result == "Head"),
         TAIL = cumsum(result == "Tail"),
         STAND = cumsum(result == "Stand"))

# Step 4: Reshape data to long format for counting purposes
flip_data_long <- flip_data %>% 
  pivot_longer(cols = c("HEAD", "TAIL", "STAND"), 
               names_to = "outcome", 
               values_to = "count") %>% 
  mutate(outcome = factor(outcome, levels = c("HEAD", "TAIL", "STAND"))) 

flip_data$result <- factor(flip_data$result, levels = c("Head", "Tail", "Stand"))

# Step 5: Create an animated bar plot for the frequency of outcomes
bar_plot <- ggplot(flip_data_long) +
  geom_bar(aes(x = outcome, y = count, fill = outcome), 
           stat = "identity", 
           position = "dodge", 
           showSelected = "toss") +
  geom_text(aes(x = outcome, y = count, label = count), 
            vjust = -0.5, 
            showSelected = "toss") +
  labs(title = "Coin Flip Frequencies (Interactive)", 
       x = "Face", 
       y = "Count") +
  scale_x_discrete(labels = c("Head", "Tail", "Stand")) +  # Set the x-axis labels
  scale_fill_manual(values = c("HEAD" = "red", 
                               "TAIL" = "green", 
                               "STAND" = "blue")) +  # Update color mapping
  guides(fill = "none")  # Remove the "outcome" legend

# Step 6: Create a line plot for cumulative frequencies over tosses
line_plot <- ggplot(flip_data, aes(x = toss, y = HEAD / toss, color = result, group = result)) +
  geom_line(size = 1) +
  geom_point(size = 2, showSelected = "toss") +
  geom_tallrect(aes(xmin = toss - 0.5, xmax = toss + 0.5), alpha = 0.2, clickSelects = "toss") +
  labs(title = "Cumulative Frequency Over Tosses (Interactive)", 
       x = "Toss Number", 
       y = "Cumulative Frequency") +
  scale_color_manual(values = c("Head" = "red", 
                                "Tail" = "green", 
                                "Stand" = "blue"))

# Step 7: Compile the plots into a list
plots <- list(
  frequency = bar_plot,         
  cumulative = line_plot,       
  time = list(variable = "toss", ms = 200)
)

# Step 8: Save the animation to a directory
animint2dir(plots, out.dir = "coin_flip_animation")

