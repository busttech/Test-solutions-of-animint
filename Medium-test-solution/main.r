# Load necessary libraries
library(ggplot2) 
library(animint2)
library(dplyr)   

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

# Step 3: Calculate cumulative statistics
flip_data <- flip_data %>%
  group_by(result) %>%                       
  mutate(freq = row_number()) %>%            
  ungroup() %>%                              
  mutate(cumulative = cumsum(result == "Head") / toss)  

# Step 4: Convert result column to a factor with defined levels
flip_data$result <- factor(flip_data$result, levels = faces)  

# Step 5: Create a bar plot for the frequency of outcomes
bar_plot <- ggplot(flip_data, aes(x = result, fill = result)) +
  geom_bar(stat = "count", clickSelects = "toss") +          
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) + 
  labs(title = "Coin Flip Frequencies (Interactive)", 
       x = "Face", 
       y = "Count")  

# Step 6: Create a line plot for cumulative frequency over tosses
line_plot <- ggplot(flip_data, aes(x = toss, y = cumulative, color = result, group = result)) +
  geom_line(size = 1) +                                       
  geom_point(size = 2, showSelected = "toss") +               
  geom_tallrect(aes(xmin = toss - 0.5, xmax = toss + 0.5),   
                alpha = 0.2, clickSelects = "toss") +         
  labs(title = "Cumulative Frequency Over Tosses (Interactive)", 
       x = "Toss Number", 
       y = "Cumulative Frequency")  

# Step 7: Create a scatter plot for toss number vs cumulative frequency
scatter_plot <- ggplot(flip_data, aes(x = toss, y = cumulative, color = result)) +
  geom_point(size = 2, clickSelects = "toss") +               
  labs(title = "Scatterplot: Toss vs Cumulative Frequency", 
       x = "Toss Number", 
       y = "Cumulative Frequency")  

# Step 8: Combine all plots into a list for animation
plots <- list(
  frequency = bar_plot,         
  cumulative = line_plot,
  scatter = scatter_plot,       
  time = list(variable = "toss", ms = 200)  

# Step 9: Save the animation to a directory
animint2dir(plots, out.dir = "coin_flip_animation")  

