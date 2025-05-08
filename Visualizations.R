# Load dataset
family <- read.csv("Data/2024_5-5-2025.csv")

library(dplyr)

# Correct the values in the dataset
family <- family %>%
  mutate(family_type = ifelse(family_type == "Adults Without Children", 
                              "Adults without Children", 
                              family_type))
View(family)
colnames(family)

count <- nrow(subset(family, family_type == "Adults with Children"))
print(count)
adults_with_children <- subset(family, family_type == "Adults with Children")
summary(adults_with_children)




library(ggplot2)

#scatter plot between household_size and annual_income
ggplot(family, aes(x = household_size, y = annual_income)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Household Size vs. Annual Income", x = "Household Size", y = "Annual Income") +
  theme_minimal()


ggplot(family, aes(x = race_family, y = annual_income)) +
  geom_boxplot(fill = "purple", color = "black") +
  labs(title = "Annual Income by Race Family", x = "Race", y = "Annual Income") +
  theme_minimal() +
  coord_flip()

ggplot(family, aes(x = family_type, y = months_visited)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Months Visited by Family Type", x = "Family Type", y = "Months Visited") +
  theme_minimal() +
  coord_flip()




#Updated Visual
library(ggplot2)

ggplot(family, aes(x = family_type, y = months_visited)) +
  geom_boxplot(fill = "lightblue", color = "black") +  # Neutral color
  labs(title = "Number of Food Pantry Visits in 2023 by Family Type",  
       x = "Family Type",  
       y = "Number of Food Pantry Visits") +  
  theme_minimal(base_size = 14) +  # Improve readability
  scale_y_continuous(breaks = seq(0, 12, by = 1)) +  # Set y-axis (original x-axis) to integers 0-12
  coord_flip()



#testing with the percent of each family type: 

library(ggplot2)
library(dplyr)

# Calculate percentage of each family type
family_counts <- family %>%
  count(family_type) %>%
  mutate(percent = round(100 * n / sum(n), 1),  # Ensure total is 100%
         label = paste0(family_type, " (", percent, "%)"))  # Create labels with percentages

# Create the plot using family_counts for labels
ggplot(family, aes(x = factor(family_type, levels = family_counts$family_type, labels = family_counts$label), 
                   y = months_visited)) +
  geom_boxplot(fill = "lightblue", color = "black") +  # Neutral color
  labs(title = "Number of Food Pantry Visits in 2023 by Family Type",  
       x = "Family Type",  
       y = "Number of Food Pantry Visits") +  
  theme_minimal(base_size = 14) +  # Improve readability
  scale_y_continuous(breaks = seq(0, 12, by = 1)) +  # Ensure y-axis has integer breaks
  coord_flip()



