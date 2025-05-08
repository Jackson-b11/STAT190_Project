# Load required packages
library(randomForest)
library(pROC)
library(ggplot2)

# Load dataset
family <- read.csv("Data/2024_5-5-2025.csv")

# Convert response variable to factor (classification)
family$X12_visits <- factor(family$X12_visits, levels = c(0, 1))

# Remove unused variables
family <- family[, !(names(family) %in% c("houseHoldIdAfn", "annualIncome", "months_visited", "X.6_visits"))]

# Split into training and testing sets
set.seed(123)
train_idx <- sample(1:nrow(family), 0.8 * nrow(family))
train_data <- family[train_idx, ]
test_data <- family[-train_idx, ]

# Define grid of values for ntree and mtry
ntree_values <- c(100, 250, 500)
mtry_values <- 1:5

# Create a data frame to store results
tune_results <- data.frame(ntree = integer(), mtry = integer(), accuracy = numeric())

# Loop over combinations of ntree and mtry
for (ntree_val in ntree_values) {
  for (mtry_val in mtry_values) {
    rf_model <- randomForest(
      X12_visits ~ ., 
      data = train_data,
      ntree = ntree_val,
      mtry = mtry_val,
      importance = TRUE
    )
    
    # Predict on test data
    preds <- predict(rf_model, test_data)
    
    # Calculate accuracy
    conf_mat <- table(Predicted = preds, Actual = test_data$X12_visits)
    acc <- sum(diag(conf_mat)) / sum(conf_mat)
    
    # Store result
    tune_results <- rbind(tune_results, data.frame(ntree = ntree_val, mtry = mtry_val, accuracy = acc))
  }
}

# Find best result
best_result <- tune_results[which.max(tune_results$accuracy), ]
cat("Best ntree:", best_result$ntree, "\n")
cat("Best mtry:", best_result$mtry, "\n")
cat("Best accuracy:", round(best_result$accuracy, 3), "\n")

# Plot accuracy by ntree and mtry
ggplot(tune_results, aes(x = ntree, y = accuracy, color = factor(mtry))) +
  geom_point() +
  geom_line() +
  labs(title = "Random Forest Tuning: Accuracy by ntree and mtry",
       x = "Number of Trees (ntree)",
       y = "Accuracy",
       color = "mtry") +
  theme_minimal()
