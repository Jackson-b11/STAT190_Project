install.packages("randomForest")
library(randomForest)


# Load dataset
data(iris)

# Split data into training and test sets
set.seed(123)
train_idx <- sample(1:nrow(iris), 0.8 * nrow(iris))  # 80% for training
train_data <- iris[train_idx, ]
test_data <- iris[-train_idx, ]

# Train the Random Forest Model
rf_model <- randomForest(
  Species ~ .,  # Predict Species using all other variables
  data = train_data,
  ntree = 500,  # Number of trees
  mtry = 2,  # Number of variables randomly selected at each split
  importance = TRUE  # Compute variable importance
)

# Make Predictions
rf_preds <- predict(rf_model, test_data)
accuracy <- mean(rf_preds == test_data$Species)
print(paste("Accuracy:", round(accuracy, 3)))

# Check Feature Importance
importance(rf_model)
varImpPlot(rf_model)

















































