# Load required libraries
library(xgboost)
library(caret)
library(Matrix)

# Load dataset
family <- read.csv("Data/2024_5-5-2025.csv")

# Create valid classification labels BEFORE removing original response
family$visit_category <- factor(
  ifelse(family$months_visited >= 12, "more_3", "less_3"),
  levels = c("less_3", "more_3")
)

# Drop unused variables
family <- family[, !(names(family) %in% c("afn", "months_visited"))]

# Create model matrix (one-hot encoding)
full_matrix <- model.matrix(visit_category ~ . - 1, data = family)
full_label <- family$visit_category

# Split into training and test sets
set.seed(123)
train_idx <- sample(1:nrow(family), 0.8 * nrow(family))
train_data <- full_matrix[train_idx, ]
test_data <- full_matrix[-train_idx, ]
train_label <- full_label[train_idx]
test_label <- full_label[-train_idx]

# Prepare data for caret
train_df <- as.data.frame(train_data)
train_df$visit_category <- train_label

# 🔍 Modified tuning grid with max_depth limited to a range from 3 to 6
xgb_grid <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c(3, 4, 5, 6),  # Test values from 3 to 6
  eta = c(0.05, 0.1, 0.3),
  gamma = c(0, 1),
  colsample_bytree = c(0.6, 0.8, 1),
  min_child_weight = c(1, 3),
  subsample = c(0.6, 0.8, 1)
)

# Cross-validation setup
train_control <- trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter = TRUE,
  allowParallel = TRUE
)

# Train XGBoost with the updated grid
set.seed(123)
xgb_tuned <- train(
  visit_category ~ .,
  data = train_df,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = xgb_grid,
  metric = "ROC",
  verbose = TRUE
)

# Show best parameters
print(xgb_tuned$bestTune)

# Test set prediction
test_df <- as.data.frame(test_data)
xgb_preds_
