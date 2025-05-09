# --- Required Libraries ---
library(xgboost)
library(pROC)
library(caret)
library(ggplot2)
library(dplyr)
library(vip)
library(Matrix)

# --- Load Data ---
family <- read.csv("Data/2024_5-5-2025.csv")
family$X12_visits <- factor(family$X12_visits, levels = c(0, 1))
family <- family[, !(names(family) %in% c("houseHoldIdAfn", "annualIncome", "months_visited", "X.6_visits"))]

# --- Create Matrix and Label ---
xgb_matrix <- model.matrix(X12_visits ~ . - 1, data = family)
xgb_label <- as.numeric(family$X12_visits) - 1

# --- Split into Train/Test ---
set.seed(123)
train_idx <- sample(seq_len(nrow(family)), size = 0.8 * nrow(family))
train_matrix <- xgb_matrix[train_idx, ]
test_matrix <- xgb_matrix[-train_idx, ]
train_label <- xgb_label[train_idx]
test_label <- xgb_label[-train_idx]

# --- Build DMatrices ---
dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dtest <- xgb.DMatrix(data = test_matrix, label = test_label)

# --- Train Model ---
params <- list(objective = "binary:logistic", eval_metric = "auc", max_depth = 3, eta = 0.1)
xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100, verbose = 0)

# --- Predict Probabilities ---
xgb_probs <- predict(xgb_model, dtest)

# --- ROC and AUC ---
true_labels <- getinfo(dtest, "label")  # Get true labels from dtest
roc_obj <- roc(response = true_labels, predictor = xgb_probs)
plot(roc_obj, col = "blue", lwd = 2, main = "ROC Curve")
abline(a = 0, b = 1, lty = 2)
auc_val <- auc(roc_obj)
cat("AUC:", round(auc_val, 3), "\n")

# --- Optimal Threshold ---
opt_thresh <- coords(roc_obj, "best", best.method = "youden")[["threshold"]]
cat("Optimal Threshold:", round(opt_thresh, 3), "\n")

# --- Generate Predictions ---
xgb_preds <- ifelse(xgb_probs > opt_thresh, 1, 0)

# --- Convert to factors ---
xgb_preds <- factor(xgb_preds, levels = c(0, 1))
actual <- factor(true_labels, levels = c(0, 1))

# --- Sanity check lengths before table ---
cat("Length of predictions:", length(xgb_preds), "\n")
cat("Length of actual labels:", length(actual), "\n")

# --- If lengths match, print confusion matrix ---
if (length(xgb_preds) == length(actual)) {
  conf_mat <- table(Predicted = xgb_preds, Actual = actual)
  print(conf_mat)
  
  # --- Metrics ---
  accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
  sensitivity <- conf_mat["1", "1"] / sum(conf_mat[, "1"])
  specificity <- conf_mat["0", "0"] / sum(conf_mat[, "0"])
  
  cat("Accuracy:", round(accuracy, 3), "\n")
  cat("Sensitivity:", round(sensitivity, 3), "\n")
  cat("Specificity:", round(specificity, 3), "\n")
  
} else {
  cat("ERROR: Predictions and labels are different lengths. Please check dataset dimensions.\n")
}

# --- Variable Importance Plot ---
vip::vip(xgb_model, num_features = 10, geom = "col", aesthetics = list(fill = "gray30")) +
  ggtitle("Top 10 Variable Importance - XGBoost")
