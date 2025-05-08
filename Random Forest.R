# Install and load required packages (uncomment if not installed)
# install.packages("randomForest")
# install.packages("pROC")
# install.packages("caret")
# install.packages("viridis")
library(randomForest)
library(pROC)
library(ggplot2)
library(caret)
library(viridis)
library(dplyr)
library(vip)


# Load dataset
family <- read.csv("Data/2024_5-5-2025.csv")

# Convert response variable to a factor for classification (0 = less than 12 visits, 1 = exactly 12)
family$X12_visits <- factor(family$X12_visits, levels = c(0, 1))

# Remove unwanted columns
family <- family[, !(names(family) %in% c("houseHoldIdAfn", "annualIncome", "months_visited", "X.6_visits"))]
View(family)
# Split into training and testing sets
set.seed(123)
train_idx <- sample(1:nrow(family), 0.8 * nrow(family))
train_data <- family[train_idx, ]
test_data <- family[-train_idx, ]

# Train the Random Forest classification model
rf_model <- randomForest(
  X12_visits ~ .,  
  data = train_data,
  ntree = 250,
  mtry = 3,
  importance = TRUE
)

# Predict class probabilities for class "1" (exactly 12 visits)
rf_probs <- predict(rf_model, test_data, type = "prob")[, "1"]

# ROC analysis
roc_obj <- roc(response = test_data$X12_visits,
               predictor = rf_probs,
               levels = c("0", "1"),
               direction = "<")

# Plot ROC curve
plot(roc_obj, col = "blue", lwd = 2, main = "ROC Curve for Random Forest")
abline(a = 0, b = 1, lty = 2, col = "gray")

# Print AUC
auc_val <- auc(roc_obj)
print(paste("AUC:", round(auc_val, 3)))

# Find optimal threshold using Youden's index
opt_coords <- coords(roc_obj, "best", best.method = "youden", transpose = FALSE)
optimal_thresh <- as.numeric(opt_coords["threshold"])
print(paste("Optimal Threshold:", round(optimal_thresh, 3)))

# Reclassify using optimal threshold
rf_preds_opt <- ifelse(rf_probs > optimal_thresh, "1", "0")
rf_preds_opt <- factor(rf_preds_opt, levels = c("0", "1"))

# Confusion matrix
conf_mat_opt <- table(Predicted = rf_preds_opt, Actual = test_data$X12_visits)
print(conf_mat_opt)

# Accuracy
accuracy_opt <- sum(diag(conf_mat_opt)) / sum(conf_mat_opt)

# Sensitivity (Recall)
tp <- conf_mat_opt["1", "1"]
fn <- conf_mat_opt["0", "1"]
sensitivity_opt <- tp / (tp + fn)

# Specificity
tn <- conf_mat_opt["0", "0"]
fp <- conf_mat_opt["1", "0"]
specificity_opt <- tn / (tn + fp)

# Print evaluation metrics
print(paste("Optimized Accuracy:", round(accuracy_opt, 3)))
print(paste("Optimized Sensitivity:", round(sensitivity_opt, 3)))
print(paste("Optimized Specificity:", round(specificity_opt, 3)))

# Feature importance plot

library(vip)
library(dplyr)

# Extract importance data
imp_data <- vi(rf_model)  # from the vip package

# Rename variables
imp_data$Variable <- recode(imp_data$Variable,
                            "fedPovertyLevel" = "Federal Poverty Level",
                            "race_family" = "Race of the Family",
                            "category_family" = "Employment Category",
                            "income_bin" = "Income",
                            "household_size" = "Household Size",
                            "family_foodstamps" = "Uses Foodstamps",
                            "location" = "Location", 
                            "percent_male" = "Percentage of Males",
                            "education_count" = "Education Level",
                            "family_veteran" = "Veteran Status",
                            "family_dietaryIssue"= "Dietary Needs"
)

# Plot manually
ggplot(imp_data %>% top_n(10, Importance), aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "gray30") +
  geom_text(aes(label = round(Importance, 3)), hjust = 1.1, color = "white", size = 4) +
  coord_flip() +
  labs(title = "Variable Importance Scores",
       x = "Variables", y = "Importance Score") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.text.y = element_text(size = 12))


# --- Updated Confusion Matrix Heatmap ---

# Prepare confusion matrix data
cm_df <- as.data.frame(conf_mat_opt)
names(cm_df) <- c("Predicted", "Actual", "Freq")

# Reverse factor levels to ensure (0,0) is top-left, (1,1) bottom-right
cm_df$Actual <- factor(cm_df$Actual, levels = c("1", "0"))        # reverse y-axis
cm_df$Predicted <- factor(cm_df$Predicted, levels = c("0", "1"))  # x-axis as usual

# Replace numeric labels with descriptive ones
cm_df$Actual <- factor(cm_df$Actual, levels = c("1", "0"), labels = c("12 Visits", "<12 Visits"))
cm_df$Predicted <- factor(cm_df$Predicted, levels = c("0", "1"), labels = c("<12 Visits", "12 Visits"))

# Plot confusion matrix with white fill and clear labels
ggplot(data = cm_df, aes(x = Actual, y = Predicted)) +
  geom_tile(fill = "white", color = "black", linewidth = 1.2) +
  geom_text(aes(label = Freq), size = 6, fontface = "bold") +
  labs(title = "Confusion Matrix",
       x = "Actual Class", y = "Predicted Class") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 14)
  )


#Boxplot: 
ggplot(family, aes(x = X12_visits, y = as.numeric(as.character(household_size)), fill = X12_visits)) +
  geom_boxplot() +
  labs(
    title = "Household Size Distribution by Pantry Visit Category",
    x = "Visited All 12 Months",
    y = "Household Size"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#Visit Rate: MAYBE SPLIT THIS INTO BUCKETS 
family %>%
  group_by(household_size) %>%
  summarise(percent_12mo = mean(as.numeric(as.character(X12_visits))) * 100) %>%
  ggplot(aes(x = as.numeric(as.character(household_size)), y = percent_12mo)) +
  geom_line(size = 1.5, color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  labs(
    title = "Percentage of Families Visiting All 12 Months by Household Size",
    x = "Household Size",
    y = "% Visiting All 12 Months"
  ) +
  theme_minimal()

#Bar plot
library(dplyr)
library(ggplot2)
library(scales)

# Convert household_size to numeric if not already
family$household_size <- as.numeric(as.character(family$household_size))

# Create grouped household size categories
family <- family %>%
  mutate(household_group = case_when(
    household_size == 1 ~ "1",
    household_size == 2 ~ "2",
    household_size >= 3 & household_size <= 5 ~ "3-5",
    household_size >= 6 & household_size <= 9 ~ "6-9",
    household_size >= 10 ~ "10+"
  ))

# Convert to ordered factor for proper axis sorting
family$household_group <- factor(family$household_group, levels = c("1", "2", "3-5", "6-9", "10+"))
# Recode X12_visits for better labels
family$X12_visits <- factor(family$X12_visits, levels = c(0, 1), labels = c("<12", "12"))

# Plot with custom blue shades
ggplot(family, aes(x = household_group, fill = X12_visits)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = c("<12" = "#ADD8E6", "12" = "#00008B")  # light blue, dark blue
  ) +
  labs(
    title = "Proportion of Families Visiting All 12 Months by Household Size",
    x = "Household Size",
    y = "Percentage of Dependent Families",
    fill = "Classification of Families"
  ) +
  theme_minimal(base_size = 14)



#Summarizing large households by location: 
library(dplyr)

# Ensure household_size is numeric
family$household_size <- as.numeric(as.character(family$household_size))

# Filter for:
# - households with 6 or more people
# - visited all 12 months (X12_visits == 1)
large_frequent_users <- family %>%
  filter(household_size >= 6, X12_visits == 1)

# Count how many of these families are in each location
location_summary <- large_frequent_users %>%
  count(location, sort = TRUE)

# View result
print(location_summary)


#Getting average household size by food pantry
library(dplyr)

# Ensure household_size is numeric
family$household_size <- as.numeric(as.character(family$household_size))

# Group by location and calculate average
avg_by_location <- family %>%
  group_by(location) %>%
  summarise(
    average_household_size = round(mean(household_size, na.rm = TRUE), 2),
    n_families = n()
  ) %>%
  arrange(desc(average_household_size))

# View result
print(avg_by_location)


#updated average family by location:
library(dplyr)

# Ensure household_size is numeric
family$household_size <- as.numeric(as.character(family$household_size))

# Group and summarize
household_summary <- family %>%
  group_by(location) %>%
  summarise(
    average_household_size = round(mean(household_size, na.rm = TRUE), 2),
    n_families_total = n(),
    n_families_1 = sum(household_size == 1, na.rm = TRUE),
    n_families_2 = sum(household_size == 2, na.rm = TRUE),
    n_families_3_5 = sum(household_size >= 3 & household_size <= 5, na.rm = TRUE),
    n_families_6_9 = sum(household_size >= 6 & household_size <= 9, na.rm = TRUE),
    n_families_10plus = sum(household_size >= 10, na.rm = TRUE),
    n_families_6plus = sum(household_size >= 6, na.rm = TRUE)
  ) %>%
  arrange(desc(n_families_total))

print(household_summary)


#PROPORTIONS TABLE: 
# Add proportions while keeping total family count
household_proportions <- household_summary %>%
  mutate(
    prop_1 = round(n_families_1 / n_families_total, 3),
    prop_2 = round(n_families_2 / n_families_total, 3),
    prop_3_5 = round(n_families_3_5 / n_families_total, 3),
    prop_6_9 = round(n_families_6_9 / n_families_total, 3),
    prop_10plus = round(n_families_10plus / n_families_total, 3),
    prop_6plus = round(n_families_6plus / n_families_total, 3)
  ) %>%
  select(
    location,
    average_household_size,
    n_families_total,
    prop_1,
    prop_2,
    prop_3_5,
    prop_6_9,
    prop_10plus,
    prop_6plus
  )

print(household_proportions)


