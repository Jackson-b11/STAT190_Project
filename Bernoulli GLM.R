#Install libraries
rm(list = ls())
library(tidyverse)
library(lubridate)
library(ggplot2)
library(MASS)
library(dplyr)
library(pscl)
library(pROC)
library(vip)
library(caret)
library(glmnet)

#Load 2025 Dataset
cleandata <- read.csv("2024_5-5-2025.csv")

#Remove afn, more than 6 visits, months visited, and annual income variables
cleandata <- cleandata[, !names(cleandata) %in% "houseHoldIdAfn"]

cleandata <- cleandata[, !names(cleandata) %in% "X.6_visits"]
cleandata <- cleandata[, !names(cleandata) %in% "months_visited"]
cleandata <- cleandata[, !names(cleandata) %in% "annualIncome"]
View(cleandata)

#Split data into train and test data sets
set.seed(123)
train_index <- createDataPartition(cleandata$X12_visits, p = 0.8, list = FALSE)
train_data <- cleandata[train_index, ]
test_data <- cleandata[-train_index, ]

#Created a Bernoulli model for predicting families that visit 12 times in a year
model <- glm(X12_visits ~ ., 
             data = cleandata, 
             family = binomial(link = "logit"))

summary(model)

#Tried using 6 or more months as the predicted variable but we decided not to go
#this route and instead went with 12 months.
'''
cleandata$predicted_probs <- predict(model, type = "response")

roc_obj <- roc(cleandata$X12_visits, cleandata$predicted_probs)

# Plot the ROC curve
plot(roc_obj, col = "blue", lwd = 2, main = "ROC Curve")
abline(a = 0, b = 1, lty = 2, col = "gray")  # reference line


predicted_class <- ifelse(cleandata$predicted_probs > .5 ,1, 0)
confusionMatrix(as.factor(predicted_class), as.factor(cleandata$X.6_visits))

#Perform stepwise model
model_step <- step(model, direction = "both")

summary(model_step)
predicted_probs_step <- predict(model_step, type = "response")
predicted_class_step <- ifelse(predicted_probs_step >= 0.5, 1, 0)
confusionMatrix(as.factor(predicted_class_step),as.factor(cleandata$X.6_visits)
)
'''
#Use Lasso to tune a better model
x_train <- model.matrix(X12_visits ~ . - 1, data = train_data)
y_train <- train_data$X12_visits
x_test <- model.matrix(X12_visits ~ . - 1, data = test_data)

cv_model <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)
best_lambda <- cv_model$lambda.min

lasso_model <- glmnet(x_train, y_train, family = "binomial", alpha = 1, lambda = best_lambda)
#Test new model
predictions <- predict(lasso_model, newx = x_test, type = "response")
predicted_class2 <- ifelse(predictions > 0.5, 1, 0)
confusionMatrix(as.factor(predicted_class2), as.factor(test_data$X12_visits))

# Compute ROC curve
roc_obj <- roc(cleandata$X12_visits, predictions)

# Plot the ROC curve
roc_obj_lasso <- roc(test_data$X12_visits, predictions)
plot(roc_obj_lasso, col = "blue", print.auc = TRUE, main = "ROC Curve - LASSO Model")
abline(a = 0, b = 1, lty = 2, col = "gray")

#found the optimal threshold for predictions
opt_thresh <- coords(roc_obj_lasso, "best", ret = "threshold", best.method = "youden")
print(opt_thresh)

predicted_class3 <- ifelse(predictions > .0958, 1, 0)
#Created a Confusion Matrix
cm <- confusionMatrix(as.factor(predicted_class3), as.factor(test_data$X12_visits))
#Visualize Confusion Matrix
cm_table <- as.data.frame(cm$table)
colnames(cm_table) <- c("Predicted", "Actual", "Freq")
cm_table$Actual <- factor(cm_table$Actual, levels = c("1", "0"))
cm_table$Predicted <- factor(cm_table$Predicted, levels = c("1", "0"))

ggplot(cm_table, aes(x = Actual, y = Predicted)) +
  geom_tile(fill = "white", color = "black") +      # White tiles, black borders
  geom_text(aes(label = Freq), size = 5) +          # Show count labels
  theme_minimal() +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
  scale_y_discrete(limits = rev) +                  # Flip Y-axis
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Make x labels horizontal
    axis.text.y = element_text(angle = 0, vjust = 0.5)   # Make y labels upright
  )
#found the most important variables from the model
vip(lasso_model)
table(cleandata$family_dietaryIssue)

library(dplyr)
#Looked at the proportion of families with dietary issues at each location
result <- cleandata %>%
  group_by(location) %>%
  summarise(
    total = n(),
    family_dietaryIssue = sum(family_dietaryIssue == "True"),
    proportion = family_dietaryIssue / total
  )

print(result, n = Inf)


coefs <- as.matrix(coef(lasso_model))  # includes intercept
coefs <- data.frame(
  variable = rownames(coefs),
  importance = abs(coefs[, 1])  # use abs value for magnitude
)
 
coefs_grouped <- coefs %>%
  filter(variable != "(Intercept)") %>%
  mutate(base_var = sub("_.*", "", variable)) %>%
  group_by(base_var) %>%
  summarise(total_importance = sum(importance)) %>%
  arrange(desc(total_importance)) 

ggplot(coefs_grouped, aes(x = reorder(base_var, total_importance), y = total_importance)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(
    title = "Grouped Variable Importance (Lasso)",
    x = "Variable",
    y = "Total Importance"
  ) +
  theme_minimal(base_size = 13)
#Looked at the proportion of dependent families by race
result <- cleandata %>%
  group_by(race_family) %>%
  summarise(
    total = n(),
    X12_visits = sum(X12_visits == 1),
    proportion = X12_visits / total
  )

print(result, n = Inf)

#attempted the same process as above with 3 months or less being the target but 
#ultimately we decided to go with the 12 month so the families would be highly dependent.
'''
#Try with the prediction variable being greater than or equal to 3 months visited
cleandata2 <- read.csv("2023_4-1-25.csv")
cleandata2 <- cleandata2[, !names(cleandata2) %in% "afn"]
cleandata2 <- cleandata2[, !names(cleandata2) %in% "X12_visits"]
cleandata2 <- cleandata2[, !names(cleandata2) %in% "X.6_visits"]
cleandata2$X3_visits <- ifelse(cleandata2$months_visited >= 3, 1, 0)
cleandata2 <- cleandata2[, !names(cleandata2) %in% "months_visited"]
View(cleandata2)

model2 <- glm(X3_visits ~ ., 
             data = cleandata2, 
             family = binomial(link = "logit"))

summary(model2)

cleandata2$predicted_probs <- predict(model2, type = "response")

predicted_class2 <- ifelse(cleandata2$predicted_probs > .5 ,1, 0)
confusionMatrix(as.factor(predicted_class2), as.factor(cleandata2$X3_visits))

#Tune with lasso again for new variable
x2 <- model.matrix(X3_visits ~ . - 1, data = cleandata2)
y2 <- cleandata2$X3_visits

cv_model2 <- cv.glmnet(x2, y2, family = "binomial", alpha = 1) 
best_lambda2 <- cv_model2$lambda.min

lasso_model2 <- glmnet(x2, y2, family = "binomial", alpha = 1, lambda = best_lambda2)
#Test new model
predictions2 <- predict(lasso_model2, newx = x, type = "response")
predicted_class2 <- ifelse(predictions2 > .5 ,1, 0)
confusionMatrix(as.factor(predicted_class2), as.factor(cleandata2$X3_visits))
'''

