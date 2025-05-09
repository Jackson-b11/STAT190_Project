# Read in Data/Install Packages ----
# Clear Working Environment
rm(list=ls())

# Install necessary packages. Un-comment these lines if not already installed.
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("randomForest")
#install.packages("pROC")
#install.packages("scales")

# Load Necessary packages
library(tidyverse)
library(lubridate)
library(randomForest)
library(pROC)
library(scales)

#Load Data

#IMPORTANT: Set this to the directory where data is stored
setwd('/Users/gabemeier/Desktop/STAT 190')
all <- read.csv(
  "drake_export_v8_2024-02-13_100754_rev2_nolatlong.csv.crdownload")

# Clean and Format Data ----
# Get the served_date column in date format
all <- all %>% mutate(served_date = ymd(served_date),
                      dob = ymd(dob))

# Group data by families and find the first date that family was served
families <- all %>% 
  group_by(afn) %>% 
  summarise(
    first_date = min(served_date),
    visits = n()
    # We will want more columns here
  ) %>% 
  mutate(
    first_year = year(first_date),
    # Create a column indicating if a family visited for the first time in 2023
    new_2023 = ifelse(first_year == 2023,1,0)
  )


# Read in cleaned data
df <- read.csv('df_2_25_2025.csv')
# Use a left join to add column indicating if a family's first visit was in 2023
df <- left_join(df, families, "afn")
df$education_per <- df$education_count/df$household_size
# Remove columns that won't serve as predictor variables
df <- df %>% select(-c(first_date, visits, months_visited, first_year))
df$new_2023 <- as.factor(df$new_2023)

# Train/test split ----
RNGkind(sample.kind="default")
set.seed(19086)
# Create a random sample for training data
train.idx <- sample(x = 1:nrow(df), size = 0.7*nrow(df))
# Split into train and test sets
train.df <- df[train.idx,]
test.df <- df[-train.idx,]
train.df <- train.df %>% subset(select = -c(afn))

# Tune for mtry ----
# Un-comment this code to run the tuning process
# Currently commented as this section is computationally intensive
# mtry <- seq(1,ncol(train.df)-1)
# keeps <- data.frame(m=rep(NA,length(mtry)),
#                     OOB_err_rate = rep(NA,length(mtry)))
# 
# for(idx in 1:length(mtry)){
#   print(paste0("Trying m = ",mtry[idx]))
#   tempforest <- randomForest(new_2023~.,
#                              data = train.df,
#                              ntree=500,
#                              mtry = mtry[idx])
#   keeps[idx,"m"] <-mtry[idx]
#   keeps[idx,"OOB_err_rate"] <- mean(predict(tempforest)!=train.df$new_2023)
# }
# 
# ggplot(data = keeps)+
#   geom_line(aes(x=m, y=OOB_err_rate)) +
#   theme_bw() + labs(x = "m (mtry) value",y="OOB error rate")

# The best mtry is 2

# Make forest, AUC curve ----
# Create a random forest using optimal tuning parameters
rf <- randomForest(new_2023~.,
                   data = train.df,
                   ntree=500,
                   mtry = 2,
                   importance = TRUE)

# Test accuracy of the model using AUC
pi_hat <- predict(rf,test.df, type = "prob")[,"1"]

rocCurve <- roc(response = test.df$new_2023, #Truth
                predictor = pi_hat, #Predictions
                levels = c("0","1")) #Supply negative event, then positive event
plot(rocCurve, print.thres = TRUE, print.auc = TRUE)

pi_star <- coords(rocCurve, "best", ret="threshold")$threshold[1] 
test.df$new_2023_pred_rf <- as.factor(ifelse(pi_hat>pi_star,"1","0"))
test.df$correct_pred <- test.df$new_2023 == test.df$new_2023_pred_rf

# Variable Importance ----
varImpPlot(rf, type=1)

vi<-as.data.frame(varImpPlot(rf,type=1))
vi$Variable <- rownames(vi)
# This plot identifies the most important variables in the analysis
# These are the variables that change the most between the new and traditional customer bases
ggplot(data = vi) +
  geom_bar(aes(x=reorder(Variable, MeanDecreaseAccuracy), weight = MeanDecreaseAccuracy)
           , position = "identity")+
  coord_flip()+
  labs(x="Variable Name",y="Mean Decrease Accuracy", title = "Variable Importance")

# Income Visualization ----
# Create income buckets for visualization
df$income_bucket <- ifelse(df$annual_income < 5000, "Under 5k", 
                           ifelse(df$annual_income < 10000, "5k - 10k",
                                  ifelse(df$annual_income < 20000, "10k - 20k",
                                         ifelse(df$annual_income < 35000, "20k - 35k",
                                                "Over 35k"))))

df$income_bucket <- factor(
  df$income_bucket, levels = c("Under 5k", "5k - 10k", "10k - 20k",
                               "20k - 35k", "Over 35k"))

# Modify the new_2023 variable to make it more descriptive for graphs
dfv <- df
dfv$new_2023 = ifelse(dfv$new_2023 == 0, 
                      "Traditional Customer Base \n (Families who had visited DMARC in past years)",
                      "New Customer Base \n (Families who visited DMARC for the first time in 2023)")

dfv$new_2023 <- factor(dfv$new_2023, levels = unique(dfv$new_2023))

# Manually calculate proportions by income_bucket and new_2023
inc_bucket_prop <- dfv %>%
  group_by(income_bucket, new_2023) %>%
  summarise(count = n()) %>%
  group_by(new_2023) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()

# Define a monochrome blue scale with 5 categories
blue_shades <- c("Under 5k" = "#c6dbef", "5k - 10k" = "#9ecae1", "10k - 20k" = "#6baed6", "20k - 35k" = "#2171b5",  "Over 35k" = "#084594")

# Plot income by bucket
inc_bucket_newfam <- ggplot(inc_bucket_prop, aes(x = income_bucket, y = prop, fill = income_bucket)) +
  geom_bar(stat = "identity", color = "black") +  # Use stat = "identity" for manually computed proportions
  scale_fill_manual(values = blue_shades) +  # Apply the custom blue scale
  labs(title = "Percentage of Families Visiting DMARC by Income Bucket", 
       x = "Income Bucket ($)", y = "Percentage") +
  facet_wrap(~new_2023) +  # Facet by new_2023
  scale_y_continuous(labels = percent_format(accuracy = 1)) +  # Convert y-axis to percentage format
  theme_minimal() +
  theme(
    plot.title = element_text(size = 40, face = "bold"),
    strip.text = element_text(size = 20),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 18),
    legend.position = "none"  # No legend
  )

# Save the visualization in the Visualizations folder
ggsave("Visualizations/New Family Income Bucket Bar Chart.png",plot = inc_bucket_newfam,
       width = 16, height = 9, units="in",bg="white", create.dir=TRUE)

dfv <- df[df$annual_income<=80000&df$annual_income>=0,]
dfv$new_2023 = ifelse(dfv$new_2023 == 0, 
                      "Traditional Customer Base \n (Families who had visited DMARC in past years)",
                      "New Customer Base \n (Families who visited DMARC for the first time in 2023)")

#Faceted Histogram
# Create income buckets (more detailed)
dfv_processed <- dfv %>%
  mutate(income_bin = cut(annual_income, breaks = seq(-5000, 85000, by = 10000), include.lowest = TRUE)) %>%
  group_by(new_2023, income_bin) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(new_2023) %>%
  mutate(prop = n / sum(n))

# Make names more descriptive for the graph
dfv_processed$income_bin <- ifelse(
  dfv_processed$income_bin == "[-5e+03,5e+03]", "0",
  ifelse(dfv_processed$income_bin =="(5e+03,1.5e+04]","10,000",
  ifelse(dfv_processed$income_bin =="(1.5e+04,2.5e+04]","20,000",
  ifelse(dfv_processed$income_bin =="(2.5e+04,3.5e+04]","30,000",
  ifelse(dfv_processed$income_bin =="(3.5e+04,4.5e+04]","40,000",
  ifelse(dfv_processed$income_bin =="(4.5e+04,5.5e+04]","50,000",
  ifelse(dfv_processed$income_bin =="(5.5e+04,6.5e+04]","60,000",
  ifelse(dfv_processed$income_bin =="(6.5e+04,7.5e+04]","70,000",
  ifelse(dfv_processed$income_bin =="(7.5e+04,8.5e+04]","80,000", "Error"
  )))))))))

# Reverse order of factor so that graph follows a logical sequence
dfv_processed$new_2023 <- factor(dfv_processed$new_2023, levels = rev(unique(dfv_processed$new_2023)))

# Define 9 colors for each bar of the graph
blue_shades <- rev(c(
  "#deebf7", "#c6dbef", "#9ecae1", "#6baed6",
  "#4292c6", "#2171b5", "#08519c", "#08306b", "#041f47"
))

# Create graph
inc_hist_newfam <- ggplot(dfv_processed, aes(x = income_bin, y = prop, fill = income_bin)) +
  geom_col(color = "black") +
  facet_wrap(~ new_2023) +
  scale_fill_manual(values = blue_shades) +
  labs(
    title = "Percentage of Families Visiting DMARC by Income",
    x = "Annual Income ($)",
    y = "Proportion of Families",
    fill = "Income Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 40, face = "bold"),
    strip.text = element_text(size = 20),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 18),
    legend.position = "none"
  )


# Save graph to visualization directory
ggsave("Visualizations/New Family Income Histogram.png",plot = inc_hist_newfam,
       width = 16, height = 9, units="in",bg="white")

# Ethnicity Visualization ----  
# Remove unknown or NA ethnicities
dfv <- df[df$ethnicity_family %in% c("Hispanic or Latino", "Not Hispanic or Latino", "Mixed"),]

# Make new_2023 more descriptive for the graph
dfv$new_2023 = ifelse(dfv$new_2023 == 0, 
                      "Traditional Customer Base \n (Families who had visited DMARC in past years)",
                      "New Customer Base \n (Families who visited DMARC for the first time in 2023)")

# Ensure logical order for faceted graph
dfv$new_2023 <- factor(dfv$new_2023, levels = unique(dfv$new_2023))

# Calculate proportions
eth_vis <- dfv %>% 
  group_by(ethnicity_family, new_2023) %>% 
  summarise(count = n()) %>%
  group_by(new_2023) %>%
  mutate(prop = count / sum(count))

# Define the blue shades
blue_shades <- rev(c("#c6dbef", "#6baed6", "#2171b5"))

# Plot
eth_vis_newfam <- ggplot(eth_vis, aes(x = ethnicity_family, y = prop, fill = ethnicity_family)) +
  geom_col(color = "black") +  # Use geom_col to show precomputed proportions
  scale_fill_manual(values = blue_shades) +
  facet_wrap(~ new_2023) +
  labs(
    title = "Percentage of Families Visiting DMARC by Ethnicity",
    x = "Ethnicity",
    y = "Proportion of Families"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 40, face = "bold"),
    strip.text = element_text(size = 20),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 18),
    legend.position = "none"  # No legend
  )

# Save graph in Visualizations directory
ggsave("Visualizations/New Family Ethnicity Bar Chart.png",plot = eth_vis_newfam,
       width = 16, height = 9, units="in",bg="white")

# Race Visualization ----  

dfv <- df
# Change naming of variable to make graph easier to read
dfv$race_family = ifelse(df$race_family=="Asian", "Asian",
                         ifelse(df$race_family=="Black/African American", "Black",
                                ifelse(df$race_family=="Mixed", "Mixed",
                                       ifelse(df$race_family=="Multi-Race", "Mixed",
                                              ifelse(df$race_family=="White", "White", "Other")))))

# Change naming for new_2023 to make it more descriptive
dfv$new_2023 = ifelse(dfv$new_2023 == 0, 
                      "Traditional Customer Base \n (Families who had visited DMARC in past years)",
                      "New Customer Base \n (Families who visited DMARC for the first time in 2023)")

# Reserve order for logical flow of graph
dfv$new_2023 <- factor(dfv$new_2023, levels = unique(dfv$new_2023))

# Calculate proportions by race_family and new_2023
race_vis <- dfv %>%
  group_by(race_family, new_2023) %>%
  summarise(count = n()) %>%
  group_by(new_2023) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()

# Define shades of blue
blue_shades <- c("White" = "#c6dbef", "Other" = "#9ecae1", "Mixed" = "#6baed6", "Black" = "#2171b5", "Asian" = "#084594")

# Plot
race_vis_newfam <- ggplot(race_vis, aes(x = race_family, y = prop, fill = race_family)) +
  geom_col(color = "black") +  # Using geom_col to show precomputed proportions
  scale_fill_manual(values = blue_shades) +  # Apply the custom color mapping for each category
  facet_wrap(~ new_2023) +
  labs(
    title = "Percentage of Families Visiting DMARC by Race",
    x = "Race",
    y = "Proportion of Families"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 40, face = "bold"),
    strip.text = element_text(size = 20),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 18),
    legend.position = "none"  # No legend
  )

# Save plot in Visualization directory
ggsave("Visualizations/New Family Race Bar Chart.png",plot = race_vis_newfam,
       width = 16, height = 9, units="in",bg="white")

