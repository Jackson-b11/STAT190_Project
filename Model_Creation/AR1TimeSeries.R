rm(list=ls())
library(tidyverse)
library(lubridate)

all <- read.csv(
  "/Users/gabemeier/Desktop/STAT 190/drake_export_v8_2024-02-13_100754_rev2_nolatlong.csv.crdownload")

# Per Visit Level ----
all <- all %>% mutate(served_date = ymd(served_date),
                      dob = ymd(dob))
# Will have to figure out what to do with the 57 that failed to parse

visit <- all %>% 
  group_by(afn, served_date) %>% 
  summarise(
    n_household = n(), #number of people in household when they visited, note these can change overtime
    zip = first(zip)
    # We will want more columns here
  ) %>% 
  mutate(
    served_year = year(served_date),
    served_month = month(served_date),
    served_day_of_month = mday(served_date),
    round_month = round_date(served_date, unit = 'month')
    #probably add even more here
  )

# now we can count visits
monthly_counts <- visit %>% 
  group_by(round_month) %>% 
  summarise(num_Visits = n(),
            num_People_Served = sum(n_household)) %>% 
  mutate(people_per_visit <- num_People_Served/num_Visits)

# Add a var for month of year
monthly_counts$month <- as.factor(month(monthly_counts$round_month))
monthly_counts <- monthly_counts[c(1:(nrow(monthly_counts)-2)),]

# Create y variable, x variables for a time series
monthly_visits <- monthly_counts$num_Visits[c(2:nrow(monthly_counts))]
monthly_visits_lag1 <- monthly_counts$num_Visits[c(1:nrow(monthly_counts)-1)]
month_x <- monthly_counts$month[c(2:nrow(monthly_counts)-1)]

# Create a time series AR model
time_series_m <- lm(monthly_visits ~ monthly_visits_lag1 + month_x)
summary(time_series_m)

# Define forecast horizon
n_forecast <- 36  # Number of months to forecast
last_date <- max(monthly_counts$round_month)  # Get last date in dataset
future_dates <- seq.Date(last_date + 30, by = "month", length.out = n_forecast)  # Monthly steps

# Create future dataframe with lags and seasonal dummies
future_df <- data.frame(date = future_dates)

# Add seasonal dummies based on month
future_df$month_x <- factor(format(future_df$date, "%m"), levels = unique(format(monthly_counts$round_month, "%m")))
future_df$monthly_visits_lag1 <- c(tail(monthly_counts$num_Visits, 1), rep(NA, n_forecast - 1))  # Start with last observed visit count
library(stringr)
future_df$month_x = str_remove(future_df$month_x, "^0+")

# Predict recursively using AR(1) structure
for (i in 1:n_forecast) {
  future_df$monthly_visits_lag1[i] <- ifelse(i == 1, tail(monthly_counts$num_Visits, 1), future_df$predicted_visits[i - 1])
  future_df$predicted_visits[i] <- predict(time_series_m, newdata = future_df[i, ])
}

# Add confidence intervals (assume standard error)
se <- summary(time_series_m)$sigma  # Get model standard error
future_df$lower <- future_df$predicted_visits - 1.96 * se
future_df$upper <- future_df$predicted_visits + 1.96 * se

# Rename column to match historical df
colnames(monthly_counts)[colnames(monthly_counts) == "num_Visits"] <- "predicted_visits"
colnames(monthly_counts)[colnames(monthly_counts) == "round_month"] <- "date"

# Combine past and future data
plot_data <- monthly_counts %>%
  mutate(type = "Actual") %>%
  bind_rows(future_df %>% mutate(type = "Forecast"))

future_df$type <- "Forecast"
monthly_counts$type <- "Actual"

# Plot with ggplot2
ggplot(data = plot_data, aes(x = as.Date(date), y = predicted_visits, colour = type)) +
  geom_line(size = 1) +
  geom_line(data = future_df, aes(y = predicted_visits), linetype = "dashed", size = 1) +
  geom_ribbon(data = future_df, aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  scale_color_manual(values = c("Actual" = "black", "Forecast" = "blue")) +
  labs(title = "Total Visits: Historical and Forecasted",
       x = "Date", y = "Visits",
       color = "Legend") +
  theme_minimal()
