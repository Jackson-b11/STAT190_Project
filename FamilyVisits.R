# Read in Data ----
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

households_monthly <- visit %>% 
  group_by(round_month, afn) %>% 
  summarise(visits = n())

households_monthly <- households_monthly %>% 
  group_by(round_month) %>% 
  summarise(unique_household = n(),
            avg_visits_per_household = mean(visits))

# now we can count visits
monthly_counts <- visit %>% 
  group_by(round_month) %>% 
  summarise(num_Visits = n(),
            num_People_Served = sum(n_household)) %>% 
  mutate(people_per_visit = num_People_Served/num_Visits)

households_monthly$total_visits <- monthly_counts$num_Visits
avg_visits <- select(households_monthly, c(round_month,avg_visits_per_household))
households_monthly <- select(households_monthly, -avg_visits_per_household)

hm_long <- households_monthly %>%
  pivot_longer(cols = c(unique_household, total_visits), names_to = "variable", values_to = "value")

ggplot(hm_long, aes(x = round_month, y = value, color = variable)) +
  geom_line(size = 1) +
  labs(title = "Total Visits and Total Households Served by Month",
       x = "Date",
       y = "Visits",
       color = "Variable") +
  scale_color_brewer(palette = "Set1")

ggplot(avg_visits, aes(x = round_month, y = avg_visits_per_household)) +
  geom_line(color = "orange") + 
  labs(title = "Average Times each household visited per month",
       x = "Date",
       y = "Average visits per household per month") +
  scale_color_brewer("Purples")
