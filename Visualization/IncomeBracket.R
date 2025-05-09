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

# now we can count visits
monthly_counts <- visit %>% 
  group_by(round_month) %>% 
  summarise(num_Visits = n(),
            num_People_Served = sum(n_household)) %>% 
  mutate(people_per_visit <- num_People_Served/num_Visits)

# Big question - how many people in the Des Moines metro look like our new customers?

# Income Visualization ----
# Create Income Bin variable
all$income_bucket <- ifelse(all$annual_income < 5000, "Under 5k", 
                            ifelse(all$annual_income < 15000, "5k - 15k",
                                   ifelse(all$annual_income < 25000, "15k - 25k",
                                          "Over 25k")))
all$served_year = year(all$served_date)
all$served_month = month(all$served_date)

monthly_inc_count <- all %>% 
  group_by(served_year, served_month, income_bucket) %>% 
  summarise(num_Visits = n())

monthly_inc_count$Date<-as.Date(
  with(monthly_inc_count,paste(served_year,served_month,1,sep="-")),"%Y-%m-%d")

monthly_inc_count$income_bucket <- factor(
  monthly_inc_count$income_bucket, levels = c("Under 5k", "5k - 15k",
                                              "15k - 25k", "Over 25k"))

monthly_inc_count_g <-  monthly_inc_count %>% 
  filter(served_year != 2024 | served_month != 2)

ggplot(data = monthly_inc_count_g, aes(x = Date, y = num_Visits, color = income_bucket))+
  geom_line()
