# Programming with dplyr
# 
# Exercise
######### ######### ######### ######### #########
# Task 1: Filter days where there are missing values in bike counts and weather information. 
#    Count number of days with missing values on either bike counts or weather information.

# Bike data
source("code/load_data.R")

noBikeData <- bikecounts %>% 
  filter(is.na(westbound)| is.na(eastbound)) %>% 
  summarise(count = n())
noBikeData
# 387 no data records
#instructor code
bikecounts %>% 
  filter(is.na(westbound)| is.na(eastbound)) %>%
  group_by(date) %>%
  tally
# 354 no data days

# weather data
# NCDC-CDO-USC00356750.csv
library(tidyverse)
weatherData <- read_csv("data/NCDC-CDO-USC00356750.csv")
weatherData
# 2768 records
noWeatherData <- weatherData %>% 
  filter(is.na(TMAX)) %>% 
  summarise(count = n())
noWeatherData
# 0 records with max temperature, no data
# Task 1 DONE
######### ######### ######### ######### #########
######### ######### ######### ######### #########
# Task 2: Calculate weekly, monthly, and annual bike counts from the daily bike counts data.
source("code/load_data.R")
# weekly bike counts
bikecounts %>% 
  # first create ym column as a unique month identifier
  group_by(wk=floor_date(date, "week")) %>%
  summarize(total_weekly_counts=sum(total), counts=n()) %>% 
  # then average by month over years for each bridge
  group_by(week(wk)) %>% 
  summarize(avg_weekly_counts=mean(total_weekly_counts))
bikecounts %>% 
  # first create ym column as a unique month identifier
  group_by(wk=floor_date(date, "week")) %>%
  summarize(total_weekly_counts=sum(total), counts=n()) %>% 
  # then average by month over years for each bridge
  group_by(week(wk)) %>% 
  summarize(sum_weekly_counts=sum(total_weekly_counts))

# monthly
bikecounts %>% 
  # first create ym column as a unique month identifier
  group_by(ym=floor_date(date, "month")) %>%
  summarize(total_monthly_counts=sum(total), counts=n()) %>% 
  # then average by month over years for each bridge
  group_by(month(ym)) %>% 
  summarize(avg_monthly_counts=mean(total_monthly_counts))
bikecounts %>% 
  # first create ym column as a unique month identifier
  group_by(ym=floor_date(date, "month")) %>%
  summarize(total_monthly_counts=sum(total), counts=n()) %>% 
  # then average by month over years for each bridge
  group_by(month(ym)) %>% 
  summarize(sum_monthly_counts=sum(total_monthly_counts))

# annual
bikecounts %>% 
  # first create ym column as a unique month identifier
  group_by(yr=floor_date(date, "year")) %>%
  summarize(total_annual_counts=sum(total), counts=n()) %>% 
  # then average by month over years for each bridge
  group_by(year(yr)) %>% 
  summarize(sum_annual_counts=sum(total_annual_counts))

bikecounts %>% 
  # first create ym column as a unique month identifier
  group_by(yr=floor_date(date, "year")) %>%
  summarize(total_annual_counts=sum(total), counts=n()) %>% 
  # then average by month over years for each bridge
  group_by(year(yr)) %>% 
  summarize(avg_annual_counts=mean(total_annual_counts))
#qqq not sure why get same results bfrom both of above

# instructor code
bikecounts %>% 
  mutate(year=year(date)) %>%
  group_by(year) %>%
  summarize(annual_total = sum(total))
# Task 2 DONE

# class examples for top counts by bridge
topDays <- bikecounts %>%
  group_by(brname) %>%
  mutate(rank=rank(desc(total))) %>%
  filter(rank <=3) %>%
  arrange(brname, rank)

# next does NOT work
topDays <- bikecounts %>%
  group_by(name %>%
  top_n(3))

topDays <- bikecounts %>%
  group_by(brname) %>%
  mutate(rank=rank(desc(total))) %>%
  filter(rank <= 3) %>%
  arrange(brname,rank)
topDays 

bikecounts %>%
  mutate(week=floor_date(date, "month"))
######### ######### ######### ######### #########
######### ######### ######### ######### #########
# qqq Task 3: Join the bike counts data with the weather data. 
#   Which type of joins works best here?
library(dplyr)
# bikecounts data as ddtm type
# weatherData DATE as date type
#   so need to get them to be the same data type before can make join
bikeCntWeather <- bikecounts %>%
  mutate(DATE=as_date(date)) %>%
  left_join(weatherData)
bikeCntWeather
# 4771 records
# there may be dates with no bikecounts data
#  if looking at trends, want to deal with missing dates
#  but if confident weather data contains all dates, 
#  may wantdates with no bikecounts
#  so either full join or right join

# diagnostic for only rows where we have weather data, ck same number of rows
bikeCntWeather <- bikecounts %>%
  mutate(DATE=as_date(date)) %>%
  semi_join(weatherData) 
bikeCntWeather

# anti-join, days with no weather data should be zero if there are weather data for all bikecount days
bikeCntWeather <- bikecounts %>%
  mutate(DATE=as_date(date)) %>%
  anti_join(weatherData) 
bikeCntWeather

# Task 3 done
######### ######### ######### ######### #########
######### ######### ######### ######### #########
# Task 4: With the NHTS2009 travel diaries data, 
# how do you calculate total miles traveled (using any modes) 
# and miles traveled by driving for each household 
# (hint: the TRPMILES column contains information of trip distance 
# for each member of a household).

# load NHTS2009 travel diaries subset
dd <- read_csv("data/NHTS2009_dd.csv")

# Total Miles travelled, all modes
tmtrav <- dd %>%
  summarize(tmt=sum(TRPMILES, na.rm = TRUE))
# 2390.667 total miles travelled
# code shown in class
tmtrav <- dd %>%
  group_by(HOUSEID) %>%
  summarize(tmt=sum(TRPMILES, na.rm = TRUE))


# miles travelled driving by household
# mutate to add column driving
tmDriven <- dd %>% 
  mutate(driving=ifelse(TRPTRANS %in% c("01", "02", "03", "04", "05", "06", "07"), 1, 0),
        driving=ifelse(TRPTRANS %in% c("-1", "-7", "-8", "-9"), NA, driving) # retain missing values as NA
              ) 
tmDrivenHH <- dd %>%
  mutate(driving=ifelse(TRPTRANS %in% c("01", "02", "03", "04", "05", "06", "07"), 1, 0),
         drivingtmtravHH=ifelse(TRPTRANS %in% c("-1", "-7", "-8", "-9"), NA, driving) # retain missing values as NA
          ) %>%
  filter(driving == 1) %>%
  group_by(HOUSEID) %>%
  summarize(tmDriven=sum(TRPMILES, na.rm = TRUE))
# alternate mutate, using case_when
tmDrivenHH <- dd %>%
  mutate(driving=case_when(
    TRPTRANS %in% c("01", "02", "03", "04", "05", "06", "07") ~ 1, 
    TRPTRANS %in% c("-1", "-7", "-8", "-9") ~ as.double(NA), # retain missing values as NA
    TRUE ~ 0)) %>%
  filter(driving == 1) %>%
  group_by(HOUSEID) %>%
  summarize(tmDriven=sum(TRPMILES, na.rm = TRUE))
# if function defined in multiple packages

# Task 4 done
######### ######### ######### ######### #########
######### ######### ######### ######### #########
# qqq Task 5: [Challenge] How do you compute the 
#   average household-level miles driving 
#   per capita by income categories (low, med, high)?
######### ######### ######### ######### #########
# reclassify households into low, med, high income based on 
#   HHFAMINC column data dictionary: http://nhts.ornl.gov/tables09/CodebookPage.aspx?id=949 
#   with brackets [0, 30000, 6000]
dd <- dd %>% mutate(income_cat=case_when(
  HHFAMINC %in% c("01", "02", "03", "04", "05", "06") ~ "low income",
  HHFAMINC %in% c("07", "08", "09", "10", "11", "12") ~ "med income",
  HHFAMINC %in% c("13", "14", "15", "16", "17", "18") ~ "high income",
  TRUE ~ as.character(NA) # retain missing values as NA
))
# verify recoding results with group_by & tally
dd %>% group_by(HHFAMINC, income_cat) %>% 
  tally()


# sample code from class notes
# recode race (HH_RACE column) according to data dictionary: http://nhts.ornl.gov/tables09/CodebookPage.aspx?id=951
dd %>% mutate(hh_race_str=recode(HH_RACE, 
                                 "01"="White",
                                 "02"="African American, Black",
                                 "03"="Asian Only",
                                 "04"="American Indian, Alaskan Native",
                                 "05"="Native Hawaiian, other Pacific",
                                 "06"="Multiracial",
                                 "07"="Hispanic/Mexican",
                                 "97"="Other specify",
                                 .default = as.character(NA) # any unspecified values would be assigned NA
                                  )) %>% 
  select(HH_RACE, hh_race_str)

# code driving & non-driving based on travel modes 
#   (TRPTRANS column) data dictionary: http://nhts.ornl.gov/tables09/CodebookPage.aspx?id=1084
dd %>% mutate(driving=ifelse(TRPTRANS %in% c("01", "02", "03", "04", "05", "06", "07"), 1, 0),
              driving=ifelse(TRPTRANS %in% c("-1", "-7", "-8", "-9"), NA, driving) # retain missing values as NA
              ) %>% 
  select(TRPTRANS, driving)
# code driving & non-driving based on travel modes 
#   (TRPTRANS column) data dictionary: http://nhts.ornl.gov/tables09/CodebookPage.aspx?id=1084 use case_when
dd %>% mutate(driving=case_when(
  TRPTRANS %in% c("01", "02", "03", "04", "05", "06", "07") ~ 1, 
  TRPTRANS %in% c("-1", "-7", "-8", "-9") ~ as.double(NA), # retain missing values as NA
  TRUE ~ 0)) %>% 
  select(TRPTRANS, driving)

# reclassify households into low, med, high income based on 
#   HHFAMINC column data dictionary: http://nhts.ornl.gov/tables09/CodebookPage.aspx?id=949 
#   with brackets [0, 30000, 6000]
dd <- dd %>% mutate(income_cat=case_when(
  HHFAMINC %in% c("01", "02", "03", "04", "05", "06") ~ "low income",
  HHFAMINC %in% c("07", "08", "09", "10", "11", "12") ~ "med income",
  HHFAMINC %in% c("13", "14", "15", "16", "17", "18") ~ "high income",
  TRUE ~ as.character(NA) # retain missing values as NA
))
# verify recoding results with group_by & tally
dd %>% group_by(HHFAMINC, income_cat) %>% 
  tally()

######### ######### ######### ######### #########
