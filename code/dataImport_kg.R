# reference
# http://cities.github.io/datascience/import.html
######### ######### ######### ######### #########
# Task 2:  Import the bike counts data for the three bridges in Microsoft Excel format
# code from Liming Wang, instructor, for Day 1 project task
source("code/load_data.R")
# DONE - Task 2
######### ######### ######### ######### #########

######### ######### ######### ######### #########
# Task 3:  Import the Portland weather data in csv format
# NCDC-CDO-USC00356750.csv
library(tidyverse)
load_csv <- function(input_file) {
  weatherData <- read_csv(input_file)
  weatherData
 }
load_csv("data/NCDC-CDO-USC00356750.csv")

# DONE - Task 3:  Import the Portland weather data in csv format
######### ######### ######### ######### #########

######### ######### ######### ######### #########
# Task 4:  [Challenge] Import the Portland weather data in fixed width format;
library(tidyverse)

load_fwf <- function(input_file) {
  weatherDataF <- read_table(input_file, comment = "-")
  weatherDataF
}
inputFile <- "data/NCDC-CDO-USC00356750.txt"
weatherDataF <- load_fwf(inputFile)

# alternate method, trying to come with clever way to get column widths
#   without having to manually count
#   sometimes provided with data dictionary telling you column widths and names
# count dashes in 2nd header row, space delimits column
library(readr)
inputFile <- "data/NCDC-CDO-USC00356750.txt"
# widths <- read_(inputFile, \delim=" ", skip = 1) %>%
#   names() %>% map_dbl(nchar)

read_table(input_file, comment = "-")  #treat any row beginning with dash as a comment
cols_df <- read_table(inputFile, n_max = 3)
 (data_df <- read_table(inputFile, 
                        comment = "-", skip = 2, col_names = FALSE))
names(data_df) <- names(cols_df)
# not working
read_fwf(inputFile, fsf_widths(c(18, 69, . . . . )))
                     )
# DONE  Task 4:  [Challenge] Import the Portland weather data in fixed width format;
######### ######### ######### ######### #########

######### ######### ######### ######### #########
# Task 5: [Challenge] Import the iris dataset in Stata data format iris.dta 
#  hint:  use haven data import
library(haven)
load_iris <- function(input_file) {
  irisData <- read_dta(input_file, encoding = NULL)
  irisData
}
load_iris("data/iris.dta")

# try2 - either way works
load_iris2 <- function(input_file) {
  irisData <- read_stata(input_file, encoding = NULL)
  irisData
}

load_iris2("data/iris.dta")

# DONE Task 5: [Challenge] Import the iris dataset in Stata data format iris.dta 
######### ######### ######### ######### #########

######### ######### ######### ######### #########
# Task 6:  create a R script that loads, cleans, 
#   and visualizes the bike counts data 
#   as well as temperature and precipitation data 
#   (using data from Weather Station USC00356750);
# LOAD bicycle data
source("code/load_data.R")
bikecounts
# 4771 records
# CLEAN

delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
bikecounts %>% 
  summarize(total, westbound, na.rm = TRUE| eastbound, na.rm = TRUE)
bikecounts <- bikecounts %>%
  group_by(date, na.rm = TRUE) 
# 4761 records
######### ######### ######### ######### #########
# VISUALIZE bike data
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
# qqq read in weather data
library(tidyverse)
load_csv <- function(input_file) {
  weatherData <- read_csv(input_file)
  weatherData
}
load_csv("data/NCDC-CDO-USC00356750.csv")

# qqq combine weather data
# qqq visualize with temperature, precipitation dat
qqq
# average daily bike counts by bridge
bikecounts %>% 
  group_by(name) %>% 
  summarize(avg_daily_counts=mean(total, na.rm=TRUE))

# average monthly bike counts by bridge
bikecounts %>% 
  # first create ym column as a unique month identifier
  group_by(name, ym=floor_date(date, "month")) %>%
  summarize(total_monthly_counts=sum(total), counts=n()) %>% 
  # then average by month over years for each bridge
  group_by(name, month(ym)) %>% 
  summarize(avg_monthly_counts=mean(total_monthly_counts))
# not done: Task 6
######### ######### ######### ######### #########
######### ######### ######### ######### #########

