library(readxl)
library(tidyverse)
Hawthorne <- read_excel("data/Hawthorne Tilikum Steel daily bike counts 073118.xlsx",
                        sheet = "Hawthorne",
                        skip=1) #skip first line since header was two lines long
Tilikum <- read_excel("data/Hawthorne Tilikum Steel daily bike counts 073118.xlsx",
                        sheet = "Tilikum",
                        skip=1) #skip first line since header was two lines long
Steel <- read_excel("data/Hawthorne Tilikum Steel daily bike counts 073118.xlsx",
                        sheet = "Steel",
                        skip=1) #skip first line since header was two lines long

# learning some functions
##filter(select(Hawthorne, desc(contains("westbound"))))
##arrange(select(Hawthorne, desc(contains("westbound"))),desc(date))
##HawthorneEB <- select(Hawthorne, contains("eastbound"))
# not working  arrange(HawthorneEB, desc(contains("eastbound")))
select(Tilikum, contains("westbound"))
select(Tilikum, contains("eastbound"))
select(Steel, contains("westbound"))
select(Steel, contains("eastbound"))

# back to work
# simplify column names to be EB, WB
transmute(Hawthorne, date, WB= 'north side (westbound)', EB = 'south side (eastbound)',total)
transmute(Steel, date, WB= 'north side (westbound)', EB = 'south side (eastbound)', total)
transmute(Tilikum, date,WB= westbound, EB = eastbound, total)

Hinput <- transmute(Hawthorne, date, WB= 'north side (westbound)', EB = 'south side (eastbound)',total)


# code that can work for all 3 bridges
input_file <- transmute(Hawthorne, date, WB= 'north side (westbound)', EB = 'south side (eastbound)')
bridgeName ,- "Hawthorne"
load_data<- function(input_file,bridgeName)
  bikecounts <- read_excel(input_file, sheet = bridgeName, skip = 1)
  bikecounts$name <- BridgeName
  bikecounts)

# different ways to get all files same column names
Hawthorne <- load_data(input_file, "Hawthorne")
names(Hawthorne) <- names(Tilikum)

Steel <- load_data(input_file, "Steel")
names(Steel) <- c("date", "westbound", "eastbound", "total","name")

# combine data from all three bridges

bikecounts <- bind_rows(Hawthorne, Tilikum, Steel %>% select (-lower))
bikecounts %>%
     group_by(name) %>%
       summarize(avg_daily_counts=mean(total, na.rm=TRUE))

library(lubridate)       #need this for monthfunction that follows
bikecounts %>%
  group_by(name, month(date) %>%
  summarize(total_counts=sum(total), counts=n())

  # %>% is pipe operator
  #   passes argumaent on left as first argument to right side of pipe
  #  x %>% f is really f(x)
  # real power is chaining pipes togher
bikecounts %>%
    group_by(name, ym =floor_date(date, "month")) %>%
    summarize(total_monthly_counts=sum(total), counts=n())
    group_by(name, month(ym) %>%
    summarize(avg_monthly_count=mean(total_monthly_counts))
               )  

  # stuck, above is not working for him
  # he does not like hacky solution
  # see if you can Google something:  use floor_date, generated first day of month
  # workshop with Greg Wilson, even advanced users can stumble

Hawthorne <- read_excel(input_file,
                        sheet = bridge_name,
                        skip = 1)$name <- bridge_name

#he will save script and upload to course website
%>% 
  # from instructor
  library(readxl)
library(lubridate)

input_file <- "data/Hawthorne Tilikum Steel daily bike counts 073118.xlsx"
bridge_name <- "Hawthorne"

# define a funtion that load bike counts data
load_data <- function(input_file, bridge_name) {
  bikecounts <- read_excel(input_file,
                           sheet = bridge_name,
                           skip = 1)
  bikecounts$name <- bridge_name
  bikecounts
}

Tilikum <- load_data(input_file, "Tilikum")
Hawthorne <- load_data(input_file, "Hawthorne")

# use the column names of Tilikum for Hawthorne
names(Hawthorne) <- names(Tilikum)

Steel <- load_data(input_file, "Steel")
names(Steel) <- c("date", "lower", "westbound", "eastbound", "total", "name")

# combine all three data frame for all three bridges
bikecounts <- bind_rows(Hawthorne, 
                        Tilikum, 
                        Steel %>% select(-lower)) # exclude the `lower` col in Steel data frame

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
