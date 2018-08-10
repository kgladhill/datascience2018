# code from Liming Wang, instructor, for Day 1 project task
#KG modified to use brname instead of name for bridge name
library(readxl)
library(lubridate)
library(tidyverse)

input_file <- "data/Hawthorne Tilikum Steel daily bike counts 073118.xlsx"
bridge_name <- "Hawthorne"

# define a funtion that load bike counts data
load_data <- function(input_file, bridge_name) {
  bikecounts <- read_excel(input_file,
                           sheet = bridge_name,
                           skip = 1)
  bikecounts$brname <- bridge_name
  bikecounts
}

Tilikum <- load_data(input_file, "Tilikum")
Hawthorne <- load_data(input_file, "Hawthorne")

# use the column names of Tilikum for Hawthorne
names(Hawthorne) <- names(Tilikum)

Steel <- load_data(input_file, "Steel")
names(Steel) <- c("date", "lower", "westbound", "eastbound", "total", "brname")

# combine all three data frames for all three bridges
bikecounts <- bind_rows(Hawthorne, 
                        Tilikum, 
                        Steel %>% select(-lower)) # exclude the `lower` col in Steel data frame

#qqq combine Hawthorne and Tilikum bridge data
# could I do join by data for data on two bridges?
bcH <- bikecounts %>%
  filter(brname=="Hawthorne")%>% 
  arrange(desc(date))
bcT <- bikecounts %>%
  filter(brname=="Tilikum")%>% 
  arrange(desc(date))
# bcHT <-bcH %>%
#   left_join(bcT)
# temp <- bikecounts %>% 
#   filter(brname=="Hawthorne") %>% 
#   arrange(desc(date))
# temp
# temp <- bikecounts %>% 
#   filter(brname=="Tilikum") %>% 
#   arrange(desc(date))
# temp
# filter(brname="Hawthorne"|brname="Tilikum")
# group_by(week = floor_date(date, "week")) %>%
# 
# bcHT <-bikecounts %>% filter(brname=="Hawthorne") %>%
#   left_join(bikecounts %>% filter(brname=="Tilikum"))
# 
# bikeCntTbl2 <- bikecounts %>% spread(key = "brname", value = "total")
# bikeCntTbl2

# weather data
weatherData <- read_csv("data/NCDC-CDO-USC00356750.csv")
weatherData
# join bikeconts and weatherData
bikeCntWeather <- bikecounts %>%
  mutate(DATE=as_date(date)) %>%
  left_join(weatherData)
bikeCntWeather
saveRDS(bikeCntWeather, "data/bikeCntWeather.rds")

# average daily bike counts by bridge
bikecounts %>% 
  group_by(brname) %>% 
  summarize(avg_daily_counts=mean(total, na.rm=TRUE))

# average monthly bike counts by bridge
bikecounts %>% 
  # first create ym column as a unique month identifier
  group_by(brname, ym=floor_date(date, "month")) %>%
  summarize(total_monthly_counts=sum(total), counts=n()) %>% 
  # then average by month over years for each bridge
  group_by(brname, month(ym)) %>% 
  summarize(avg_monthly_counts=mean(total_monthly_counts))
# © 2018 GitHub, Inc.
#Run rmarkdown::render("/docs/index_kg.Rmd")
