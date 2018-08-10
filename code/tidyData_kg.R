######### ######### ######### ######### #########
######### ######### ######### ######### #########
#  Wed. afternoon

library(tidyverse)
require(tidyverse)

#table1-5 are loaded with tidyverse

# create table6
table6 <- tribble(
  ~traveler,               ~modes,  ~time,
  1,    "walk, transit, walk",  "5,25,3",
  2,                   "auto",        15,
  3,          "bike, transit",    "6,15"
)
table6
separate_rows(table6, modes, time, convert = TRUE              )

table4 <- table4a %>% 
  left_join(table4b, by="country", 
            suffix=c("_cases", "_population"))
# reformat the data from table4
#   column for year
gather(table4, 
       `1999_cases`:`2000_population`, 
       key='year_type', value="counts") %>% 
  separate(year_type, into=c("year", "type")) %>% 
  spread(key=type, value=counts)

table4a %>%
  gather ("1999":"2000", key = "year", value = "cases")
table4a

######### ######### ######### ######### #########
######### ######### ######### ######### #########
#  EXERCISE
# After tidying the bike counts, using functions in the tidyr package, create tables summarizing the 
# average bike counts by bridge and day of week in two different formats:
source("code/load_data.R")
# bikecounts
# 4771 records
######### ######### ######### ######### #########
# get day of week for given dates
bikecounts$dow <- wday(as.Date(bikecounts$date), label = TRUE)
bikecounts
######### ######### ######### ######### #########
# averages by bridge, day of week
bikecounts_dow <- bikecounts %>%
  group_by(brname, dow) %>%
  summarize(dowAvg=mean(total, na.rm = TRUE))
# summarize follows group_by
bikecounts_dow

######### ######### ######### ######### #########
# spread data into columns by DOW for each bridge
## spread(table2, key = type, value = count)
bikeCntTbl11 <- bikecounts_dow %>% spread(key = "dow", value = "dowAvg")
bikeCntTbl11
##   Table 10.1: Bike Counts by Day of Week and Bridge (1st Format)
## Bridge	Sun	Mon	Tue	Wed	Thur	Fri	Sat
## Hawthorne
## Tilikum

######### ######### ######### ######### #########
# gather data in format below
## Table 10.1: Bike Counts by Day of Week and Bridge (2nd Format)
## Day of Week	Hawthorne	Tilikum
## Fri		
## Mon		
## Sat		
## Sun		
## Thur		
# Tue		
# Wed
bikeCntTbl12 <- bikecounts_dow %>% spread(key = "brname", value = "dowAvg")
bikeCntTbl12

bikeCntTbl2 <- bikecounts %>% spread(key = "brname", value = "total")
bikeCntTbl2
#
######### ######### ######### ######### #########
# convert back to wide format
bikecounts_dow %>%
  spread(brname,dowAvg)
bikecounts_dow
######### ######### ######### ######### #########

#see script tidy_counts

