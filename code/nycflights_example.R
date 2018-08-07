library(nycflights13)
library(tidyverse)
nycflights13::flights                              #gets data frame
filter(flights, month == 1, day == 1)              #filter data frame     
jan1 <- filter(flights, month == 1, day == 1)      #assigns value
(dec25 <- filter(flights, month == 12, day == 25)) #both assigns to variable and displays
filter(flights, month == 11 | month == 12)         #flights in November and December
nov_dec <- filter(flights, month %in% c(11, 12))   #different code for same Nov, Dec data
nov_dec
df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)                       #have to explicitly ask to include NA entries
filter(flights, arr_delay >120)                    #delay > 2 hours
arrange(flights, year, desc(month), day)           #sort
select(flights, year, month, day)                  #select only named columns
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
)
