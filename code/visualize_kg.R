library(tidyverse)
library(ggplot2)
# sample data in ggplot2
mpg
# initial plot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
# color plot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
# different size by class, not applied well - generates warning
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

# different color by class
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# different shape by class
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# blue points
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

# FACETS
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

# facet grid
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

# GEOMETRY
# data points
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# smoothed line from data
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

# smoothed line for each drv
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

# DIAMONDS sample data
# stacked bar
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))
# grouped bar chart
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()
######### ######### ######### ######### #########
# Task 1: Plot the daily bike counts with ggplot
#         Experiment with different options (bar, line, etc) 
#         and select the one works best for you;
# Bike data
source("code/load_data.R")

# simple plot, brname different colors
p <- ggplot(data = bikecounts) + 
  geom_point(mapping = aes(x = date, y = total,color = brname)) 
p + labs(color="Bridge")
p + labs(title = "Daily Total Bike Counts")

# bar chart, avg bikes per day by dat of wekk vs. brname
bikecounts$dow <- wday(as.Date(bikecounts$date), label = TRUE)
bikeCNTdow <- bikecounts %>%
  group_by(dow,brname) %>%
  summarise(avgBikesPerDay=mean(total))
# bar chart by dow, bridge
p <- ggplot(bikeCNTdow) + 
  geom_bar(mapping = aes(x = brname, y = avgBikesPerDay, fill = dow),  stat = "identity", position = "dodge") 
p + labs(x = "Bridge")
# box plot
ggplot(data = bikeCNTdow) +
  geom_boxplot(aes(x = dow, y =avgBikesPerDay))
# Task 1 DONE
######### ######### ######### ######### #########
######### ######### ######### ######### #########
# Task 2: Plot weekly, monthly, and annual bike counts 
#         for the three bridges;
# WEEKLY bike counts
bikeCntWeekly <- bikecounts %>% 
  # first create ym column as a unique week identifier
  group_by(brname,week=floor_date(date, "week")) %>%
  summarize(weeklyTotal=sum(total), counts=n())
bikeCntWeekly
ggplot(data = bikeCntWeekly) + 
  geom_point(mapping = aes(x = week, y = weeklyTotal,color = brname)) + 
  facet_wrap(~ brname, nrow = 1)
# from instructor
# BOXPLOT
ggplot(data = bikecounts) +
  geom_boxplot(aes(x=brname, y = total))
# in boxplot want x axis to be character or factore, not numeric

# qqq
# add Hawthorne plus Tilikum to see if Tilikum has increased total bike traffic 
#   or only diverted traffic from Hawthorne
#    could add separate line, give it its own label
# qqq

bikeCntTbl2 <- bikecounts %>% spread(key = "brname", value = "total")
bikeCntTbl2

bikeCntWeekly <- bikecounts %>% 
  filter(brname="Hawthorne"|brname="Tilikum")
  group_by(week = floor_date(date, "week")) %>%
  summarize(weeklyTotal=sum(total), counts=n())
bikeCntWeekly
ggplot(data = bikeCntWeekly) + 
  geom_point(mapping = aes(x = week, y = weeklyTotal,color = brname))
# following does not work, numeric value for x in boxplot
ggplot(data = bikeCntWeekly) +
  geom_boxplot(aes(x=week, y = total))

# monthly
bikeCntMonthly <- bikecounts %>% 
  group_by(brname,yrmonth=floor_date(date, "month")) %>%
  summarize(monthlyTotal=sum(total), counts=n())
bikeCntMonthly
ggplot(data = bikeCntMonthly) + 
  geom_point(mapping = aes(x = yrmonth, y = monthlyTotal,color = brname))

# annual
bikeCntAnnual <- bikecounts %>% 
  group_by(brname,year_=floor_date(date, "year")) %>%
  summarize(annualTotal=sum(total), counts=n())
bikeCntAnnual
ggplot(data = bikeCntAnnual) + 
  geom_point(mapping = aes(x = year_, y = annualTotal,color = brname))
# Task 2 DONE
######### ######### ######### ######### #########
######### ######### ######### ######### #########
# Task 3:  Now think about and try out different ways to plot weather 
#          information along with your daily bike counts (separately or together);
library(dplyr)
source("code/load_data.R")
weatherData <- read_csv("data/NCDC-CDO-USC00356750.csv")
weatherData
bikeCntWeather <- bikecounts %>%
  mutate(DATE=as_date(date)) %>%
  left_join(weatherData)
bikeCntWeather

# total riders vs. PRCP, bridges different color
ggplot(data = bikeCntWeather) + 
  geom_point(mapping = aes(x = PRCP, y = total,color = brname))
# total riders vs. PRCP, separate graph for each bridge, bridges different color
ggplot(data = bikeCntWeather) + 
  geom_point(mapping = aes(x = PRCP, y = total,color = brname)) + 
  facet_wrap(~ brname, nrow = 1) +
  labs(x = "Precipitation") + labs(color = "Bridge")

# total riders vs. SNOW, bridges different color
ggplot(data = bikeCntWeather) + 
  geom_point(mapping = aes(x = SNOW, y = total,color = brname))
# total riders vs. SNOW, separate graph for each bridge, bridges different color
ggplot(data = bikeCntWeather) + 
  geom_point(mapping = aes(x = SNOW, y = total,color = brname)) + 
  facet_wrap(~ brname, nrow = 1)

# total riders vs. TMAX, bridges different color
ggplot(data = bikeCntWeather) + 
  geom_point(mapping = aes(x = TMAX, y = total,color = brname))
# total riders vs. TMAX, separate graph for each bridge, bridges different color
ggplot(data = bikeCntWeather) + 
  geom_point(mapping = aes(x = TMAX, y = total,color = brname)) + 
  facet_wrap(~ brname, nrow = 1)

# total riders vs. TMIN, bridges different color
ggplot(data = bikeCntWeather) + 
  geom_point(mapping = aes(x = TMIN, y = total,color = brname))
# total riders vs. TMAX, separate graph for each bridge, bridges different color
ggplot(data = bikeCntWeather) + 
  geom_point(mapping = aes(x = TMIN, y = total,color = brname)) + 
  facet_wrap(~ brname, nrow = 1) +
  labs(x = "Minimum Temperature") + labs(color = "Bridge")

# from instructor
# ggplot3 now allows 2nd y axis()
bikeCntWeather <- bikecounts %>%
  mutate(DATE=as_date(date)) %>%
  left_join(weatherData) 
bikeCntWeather %>%
  ggplot() +
  geom_col(aes(x=DATE, y=TMIN * 100)) +
  geom_line(aes(x=DATE, y=TMIN * 100)) +
  scale_y_continuous(sec.axis = sec_axis(~./100, name = 'Temp(F)'))

# Task 3 DONE
######### ######### ######### ######### #########
######### ######### ######### ######### #########
# Task 4:  You can combine time series stats and ggplot2/ggfortify 
#          to plot seasonal variation, trend and noise separately. 
#          See here for an example of how to do this.
## DONE install.packages('ggfortify')
library(ggfortify)
qqq 
#see if can get code from instructor

autoplot(stl(AirPassengers, s.window = 'periodic'), ts.colour = 'blue')
autoplot(acf(AirPassengers, plot = FALSE))
autoplot(spec.ar(AirPassengers, plot = FALSE))
ggcpgram(arima.sim(list(ar = c(0.7, -0.5)), n = 50))
# cumulative plot
autoplot(acf(AirPassengers, plot = FALSE), conf.int.fill = '#0000FF', conf.int.value = 0.8, conf.int.type = 'ma')
library(forecast)
ggtsdiag(auto.arima(AirPassengers))
gglagplot(AirPassengers, lags = 4)
ggfreqplot(AirPassengers)
ggfreqplot(AirPassengers, freq = 4)
# from instructor

######### ######### ######### ######### #########
