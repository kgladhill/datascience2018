library(gapminder)
library(tidyverse)

#group_by
gapminder %>%
  group_by(continent) %>%
  summarize_each(funs(min, max), lifeExp)

# nesting
#    creates dataframe within dataframe
(gap_nested <- gapminder %>% 
    group_by(continent, country) %>% 
    nest())
# view one element
gap_nested[[1, "data"]]
#
(fit <- lm(lifeExp ~ I(year - 1950), data = gap_nested[[1, "data"]]))
# create function of above code
le_vs_yr <- function(df) {
  lm(lifeExp ~ I(year - 1950), data = df)
}
le_vs_yr(gap_nested[[1, "data"]])
# use purrr to run function for specified subsets of country 
fits <- purrr::map(gap_nested$data[1:2], le_vs_yr)
fits
# use purrr to run function for all subsets
(gap_nested <- gap_nested %>% 
    mutate(fit = purrr::map(data, le_vs_yr)))
# getting data back out
library(broom)
tidy(gap_nested$fit[[1]])
# apply tidy to all subsets
(gap_nested <- gap_nested %>% 
    mutate(tidy = purrr::map(fit, tidy)))

# unnest 
(gap_coefs <- gap_nested %>% 
    select(continent, country, tidy) %>% 
    unnest(tidy))
# so with just above lines of code, ran all 144 models
#
gap_nested <- gapminder %>% 
  group_by(continent, country) %>% 
  nest()

le_vs_yr <- function(df) {
  lm(lifeExp ~ I(year - 1950), data = df)
}

gap_coefs <- gap_nested %>% 
  mutate(fit = purrr::map(data, le_vs_yr),
         tidy = purrr::map(fit, tidy)) %>% 
  select(continent, country, tidy) %>% 
  unnest(tidy)

# payoff
(gap_coefs <- gap_coefs %>%
    mutate(term = recode(term,
                         `(Intercept)` = "intercept",
                         `I(year - 1950)` = "slope")))

(gap_ests <- gap_coefs %>% 
    select(continent:estimate) %>% 
    spread(key = term, value = estimate))

gap_ests %>% 
  select(intercept, slope) %>% 
  summary()

ggplot(gap_coefs, aes(x = estimate)) +
  geom_density() + geom_rug() + facet_wrap(~ term, scales = "free")

ggplot(gap_ests, aes(x = intercept, y = slope)) +
  geom_point() +
  geom_smooth(se = FALSE, lwd = 2)
# get this result with a lot less work than if you did not have these tools
# particularly tidy function
######### ######### ######### ######### #########
######### ######### ######### ######### #########
#  EXERCISES
# Task 1: Fit linear regression models of the daily bike counts on 
#         precipitation, min and max temperature, 
#         PRCP, TMIN, TMAX
#         first for all bridges together and 
#         then for each bridge separately using the split-apply-combine pattern;

# get bikecounts, weatherData, bikeCntWeather
source("code/load_data.R")
# create function to do linear regression using lm (linear model)
# lr_PRCP <- function(df) {
#   lm(total ~ PRCP, data = df)
# }
# PRCPvsCnt <- lr_PRCP(bikeCntWeather)
# PRCPvsCnt
# 
# lr_TMIN <- function(df) {
#   lm(total ~ TMIN, data = df)
# }
# TMINvsCnt <- lr_TMIN(bikeCntWeather)
# TMINvsCnt
# 
# lr_TMAX <- function(df) {
#   lm(total ~ TMAX, data = df)
# }
# TMAXvsCnt <- lr_TMAX(bikeCntWeather)
# TMAXvsCnt

# model all three factors together
lr_CntWeather <- function(df) {
  lm(total ~ TMIN + TMAX + PRCP, data = df)
}
WvsCnt <- lr_CntWeather(bikeCntWeather)
WvsCnt

# split, appply, combine
(br_nested <- bikeCntWeather %>% 
    group_by(brname) %>% 
    nest())
##WvsCnt_br <- lr_CntWeather(br_nested)
# use purrr to run function for all subsets
(WvsCnt_br <- br_nested %>% 
    mutate(fit = purrr::map(data, lr_CntWeather)))
## could do multiple models
## (WvsCnt_br <- br_nested %>% 
##     mutate(fit1 = purrr::map(data, lr_CntWeather))
##              fit2 = . . . )
# getting data back out
library(broom)# using tidy
# apply tidy to all subsets
(WvsCnt_brTidy <- WvsCnt_br %>% 
    mutate(tidy = purrr::map(fit, tidy)))
# unnest 
(CntWthr_coeff <- WvsCnt_brTidy %>% 
    select(brname, tidy) %>% 
    unnest(tidy))
#  my output at this point matches what he did in class
# what if we have dozens of models
#  which model has max slope for PRCP? (too many to eyeball data)
#  can range and filter data
CntWthr_coeff %>% 
  filter(term == "PRCP") %>% 
         arrange(estimate) %>% 
         slice(1)
# use glance to get R squared
library(broom)
# apply glance to all subsets
(WvsCnt_brGlance <- WvsCnt_br %>% 
    mutate(tidy = purrr::map(fit, tidy), glance=map(fit,glance)))
# unnest 
(CntWthr_coeff <- WvsCnt_brGlance %>% 
    select(brname, tidy) %>% 
    unnest(tidy))

# Task 1 end
######### ######### ######### ######### #########
######### ######### ######### ######### #########
# Task 2: Extract the results from models in the above step:
#         2.1  Compare the R-squares of the bridge-specific model. 
#              Which bridge has the highest correlation with precipitation, 
#              min and max temperature?
#         2.2  Which model has the largest precipitation coefficient? 
#              Temperature coefficient?

######### ######### ######### ######### #########
