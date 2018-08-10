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
# use purrr to fun function for specified subsets of country 
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