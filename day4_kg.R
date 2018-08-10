library(magrittr)
# don't have lines with source of the data
# dplyr costs with large data frame, if limited memory in computer
my_gap <- gapminder %>%
  group_by(country) %>%
  mutate(life_exp_gain = lifeExp - first(lifeExp)) %>%
  top_n(3)

# dplyr left join example
test_data <- data.frame(first_name = c("john", "bill", "madison", "abby", "zzz"),
                        stringsAsFactors = FALSE)
test_data

nameList <- structure(list(name = c("john", "bill", "madison", "abby", "thomas"), gender = c("M", "either", "M", "either", "M")), .Names = c("name", "gender"), row.names = c(NA, 5L), class = c("tbl_df", "tbl", "data.frame"))
nameList 

library(dplyr)
left_join(test_data, nameList , by = c("first_name" = "name"))


