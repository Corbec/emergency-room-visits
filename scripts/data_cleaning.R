library(tidyverse)
library(tidycensus)
library(jsonlite)

# Loading census key from json
key <- fromJSON("../data/census_key.json")

census_api_key(key)

#getting available variables for the year
v2006 <- load_variables(2006, "acs1", cache = TRUE)

#calling the census API to get 2006 population data broken up by sex and age group
pop_2006 <- get_acs(geography = "us",
                    survey = "acs1",
                    table = "B01001",
                    year = "2006",
                    show_call = TRUE)

# Joining variable name to get label for age groups
pop_2006 <- left_join(pop_2006, v2006, by = c("variable" = "name"))

# Breaking age group label into separate columns
pop_2006 <- pop_2006 %>% 
  separate(label,
           sep = "!!",
           into = c("estimate!!", "total", "sex", "age_group"))

# Dropping un-needed columns
pop_2006 <- pop_2006 %>% 
  select(-c("estimate!!", "concept", "total", "moe", "GEOID", "NAME", "variable"))

# filtering by sex and age ranges to be aggregated
male_age_20_24 <- pop_2006 %>% 
  filter(sex == "Male" & (age_group == "20 years" | age_group == "21 years" | age_group == "22 to 24 years"))
  
# changing name of age groups to aggregated name
male_age_20_24 <- male_age_20_24 %>%
  mutate(age_group = "20 to 24 years")   

# adding rows together and leaving one row
male_age_20_24 <- aggregate(.~ sex + age_group, data = male_age_20_24, sum)

# reordering columns
male_age_20_24 <- relocate(male_age_20_24, estimate, .before = sex)  

# Replacing estimate and age_group name in original tibble with new values
pop_2006 <- pop_2006 %>% 
  mutate(estimate = replace(estimate,
                            ((age_group == "20 years" | age_group == "21 years" | age_group == "22 to 24 years") & sex == "Male"),
                            male_age_20_24$estimate)) %>% 
  mutate(age_group = replace(age_group,
                             ((age_group == "20 years" | age_group == "21 years" | age_group == "22 to 24 years") & sex == "Male"),
                             male_age_20_24$age_group))

# Dropping duplicate values         
pop_2006 <- pop_2006 %>% 
  distinct()

# Need to repeat for other groups and turn into a function to easily cover all 10 years