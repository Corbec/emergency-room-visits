library(tidyverse)
library(tidycensus)
library(jsonlite)

options(scipen = 100)

ed_visits <- read_csv('../data/ED_spending/IHME_DEX_ED_SPENDING_2006_2016_DATA_Y2021M09D23.CSV')

year <- ed_visits %>% 
  filter(year_id == 2006)

year %>% 
  group_by(age_group_name) %>% 
  #filter(agg_cause == 'Communicable and nutrition disorders') %>% 
  ggplot(aes(x = lower_oop)) +
  geom_histogram()

categories <- year %>% 
  group_by(agg_cause) %>% 
  count()

year %>% 
  filter(lower_oop == max(year$lower_oop))

ed_visits %>% 
  group_by(agg_cause) %>% 
  filter(agg_cause == 'Communicable and nutrition disorders') %>% 
  filter(!age_group_name == 'All Ages') %>% 
  filter(sex == 'Both') %>% 
  group_by(year_id) %>% 
  ggplot(aes(x = year_id, y = lower_oop)) +
  geom_point() +
  geom_smooth()

year %>% 
  group_by(agg_cause) %>% 
  filter(agg_cause == 'Communicable and nutrition disorders') %>% 
  filter(!age_group_name == 'All Ages') %>% 
  filter(sex == 'Both') %>% 
  filter(lower_oop < 10000000)
# ^ age ranges with lowest oop are 65+. Most likely because of Medicare

year %>% 
  group_by(agg_cause) %>% 
  filter(agg_cause == 'Communicable and nutrition disorders') %>% 
  filter(!age_group_name == 'All Ages') %>% 
  filter(sex == 'Both') %>% 
  filter(lower_oop > 30000000)
# ^ ages 1 to 4 have the highest oop, followed by 20 to 29 age ranges 

key <- fromJSON("../data/census_key.json")

census_api_key(key)

v2006 <- load_variables(2006, "acs1", cache = TRUE)

pop_2006 <- get_acs(geography = "us",
                    survey = "acs1",
                    table = "B01001",
                    year = "2006",
                    show_call = TRUE)

pop_2006 <- left_join(pop_2006, v2006, by = c("variable" = "name"))
