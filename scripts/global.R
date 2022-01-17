library(tidyverse)
library(tidycensus)
library(jsonlite)
library(plotly)
library(shinythemes)
library(viridis)

options(scipen = 100)

# read in the data

ed_visits <- read_csv("../data/ED_spending/IHME_DEX_ED_SPENDING_2006_2016_DATA_Y2021M09D23.CSV")

census <- read_rds("../data/cleaned_census.rds")

total_spending <- read_csv("../data/global_spending/IHME_HEALTH_SPENDING_1995_2018_Y2021M09D22.CSV")

# modify ed_visits to drop rows for under 1 years and rename age groups (may fix this later if I have time)

ed_visits <- ed_visits %>% 
  filter(age_group_id != 28) %>% 
  mutate(age_group_name = ifelse(age_group_name == "1 to 4", "Under 5", age_group_name)) %>% 
  mutate(age_group_name = ifelse(age_group_name == "85 plus", "85 and over", age_group_name))

# merging census and ed_visit data to allow for normalization

census <- census %>%
  rename(age_group_name = age_group) %>%
  rename(year_id = year)

census <- census %>%
  mutate(age_group_name = str_trim(age_group_name, side = "right"))

ed_visits <- left_join(ed_visits,
                       census)

ed_visits <- ed_visits %>%
  mutate_at(vars(-c(estimate, year_id, age_group_name, sex, age_group_id, sex_id, agg_cause)), funs(. / estimate))

# min and max years in ed_visits for slider filter

min_year <- min(ed_visits$year_id)
max_year <- max(ed_visits$year_id)

# getting choices for condition dropdown from ed_visits

condition_items <- unique(as.character(ed_visits$agg_cause))

# getting choices for sex radio buttons

sex_items <- unique(as.character(ed_visits$sex))

# getting choices for spending selector

spending_choices <- colnames(ed_visits[ , grepl( "mean" , names( ed_visits))])

# getting choices for age range selector

age_items <- unique(as.character(ed_visits$age_group_name))

# getting a list of years for drop down
year_items <- unique(as.character(ed_visits$year_id))