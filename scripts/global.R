library(tidyverse)
library(tidycensus)
library(jsonlite)
library(plotly)
library(shinythemes)
library(viridis)
library(RColorBrewer)
library("wesanderson")

options(scipen = 100)

# read in the data

ed_visits <- read_csv("../data/ED_spending/IHME_DEX_ED_SPENDING_2006_2016_DATA_Y2021M09D23.CSV")

census <- read_rds("../data/cleaned_census.rds")

total_spending <- read_csv("../data/global_spending/IHME_HEALTH_SPENDING_1995_2018_Y2021M09D22.CSV")

total_spending <- total_spending %>% 
  filter(iso3 == "USA" & year >= 2006) %>% 
  select(year, the_per_cap_mean, the_per_cap_lower, the_per_cap_upper,
         ghes_per_cap_mean, ghes_per_cap_lower, ghes_per_cap_upper,
         ppp_per_cap_mean, ppp_per_cap_lower, ppp_per_cap_upper,
         oop_per_cap_mean, oop_per_cap_lower, oop_per_cap_upper) %>% 
  rename(mean_all = the_per_cap_mean,
         lower_all = the_per_cap_lower,
         upper_all = the_per_cap_upper,
         mean_pub = ghes_per_cap_mean,
         lower_pub = ghes_per_cap_lower,
         upper_pub = ghes_per_cap_upper,
         mean_pri = ppp_per_cap_mean,
         lower_pri = ppp_per_cap_lower,
         upper_pri = ppp_per_cap_upper,
         mean_oop = oop_per_cap_mean,
         lower_oop = oop_per_cap_lower,
         upper_oop = oop_per_cap_upper)

# modify ed_visits to drop rows for under 1 years and rename age groups (may fix this later if I have time)

ed_visits <- ed_visits %>% 
  filter(age_group_id != 28) %>% 
  mutate(age_group_name = ifelse(age_group_name == "1 to 4", "Under 5", age_group_name)) %>% 
  mutate(age_group_name = ifelse(age_group_name == "85 plus", "85 and over", age_group_name))

# dropping age_group_id and sex_id

ed_visits <- ed_visits %>% 
  select(-c(age_group_id, sex_id))

# merging census and ed_visit data to allow for normalization

census <- census %>%
  rename(age_group_name = age_group) %>%
  rename(year_id = year)

census <- census %>%
  mutate(age_group_name = str_trim(age_group_name, side = "right"))

ed_visits <- left_join(ed_visits,
                       census)

# getting choices for sex radio buttons

sex_items <- unique(as.character(ed_visits$sex))

# getting choices for age range selector

age_items <- unique(as.character(ed_visits$age_group_name))

# adding in rows for all conditions

for (s in sex_items){
  for (a in age_items){
    for (i in 2006:2016){
      new_row <- ed_visits %>% 
        filter(sex == s & age_group_name == a & year_id == i) %>% 
        summarise_at(vars(-c(estimate, year_id, age_group_name, sex, agg_cause)), sum)
      
      pop <- census %>% 
        filter(year_id == i & sex == s & age_group_name == a) %>% 
        select(estimate)
      
      ed_visits <- ed_visits %>% 
        add_row(year_id = i,
                age_group_name = a,
                sex = s,
                agg_cause = "All Conditions",
                mean_all = new_row$mean_all,
                lower_all = new_row$lower_all,
                upper_all = new_row$upper_all,
                mean_pub = new_row$mean_pub,
                lower_pub = new_row$lower_pub,
                upper_pub = new_row$upper_pub,
                mean_pri = new_row$mean_pri,
                lower_pri = new_row$lower_pri,
                upper_pri = new_row$upper_pri,
                mean_oop = new_row$mean_oop,
                lower_oop = new_row$lower_oop,
                upper_oop = new_row$upper_oop,
                estimate = pop$estimate)
    }
  }
}


# calculating per capita spending

ed_visits <- ed_visits %>%
  mutate_at(vars(-c(estimate, year_id, age_group_name, sex, agg_cause)), funs(. / estimate))

# min and max years in ed_visits for slider filter

min_year <- min(ed_visits$year_id)
max_year <- max(ed_visits$year_id)

# getting choices for condition dropdown from ed_visits

condition_items <- unique(as.character(ed_visits$agg_cause))

# getting choices for spending selector

spending_choices <- colnames(ed_visits[ , grepl( "mean" , names( ed_visits))])

# getting a list of years for drop down
year_items <- unique(as.character(ed_visits$year_id))

# extending color palette for age group plots
age_group_colors <- colorRampPalette(wes_palette("Zissou1")) (40)

# extending color palette for condition groups
condition_group_colors <- colorRampPalette(wes_palette("FantasticFox1")) (15)