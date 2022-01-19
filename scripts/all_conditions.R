library(tidyverse)

total_spending <- read_csv("../data/global_spending/IHME_HEALTH_SPENDING_1995_2018_Y2021M09D22.CSV")

ed_visits <- read_csv("../data/ED_spending/IHME_DEX_ED_SPENDING_2006_2016_DATA_Y2021M09D23.CSV")

census <- read_rds("../data/cleaned_census.rds")

ed_visits <- ed_visits %>% 
  filter(age_group_id != 28) %>% 
  mutate(age_group_name = ifelse(age_group_name == "1 to 4", "Under 5", age_group_name)) %>% 
  mutate(age_group_name = ifelse(age_group_name == "85 plus", "85 and over", age_group_name))

census <- census %>%
  rename(age_group_name = age_group) %>%
  rename(year_id = year)

census <- census %>%
  mutate(age_group_name = str_trim(age_group_name, side = "right"))

ed_visits <- left_join(ed_visits,
                       census)

ed_visits <- ed_visits %>%
  mutate_at(vars(-c(estimate, year_id, age_group_name, sex, age_group_id, sex_id, agg_cause)), funs(. / estimate))

# calculating total healthcare yearly percent change

total_spending <- total_spending %>% 
  filter(iso3 == "USA" & year >= 2006)


ed_spending_2006 <- 
  ed_visits %>% 
  filter(sex == "Both" & age_group_name == "All Ages" & year_id == 2006) %>% 
  summarise_at(vars(-c(estimate, year_id, age_group_name, sex, age_group_id, sex_id, agg_cause)), sum)
  
ed_spending_2016 <- 
  ed_visits %>% 
  filter(sex == "Both" & age_group_name == "All Ages" & year_id == 2016) %>% 
  summarise_at(vars(-c(estimate, year_id, age_group_name, sex, age_group_id, sex_id, agg_cause)), sum)

year_over_year <- (ed_spending_2007 - ed_spending_2006) / ed_spending_2006

yearly <- function(year1, year2){
  ed_spending_1 <- 
    ed_visits %>% 
    filter(sex == "Both" & age_group_name == "All Ages" & year_id == year1) %>% 
    summarise_at(vars(-c(estimate, year_id, age_group_name, sex, age_group_id, sex_id, agg_cause)), sum)
  
  ed_spending_2 <- 
    ed_visits %>% 
    filter(sex == "Both" & age_group_name == "All Ages" & year_id == year2) %>% 
    summarise_at(vars(-c(estimate, year_id, age_group_name, sex, age_group_id, sex_id, agg_cause)), sum)
  
  (ed_spending_2 - ed_spending_1) / ed_spending_1
}

year_over_year_2007 <- yearly(2007,2008)
yoy_2008 <- yearly(2008,2009)
yoy_2009 <- yearly(2009,2010)
yoy_2010 <- yearly(2010,2011)
yoy_2011 <- yearly(2011,2012)
yoy_2012 <- yearly(2012,2013)
yoy_2013 <- yearly(2013,2014)
yoy_2014 <- yearly(2014,2015)
yoy_2015 <- yearly(2015,2016)


total_ed_change <- yearly(2006,2016)

ed_visits <- ed_visits %>% 
  select(-c(age_group_id, sex_id))

age_range <- as.vector(unique(as.character(ed_visits$age_group_name)))
sex_items <- unique(as.character(ed_visits$sex))

for (s in sex_items){
  for (a in age_range){
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


pop <- census %>% 
  filter(year_id == 2006 & sex == "Male" & age_group_name == "Under 5") %>% 
  select(estimate)
