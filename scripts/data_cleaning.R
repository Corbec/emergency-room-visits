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





#Building function to pull and clean census data


census_cleaned <- function(date){
  vars <- load_variables(date, "acs1", cache = FALSE)
  
  population <- get_acs(geography = "us",
                      survey = "acs1",
                      table = "B01001",
                      year = toString(date),
                      show_call = TRUE)
  
  population <- left_join(population, vars, by = c("variable" = "name"))
  
  population <- population %>% 
    separate(label,
             sep = "!!",
             into = c("estimate!!", "total", "sex", "age_group")) %>% 
    select(-c("estimate!!", "concept", "total", "moe", "GEOID", "NAME", "variable"))
  
  # aggregating males age 20-24
  male_age_20_24 <- population %>% 
    filter(sex == "Male" & (age_group == "20 years" | age_group == "21 years" | age_group == "22 to 24 years")) %>% 
    mutate(age_group = "20 to 24") 
  
  male_age_20_24 <- aggregate(.~ sex + age_group, data = male_age_20_24, sum)
  
  #aggregating females age 20-24
  female_age_20_24 <- population %>% 
    filter(sex == "Female" & (age_group == "20 years" | age_group == "21 years" | age_group == "22 to 24 years")) %>% 
    mutate(age_group = "20 to 24") 
  
  female_age_20_24 <- aggregate(.~ sex + age_group, data = female_age_20_24, sum)
  
  # aggregating males 15-19
  male_age_15_19 <- population %>% 
    filter(sex == "Male" & (age_group == "15 to 17 years" | age_group == "18 and 19 years")) %>% 
    mutate(age_group = "15 to 19") 
  
  male_age_15_19 <- aggregate(.~ sex + age_group, data = male_age_15_19, sum)
  
  #aggregating females 15-19
  female_age_15_19 <- population %>% 
    filter(sex == "Female" & (age_group == "15 to 17 years" | age_group == "18 and 19 years")) %>% 
    mutate(age_group = "15 to 19") 
  
  female_age_15_19 <- aggregate(.~ sex + age_group, data = female_age_15_19, sum)
  
  #aggregating males age 60-64
  male_age_60_64 <- population %>% 
    filter(sex == "Male" & (age_group == "60 and 61 years" | age_group == "62 to 64 years")) %>% 
    mutate(age_group = "60 to 64") 
  
  male_age_60_64 <- aggregate(.~ sex + age_group, data = male_age_60_64, sum)
  
  #aggregating females age 60-64
  female_age_60_64 <- population %>% 
    filter(sex == "Female" & (age_group == "60 and 61 years" | age_group == "62 to 64 years")) %>% 
    mutate(age_group = "60 to 64") 
  
  female_age_60_64 <- aggregate(.~ sex + age_group, data = female_age_60_64, sum)
  
  #aggregating males age 65-69
  male_age_65_69 <- population %>% 
    filter(sex == "Male" & (age_group == "65 and 66 years" | age_group == "67 to 69 years")) %>% 
    mutate(age_group = "65 to 69") 
  
  male_age_65_69 <- aggregate(.~ sex + age_group, data = male_age_65_69, sum)
  
  #aggregating females age 65-69
  female_age_65_69 <- population %>% 
    filter(sex == "Female" & (age_group == "65 and 66 years" | age_group == "67 to 69 years")) %>% 
    mutate(age_group = "65 to 69") 
  
  female_age_65_69 <- aggregate(.~ sex + age_group, data = female_age_65_69, sum)
  
  #adding each group to population tibble
  population <- population %>% 
    mutate(estimate = replace(estimate,
                              ((age_group == "20 years" | age_group == "21 years" | age_group == "22 to 24 years") & sex == "Male"),
                              male_age_20_24$estimate)) %>% 
    mutate(age_group = replace(age_group,
                               ((age_group == "20 years" | age_group == "21 years" | age_group == "22 to 24 years") & sex == "Male"),
                               male_age_20_24$age_group)) %>% 
    mutate(estimate = replace(estimate,
                              ((age_group == "20 years" | age_group == "21 years" | age_group == "22 to 24 years") & sex == "Female"),
                              female_age_20_24$estimate)) %>% 
    mutate(age_group = replace(age_group,
                               ((age_group == "20 years" | age_group == "21 years" | age_group == "22 to 24 years") & sex == "Female"),
                               female_age_20_24$age_group)) %>% 
    mutate(estimate = replace(estimate,
                              ((age_group == "15 to 17 years" | age_group == "18 and 19 years") & sex == "Male"),
                              male_age_15_19$estimate)) %>% 
    mutate(age_group = replace(age_group,
                               ((age_group == "15 to 17 years" | age_group == "18 and 19 years") & sex == "Male"),
                               male_age_15_19$age_group)) %>% 
    mutate(estimate = replace(estimate,
                              ((age_group == "15 to 17 years" | age_group == "18 and 19 years") & sex == "Female"),
                              female_age_15_19$estimate)) %>% 
    mutate(age_group = replace(age_group,
                               ((age_group == "15 to 17 years" | age_group == "18 and 19 years") & sex == "Female"),
                               female_age_15_19$age_group)) %>% 
    mutate(estimate = replace(estimate,
                              ((age_group == "60 and 61 years" | age_group == "62 to 64 years") & sex == "Male"),
                              male_age_60_64$estimate)) %>% 
    mutate(age_group = replace(age_group,
                               ((age_group == "60 and 61 years" | age_group == "62 to 64 years") & sex == "Male"),
                               male_age_60_64$age_group)) %>%
    mutate(estimate = replace(estimate,
                              ((age_group == "65 and 66 years" | age_group == "67 to 69 years") & sex == "Male"),
                              male_age_65_69$estimate)) %>% 
    mutate(age_group = replace(age_group,
                               ((age_group == "65 and 66 years" | age_group == "67 to 69 years") & sex == "Male"),
                               male_age_65_69$age_group)) %>% 
    mutate(estimate = replace(estimate,
                              ((age_group == "65 and 66 years" | age_group == "67 to 69 years") & sex == "Female"),
                              female_age_65_69$estimate)) %>% 
    mutate(age_group = replace(age_group,
                               ((age_group == "65 and 66 years" | age_group == "67 to 69 years") & sex == "Female"),
                               female_age_65_69$age_group)) %>% 
    mutate(estimate = replace(estimate,
                              ((age_group == "60 and 61 years" | age_group == "62 to 64 years") & sex == "Female"),
                              female_age_60_64$estimate)) %>% 
    mutate(age_group = replace(age_group,
                               ((age_group == "60 and 61 years" | age_group == "62 to 64 years") & sex == "Female"),
                               female_age_60_64$age_group))
  
  population <- population %>% 
    distinct()
  
  population <- population %>% 
    mutate(age_group = str_remove(population$age_group, " years")) %>% 
    mutate(year = date)
  
  both <- population %>% 
    group_by(age_group) %>% 
    filter(!is.na(sex)) %>% 
    summarise(sum(estimate))
  
  both <- both %>% 
    mutate(year = date) %>% 
    mutate(sex = "Both")
  
  both <- both %>% 
    rename(estimate = `sum(estimate)`)
  
  population <- population %>% 
    rbind(both)
}

#Loop to pull all years census data
for(i in 2006:2016) { 
  nam <- paste("pop", i, sep = "_")
  assign(nam, census_cleaned(i))
}

#Combining all tibbles into one

tibbleList <- list(pop_2006, pop_2007, pop_2008, pop_2009, pop_2010, pop_2011, pop_2012, pop_2013, pop_2014, pop_2015, pop_2016)

pop_all <- bind_rows(tibbleList)

#saving to RDS

saveRDS(pop_all, file = '../data/cleaned_census.rds')




# function for combining age buckets under 5 in ed data
ed_visits <- read_csv("../data/ED_spending/IHME_DEX_ED_SPENDING_2006_2016_DATA_Y2021M09D23.CSV")

census <- read_rds("../data/cleaned_census.rds")

ed_clean <- function(date){
  male_under_5 <-  ed_visits %>% 
    filter(sex == "Male" & (age_group_id == 5 | age_group_id == 28)) %>% 
    mutate(age_group_name = "under_5")
  
  male_age_20_24 <- aggregate(.~ sex + age_group, data = male_age_20_24, sum)
}

ed_visits <- ed_visits %>% 
  filter(age_group_id != 28) %>% 
  mutate(age_group_name = ifelse(age_group_name == "1 to 4", "Under 5", age_group_name)) %>% 
  mutate(age_group_name = ifelse(age_group_name == "85 plus", "85 and over", age_group_name))

ed_visits %>% 
  select(starts_with("mean_"))


colnames(ed_visits[ , grepl( "mean" , names( ed_visits))])

ed_visits %>% 
  filter(age_group_name == "All Ages")

pop_all <- pop_all %>% 
  mutate(sex = replace_na(sex, "Both")) %>% 
  mutate(age_group = replace_na(age_group, "All Ages"))

saveRDS(census, file = '../data/cleaned_census.rds')

census <- census %>% 
  rename(age_group_name = age_group) %>% 
  rename(year_id = year)

census <- census %>% 
  mutate(age_group_name = str_trim(age_group_name, side = "right"))

  ed_visits <- left_join(ed_visits,
        census)
  
# ed_visits <- ed_visits %>% 
#   mutate(mean_all = mean_all / estimate)
# 
# ed_visits <- ed_visits %>% 
#   mutate(mean_all = lower_all / estimate)

ed_visits <- ed_visits %>% 
  mutate_at(vars(-c(estimate, year_id, age_group_name, sex, age_group_id, sex_id, agg_cause)), funs(. / estimate))

all_groups <- unique(as.character(census$age_group_name))



