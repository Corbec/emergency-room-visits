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




total_spending <- read_csv("../data/global_spending/IHME_HEALTH_SPENDING_1995_2018_Y2021M09D22.CSV")

total_spending <- total_spending %>% 
  filter(iso3 == "USA" & year >= 2006)

total_spending %>%
  ggplot(aes(x = year)) +
  geom_line(aes_string(y = "the_per_cap_mean"), color = "red") +
  geom_ribbon(aes_string(ymin = "the_per_cap_lower",
                         ymax = "the_per_cap_upper"), alpha=0.1)

total_spending %>% 
  filter(year >= 2006) %>% 
  summarise(max(the_per_cap_mean) - min(the_per_cap_mean))

ed_visits %>% 
  filter(year_id == 2006) %>% 
  summarise(sum(mean_all))

ed_visits %>% 
  filter(year_id == 2016 & age_group_name == "All Ages" & sex == "Both") %>% 
  summarise(sum(mean_all))


year_visits <- ed_visits %>% 
  filter((year_id == 2006 | year_id == 2008) &
           agg_cause == 'Communicable and nutrition disorders' &
           (sex == "Both" | sex == "Male") &
           age_group_name != "All Ages")

year_visits %>%
  ggplot(aes_string(x = "age_group_name", y = "mean_all", fill = "year_id")) +
  geom_col(position = position_dodge()) +
  coord_flip() +
  theme(legend.position = "none") +
  scale_fill_hue(c=60, l=40) +
  ylab("USD Spent Per Capita") +
  xlab(FALSE) +
  ggtitle(paste(2006," ", 2008))

age_range <- as.vector(unique(as.character(ed_visits$age_group_name)))

year_visits$age_group_name <- factor(year_visits$age_group_name, levels = year_visits$age_group_name)

year_visits %>% 
  ggplot(aes(x = age_group_name, y = mean_all)) +
  geom_col() +
  coord_flip()

year_visits <- within(year_visits, 
                   age_group_name <- factor(age_group_name, 
                                      levels=names(sort(table(age_group_name), 
                                                        decreasing=TRUE))))
