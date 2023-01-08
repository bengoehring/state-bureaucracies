library(tidyverse)
library(lubridate)
BeRn::set_my_theme()


# read data
va_voter_raw <- readRDS("../data/voter-data/unmerged-voter-files/VA-unmerged.rds")
va_personnel_raw <- readRDS("../data/state-employee-data/VA/clean/va-19-22-premerge.rds")


# create datasets for merge
va_voter_merge <- va_voter_raw %>% 
  select(first_name = Voters_FirstName,
         last_name = Voters_LastName,
         suffix = Voters_NameSuffix,
         party = Parties_Description) %>% 
  mutate(across(where(is.character),
                tolower)) %>% 
  mutate(dem = if_else(party == 'democratic', 
                       1, 
                       0)) %>% 
  mutate(rep = if_else(party == 'republican',
                       1,
                       0)) %>% 
  mutate(non = if_else(party == 'non-partisan',
                       1,
                       0)) %>%
  group_by(first_name,
           last_name,
           suffix) %>% 
  summarise(across(dem:non,
                   ~sum(., na.rm = T) / n(),
                   .names = str_c("{.col}", 
                                  "_share")),
            across(dem:non,
                   ~n(),
                   .names = str_c("{.col}", 
                                  "_n"))) %>% 
  ungroup()


va_personnel_merge <- va_personnel_raw %>% 
  select(dedupe.ids, 
         first_name, 
         last_name, 
         suffix) %>% 
  distinct()


# direct join
va_merge_raw <-left_join(va_personnel_merge,
                         va_voter_merge,
                         by = c("first_name",
                                "last_name",
                                "suffix"))

va_merge_clean <- va_merge_raw %>% 
  mutate(party_final = case_when(
    non_share > dem_share & non_share > rep_share ~ 'non',
    dem_share > rep_share & dem_share > non_share ~ 'dem',
    rep_share > dem_share & rep_share > non_share ~ 'rep',
    rep_share == dem_share & dem_share == non_share ~ sample(c('rep', 
                                                               'dem', 
                                                               'non'), 
                                                             1),
    rep_share > non_share & rep_share == dem_share ~ sample(c('rep', 
                                                              'dem'), 
                                                            1),
    rep_share > dem_share & rep_share == non_share ~ sample(c('rep', 
                                                              'non'), 
                                                            1),
    dem_share > rep_share & dem_share == non_share ~ sample(c('non', 
                                                              'dem'), 
                                                            1),
    TRUE ~ NA_character_))
    
    
# check for dupe parties within the same id. This could happen due to prob
#   assigning of unique ids.

boop <- va_merge_clean %>% 
  group_by(dedupe.ids, party_final) %>% 
  filter(n() > 1)
# COME BACK TO THIS



va_merge_final <- left_join(va_personnel_raw,
                            va_merge_clean)


# read in aggregated agency file
#   created using this https://www.commonwealth.virginia.gov/va-government/orgchart22/
aggregated_va_agencies <- read_csv("../data/other/va_agencies.csv")
aggregated_va_agencies <- aggregated_va_agencies %>% 
  select(agency_name, 
         umbrella_agency) %>% 
  mutate(umbrella_agency = case_when(
    is.na(umbrella_agency) ~ tolower(agency_name), 
    TRUE ~ umbrella_agency
  )) %>% 
  mutate(across(.fns = tolower))


va_merge_final <- left_join(va_merge_final,
                            aggregated_va_agencies,
                            by = "agency_name")

va_merge_analysis <- va_merge_final %>% 
  filter(!umbrella_agency %in% c("judicial branch",
                                'legislative branch')) %>% 
  filter(!employee_category %in% c("faculty",
                                  "university staff"))


# simplify this so each row is an employee and it contains their departure date
boop <- va_merge_analysis %>% 
  filter(!str_detect(agency_name, 
                     "univ|college")) %>% 
  filter(data_source_date != ymd("2019-04-04") & data_source_date != ymd("2021-01-04")) %>% 
  filter(employee_category != "political jurisdiction staff (non-classified)") %>%  # this was new to later data -- dropping bc unsure how to incorporate
  select(data_source_date,
         employee_category,
         dedupe.ids,
         party_final,
         umbrella_agency,
         state_salary_or_wage_hourly_rate) 

# Number of departures by date
get_last_date <- boop %>% 
  select(dedupe.ids,
         data_source_date, 
         employee_category) %>% 
  group_by(dedupe.ids) %>% 
  arrange(data_source_date, .by_group = T) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  group_by(data_source_date, 
           employee_category) %>% 
  summarise(n_exits = n())

# Number of employees by date
get_n_employees <- boop %>% 
  select(dedupe.ids,
         data_source_date,
         employee_category) %>% 
  group_by(data_source_date, 
           employee_category) %>% 
  summarise(n_employees = n())


exits_date <- left_join(get_n_employees,
                        get_last_date,
                        by = c("data_source_date", 
                               "employee_category")) %>% 
  mutate(share_exits = n_exits / n_employees)


exits_date %>% 
  filter(data_source_date < ymd("2022-06-30")) %>% 
  ggplot() + 
  geom_line(aes(x = data_source_date,
               y = share_exits * 100,
               color = employee_category)) + 
  geom_vline(aes(xintercept = ymd('2021-11-02')))


# just classified, by partisanship
classified_emps <- va_merge_analysis %>% 
  filter(!str_detect(agency_name, 
                     "univ|college")) %>% 
  filter(data_source_date != ymd("2019-04-04") & data_source_date != ymd("2021-01-04")) %>% 
  filter(employee_category == 'classified') %>%  # this was new to later data -- dropping bc unsure how to incorporate
  select(data_source_date,
         dedupe.ids,
         party_final,
         umbrella_agency,
         state_salary_or_wage_hourly_rate) 

# Number of departures by date
get_last_date_party <- classified_emps %>% 
  select(dedupe.ids,
         data_source_date, 
         party_final) %>% 
  group_by(dedupe.ids) %>% 
  arrange(data_source_date, .by_group = T) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  group_by(data_source_date, 
           party_final) %>% 
  summarise(n_exits = n())

# Number of employees by date
get_n_employees_party <- classified_emps %>% 
  select(dedupe.ids,
         data_source_date,
         party_final) %>% 
  group_by(data_source_date, 
           party_final) %>% 
  summarise(n_employees = n())

exits_date_party <- left_join(get_n_employees_party,
                        get_last_date_party,
                        by = c("data_source_date", 
                               "party_final")) %>% 
  mutate(share_exits = n_exits / n_employees)

exits_date_party %>% 
  filter(data_source_date < ymd("2022-06-30")) %>% 
  ggplot() + 
  geom_line(aes(x = data_source_date,
                y = share_exits * 100,
                color = party_final)) + 
  geom_vline(aes(xintercept = ymd('2021-11-02')))


