# calulates the number of governor party switches over last decade
# also calculates the number of partisan changes for state-years in which
# I have data. 

library(tidyverse)
library(readxl)

# read in governor data file
govs_raw <- read_csv("../data/united_states_governors_1775_2020.csv")

# clean it and order it by state and year
govs_clean <- govs_raw %>% 
  filter(year >= 2010) %>% 
  mutate(for_ordering = as.numeric(str_remove(time_in_office, " - "))) %>% 
  arrange(state, year, for_ordering)

# find instances of partisan change and drop state/territories for which I lack 
#   data.
govs_clean_2 <- govs_clean %>% 
  group_by(state) %>% 
  mutate(party_switch = ifelse(party != lag(party), 1, 
                               0)) %>% 
  filter(!state %in% c("Virgin Islands", 
                      "Puerto Rico",
                      "Northern Mariana Islands",
                      "South Dakota",
                      "Guam",
                      "American Samoa")) %>% 
  ungroup()

# 26 changes in partisan control of governor's office since 2010
sum(govs_clean_2$party_switch, na.rm = T)


# add in indicators for the state years in which I already have data.
state_xwalk <- tibble(state = state.name,
                      state_abbv = state.abb)



govs_clean_2_abbv <- left_join(govs_clean_2, 
                               state_xwalk,
                               by = "state")

# what do I have data for? 
govs_clean_3 <- govs_clean_2_abbv %>% 
  mutate(data_in_hand = case_when(
    state_abbv == "AK" & year %in% 2015:2022 ~ 1,
    state_abbv == "CA" & year %in% 2013:2020 ~ 1,
    state_abbv == "CT" & year %in% 2015:2021 ~ 1,
    state_abbv == "GA" & year %in% 2011:2021 ~ 1,
    state_abbv == "HI" & year %in% c(2011:2013, 2016, 2018, 2020, 2022) ~ 1,
    state_abbv == "IA" & year %in% 2007:2021 ~ 1,
    state_abbv == "ID" & year %in% 1999:2021 ~ 1,
    state_abbv == "IL" & year %in% 2011:2021 ~ 1,
    state_abbv == "KS" & year %in% 2012:2021 ~ 1,
    state_abbv == "MA" & year %in% 2010:2022 ~ 1,
    state_abbv == "MD" & year %in% 2012:2019 ~ 1,
    state_abbv == "ME" & year %in% 2006:2021 ~ 1,
    state_abbv == "MI" & year %in% 2014:2020 ~ 1,
    state_abbv == "MN" & year %in% 2011:2021 ~ 1,
    state_abbv == "MO" & year %in% 2007:2022 ~ 1,
    state_abbv == "MS" & year %in% 2019:2021 ~ 1,
    state_abbv == "MT" & year %in% 2011:2021 ~ 1,
    state_abbv == "NC" & year %in% 2014:2022 ~ 1,
    state_abbv == "NH" & year %in% 2012:2021 ~ 1,
    state_abbv == "NJ" & year %in% 2010:2022 ~ 1,
    state_abbv == "NM" & year %in% 2012:2022 ~ 1,
    state_abbv == "NV" & year %in% 2010:2021 ~ 1,
    state_abbv == "NY" & year %in% 2013:2020 ~ 1,
    state_abbv == "OH" & year %in% 2012:2020 ~ 1,
    state_abbv == "OK" & year %in% 2011:2022 ~ 1,
    state_abbv == "OR" & year %in% 2019:2022 ~ 1,
    state_abbv == "PA" & year %in% 2011:2021 ~ 1,
    state_abbv == "RI" & year %in% 2011:2022 ~ 1,
    state_abbv == "TN" & year %in% 2019:2022 ~ 1,
    state_abbv == "TX" & year %in% 2019:2022 ~ 1,
    state_abbv == "UT" & year %in% 2014:2021 ~ 1,
    state_abbv == "VT" & year %in% 2009:2021 ~ 1,
    state_abbv == "WA" & year %in% 2016:2020 ~ 1,
    state_abbv == "WV" & year %in% 2007:2021 ~ 1,
    state_abbv == "WY" & year %in% 2010:2021 ~ 1,
    state_abbv == "DC" & year %in% 2011:2022 ~ 1,
    TRUE ~ 0
  ))


govs_clean_3 %>% 
  filter(data_in_hand == 1) %>% 
  group_by(state) %>% 
  summarise(sum = sum(party_switch, 
                      na.rm = T)) %>% 
  filter(sum > 0)
  

# 12 instances of party control switching in hand
# one each in the following states: 
# Illinois    
# Iowa         
# Maine        
# Minnesota    
# Missouri     
# New Hampshire
# New Jersey   
# Oklahoma     
# Pennsylvania 
# Rhode Island 
# Vermont      
# Wyoming      
# plus the following not in the above datasets: 
# Virginia (2021)



