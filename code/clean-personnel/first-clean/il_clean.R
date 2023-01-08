# ct_clean.R
# Description: This file reads in and cleans the IL personnel data. 
# Notes: 
##########################################################################

# preamble ####
library(tidyverse)
library(vroom)


# read in data ####
il_raw <- vroom("../data/state-employee-data/IL/raw/1a0cd05c-7d17-4e3d-938d-c2bfa2a4a0b1.csv")


# basic cleaning and name parsing ####
colnames(il_raw) <- str_replace_all(tolower(colnames(il_raw)),
                                   " ",
                                   "_")

length(unique(il_raw$id))




# neither id column is meaningful
suffixes <- "\\ssr$|\\sjr$|\\sii$|\\siii$|\\siv$|\\sv$|\\siiii$"

il_clean <- il_raw %>% 
  select(-`_id`, -id) %>% 
  mutate(across(where(is.character), 
                ~tolower(str_squish(str_trim(.))))) %>% 
  mutate(employee_name = str_remove_all(employee_name, 
                                        "[:punct:]")) %>% 
  mutate(first_name = str_trim(str_extract(employee_name, "\\w+\\s"))) %>% 
  mutate(last_name = str_trim(str_extract(employee_name, "\\s\\w+"))) %>% 
  mutate(suffix = str_extract(employee_name, suffixes)) %>% 
  mutate(military_veteran = case_when(
    military_veteran == 'unknown' ~ 'no',
    TRUE ~ military_veteran
  ))


# too many missing with suffix -- going to concat for join
il_clean <- il_clean %>% 
  mutate(last_name_join = case_when(
    !is.na(suffix) ~ str_c(last_name, 
                           " ", 
                           suffix), 
    TRUE ~ last_name
  ))


# add in info about combined statistical areas for extra blocking
csa_clean <- read_rds("../data/other/cbsa-csa-clean.rds") 

il_csa <- csa_clean %>% 
  filter(state_name == 'illinois') %>% 
  mutate(csa_title = case_when(
    is.na(csa_title) ~ "none",
    TRUE ~ csa_title
  )) %>% 
  select(county = county_county_equivalent,
         csa = csa_title) %>% 
  mutate(county = str_trim(str_squish(str_remove_all(county, 
                                                     "[:punct:]"))))

il_clean <- il_clean %>% 
  mutate(work_county = str_trim(str_squish(str_remove_all(work_county, 
                                                          "[:punct:]"))))


il_df_with_csa <- left_join(il_clean,
                            il_csa,
                            by = c("work_county" = "county"))

il_df_with_csa <- il_df_with_csa %>% 
  mutate(csa = case_when(
    is.na(csa) ~ "none",
    TRUE ~ csa
  ))



saveRDS(il_df_with_csa, 
        '../data/state-employee-data/IL/clean/il-fastlink-pre.rds')











