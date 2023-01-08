# ny_clean.R
# Description: This file reads in and cleans the NY personnel data. 
# Notes: 
##########################################################################

# preamble ####
library(tidyverse) 
library(vroom)
library(lubridate)


ny_raw <- vroom("../data/state-employee-data/NY/raw/Salary_Information_for_State_Authorities.csv",
                   col_types = cols(`Has Employees` = "character",
                                    `Paid by State or Local Government` = "character"))


# clean ####
colnames(ny_raw) <- str_replace_all(colnames(ny_raw),
                                    " ",
                                    "_")
colnames(ny_raw) <- tolower(colnames(ny_raw))

ny_clean <- ny_raw %>% 
  mutate(across(where(is.character),
                tolower)) %>% 
  mutate(fiscal_year_end_date = mdy(fiscal_year_end_date)) %>% 
  distinct()

# only using actual salary info
ny_clean <- ny_clean %>% 
  select(-base_annualized_salary)

# dedup and create person-level id
# It appears that there are duplicate rows for a given person -- often due
#   to someone changing jobs. Making assumption that these are all real changes 
#   in jobs and not different people. 

# for each name within a given authority and payroll date, sum up the real salary
#   paid and select the row where the larger salary was paid
dedup_static <- ny_clean %>%
  mutate(id = str_c(str_replace_na(first_name),
                    str_replace_na(middle_initial),
                    str_replace_na(last_name),
                    str_replace_na(as.character(fiscal_year_end_date)),
                    str_replace_na(authority_name)
                    )) %>% 
  group_by(id) %>% 
  mutate(compensation_max_row = max(total_compensation, na.rm = T)) %>% 
  mutate(compensation_row = total_compensation) %>% 
  mutate(across(actual_salary_paid:total_compensation,
                ~sum(., 
                     na.rm = T))) %>% 
  filter(compensation_max_row == compensation_row) %>% 
  ungroup() %>% 
  select(-compensation_max_row, 
         -compensation_row)

# there are a handful of duplicates remaining (18) -- just dropping
remaining_dups <- dedup_static %>% 
  filter(duplicated(id)) %>% 
  pull(id)

dedup_static <- dedup_static %>% 
  filter(!id %in% remaining_dups)


# now need to track people over time -- need an id that is longitudinal
# might need to assume that people stay in the same authority

boop <- dedup_static %>% 
  mutate(person_id = str_c(str_replace_na(first_name),
                           str_replace_na(middle_initial),
                           str_replace_na(last_name),
                           str_replace_na(authority_name))) %>% 
  group_by(fiscal_year_end_date) %>% 
  filter(duplicated(person_id))












  group_by(id) %>% 
  summarise(sum(duplicated(id), na.rm = T))



865174
  
  
  
  group_by(fiscal_year_end_date,
           authority_name,
           full_name) %>% 
  
  
  
  group_by()


boop <- ny_clean %>% 
  mutate(id = str_c(str_replace_na(first_name),
                    str_replace_na(middle_initial),
                    str_replace_na(last_name))) %>% 
  group_by(fiscal_year_end_date) %>% 
  filter(duplicated(id))


dup_count <- ny_clean %>% 
  mutate(id = str_c(str_replace_na(first_name),
                    str_replace_na(middle_initial),
                    str_replace_na(last_name))) %>% 
  group_by(fiscal_year_end_date) %>% 
  summarise(dups = sum(duplicated(id), na.rm = T))
