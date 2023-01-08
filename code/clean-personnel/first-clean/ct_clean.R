# ct_clean.R
# Description: This file reads in and cleans the CT personnel data. 
# Notes: 
##########################################################################

# preamble ####
library(tidyverse)
library(vroom)
library(lubridate)

# read data ####
ct_raw <- vroom("../data/state-employee-data/CT/raw/State_Employee_Payroll_Data_Calendar_Year_2015_through_Present.csv",
                col_types = cols(
                  `Chk Status` = col_character(),
                  `Term Date`= col_character()
                ))

# basic cleaning ####
colnames(ct_raw) <- str_replace_all(tolower(colnames(ct_raw)),
                                   "[:space:]|-",
                                   "_")

ct_clean <- ct_raw %>% 
  mutate(across(where(is.character),
                ~case_when(. == "Undisclosed" ~ NA_character_,
                           TRUE ~ tolower(.)))) %>% 
  mutate(across(c(check_dt, 
                  orig_hire), 
                ~date(mdy_hms(.)))) %>% 
  mutate(term_date = as_date(str_remove(term_date, 
                                       " 00:00:00.0"))) %>% 
  mutate(across(first_name:name_suffix,
                ~str_trim(str_squish(str_remove_all(., "[:punct:]"))))) %>% 
  rename(middle_name = middle_initial)




# based on data dictionary, dropping the salary variables that do not correspond
# to the actual pay cycle. Also dropping check-specific variables and year variables. 
# the period here is just the date of the check.
ct_clean_2 <- ct_clean %>% 
  select(-c(bi_weekly_comp_rate,
            annual_rate,
            `check_#`,
            chk_status,
            chk_option,
            state,
            pyrl_fiscal_yr,
            calendar_year)) %>% 
  mutate(job_indicator = case_when(
    job_indicator == 'p' ~ "primary",
    job_indicator == 's' ~ "secondary",
    job_indicator == 'n' ~ NA_character_
  ))


# fix any missing values by group ####
ct_clean_2 <- ct_clean_2 %>% 
  group_by(emplid_empl_rcd) %>% 
  fill(last_name,
       .direction = 'downup') %>% 
  fill(first_name,
       .direction = 'downup') %>% 
  fill(middle_name,
       .direction = 'downup') %>% 
  fill(sex, 
       .direction = 'downup') %>% 
  fill(name_suffix, 
       .direction = 'downup') %>% 
  fill(orig_hire, 
       .direction = 'downup') %>% 
  ungroup()

# check for duplicate entries per person-period ####
ct_unit_id <- ct_clean_2 %>% 
  group_by(check_dt, 
           emplid_empl_rcd) %>% 
  mutate(unit_id = cur_group_id()) %>% 
  ungroup

ct_dup_ids <- ct_unit_id %>%
  filter(duplicated(unit_id)) %>% 
  pull(unit_id)

# looks like there are very few dups -- just summing up the cases and condensing
11825 / nrow(ct_unit_id)

ct_dedupe <- ct_unit_id %>% 
  filter(unit_id %in% ct_dup_ids) %>% 
  mutate(ungrouped_tot_gross = tot_gross) %>% 
  group_by(unit_id) %>% 
  mutate(across(other:tot_gross,
         ~sum(., na.rm = T))) %>% 
  slice_max(ungrouped_tot_gross, 
            with_ties = FALSE,
            n = 1) %>% 
  ungroup() %>% 
  select(-ungrouped_tot_gross)
  
# bind back with unique data
ct_unique <- ct_unit_id %>% 
  filter(!unit_id %in% ct_dup_ids)

ct_deduped_final <- rbind(ct_unique, 
                          ct_dedupe)

# arrange columns and save ####
ct_final <- ct_deduped_final %>% 
  select(check_dt, 
          emplid_empl_rcd,
          first_name,
          middle_name, 
          last_name, 
          name_suffix, 
          ethnic_grp, 
          sex,
          orig_hire,
          everything(),
         -unit_id)

# Finally, 13% of rows are NA in first and last names -- dropping. 
# These are almost all employees in CT universities. 
ct_final %>% 
  summarise(sum(is.na(first_name) & is.na(last_name), na.rm = T) / n() * 100)

all_na_agencies <- ct_final %>% 
  filter(is.na(first_name) & is.na(last_name)) %>% 
  group_by(agency) %>% 
  summarise(n_missing = n())

ct_final <- ct_final %>% 
  filter(!(is.na(first_name) & is.na(last_name)))

saveRDS(ct_final, 
        "../data/state-employee-data/CT/clean/ct-15-22-premerge.rds")










