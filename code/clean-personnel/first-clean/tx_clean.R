# tx_clean.R
# Description: This file reads in and cleans the TX personnel data. 
# Notes: 
##########################################################################

# preamble ####
library(tidyverse)
library(lubridate)
library(readxl)

# read in data ####
tx_files <- list.files("../data/state-employee-data/TX/raw/")
tx_paths <- str_c("../data/state-employee-data/TX/raw/", 
                  tx_files)

tx_list <- map(tx_paths, 
               ~read_xlsx(., 
                          guess_max = 500000,
                          na = c("", ".")))
names(tx_list) <- tx_files

tx_list_clean <- map(1:length(tx_list),
                     ~mutate(tx_list[[.]], 
                             AGENCY = as.character(AGENCY),
                             `TERM DATE` = as.character(`TERM DATE`)))

names(tx_list_clean) <- tx_files
tx_raw_df <- bind_rows(tx_list_clean)

# basic cleaning ####
colnames(tx_raw_df) <- str_replace_all(tolower(colnames(tx_raw_df)), 
                                       " ",
                                       "_")

suffixes <- "\\ssr$|\\sjr$|\\sii$|\\siii$|\\siv$|\\sv$|\\siiii$"

tx_clean_df <- tx_raw_df %>% 
  mutate(across(where(is.character),
                ~tolower(str_squish(str_trim(.))))) %>% 
  mutate(across(c(first_name,
                  mi,
                  last_name),
                ~str_remove_all(., 
                                "[:punct:]"))) %>% 
  mutate(term_date = as_date(parse_date_time(term_date, 
                                     orders = c("%Y/%m/%d", 
                                                "%m/%d/%Y")))) %>% 
  select(-agency,
         -class_code,
         -org_code, 
         -location) %>% 
  mutate(suffix = str_trim(str_extract(last_name, 
                              suffixes))) %>% 
  mutate(last_name = str_remove(last_name, 
                                suffixes))
  
# .2% dup observations -- taking the max salary
dups <- tx_clean_df %>% 
  group_by(month, 
           statenum) %>% 
  filter(n() > 1)

nrow(dups) / nrow(tx_clean_df)


tx_clean_df_unique <- tx_clean_df %>% 
  group_by(month, 
           statenum) %>% 
  filter(n() == 1)

tx_clean_df_dup <- tx_clean_df %>% 
  group_by(month, 
           statenum) %>% 
  filter(n() > 1) %>% 
  mutate(max_row = case_when(
    salary == max(salary) ~ 1, 
    TRUE ~ 0
  )) %>% 
  filter(max_row == 1) %>% 
  slice_sample(n = 1) %>% # just in case there are any extra dups with matching salaries
  ungroup()

tx_clean_df_dedup <- rbind(tx_clean_df_unique, 
                           tx_clean_df_dup)

tx_clean_df_dedup <- tx_clean_df_dedup %>% 
  select(month,
         employee_id = statenum,
         first_name, 
         mi,
         last_name, 
         ethnicity, 
         gender, 
         hire_date, 
         term_date, 
         everything(),
         -max_row,)

# save data
saveRDS(tx_clean_df_dedup, 
        "../data/state-employee-data/TX/clean/tx-19-22-premerge.rds")



         
