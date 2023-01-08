# vt_clean.R
# Description: This file reads in and cleans the WI personnel data. 
# Notes: 
##########################################################################

# preamble ####
library(tidyverse)
library(lubridate)


# read in data ####
vt_files <- list.files("../data/state-employee-data/VT/raw/")
vt_paths <- str_c("../data/state-employee-data/VT/raw/", 
                  vt_files)

read_vt_data <- function(path) {
  out <- read_csv(path)
  
  colnames(out) <- str_squish(str_trim(tolower(colnames(out))))
  colnames(out) <- str_replace_all(colnames(out),
                                   " ",
                                   "_")
  
  if("type_of_job" %in% colnames(out)) {
    out <- out %>% 
      rename(job_type = type_of_job)
  }
  if("year" %in% colnames(out)) {
    out <- out %>% 
      rename(fiscal_year = year)
  }
  if("multiple_records" %in% colnames(out)) {
    out <- out %>% 
      rename(multiple_record = multiple_records)
  }
  if("total_expenses" %in% colnames(out)) {
    out <- out %>% 
      rename(expenses = total_expenses)
  }
  
  return(out)
}

vt_list <- map(vt_paths, read_vt_data)

vt_df_raw <- vt_list %>% 
  bind_rows()


# basic cleaning ####
# need to do some standardizing. The 2009-2014 file does not include any information
#   about benefits nor include a total pay column. It also includes expenses in the 
#   total column which are not inlcuded in the total compensation column. 

vt_df_clean <- vt_df_raw %>% 
  select(-total, 
         -total_compensation, 
         -overtime_units) %>% 
  rowwise() %>% 
  mutate(total_pay = sum(c_across(c(pay, 
                                    other_pay, 
                                    overtime_pay)), 
                         na.rm = T)) %>% 
  ungroup() %>% 
  select(fiscal_year, 
         everything()) %>% 
  mutate(across(where(is.character),
                ~str_squish(str_trim(tolower(.)))))

suffixes <- "\\ssr$|\\sjr$|\\sii$|\\siii$|\\siv$|\\sv$|\\siiii$"

vt_df_names <- vt_df_clean %>% 
  mutate(last_name = str_extract(name, 
                                 "^.*(?=,)")) %>% 
  mutate(first_name = str_extract(name, 
                                  "(?<=,).*")) %>% 
  mutate(middle_name_initial = case_when(
    str_detect(first_name, 
               "\\s\\w{1}$") ~ str_extract(first_name, 
                                           "\\s\\w{1}$"),
    str_detect(first_name, 
               "\\s") ~ str_extract(first_name, 
                                    "\\s.*$"),
    TRUE ~ NA_character_
  )) %>% 
  mutate(first_name = case_when(
    !is.na(middle_name_initial) ~ str_remove(first_name, 
                                             middle_name_initial),
    TRUE ~ first_name
  ))%>% 
  mutate(across(c(first_name, 
                  middle_name_initial, 
                  last_name),
                ~str_trim(str_squish((str_remove_all(.,
                                                     "[:punct:]")))))) %>% 
  mutate(suffix = str_extract(last_name, 
                              suffixes)) %>% 
  mutate(last_name = str_remove(last_name, 
                                suffixes)) %>% 
  mutate(last_name_join = case_when(
    is.na(suffix) ~ last_name, 
    TRUE ~ str_c(last_name, 
                 " ", 
                 suffix)
  )) %>% 
  mutate(middle_initial = str_sub(middle_name_initial,
                                  1,
                                  1))

# Helpfully, VT notes if there are two records for the same person in a given
#   year. When this is the case, going to take the row that has the larger 
#   salary and add the income from the other line. 

vt_df_multiples <- vt_df_names %>% 
  mutate(multiple_record = case_when(
    str_detect(multiple_record,
               "multiple record") ~ 1, 
    TRUE ~ 0
  )) 

vt_df_multiple_1 <- vt_df_multiples%>% 
  filter(multiple_record == 1)

vt_df_multiple_0 <- vt_df_multiples%>% 
  filter(multiple_record == 0)

vt_df_multiple_1 <- vt_df_multiple_1 %>% 
  group_by(fiscal_year, 
           name) %>% 
  mutate(max_salary_row = case_when(
    total_pay == max(total_pay) ~ 1, 
    TRUE ~ 0
  )) %>% 
  mutate(across(c(pay, 
                  other_pay,
                  overtime_pay, 
                  total_pay, 
                  total_benefits, 
                  expenses),
                ~sum(., 
                     na.rm = T))) %>% 
  filter(max_salary_row == 1) %>% 
  ungroup()

# just eight instances where they made the same amount in both
#   jobs
vt_df_multiple_1 <- vt_df_multiple_1 %>% 
  group_by(fiscal_year,
           name) %>% 
  slice_sample(n = 1) %>% 
  ungroup() %>% 
  select(-max_salary_row)
  

vt_df_deduped <- rbind(vt_df_multiple_1,
                       vt_df_multiple_0) %>% 
  select(-name)

# set up data to be fun through fastlink

vt_df_fastlink <- vt_df_deduped %>% 
  select(fiscal_year, 
         first_name, 
         middle_initial, 
         last_name, 
         last_name_join, 
         suffix, 
         everything())

saveRDS(vt_df_fastlink,
        "../data/state-employee-data/VT/clean/vt-fastlink-pre.rds")


