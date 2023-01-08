# nv_clean.R
# Description: This file reads in and cleans the FL personnel data. 
# Notes: 
##########################################################################

# preamble ####
library(tidyverse)
library(vroom)
library(lubridate)
library(readxl)


nv_files <- list.files("../data/state-employee-data/NV/raw/")
nv_paths <- str_c("../data/state-employee-data/NV/raw/",
                  nv_files)

nv_list_out <- map(nv_paths, 
                   ~read_excel(.,
                               guess_max = 50000))
names(nv_list_out) <- str_extract(nv_files, 
                                  "[:digit:]{4}")

cleaner <- function(data, data_name) {
  colnames(data) <- tolower(colnames(data))
  colnames(data) <- str_remove_all(colnames(data),
                                   "%|\\+|\\>|\\.|/")
  colnames(data) <- str_trim(str_squish(colnames(data)))
  colnames(data) <- str_replace_all(colnames(data), 
                                    " ",
                                    "_")
  if("pay_off_amtspecial_pay" %in% colnames(data)) {
    data <- data %>% 
      rename(pay_off_amt_special_pay = pay_off_amtspecial_pay)
  }
  if("pay_off_amtspec,al_pay" %in% colnames(data)) {
    data <- data %>% 
      rename(pay_off_amt_special_pay = `pay_off_amtspec,al_pay`)
  }
  if("retirement_&_fringe_benefits" %in% colnames(data)) {
    data <- data %>% 
      rename(retirement_fringe_benefits = `retirement_&_fringe_benefits`)
  }
  
  # should just be relevant for 2021
  if(!"cal_yr" %in% colnames(data)) {
    data <- data %>% 
      mutate(cal_yr = data_name)
  }

  data <- data %>% 
    mutate(cal_yr = as.numeric(cal_yr)) %>% 
    mutate(agency = as.character(agency)) %>% 
    mutate(base_salary = as.numeric(base_salary))
  
  return(data)
}

nv_list_clean <- map2(nv_list_out, 
                      names(nv_list_out), 
                      cleaner)

nv_df_clean <- bind_rows(nv_list_clean)

# continuing to standardize variables
nv_df_clean <- nv_df_clean %>% 
  select(-sal) %>% 
  rowwise() %>% 
  mutate(retirement_fringe_benefits = case_when(
    is.na(retirement_fringe_benefits) ~ sum(retirement_amt, 
                                            fringe_benefits,
                                            na.rm = T),
    !is.na(retirement_fringe_benefits) ~ retirement_fringe_benefits,
    TRUE ~ NA_real_
  )) %>% 
  ungroup() %>% 
  select(-retirement_amt, 
         -fringe_benefits) %>% 
  mutate(total = case_when(
    is.na(total) ~ gross_salary,
    TRUE ~ total
  )) %>% 
  select(-gross_salary) %>% 
  mutate(pay_off_amt_special_pay = case_when(
    is.na(pay_off_amt_special_pay) ~ pay_off_amt,
    TRUE ~ pay_off_amt_special_pay
  )) %>% 
  select(-pay_off_amt)

# basic cleaning ####
nv_clean_df_2 <- nv_df_clean %>% 
  mutate(across(where(is.character),
                ~str_squish(str_trim(tolower(.))))) %>% 
  select(-agency) %>% 
  mutate(across(where(is.character),
                ~str_remove_all(., 
                                '"')))

# parse names
suffixes <- "\\ssr$|\\sjr$|\\sii$|\\siii$|\\siv$|\\sv$|\\siiii$"

nv_parse_names <- nv_clean_df_2 %>% 
  mutate(last_name = str_extract(employee_name, 
                                 "^.*(?=,)")) %>% 
  mutate(suffix = str_trim(str_extract(last_name, 
                              suffixes))) %>% 
  mutate(last_name = str_remove(last_name, 
                                suffixes)) %>% 
  mutate(middle_initial = str_trim(str_extract(employee_name, 
                                               "\\s\\w$"))) %>% 
  mutate(first_name = str_extract(employee_name, 
                                  "(?<=,).*$")) %>% 
  mutate(first_name = case_when(
    is.na(middle_initial) ~ str_trim(first_name),
    TRUE ~ str_trim(str_remove(first_name, 
                               str_c(middle_initial, "$")))
  )) %>% 
  mutate(across(c(first_name, 
                  middle_initial, 
                  last_name, 
                  suffix),
         ~str_squish(str_trim(str_remove_all(.,
                                             "[:punct:]"))))) %>% 
  select(-employee_name) %>% 
  mutate(last_name_join = case_when(
    is.na(suffix) ~ last_name, 
    TRUE ~ str_c(last_name, " ", suffix)
    ))

nv_dates <- nv_parse_names %>% 
  mutate(across(c(contsvc, 
                term_dt),
         as.character)) %>% 
  mutate(term_dt_parse = str_c(str_sub(term_dt, 1, 4),
                               "-",
                               str_sub(term_dt, 5, 6),
                               "-",
                               str_sub(term_dt, 7, 8))) %>% 
  mutate(term_dt_parse = ymd(term_dt_parse)) %>% 
  mutate(hire_date_parse = case_when(
    str_length(contsvc) == 8 ~ str_c(str_sub(contsvc, 5, 8),
                                     "-",
                                     str_sub(contsvc, 1, 2),
                                     "-",
                                     str_sub(contsvc, 3, 4)),
    str_length(contsvc) == 7 ~ str_c(str_sub(contsvc, 4, 7),
                                     "-",
                                     str_sub(contsvc, 1, 1),
                                     "-",
                                     str_sub(contsvc, 2, 3))
  )) %>%  
  mutate(hire_date_parse = ymd(hire_date_parse)) %>% 
  select(-contsvc,
         -term_dt)
  
# inconsistent definitions of total salary variable so redefining
nv_salaries <- nv_dates %>% 
  select(-total) %>% 
  rowwise() %>% 
  mutate(total = sum(base_salary, 
                     overtime_amt, 
                     pay_off_amt_special_pay, 
                     retirement_fringe_benefits,
                     na.rm = T)) %>% 
  ungroup()


# job_class is not defined in 2015 and 2016 -- per correspondence with NV, 
#     imputing based on information in title
nv_class <- nv_salaries %>% 
  mutate(job_class = case_when(
    is.na(job_class) & str_detect(title, 
                                  "u") ~ "unclass",
    is.na(job_class) & (str_detect(title_description, 
                                   "non class") | str_detect(title, 
                                                             "cb")) ~ "non class",
    is.na(job_class) & (str_detect(title, 
                                   "e") | title == "80") ~ "elect",
    is.na(job_class) ~ "class",
    TRUE ~ job_class
  )) %>% 
  mutate(job_class = case_when(
    job_class == "class" ~ "classified",
    job_class %in% c("ncls", "nonclas", "non class", "board") ~ "non-classified", # per title description: all board are non classified 
    job_class %in% c("uncls", "unclass") ~ "unclassified",
    job_class == "elect" ~ "elected",
    TRUE ~ NA_character_
  ))

# employee id and start date only in a few years -- so will use for checking
#   fastlink id assignment but will not use for actually assigning ids
nv_fastlink_pre <- nv_class %>% 
  select(cal_yr, 
         empl_id, 
         first_name,
         middle_initial,
         last_name,
         suffix,
         base_salary:pay_off_amt_special_pay,
         retirement_fringe_benefits,
         total,
         everything(),
         -base_ot_amt)

saveRDS(nv_fastlink_pre,
        "../data/state-employee-data/NV/clean/nv-fastlink-pre.rds")













