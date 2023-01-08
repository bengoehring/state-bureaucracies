# wi_clean.R
# Description: This file reads in and cleans the WI personnel data. 
# Notes: 
##########################################################################

# preamble ####
library(tidyverse)
library(lubridate)
library(readxl)


# read in data ####
wi_files <- list.files("../data/state-employee-data/WI/raw")
wi_paths <- str_c("../data/state-employee-data/WI/raw/", 
                  wi_files)
wi_file_years <- str_extract(wi_files, 
                             "\\d{4}")

read_wi_files <- function(path) {
  path_year <- as.numeric(str_extract(path, "\\d{4}"))
  
  if(path_year %in% c(2016, 2019, 2020)) {
    excel_out <- read_excel(path, 
                            guess_max = 100000, 
                            skip = 7,
                            na = c("0000/00/00",
                                   ""))
  } else {
    excel_out <- read_excel(path, 
                            guess_max = 100000,
                            na = c("0000/00/00",
                                   ""))
  }
  
  colnames(excel_out) <- tolower(str_trim(str_squish(colnames(excel_out))))
  colnames(excel_out) <- str_replace_all(colnames(excel_out),
                                         " ",
                                         "_")
  
  # dropping the extra payroll data because it is ambiguous whether it is hours
  #   or dollars 
  excel_out <- excel_out %>% 
    select(-contains("comp_time")) %>% 
    select(-contains("overtime")) %>% 
    select(-contains("prem_ot")) %>% 
    select(-contains("reg_ot")) %>% 
    select(-contains("premium_ot")) %>% 
    select(-contains("regular_ot"))
  
  # rename columns
  lookup <- c(employment_type = "empl_class", 
              agency = "agency_name",
              job_code_title = "job_title", 
              start_date = "continuous_service_date",
              start_date = "continuous_serve_date",
              class_date = "job_code_start",
              class_date = "class_start_date",
              class_date = "classification_start_date",
              job_code = "class_code",
              job_code_title = "class_title",
              employment_type = "emp_type_&_status",
              employment_type = "employ-ment_type")
  
  excel_out <- excel_out %>% 
    rename(any_of(lookup)) %>% 
    mutate(hourly_rate = as.character(hourly_rate)) 
  
  if("agency_number" %in% colnames(excel_out)) {
    excel_out <- excel_out %>% 
      mutate(agency_number = as.character(agency_number))
  }
  if("job_code" %in% colnames(excel_out)) {
    excel_out <- excel_out %>% 
      mutate(job_code = as.character(job_code))
  }
  if("start_date" %in% colnames(excel_out)) {
    excel_out <- excel_out %>% 
      mutate(start_date = as.character(start_date))
  }
  if("class_date" %in% colnames(excel_out)) {
    excel_out <- excel_out %>% 
      mutate(class_date = as.character(class_date))
  }
  if(!"term_date" %in% colnames(excel_out)) {
    excel_out <- excel_out %>% 
      mutate(term_date = NA_character_)
  }
  
  return(excel_out)
}


wi_list_raw <- map(wi_paths, 
                   read_wi_files)
names(wi_list_raw) <- wi_file_years

wi_df_raw <- wi_list_raw %>% 
  bind_rows(.id = "year")


# basic cleaning ####
wi_df_clean <- wi_df_raw %>% 
  mutate(across(c(job_code, 
                  hourly_rate, 
                  payroll_gross),
                as.numeric)) %>% 
  mutate(across(c(start_date,
                term_date,
                class_date),
                ~parse_date_time(.,
                                 c("ymd",
                                   "mdy")))) %>% 
  mutate(across(where(is.character),
                ~str_trim(str_squish(str_to_lower(.))))) %>%
  mutate(employment_type = case_when(
    str_detect(employment_type,
               "^0\\d$") ~ str_remove(employment_type,
                                     "^0"),
    TRUE ~ employment_type
  ))
  

# add in employment type name information
employment_type_xwalk <- tribble(
  ~employment_type, ~employment_type_name,
  "1", "Permanent classified",
  "5", "Project",
  "6", "Permanent Project",
  '10', "Limited Term Employee",
  '12', "Unclassified",
  '13', "Elected Official",
  "12Z", "Unclassified LTE",
  "PRM", "Permanent classified",
  "PRJ", "Project",
  "PPR", "Permanent Project",
  "LTE", "Limited Term Employee",
  "UNC", "Unclassified",
  "SNL", "Seasonal",
  "ELC", "Elected Official",
  "ULE", "Unclassified LTE"
)
employment_type_xwalk <- employment_type_xwalk %>% 
  mutate(across(.fns = tolower))


wi_employ <- left_join(wi_df_clean,
                       employment_type_xwalk,
                       by = "employment_type")
nrow(wi_employ) == nrow(wi_df_clean)
wi_employ_misses <- anti_join(wi_df_clean,
                              employment_type_xwalk,
                              by = "employment_type")

# name parsing 
suffixes <- "\\ssr$|\\sjr$|\\sii$|\\siii$|\\siv$|\\sv$|\\siiii$"

wi_names <- wi_employ %>% 
  mutate(employee_name = case_when(
    employee_name == "name withheld" ~ NA_character_,
    TRUE ~ str_remove_all(employee_name, 
                          "\\.")
  )) %>% 
  mutate(employee_name = str_replace(employee_name,
                                     ", ",
                                     ",")) %>% 
  mutate(last_name = str_extract(employee_name,
                                 ".*(?=,)")) %>% 
  mutate(first_name = case_when(
    str_count(str_extract(employee_name,
                          "(?<=,).*"),
              " ") == 0 ~ str_extract(employee_name,
                                      "(?<=,).*"),
    str_count(str_extract(employee_name,
                          "(?<=,).*"),
              " ") > 0 ~ str_extract(employee_name,
                                     ",.* ")
  )) %>% 
  mutate(first_name = str_trim(str_remove_all(first_name,
                                     ","))) %>% 
  mutate(middle_name = case_when(
    str_trim(str_extract(employee_name,
                         " [:alpha:]*$")) == first_name ~ NA_character_,
    TRUE ~ str_trim(str_extract(employee_name,
                       "\\s[:alpha:]*$"))
  )) %>% 
  mutate(across(c(first_name,
                  last_name, 
                  middle_name),
                ~str_remove_all(.,
                                "[:punct:]"))) %>% 
  mutate(suffix = str_extract(last_name, 
                              suffixes)) %>% 
  mutate(last_name = str_remove(last_name,
                                suffixes)) %>% 
  mutate(middle_name_temp = case_when(
    str_length(first_name) == 1 ~ first_name,
    TRUE ~ middle_name
  )) %>% 
  mutate(first_name_temp = case_when(
    str_length(first_name) == 1 ~ middle_name,
    TRUE ~ first_name
  )) %>% 
  mutate(first_name = first_name_temp,
         middle_name = middle_name_temp) %>% 
  select(-middle_name_temp,
         -first_name_temp) %>% 
  mutate(middle_initial = str_sub(middle_name,
                                  1,
                                  1)) %>% 
  mutate(last_name_join = case_when(
    is.na(suffix) ~ last_name, 
    TRUE ~ str_c(last_name, 
                 " ",
                 suffix)
  ))
      
# fastlink!
wi_fastlink <- wi_names %>% 
  select(year, 
         first_name, 
         middle_name, 
         middle_initial, 
         last_name, 
         suffix, 
         last_name_join, 
         everything())
saveRDS(wi_fastlink, 
        "../data/state-employee-data/WI/clean/wi-fastlink-pre.rds")











