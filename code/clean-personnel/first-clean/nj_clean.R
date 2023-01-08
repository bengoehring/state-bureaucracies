# nj_clean.R
# Description: This file reads in and cleans the NJ personnel data. 
# Notes: 
##########################################################################

library(tidyverse)
library(readxl)
library(vroom)
library(lubridate)

agency_raw <- vroom("../data/state-employee-data/NJ/raw/YourMoney_Agency_Payroll.csv",
                    na = c("", "NA", "null"))
authority_raw <- vroom("../data/state-employee-data/NJ/raw/YourMoney_Authority_Payroll.csv",
                       na = c("", "NA", "null"))

colnames(agency_raw) <- tolower(colnames(agency_raw))
colnames(authority_raw) <- tolower(colnames(authority_raw))

# start with just agencies
# two different sets of records -- master and detail
# per https://data.nj.gov/Government-Finance/YourMoney-Agency-Payroll/iqwc-r2w7
# really just interested in the master file
agency_clean <- agency_raw %>% 
  mutate(across(where(is.character),
                ~str_trim(str_squish(tolower(.))))) %>% 
  mutate(as_of_date = mdy(as_of_date)) %>% 
  mutate(original_employment_dte = mdy(original_employment_dte))

agency_clean <- agency_clean %>%
  group_by(payroll_id) %>% 
  fill(c(middle_initial,
         original_employment_dte,
         first_name,
         last_name, 
         full_name)) %>% 
  ungroup()

agency_master <- agency_clean %>% 
  filter(record_type == 'master') %>% 
  select(-c(paid_department_agency_desc:record_type))

# additional name cleaning
# the suffixes in this file do not have the space....

suffixes_agency <- "sr$|jr$|ii$|iii$|iv$|\\siiii$"

agency_name <- agency_master %>% 
  mutate(across(c(first_name,
                  middle_initial,
                  last_name),
                ~str_remove_all(., 
                                "[:punct:]"))) %>% 
  mutate(suffix = case_when(
    str_detect(last_name,
               suffixes_agency) ~ str_extract(last_name, 
                                              suffixes_agency),
    TRUE ~ NA_character_
  )) %>% 
  mutate(suffix = case_when(
    first_name %in% c("christina", 
                      "diane", 
                      "amanda",
                      "iryna",
                      "mariya",
                      "skyla",
                      "lesia") ~ NA_character_,
    TRUE ~ suffix
  )) %>% 
  mutate(last_name = case_when(
    !is.na(suffix) ~ str_remove(last_name, 
                                suffixes_agency),
    TRUE ~ last_name
  ))





# clean authority data -- not as nice as the agency data
suffixes <- "\\ssr$|\\sjr$|\\sii$|\\siii$|\\siv$|\\sv$|\\siiii$"

authority_clean <- authority_raw %>% 
  mutate(across(where(is.character),
                ~str_trim(str_squish(tolower(.))))) %>% 
  mutate(as_of_date = mdy(as_of_date))


authority_name <- authority_clean %>% 
  mutate(last_name = str_extract(full_name, 
                                  ".*(?=,)")) %>% 
  mutate(middle_initial = case_when(
    str_detect(full_name, 
               "\\s\\w{1}$") ~ str_extract(full_name, 
                                         "\\s\\w{1}$"),
    TRUE ~ NA_character_
  )) %>% 
  mutate(first_name = case_when(
    is.na(middle_initial) ~ str_extract(full_name, 
                                        ",.*$"),
    TRUE ~ str_extract(full_name, 
                       ",.*\\s")
  )) %>% 
  mutate(across(c(first_name, 
                  middle_initial,
                  last_name),
                ~str_trim(str_squish(str_remove_all(., 
                                                    "[:punct:]"))))) %>% 
  mutate(suffix = case_when(
    str_detect(last_name, 
               suffixes) ~ str_extract(last_name, 
                                       suffixes),
    TRUE ~ NA_character_
  )) %>% 
  mutate(last_name = case_when(
    str_detect(last_name, 
               suffixes) ~ str_remove(last_name, 
                                      suffixes),
    TRUE ~ last_name
  ))

# missed some suffixes due to inconsistent formats
another_suffix_format <- "^sr\\s|^jr\\s|^ii\\s|^iii\\s|^iv\\s|^iiii\\s"

authority_name <- authority_name %>% 
  mutate(suffix = case_when(
    str_detect(first_name, 
               another_suffix_format) ~ str_extract(first_name, 
                                                    another_suffix_format),
    TRUE ~ suffix
  )) %>% 
  mutate(first_name = case_when(
    str_detect(first_name, 
               another_suffix_format) ~ str_remove(first_name, 
                                                   another_suffix_format),
    TRUE ~ first_name
  ))


# prepare agency and authority data to be concatted 
colnames(agency_name) <- str_remove(colnames(agency_name),
                                      "master_")

agency_name_stand <- agency_name %>% 
  mutate(agency_type = "agency") %>% 
  select(-calendar_year,
         -calendar_quarter)
authority_name_stand <- authority_name %>% 
  mutate(agency_type = "authority") %>% 
  rename(ytd_regular_pay = regular_pay,
         ytd_overtime_payments = overtime_payments,
         ytd_all_other_payments = all_other_payments,
         department_agency_desc = authority_name,
         title_desc = master_title_desc,
         section_desc = master_section_desc) %>% 
  select(-calendar_year,
         -calendar_quarter)

agency_and_authority <- bind_rows(agency_name_stand,
                                  authority_name_stand)



# will need to asign unique ids to the authority employees via fastlink
nj_fastlink <- agency_and_authority %>% 
  select(-full_name) %>% 
  mutate(last_name_join = case_when(
    is.na(suffix) ~ last_name, 
    TRUE ~ str_c(last_name, 
                 " ",
                 suffix)
  ))

saveRDS(nj_fastlink,
        "../data/state-employee-data/NJ/clean/nj-fastlink-pre.rds")









