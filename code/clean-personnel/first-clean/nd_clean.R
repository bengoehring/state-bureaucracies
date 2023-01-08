# nd_clean.R
# Description: This file reads in and cleans the ND personnel data. 
# Notes: 
##########################################################################

library(tidyverse)
library(readxl)
library(lubridate)

# read in data ###
nd_19_22_sheets <- excel_sheets("../data/state-employee-data/ND/raw/2019-2022_all_employees.xlsx")

nd_19_22_list <- map(nd_19_22_sheets,
                     ~read_xlsx("../data/state-employee-data/ND/raw/2019-2022_all_employees.xlsx",
                                sheet = .))
names(nd_19_22_list) <- nd_19_22_sheets

nd_19_22_df <- nd_19_22_list %>% 
  bind_rows(.id = "Year") %>% 
  rename(Agency = Descr) %>% 
  mutate(Year = as.numeric(Year))

nd_10_18_df <- read_xlsx("../data/state-employee-data/ND/raw/2010-2018_all_employees.xlsx")
nd_10_18_df <- nd_10_18_df %>% 
  rename(`Job Code Description` = `Job Title`)

nd_all_raw <- bind_rows(nd_19_22_df, 
                        nd_10_18_df)

# basic cleaning
colnames(nd_all_raw) <- str_replace_all(tolower(colnames(nd_all_raw)),
                                        " ",
                                        "_")
colnames(nd_all_raw) <- str_replace_all(tolower(colnames(nd_all_raw)),
                                        "\\/",
                                        "_")
nd_all_clean <- nd_all_raw %>% 
  mutate(across(where(is.character),
                ~str_trim(str_squish(tolower(.))))) %>% 
  mutate(start_date = as_date(start_date)) %>% 
  mutate(eff_date = ymd(eff_date)) %>% 
  filter(str_detect(name, 
                    "^x{2}.*x{2}$",
                    negate = T))

# parse names
suffixes <- "\\ssr$|\\sjr$|\\sii$|\\siii$|\\siv$|\\sv$|\\siiii$"

nd_names <- nd_all_clean %>% 
  mutate(first_name = str_trim(str_extract(name,
                                           "[:graph:]*\\s"))) %>% 
  mutate(last_name = str_trim(str_remove(name, 
                                first_name))) %>% 
  mutate(across(c(first_name, 
                  last_name),
                  ~str_remove_all(., 
                                  "[:punct:]"))) %>% 
  mutate(suffix = case_when(
    str_detect(last_name, 
               suffixes) ~ str_extract(last_name, 
                                       suffixes),
    TRUE ~ NA_character_
  )) %>% 
  mutate(last_name = case_when(
    is.na(suffix) ~ last_name,
    TRUE ~ str_remove(last_name,
                      suffix)
  )) %>% 
  mutate(last_name_join = case_when(
    is.na(suffix) ~ last_name, 
    TRUE ~ str_c(last_name, 
                 " ",
                 suffix)
  ))

# impute classified status based on job codes 
# per email with Lynn Hart 11/14/2022
nd_classification <- nd_names %>% 
  mutate(classification_status = case_when(
    str_sub(job_code, 1, 2) %in% c("nc", "un") ~ "not classified",
    str_sub(job_code, 1, 2) %in% c("sc", "cl") ~ "classified",
    str_sub(job_code, 1, 2) == "jd" ~ "judiciary",
    str_sub(job_code, 1, 2) == "ng" ~ "national guard",
    str_sub(job_code, 1, 2) == "ag" &
      job_code_description == "asst atty gen-not classfd" ~ "not classified",
    str_sub(job_code, 1, 2) == "ag" ~ "attorney general",
    job_code == "999998" ~ "pending classification", 
    TRUE ~ NA_character_
  ))


# save for fastlink 
nd_fastlink <- nd_classification %>% 
  select(year, 
         first_name,
         last_name, 
         last_name_join,
         suffix, 
         start_date,
         everything())

saveRDS(nd_fastlink, 
        "../data/state-employee-data/ND/clean/nd-fastlink-pre.rds")



