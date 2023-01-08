# sc_clean.R
# Description: This file reads in and cleans the SC personnel data. 
# Notes: 
##########################################################################

# preamble ####
library(tidyverse)
library(lubridate)
library(readxl)

# read in data ####
sc_sheets <- excel_sheets("../data/state-employee-data/SC/raw/Muck Rock FOIA.xlsx")
  
sc_raw_list <- map(sc_sheets, 
                  ~read_xlsx("../data/state-employee-data/SC/raw/Muck Rock FOIA.xlsx",
                             sheet = .))
names(sc_raw_list) <- sc_sheets

# not using the bonus data for now. 95% of the values are less than 2000
bonus_data <- sc_raw_list[["Bonus Data"]]
sc_raw_list_months <- sc_raw_list[sc_sheets != "Bonus Data"]

sc_raw_df <- bind_rows(sc_raw_list_months,
                       .id = "month_year")


# basic cleaning ####
suffixes <- "\\ssr$|\\sjr$|\\sii$|\\siii$|\\siv$|\\sv$|\\siiii$"


colnames(sc_raw_df) <- str_replace_all(tolower(colnames(sc_raw_df)),
                                       " ",
                                       "_")

sc_clean_df <- sc_raw_df %>% 
  mutate(month_year = my(month_year)) %>% 
  mutate(across(where(is.character),
                ~tolower(str_trim(str_squish(.))))) %>% 
  mutate(across(c(first_name, 
                  middle_name,
                  last_name,
                  suffix),
                ~str_remove_all(., 
                               "[:punct:]"))) %>% 
  mutate(suffix = case_when(
    is.na(suffix) & str_detect(last_name, 
                               suffixes) ~ str_trim(str_extract(last_name, 
                                                                suffixes)),
    TRUE ~ suffix
  )) %>% 
  mutate(last_name = case_when(
    !is.na(suffix) ~ str_remove(last_name, suffixes),
    TRUE ~ last_name
  )) %>% 
  mutate(middle_initial = str_sub(middle_name, 
                                  1, 
                                  1)) %>% 
  mutate(last_name_join = case_when(
    is.na(suffix) ~ last_name, 
    TRUE ~ str_c(last_name, " ", suffix)
  ))

saveRDS(sc_clean_df, 
        "../data/state-employee-data/SC/clean/sc-fastlink-pre.rds")


