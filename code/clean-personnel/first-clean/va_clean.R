# va_clean.R
# Description: This file reads in and cleans the CT personnel data. 
# Notes: 
##########################################################################

# preamble ####
library(readxl)
library(tidyverse)
library(lubridate)

# read data ####
va_files <- list.files("../data/state-employee-data/VA/raw")
va_paths <- str_c("../data/state-employee-data/VA/raw",
                  va_files)

va_data_raw_list <- map(va_paths,
                        ~read_xlsx(.,
                                   col_types = "text",
                                   na = c("", ".")))
va_data_raw_df <- va_data_raw_list %>% 
  bind_rows()

# basic cleaning ####
colnames(va_data_raw_df) <- tolower(colnames(va_data_raw_df))
colnames(va_data_raw_df) <- str_replace_all(colnames(va_data_raw_df),
                                            " ",
                                            "_")
va_data_raw_clean <- va_data_raw_df %>% 
  mutate(across(c(data_source_date,
                  state_begin_date,
                  position_separation_date),
                ~as_date(as.numeric(.),
                         origin = "1899-12-30"))) %>% 
  mutate(across(where(is.character),
                ~str_trim(str_squish(tolower(.)))))

suffixes <- "\\ssr$|\\sjr$|\\sii$|\\siii$|\\siv$|\\sv$|\\siiii$"
va_data_names <- va_data_raw_clean %>% 
  mutate(across(c(first_name, 
                  last_name),
                  ~str_remove_all(.,
                                "[:punct:]"))) %>% 
  mutate(last_name_join = last_name) %>% 
  mutate(suffix = str_extract(last_name, 
                              suffixes),
         last_name = str_trim(str_remove(last_name,
                                         suffixes)))

# note the multiple, sometimes overlapping data systems (PMIS and cardinal)
# Remove any duplicate people shown twice in the two systems. 
#   There are cardinal system entries for the following dates: 
#   03-31-2022
#   06-30-2022
#   12-31-2021
va_data_dedup <- va_data_names %>% 
  group_by(data_source_date) %>% 
  distinct() %>% 
  ungroup()







# save data to assign ids probabilistically on the cluster. Note that we will 
#   be merging on first and last names and state begin date -- creating a 
#   numeric version of state begin date for window blocking

va_data_dedup <- va_data_dedup %>% 
  mutate(state_begin_year = year(state_begin_date))

saveRDS(va_data_dedup,
        "../data/state-employee-data/VA/clean/va-fastlink-pre.rds")









