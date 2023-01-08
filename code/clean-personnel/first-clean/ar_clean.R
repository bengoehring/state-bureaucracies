# ar_clean.R
# Description: This file reads in and cleans the AR personnel data. 
# Notes: 
##########################################################################

library(tidyverse)
library(readxl)

# read in data ####
ar_files <- list.files("../data/state-employee-data/AR/raw")
ar_paths <- str_c("../data/state-employee-data/AR/raw/", 
                  ar_files)

ar_paths <- str_subset(ar_paths, 
                       "AR/raw/\\d{2}")

ar_raw_list <- map(ar_paths, 
                   ~read_xlsx(.))
names(ar_raw_list) <- str_replace(str_extract(ar_paths, 
                                              "\\d{2}.\\d{4}"),
                                  "\\.",
                                  "_")

ar_raw_df <- ar_raw_list %>% 
  bind_rows(.id = 'month_year')



# basic cleaning and name parsing ####
suffixes <- "\\ssr$|\\sjr$|\\sii$|\\siii$|\\siv$|\\sv$|\\siiii$"

colnames(ar_raw_df) <- tolower(str_replace_all(colnames(ar_raw_df), 
                                       "\\s|-",
                                       "_"))

ar_clean_df <- ar_raw_df %>% 
  mutate(across(where(is.character),
         ~tolower(str_squish(str_trim(.))))) %>% 
  mutate(across(c(first_name, 
                initials, 
                last_name),
                ~str_remove_all(.,
                                "[:punct:]"))) %>% 
  mutate(initials = case_when(
    str_detect(first_name, 
               "\\s\\w{1}$") ~ str_extract(first_name, 
                                           "\\w{1}$"),
    TRUE ~ initials
  )) %>% 
  mutate(first_name = case_when(
    str_detect(first_name, 
               "\\s\\w{1}$") ~ str_remove(first_name, 
                                          "\\s\\w{1}$"),
    TRUE ~ first_name
  )) %>% 
  mutate(middle_initial = case_when(
    str_detect(initials, 
               "^\\w{1}$") ~ str_sub(initials, 
                                     start = 1, 
                                     end = 1),
    str_detect(initials, 
               "^\\w{3}$") ~ str_sub(initials, 
                                    start = 2, 
                                    end = 2),
    str_length(initials) == 2 & str_sub(initials,
                                        start = 2,
                                        end = 2) != str_sub(last_name,
                                                            start = 1, 
                                                            end = 1) ~ str_sub(initials,
                                                                               start = 2,
                                                                               end = 2),
    TRUE ~ NA_character_
  )) %>% 
  mutate(suffix = case_when(
    str_detect(last_name, 
               suffixes) ~ str_trim(str_extract(last_name,
                                       suffixes)),
    TRUE ~ NA_character_
  )) %>% 
  mutate(last_name = case_when(
    str_detect(last_name,
               suffixes) ~ str_remove(last_name, 
                                      suffixes),
    TRUE ~ last_name
  )) %>% 
  select(month_year,
         first_name, 
         middle_initial, 
         last_name, 
         suffix,
         gender,
         ethnic_origin,
         everything(),
         -initials) %>% 
  distinct()


# Create data for fastlink assignment of ids ####
ar_clean_df <- ar_clean_df %>% 
  mutate(last_name_join = case_when(
    !is.na(suffix) ~ str_c(last_name, 
                           " ", 
                           suffix), 
    TRUE ~ last_name
  ))


saveRDS(ar_clean_df, 
        '../data/state-employee-data/AR/clean/ar-fastlink-pre.rds')




