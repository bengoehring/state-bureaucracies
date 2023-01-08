# co_clean.R
# Description: This file reads in and cleans the CO personnel data. 
# Notes: 
##########################################################################

library(tidyverse)
library(lubridate)
library(readxl)

# read in data

co_path <- "../data/state-employee-data/CO/raw/CORA_-_6.28.2022_-_Ben_Goehring.xlsx"

read_co <- function(path, 
                    sheet_name) {
  out <- read_excel(path, 
                    sheet_name)
  out <- out %>% 
    mutate(FTE = as.numeric(FTE))
  return(out)
}

co_raw_list <- map(excel_sheets(co_path),
                   ~read_co(co_path, 
                            .))
names(co_raw_list) <- excel_sheets(co_path)

co_raw <- co_raw_list %>% 
  bind_rows()


# initial cleaning
co_clean <- co_raw %>% 
  rename_with(~str_to_lower(str_replace_all(., 
                                            " ",
                                            "_"))) %>% 
  mutate(across(where(is.character),
                ~str_trim(str_squish(str_to_lower(.))))) %>% 
  mutate(across(c(original_hiredate, 
                  termination_date),
                ymd)) %>% 
  mutate(source_date = ymd(str_c(str_sub(sourcedate,
                                         1, 
                                         4),
                                 "-",
                                 str_sub(sourcedate, 
                                         5, 
                                         6),
                                 "-01"))) %>% 
  select(-sourcedate)


# name parsing 
suffixes <- "\\ssr$|\\sjr$|\\sii$|\\siii$|\\siv$|\\sv$|\\siiii$"

co_names <- co_clean %>% 
  mutate(name = str_remove_all(name, 
                               "\\.")) %>% 
  mutate(last_name = str_extract(name, 
                                 ".*(?=,)")) %>% 
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
  mutate(first_name = case_when(
    # there is a first and middle name or initial after the comma
    str_detect(name, 
               ",.+ .+") ~ str_remove(str_extract(name, 
                                       ",.* "),
                                      ","),
    TRUE ~ str_extract(name, 
                       ",.*$")
  )) %>% 
  mutate(middle_name = case_when(
    str_detect(name, 
               ",.+ .+") ~ str_extract(name, 
                                       " [:graph:]*$")
  )) %>% 
  mutate(across(c(first_name, 
                  middle_name, 
                  last_name,
                  suffix),
         ~str_trim(str_remove_all(., 
                                  "[:punct:]")))) %>% 
  mutate(middle_initial = str_sub(middle_name, 
                                  1, 
                                  1)) %>% 
  mutate(last_name_join = case_when(
    is.na(suffix) ~ last_name, 
    TRUE ~ str_c(last_name, 
                 " ",
                 suffix)
  ))


# save for fastlink deduping
co_fastlink <- co_names %>%
  mutate(original_hire_year = as.integer(year(original_hiredate))) %>% 
  mutate(blocking_period = case_when(
    original_hire_year <= 1980 ~ 1980,
    original_hire_year > 1980 & original_hire_year <= 1990 ~ 1990,
    original_hire_year > 1990 & original_hire_year <= 2000 ~ 2000,
    original_hire_year > 2000 & original_hire_year <= 2010 ~ 2010,
    original_hire_year > 2010 ~ 2020
  )) %>% 
  mutate(original_hiredate_join = as.character(original_hiredate)) %>% 
  select(source_date, 
         first_name, 
         middle_initial, 
         middle_name, 
         last_name,
         last_name_join,
         suffix, 
         original_hiredate_join,
         everything())

saveRDS(co_fastlink,
        "../data/state-employee-data/CO/clean/co-fastlink-pre.rds")



boop <- co_fastlink %>% 
  filter(blocking_period == 2020) 

boop %>% 
  summarise(sum(is.na(first_name)) / nrow(boop),
            sum(is.na(middle_initial)) / nrow(boop),
            sum(is.na(last_name_join)) / nrow(boop),
            sum(is.na(original_hiredate_join)) / nrow(boop))



