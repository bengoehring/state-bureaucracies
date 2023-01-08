library(tidyverse)
library(lubridate)

ri_raw <- read_csv("../data/state-employee-data/RI/raw/RIpayroll_export_2022-04-27_09-11.csv",
                   guess_max = 100000)


# initial cleaning
suffixes <- "\\ssr$|\\sjr$|\\sii$|\\siii$|\\siv$|\\sv$|\\siiii$"

ri_clean <- ri_raw %>% 
  mutate(across(where(is.character),
                ~str_trim(str_squish(str_to_lower(.))))) %>% 
  rename(first_name = first, 
         last_name = last,
         middle_initial = m) %>% 
  mutate(across(first_name:middle_initial,
                ~str_remove_all(.,
                                "[:punct:]"))) 

# remove middle inital from beginning of first name
ri_names <- ri_clean %>% 
  mutate(suffix = str_extract(last_name,
                              suffixes)) %>% 
  mutate(last_name = str_remove(last_name, 
                                suffixes)) %>% 
  mutate(middle_initial_2 = case_when(
    is.na(middle_initial) & str_detect(first_name, 
                                       "^[:alpha:]{1}\\s") ~ str_trim(str_extract(first_name, 
                                                                                  "^[:alpha:]{1}\\s")),
    TRUE ~ middle_initial
  )) %>% 
  mutate(first_name = case_when(
    is.na(middle_initial) & str_detect(first_name, 
                                       "^[:alpha:]{1}\\s") ~ str_remove(first_name, 
                                                                         "^[:alpha:]{1}\\s"),
    TRUE ~ first_name
  )) %>% 
  select(-middle_initial) %>% 
  rename(middle_initial = middle_initial_2) %>% 
  mutate(last_name_join = case_when(
    !is.na(suffix) ~ str_c(last_name, 
                           " ",
                           suffix),
    TRUE ~ last_name
  )) %>% 
  select(fiscal_year, 
         first_name, 
         middle_initial,
         last_name, 
         last_name_join,
         suffix, 
         everything())
  

# not worried about the 26 misses -- looks like wonky rows that are effectively dropped
# trying to filter out colleges nad judiciary to help merges
ri_fastlink <- ri_names %>% 
  mutate(termination_date = mdy(termination)) %>%
  filter(!department %in% c("state colleges",
                           "r i college",
                           "u r i",
                           "judicial",
                           "community college of r i",
                           "legislative")) %>% 
  select(-termination)


# run on fastlink to assign ids
saveRDS(ri_fastlink, 
        "../data/state-employee-data/RI/clean/ri-fastlink-pre.rds")



