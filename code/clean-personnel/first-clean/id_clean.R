# id_clean.R
# Description: This file reads in and cleans the ID personnel data. 
# Notes: 
##########################################################################

library(tidyverse)
library(lubridate)

# read in data ####
id_files <- list.files("../data/state-employee-data/ID/raw/")
id_paths <- str_c("../data/state-employee-data/ID/raw/", 
                  id_files)


read_id <- function(path) {
  out <- read_csv(path, 
                  skip = 2)
  return(out)
}

id_list_raw <- map(id_paths, 
                   read_id)
id_df_raw <- id_list_raw %>% 
  bind_rows()


# initial cleaning ####

id_df_clean <- id_df_raw%>% 
  rename_with(~tolower(str_replace_all(., " |\\*|-|/", "_"))) %>% 
  rename(seperation_date = separation___date) %>% 
  select(-zz_filler_1, 
         -zz_filler_2) %>% 
  mutate(across(where(is.character),
                ~str_trim(str_squish(str_to_lower(.))))) %>% 
  mutate(across(c(hire_date,
                  zz_extract_date,
                  seperation_date), 
                ~ymd(.))) %>% 
  distinct()


# name parsing 
suffixes <- "\\ssr$|\\sjr$|\\sii$|\\siii$|\\siv$|\\sv$|\\siiii$"

id_names <- id_df_clean %>% 
  mutate(employee_name = str_trim(str_remove(employee_name,
                                    "\\.$"))) %>% 
  mutate(last_name = str_extract(employee_name,
                                 "^.*(?=,)")) %>% 
  mutate(middle_initial = case_when(
    str_detect(employee_name,
               ", \\w{1} ") ~ str_extract(employee_name, 
                                          ", \\w{1} "),
    str_detect(employee_name, 
               "\\s\\w{1}$") ~ str_extract(employee_name, 
                                          "\\s\\w{1}$"), 
    TRUE ~ NA_character_
  )) %>% 
  mutate(first_name = case_when(
    str_detect(employee_name,
               ", \\w{1} ") ~ str_extract(employee_name, 
                                          " [:graph:]*$"),
    is.na(middle_initial) ~ str_remove(employee_name, 
                                       last_name),
    TRUE ~ str_extract(employee_name,
                       ", [:graph:]* ")
  )) %>% 
  mutate(across(c(first_name, 
                  middle_initial, 
                  last_name),
                ~str_squish(str_remove_all(.,
                                           "[:punct:]")))) %>% 
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


# filter out people who did not work a full month
id_drop <- id_names %>% 
  filter(months_at_agency > 0)


# The ID file is unique -- it is not set up with snapshots, but instead 
#   shows the time spans that someone worked in a given row. There are a lot of
#   instances of someone with >1 row (likely due to a job change of some sort).
id_drop %>% 
  group_by(employee_name) %>% 
  filter(n() > 1)

# Therefore, I am going to run this through fastlink to assign ids to the names
#   Then, I am going to do something like: 
#boop <- id_fastlink_out %>% 
#  group_by(dedupe.ids) %>% 
#  complete(hire_date = seq(min(hire_date), max(hire_date), by = "1 month")) %>% 
#  ungroup()


# going to make my life easier with deduping by droping the college and universtiy
#   employees here. 

id_no_higher_ed <- id_drop %>% 
  filter(!agency_name %in% c("university of idaho",
                             "lewis-clark state college",
                             "boise state university",
                             "eastern idaho technical college",
                             "idaho state university"))

id_fastlink <- id_no_higher_ed %>% 
  mutate(last_name_join = case_when(
    is.na(suffix) ~ last_name, 
    TRUE ~ str_c(last_name, 
                 " ",
                 suffix)
  )) %>% 
  select(first_name, 
         middle_initial, 
         last_name, 
         suffix, 
         everything())

saveRDS(id_fastlink, 
        "../data/state-employee-data/ID/clean/id-fastlink-pre.rds")




