# wv_clean.R
# Description: This file reads in and cleans the WV personnel data. 
# Notes: 
##########################################################################
# preamble ####
library(tidyverse)
library(lubridate)
library(readxl)

wv_files <- list.files("../data/state-employee-data/WV/")
wv_paths <- str_c("../data/state-employee-data/WV/",
                  wv_files)

wv_raw_list <- map(wv_paths, 
                   ~read_csv(., 
                             skip = 2))

wv_raw_df <- wv_raw_list %>% 
  bind_rows()

colnames(wv_raw_df) <- tolower(str_replace_all(colnames(wv_raw_df),
                                               " ",
                                               "_"))

wv_clean_df <- wv_raw_df %>% 
  rename(agency = `department_/_agency`) %>% 
  mutate(across(where(is.character),
                tolower))

# clean up the names
suffixes <- "[:space:]sr$|[:space:]jr$|[:space:]ii$|[:space:]iii$|[:space:]iv$|[:space:]v$|[:space:]iiii$"

# add suffixes
wv_clean_names <- wv_clean_df %>% 
  mutate(across(first_name:last_name,
                ~str_trim(str_squish(str_remove_all(., "[:punct:]"))))) %>% 
  mutate(suffix = case_when(
    str_detect(first_name,
               suffixes) ~ str_trim(str_extract(first_name, suffixes)),
    str_detect(last_name,
               suffixes) ~ str_trim(str_extract(last_name, suffixes)),
    TRUE ~ NA_character_
  )) %>% 
  mutate(across(c(first_name, last_name),
                ~str_remove(., suffixes)))

# improve middle initial
wv_clean_names_2 <- wv_clean_names %>% 
  mutate(middle_initial = case_when(
    is.na(middle_initial) & str_detect(first_name, 
                                       "^\\w{1}[:space:]") ~ str_trim(str_extract(first_name, 
                                                                                  "^\\w{1}[:space:]")),
    is.na(middle_initial) & str_detect(first_name, 
                                       "[:space:]\\w{1}$") ~ str_trim(str_extract(first_name, 
                                                                                  "[:space:]\\w{1}$")),
    TRUE ~ middle_initial
  )) %>% 
  mutate(first_name = case_when(
    str_detect(first_name, 
               "^\\w{1}[:space:]") ~ str_remove(first_name, 
                                                "^\\w{1}[:space:]"),
    str_detect(first_name, 
               "[:space:]\\w{1}$") ~ str_remove(first_name, 
                                                "[:space:]\\w{1}$"),
    TRUE ~ first_name
  ))

# Some observations (~3k) have a space in the first name or the last name (~5k)
#     If middle initial is NA, assuming the latter name (in the first_name column) or
#     the former name in the last_name column contain the actual middle name.
wv_clean_names_3 <- wv_clean_names_2 %>% 
  mutate(middle_name = case_when(
    is.na(middle_initial) & str_detect(last_name, 
                                       "^\\w+[:space:]") ~ str_trim(str_extract(last_name,
                                                                                "^\\w+[:space:]")),
    is.na(middle_initial) & str_detect(first_name, 
                                       "[:space:]") ~ str_trim(str_extract(first_name,
                                                                           "[:space:]\\w+$")),
    TRUE ~ NA_character_
  )) %>% 
  mutate(first_name = case_when(
    str_detect(first_name, 
               "[:space:]") ~ str_remove(first_name, 
                                         "[:space:]\\w*$"),
    TRUE ~ first_name
  )) %>% 
  mutate(last_name = case_when(
    str_detect(last_name, 
               "[:space:]") ~ str_remove(last_name, 
                                         "^\\w+[:space:]"),
    TRUE ~ last_name
  )) %>% 
  mutate(middle_initial = case_when(
    !is.na(middle_name) ~ str_extract(middle_name,
                                      "^\\w{1}"),
    TRUE ~ middle_initial
  )) %>% 
  select(-middle_name)
  

#wv_clean_names_3 <- wv_clean_names_3 %>% 
#  mutate(across(c(first_name,
#                  middle_initial,
#                  last_name,
#                  suffix),
#                str_replace_na))




# turn into a list of dataframes by period for join
wv_list_clean <- split(wv_clean_names_3,
                       as.factor(wv_clean_names_3$year))

# this function runs through each dataframe in the given list and appends the 
#   period (in this case the year) to the non-key variable names. 
my_fun <- function(dataframe, 
                   grouping_col, 
                   grouping_col_value,
                   key_vars) {
  # drop the column by which the list is grouped
  dataframe_out <- dataframe[, -which(colnames(dataframe) == grouping_col)]
  
  # then add the name to the non-key variables
  non_key_names <- colnames(dataframe_out)[!colnames(dataframe_out) %in% key_vars]
  new_non_key_names <- str_c(non_key_names,
                             "_",
                             grouping_col_value)
  
  colnames(dataframe_out)[colnames(dataframe_out) %in% non_key_names] <- new_non_key_names
  
  
  return(dataframe_out)
}

wv_list_clean_2 <- map(1:length(wv_list_clean),
                      ~my_fun(wv_list_clean[[.]],
                              "year",
                              names(wv_list_clean)[.],
                              c("first_name",
                                "middle_initial",
                                "last_name",
                                "suffix")))
names(wv_list_clean_2) <- names(wv_list_clean)



boop <- fastLink(wv_list_clean_2[["2020"]],
                 wv_list_clean_2[["2021"]],
                 varnames = c("first_name",
                              "middle_initial",
                              "last_name",
                              "suffix"),
                 stringdist.match = c("first_name",
                                      "last_name"),
                 partial.match = c("first_name",
                                   "last_name"),
                 firstname.field = "first_name",
                 reweight.names = TRUE,
                 verbose = T)



booper <- getMatches(wv_list_clean_2[["2020"]],
                     wv_list_clean_2[["2021"]],
                     boop)
summary(boop)
table(wv_clean_names_3$middle_initial)

sum(is.na(wv_clean_names_3$last_name))


direct_match <- semi_join(wv_list_clean_2[["2020"]],
                          wv_list_clean_2[["2021"]], 
                          by = c("first_name",
                                 "middle_initial",
                                 "last_name",
                                 "suffix"))
anti_


wv_list_clean_2[["2020"]] %>% 
  mutate(id = str_c(str_replace_na(first_name),
                    str_replace_na(middle_initial),
                    str_replace_na(last_name),
                    str_replace_na(suffix),
                    str_replace_na(agency_2020))) %>% 
  filter(duplicated(id))
  group_by(first_name, middle_initial, last_name, suffix) %>% 
  summarise(dup = duplicated())

  
boop <- wv_list_clean_2[["2020"]]












