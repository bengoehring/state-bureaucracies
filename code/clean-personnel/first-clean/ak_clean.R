# ak_clean.R
# Description: This file reads in and cleans the AK personnel data. 
# Notes: 
##########################################################################

library(tidyverse)
library(readxl)
library(lubridate)

# read in data ####

# when you get here, you do not have the classification status of 
#   all employees (before like 2019) -- use the classification and titles
#   of the more current employees to create crosswalk of classification
#   of various titles -- this might be helpful: https://www.governmentjobs.com/careers/Alaska/classspecs

ak_files <- list.files("../data/state-employee-data/AK/raw/excel/")
ak_paths <- str_c("../data/state-employee-data/AK/raw/excel/", 
                  ak_files)

ak_names <- str_remove(ak_files, 
                       "EX_Branch_EE_List_as_of_")
ak_names <- str_remove(ak_names, 
                       "-GOV_Angela_Hull.*|-Gov_Office_Angela_Hull|-Public_Records_Requests.*")
ak_names <- str_replace(ak_names, 
                        "2020$",
                        "20")

read_ak <- function(path) {
  out <- read_xlsx(path)
  colnames(out) <- tolower(colnames(out))
  colnames(out) <- str_replace_all(colnames(out),
                                   " ",
                                   "_")
  return(out)
}

ak_raw_list <- map(ak_paths, 
                   read_ak)
names(ak_raw_list) <- ak_names

# just trying most recent files first 
# It looks like the older stuff does not have civil service information, 
#   so leaving it for now. 
ak_19_21 <- ak_raw_list[map_lgl(ak_raw_list, 
                                ~length(colnames(.)) == 12)]

ak_19_21 <- bind_rows(ak_19_21,
                      .id = "data_date") %>% 
  mutate(across(where(is.character),
                ~str_trim(str_squish(tolower(.))))) %>% 
  mutate(data_date = mdy(data_date))













