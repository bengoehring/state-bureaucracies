# mn_probs_ids.R
# Description: This file runs fastlink() on the MN data (output from mn_clean.R)
#   It probabilisticly assigns unique identifiers. It is set up to run on the 
#   Great Lakes Cluster and will not run locally. 
# Notes: 
################################################################################

# preamable and data ####
library(tidyverse)
library(fastLink)

input_data <- readRDS("input-data/mn-fastlink-pre.rds")
source("code/assign_unique_ids_fun.R")

#input_data <- readRDS("../data/state-employee-data/MN/clean/mn-fastlink-pre.rds")
#source("../code/clean-personnel/prob-unique-ids/assign_unique_ids_fun.R")

input_data <- input_data %>% 
  mutate(original_hire_date_join = as.character(original_hire_date)) %>% 
  mutate(middle_initial = str_sub(middle_name, 
                                  1, 
                                  1)) %>% 
  mutate(last_name_join = case_when(
    is.na(last_name) ~ last_name,
    TRUE ~ str_c(last_name, 
                 " ",
                 suffix)))


all_dedupe_list <- link_by_block(dataframe = input_data,
                                 blocking_object = NULL,
                                 block_i = NULL,
                                 blocking_variable = NULL, 
                                 vars_to_join = c("first_name", 
                                                  "original_hire_date_join",
                                                  "middle_initial",
                                                  "last_name_join"),
                                 vars_to_string_dist_match = c("first_name",
                                                               "last_name_join"),
                                 n_cores = 36,
                                 blocking = FALSE)

saveRDS(all_dedupe_list, 
        "output-data/mn-fastlink-post.rds")






