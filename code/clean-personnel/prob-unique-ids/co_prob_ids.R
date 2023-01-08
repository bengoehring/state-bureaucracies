# co_probs_ids.R
# Description: This file runs fastlink() on the CO data (output from co_clean.R)
#   It probabilisticly assigns unique identifiers. It is set up to run on the 
#   Great Lakes Cluster and will not run locally. 
# Notes: 
################################################################################

# preamable and data ####
library(tidyverse)
library(fastLink)

input_data <- readRDS("input-data/co-fastlink-pre.rds")
source("code/assign_unique_ids_fun.R")

#input_data <- readRDS("../data/state-employee-data/CO/clean/co-fastlink-pre.rds")
#source("../code/clean-personnel/prob-unique-ids/assign_unique_ids_fun.R")

## Run fastLink, blocking by decade (roughly) someone began working for the state
blocked_data <- blockData(input_data, 
                          input_data, 
                          varnames = c("blocking_period"))

all_dedupe_list <- map(names(blocked_data),
                       ~link_by_block(dataframe = input_data,
                                      blocking_object = blocked_data,
                                      block_i = .,
                                      blocking_variable = "blocking_period", 
                                      vars_to_join = c("first_name", 
                                                       #"middle_initial", too many NA values in 2020 block
                                                       "last_name_join",
                                                       "original_hiredate_join"),
                                      vars_to_string_dist_match = c("first_name",
                                                                    "last_name_join"),
                                      n_cores = 36))
  
  

saveRDS(all_dedupe_list, 
        "output-data/co-fastlink-post.rds")
