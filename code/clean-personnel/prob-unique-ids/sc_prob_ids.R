# sc_probs_ids.R
# Description: This file runs fastlink() on the SC data (output from sc_clean.R)
#   It probabilisticly assigns unique identifiers. It is set up to run on the 
#   Great Lakes Cluster and will not run locally. 
# Notes: 
################################################################################

library(tidyverse)
library(fastLink)
library(furrr)
plan(multisession, workers = 36)

input_data <- readRDS("input-data/sc-fastlink-pre.rds")
source("code/assign_unique_ids_fun.R")


# dropping 293 obs where gender is unknown
input_data <- input_data %>% 
  filter(gender != 'unknown')

## Run fastLink, blocking by gender
blocked_data <- blockData(input_data, 
                          input_data, 
                          varnames = c("gender"))

all_dedupe_list <- future_map(names(blocked_data), 
                              ~link_by_block(dataframe = input_data,
                                             blocking_object = blocked_data,
                                             block_i = .,
                                             blocking_variable = 'gender', 
                                             vars_to_join = c("first_name", 
                                                              "last_name_join", 
                                                              "middle_name",
                                                              "ethnic_origin"),
                                             vars_to_string_dist_match = c("first_name",
                                                                           "last_name_join",
                                                                           "middle_name")),
                              .options = furrr_options(seed = TRUE))

saveRDS(all_dedupe_list, 
        "output-data/sc-fastlink-post.rds")


