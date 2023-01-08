# nv_probs_ids.R
# Description: This file runs fastlink() on the NV data (output from nv_clean.R)
#   It probabilisticly assigns unique identifiers. It is set up to run on the 
#   Great Lakes Cluster and will not run locally. 
# Notes: 
################################################################################

library(tidyverse)
library(fastLink)
library(furrr)
plan(multisession, workers = 36)

input_data <- readRDS("input-data/nv-fastlink-pre.rds")
source("code/assign_unique_ids_fun.R")

# drop unidentified employees who do not have a first or last name in the data
input_data <- input_data %>% 
  filter(!is.na(last_name))

fastlink_out <- link_by_block(dataframe = input_data,
                              blocking_object = NULL,
                              block_i = NULL,
                              blocking_variable = NULL, 
                              vars_to_join = c("first_name", 
                                               "last_name_join",
                                               "middle_initial"),
                              vars_to_string_dist_match = c("first_name",
                                                            "last_name_join"),
                              blocking = FALSE)


saveRDS(fastlink_out, 
        "output-data/nv-fastlink-post.rds")
