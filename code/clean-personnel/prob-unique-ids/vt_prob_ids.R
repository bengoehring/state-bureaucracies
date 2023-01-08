# vt_probs_ids.R
# Description: This file runs fastlink() on the VT data (output from vt_clean.R)
#   It probabilisticly assigns unique identifiers. It is set up to run on the 
#   Great Lakes Cluster and will not run locally. 
# Notes: 
################################################################################

# preamable and data ####
library(tidyverse)
library(fastLink)

input_data <- readRDS("input-data/vt-fastlink-pre.rds")
source("code/assign_unique_ids_fun.R")


# Run fastLink, blocking on the year someone entered the state workforce.
fastlink_out <- link_by_block(dataframe = input_data,
                              blocking_object = NULL,
                              block_i = NULL,
                              blocking_variable = NULL, 
                              vars_to_join = c("first_name", 
                                               "middle_initial",
                                               "last_name_join"),
                              vars_to_string_dist_match = c("first_name", 
                                                            "last_name_join"),
                              blocking = FALSE)

saveRDS(fastlink_out, 
        "output-data/vt-fastlink-post.rds")









