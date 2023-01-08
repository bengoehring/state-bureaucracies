# md_probs_ids.R
# Description: This file runs fastlink() on the MD data (output from nd_clean.R)
#   It probabilisticly assigns unique identifiers. It is set up to run on the 
#   Great Lakes Cluster and will not run locally. 
# Notes: 
################################################################################

# preamable and data ####
library(tidyverse)
library(fastLink)

input_data <- readRDS("input-data/md-fastlink-pre.rds")
source("code/assign_unique_ids_fun.R")

#input_data <- readRDS("../data/state-employee-data/MD/clean/nd-fastlink-pre.rds")
#source("../code/clean-personnel/prob-unique-ids/assign_unique_ids_fun.R")


all_dedupe_list <- link_by_block(dataframe = input_data,
                                 blocking_object = NULL,
                                 block_i = NULL,
                                 blocking_variable = NULL, 
                                 vars_to_join = c("first_name", 
                                                  "last_name_join"),
                                 vars_to_string_dist_match = c("first_name",
                                                               "last_name_join"),
                                 n_cores = 20,
                                 blocking = FALSE)

saveRDS(all_dedupe_list, 
        "output-data/md-fastlink-post.rds")



