# id_probs_ids.R
# Description: This file runs fastlink() on the ID data (output from id_clean.R)
#   It probabilisticly assigns unique identifiers. It is set up to run on the 
#   Great Lakes Cluster and will not run locally. 
# Notes: 
################################################################################

# preamable and data ####
library(tidyverse)
library(fastLink)

input_data <- readRDS("input-data/id-fastlink-pre.rds")
source("code/assign_unique_ids_fun.R")

#input_data <- readRDS("../data/state-employee-data/ID/clean/id-fastlink-pre.rds")
#source("../code/clean-personnel/prob-unique-ids/assign_unique_ids_fun.R")


all_dedupe_list <- link_by_block(dataframe = input_data,
                                 blocking_object = NULL,
                                 block_i = NULL,
                                 blocking_variable = NULL, 
                                 vars_to_join = c("first_name", 
                                                  "middle_initial",
                                                  "last_name_join"),
                                 vars_to_string_dist_match = c("first_name",
                                                               "last_name_join"),
                                 n_cores = 36,
                                 blocking = FALSE)

saveRDS(all_dedupe_list, 
        "output-data/id-fastlink-post.rds")
