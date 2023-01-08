# This file merges the AR correctional officer data with the AR voter file. 
#   Where the latter is restricted to only those voters within X km of a 
#   facility. 
# It is set up to run on the cluster

library(fastLink)
library(tidyverse)

# read in data and code ####
fastlink_input <- readRDS("input-data/ar-fastlink-input.rds")
source("code/merge-fun.R")

#fastlink_input <- readRDS("../data/correctional-officers/merge-inputs/ar-fastlink-input.rds")

voters_within_25km <- fastlink_input$voters_within_25km
voters_within_50km <- fastlink_input$voters_within_50km
voters_within_100km <- fastlink_input$voters_within_100km
ar_correctional_join <- fastlink_input$ar_correctional_join

# blocking objects ####
block_obj_25 <- blockData(ar_correctional_join, 
                          voters_within_25km, 
                          varnames = c("facility"))
block_obj_50 <- blockData(ar_correctional_join, 
                          voters_within_50km, 
                          varnames = c("facility"))
block_obj_100 <- blockData(ar_correctional_join, 
                           voters_within_100km, 
                           varnames = c("facility"))

# merges with 25km voter set ####
out_25_.9 <- map(names(block_obj_25), 
                 ~merge_by_block(ar_correctional_join,
                                 voters_within_25km,
                                 blocking_object = block_obj_25,
                                 block_i = .,
                                 vars_to_join = c("race",
                                                  "gender",
                                                  "first_name",
                                                  "middle_initial",
                                                  "last_name_join"),
                                 vars_to_string_dist_match = c("first_name",
                                                               "last_name_join"),
                                 threshold_level = .9,
                                 n_cores = 20)
)



# merges with 50km voter set ####
out_50_.9 <- map(names(block_obj_50), 
                 ~merge_by_block(ar_correctional_join,
                                 voters_within_50km,
                                 blocking_object = block_obj_50,
                                 block_i = .,
                                 vars_to_join = c("race",
                                                  "gender",
                                                  "first_name",
                                                  "middle_initial",
                                                  "last_name_join"),
                                 vars_to_string_dist_match = c("first_name",
                                                               "last_name_join"),
                                 threshold_level = .9,
                                 n_cores = 20)
)

# merges with 100km voter set ####
out_100_.9 <- map(names(block_obj_100), 
                  ~merge_by_block(ar_correctional_join,
                                  voters_within_100km,
                                  blocking_object = block_obj_100,
                                  block_i = .,
                                  vars_to_join = c("race",
                                                   "gender",
                                                   "first_name",
                                                   "middle_initial",
                                                   "last_name_join"),
                                  vars_to_string_dist_match = c("first_name",
                                                                "last_name_join"),
                                  threshold_level = .9,
                                  n_cores = 20)
)


fastlink_output <- list("out_25_.9" = list("fl_obj" = out_25_.9,
                                           "voter_data" = voters_within_25km),
                        "out_50_.9" = list("fl_obj" = out_50_.9,
                                           "voter_data" = voters_within_50km),
                        "out_100_.9" = list("fl_obj" = out_100_.9,
                                            "voter_data" = voters_within_100km),
                        "input_correctional_data" = ar_correctional_join)
saveRDS(fastlink_output, 
        "output-data/ar-fastlink-output.rds")


