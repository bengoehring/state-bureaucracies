# ar_probs_ids.R
# Description: This file runs fastlink() on the AR data (output from ar_clean.R)
#   It probabilisticly assigns unique identifiers. It is set up to run on the 
#   Great Lakes Cluster and will not run locally. 
# Notes: 
################################################################################

library(tidyverse)
library(fastLink)
library(furrr)
plan(multisession, workers = 36)

ar_clean_df <- readRDS("input-data/ar-fastlink-pre.rds")
source("code/assign_unique_ids_fun.R")

#ar_clean_df <- readRDS("../data/state-employee-data/AR/clean/ar-fastlink-pre.rds")

ar_clean_df <- ar_clean_df %>% 
  mutate(sex_race_blocking = str_c(str_replace_na(gender),
                                   str_replace_na(ethnic_origin)))

## Run fastLink, blocking by gender and race
ar_blocking <- blockData(ar_clean_df, 
                         ar_clean_df, 
                         varnames = c("sex_race_blocking"))

all_dedupe_list <- map(names(ar_blocking), 
                       ~link_by_block(dataframe = ar_clean_df,
                                      blocking_object = ar_blocking,
                                      block_i = .,
                                      blocking_variable = 'sex_race_blocking', 
                                      vars_to_join = c("first_name", 
                                                       "last_name_join", 
                                                       "middle_initial"),
                                      vars_to_string_dist_match = c("first_name",
                                                                    "last_name_join"),
                                      n_cores = 36))

saveRDS(all_dedupe_list, 
        "output-data/ar-fastlink-post.rds")



