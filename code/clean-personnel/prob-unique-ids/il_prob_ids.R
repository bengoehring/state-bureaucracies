# il_probs_ids.R
# Description: This file runs fastlink() on the IL data (output from il_clean.R)
#   It probabilisticly assigns unique identifiers. It is set up to run on the 
#   Great Lakes Cluster and will not run locally. 
# Notes: 
################################################################################

library(tidyverse)
library(fastLink)
library(furrr)
plan(multisession, workers = 36)

il_clean_df <- readRDS("input-data/il-fastlink-pre.rds")
source("code/assign_unique_ids_fun.R")

## Run fastLink, blocking by military veteran status

il_blocking <- blockData(il_clean_df, 
                         il_clean_df, 
                         varnames = c("military_veteran"))

all_dedupe_list <- future_map(names(il_blocking), 
                              ~link_by_block(dataframe = il_clean_df,
                                             blocking_object = il_blocking,
                                             block_i = .,
                                             blocking_variable = 'military_veteran', 
                                             vars_to_join = c("first_name", 
                                                              "last_name_join",
                                                              "csa"),
                                             vars_to_string_dist_match = c("first_name",
                                                                           "last_name_join")),
                              .options = furrr_options(seed = TRUE))

saveRDS(all_dedupe_list, 
        "output-data/il-fastlink-post.rds")