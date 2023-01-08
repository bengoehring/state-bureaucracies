# fl_probs_ids.R
# Description: This file runs fastlink() on the FL data (output from ar_clean.R)
#   It probabilisticly assigns unique identifiers. It is set up to run on the 
#   Great Lakes Cluster and will not run locally. 
# Notes: 
################################################################################
options(warn = 1)
library(tidyverse)
library(fastLink)

chunk <- 10

input_data <- readRDS("input-data/fl-fastlink-pre_blocked.rds")

fl_prob_ids <- function(data){
  fl_out <- fastLink(dfA = data, 
                     dfB = data,
                     varnames = c("first_name", 
                                  "race_descr",
                                  "last_name_join"),
                     stringdist.match = c("first_name",
                                          "last_name_join"),
                     partial.match = c("first_name",
                                       "last_name_join"),
                     threshold.match = .90,
                     n.cores = 20)
  
  return(fl_out)
}

fl_out <- map(1:chunk, 
              ~fl_prob_ids(input_data[[.]]))

saveRDS(fl_out,
        str_c("output-data/fl-fastlink-post_",
              chunk,
              ".rds"))



## run fastlink on each blocked dataset
#fl_prob_ids <- function(data,
#                        block_i) {
#  
#  tryCatch(
#    {
#      fl_out <- fastLink(
#        dfA = data, 
#        dfB = data,
#        varnames = c("first_name", 
#                     "race_descr",
#                     "last_name_join"),
#        stringdist.match = c("first_name",
#                             "last_name_join"),
#        partial.match = c("first_name",
#                          "last_name_join"),
#        threshold.match = .90,
#        n.cores = 10)
#      
#      matches_out <- getMatches(
#        dfA = data, 
#        dfB = data,
#        fl.out = fl_out,
#        threshold.match = .90)
#      
#      return(matches_out)
#    },
#    warning = function(c) {
#      print(paste0("Warning for block ", 
#                   block_i))
#      print(conditionMessage(c))
#      
#      out <- data %>% 
#        mutate(dedupe.ids = NA_real_)
#      return(out)
#    },
#    error = function(c) {
#      print(paste0("Error for block ", 
#                   block_i))
#      print(conditionMessage(c))
#      
#      out <- data %>% 
#        mutate(dedupe.ids = NA_real_)
#      return(out)
#    }
#  )
#}
#
#fl_out <- map(1:length(input_data), 
#              ~fl_prob_ids(input_data[[.]],
#                           .))
#
#saveRDS(fl_out,
#        str_c("output-data/fl-fastlink-post_",
#              block_i,
#              ".rds"))

