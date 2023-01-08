# FL analysis is a pain. This file splits the dataset into a list of blocked
#   dataframes. Bocking is by first letter of first name, gender, and kmeans
#   on full first name. N blocks: 2 * 6 * 1000 == 12000
library(tidyverse)
library(fastLink)

input_data <- readRDS("input-data/fl-fastlink-pre.rds")
# need to reupload this next time you run it

input_data_letter <- input_data %>% 
  mutate(first_letter_block = case_when(
    str_extract(first_name,
                "^[:alpha:]{1}") %in% c("x", 'u', 'y', 'o', 
                                        'z', 'q', 'i', 'f') ~ 1,
    str_extract(first_name,
                "^[:alpha:]{1}") %in% c('h', 'v', 'n', 'w') ~ 2,
    str_extract(first_name,
                "^[:alpha:]{1}") %in% c('g', 'p') ~ 3,
    str_extract(first_name,
                "^[:alpha:]{1}") %in% c('e', 'b') ~ 4,
    str_extract(first_name,
                "^[:alpha:]{1}") == 'k' ~ 5,
    str_extract(first_name,
                "^[:alpha:]{1}") == 't' ~ 6,
    str_extract(first_name,
                "^[:alpha:]{1}") == 'l' ~ 7,
    str_extract(first_name,
                "^[:alpha:]{1}") == 'r' ~ 8,
    str_extract(first_name,
                "^[:alpha:]{1}") == 'a' ~ 9,
    str_extract(first_name,
                "^[:alpha:]{1}") == 'd' ~ 10,
    str_extract(first_name,
                "^[:alpha:]{1}") == 's' ~ 11,
    str_extract(first_name,
                "^[:alpha:]{1}") == 'c' ~ 12,
    str_extract(first_name,
                "^[:alpha:]{1}") == 'm' ~ 13,
    str_extract(first_name,
                "^[:alpha:]{1}") == 'j' ~ 14
  ))


# now block by gender, first letter of first name and first name similarity
blocked_data <- blockData(input_data, 
                          input_data, 
                          varnames = c("gender",
                                       "first_name"),
                          kmeans.block = 'first_name',
                          nclusters = 200,
                          n.cores = 5)

# subset input dataset using indices and save
blocked_data_data <- map(1:length(blocked_data),
                         ~input_data[blocked_data[[.]][[1]],])
names(blocked_data_data) <- names(blocked_data)

saveRDS(blocked_data_data,
        str_c("input-data/fl-fastlink-pre_blocked.rds"))




#block_fl <- function(data, 
#                     block_i) {
#  
#  data <- data %>% 
#    filter(first_letter_block == block_i)
#  
#  blocked_data <- blockData(data, 
#                            data, 
#                            varnames = c("gender",
#                                         "first_name"),
#                            kmeans.block = 'first_name',
#                            nclusters = 100,
#                            n.cores = 5)
#  
#  # subset input dataset using indices and save
#  blocked_data_data <- map(1:length(blocked_data),
#                           ~data[blocked_data[[.]][[1]],])
#  names(blocked_data_data) <- names(blocked_data)
#  
#  saveRDS(blocked_data_data,
#          str_c("input-data/fl-subsetted-data/fl-fastlink-pre_blocked_",
#                block_i,
#                ".rds"))
#}
#
#walk(1:max(input_data_letter$first_letter_block),
#     ~block_fl(input_data_letter,
#               .))









