library(tidyverse)
library(lubridate)

# read in data outputted from the code that prob assigns ids 
md_out <- readRDS("../data/state-employee-data/MD/clean/md-fastlink-post.rds")

md_out_fl_list <- md_out[[1]]

md_out_matches_df <- md_out[[2]] %>% 
  select(year, 
         dedupe.ids,
         everything()) %>% 
  distinct()

# Need to remove dups so unique by person-period
remaining_dups <- md_out_matches_df %>% 
  group_by(year, 
           dedupe.ids) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_0 <- md_out_matches_df %>% 
  filter(!dedupe.ids %in% remaining_dups)
dupes_0 <- md_out_matches_df %>% 
  filter(dedupe.ids %in% remaining_dups)
nrow(dupes_0) / nrow(md_out_matches_df) * 100
