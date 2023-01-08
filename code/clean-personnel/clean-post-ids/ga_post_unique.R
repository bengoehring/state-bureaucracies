library(tidyverse)
library(lubridate)

# read in data outputted from the code that prob assigns ids 
ga_out <- readRDS("../data/state-employee-data/GA/clean/ga-fastlink-post.rds")

ga_out_fl_list <- ga_out[[1]]

ga_out_matches_df <- ga_out[[2]] %>% 
  select(fiscal_year, 
         dedupe.ids,
         everything()) %>% 
  distinct()


# Need to remove dups so unique by person-period
remaining_dups <- ga_out_matches_df %>% 
  group_by(fiscal_year, 
           dedupe.ids) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_0 <- ga_out_matches_df %>% 
  filter(!dedupe.ids %in% remaining_dups)
dupes_0 <- ga_out_matches_df %>% 
  filter(dedupe.ids %in% remaining_dups)
nrow(dupes_0) / nrow(ga_out_matches_df) * 100


# for remaining duplicates, append middle initial to id
test_1 <- dupes_0 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(middle_name_initial)))

test_1_dup_ids <- test_1 %>% 
  group_by(dedupe.ids,
           fiscal_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_1 <- test_1 %>% 
  filter(!dedupe.ids %in% test_1_dup_ids)

dupes_1 <- test_1 %>% 
  filter(dedupe.ids %in% test_1_dup_ids)
nrow(dupes_1) / nrow(ga_out_matches_df) * 100


# for remaining duplicates, append last name to id
test_1.1 <- dupes_1 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(last_name))) 

test_1.1_dup_ids <- test_1.1 %>% 
  group_by(dedupe.ids,
           fiscal_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_1.1 <- test_1.1 %>% 
  filter(!dedupe.ids %in% test_1.1_dup_ids)

dupes_1.1 <- test_1.1 %>% 
  filter(dedupe.ids %in% test_1.1_dup_ids)
nrow(dupes_1.1) / nrow(ga_out_matches_df) * 100

# for remaining duplicates, append first name to id
test_1.2 <- dupes_1.1 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(first_name))) 

test_1.2_dup_ids <- test_1.2 %>% 
  group_by(dedupe.ids,
           fiscal_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_1.2 <- test_1.2 %>% 
  filter(!dedupe.ids %in% test_1.2_dup_ids)

dupes_1.2 <- test_1.2 %>% 
  filter(dedupe.ids %in% test_1.2_dup_ids)
nrow(dupes_1.2) / nrow(ga_out_matches_df) * 100



