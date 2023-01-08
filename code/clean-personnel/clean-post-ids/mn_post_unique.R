library(tidyverse)
library(lubridate)

# read in data outputted from the code that prob assigns ids 
mn_out <- readRDS("../data/state-employee-data/MN/clean/mn-fastlink-post.rds")

mn_out_fl_list <- mn_out[[1]]

mn_out_matches_df <- mn_out[[2]] %>% 
  select(fiscal_year, 
         dedupe.ids,
         everything()) %>% 
  distinct()


# Need to remove dups so unique by person-period
remaining_dups <- mn_out_matches_df %>% 
  group_by(fiscal_year, 
           dedupe.ids) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_0 <- mn_out_matches_df %>% 
  filter(!dedupe.ids %in% remaining_dups)
dupes_0 <- mn_out_matches_df %>% 
  filter(dedupe.ids %in% remaining_dups)
nrow(dupes_0) / nrow(mn_out_matches_df) * 100


# for remaining duplicates, append last name to id
test_1 <- dupes_0 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(last_name_join)))

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
nrow(dupes_1) / nrow(mn_out_matches_df) * 100



# for remaining duplicates, append middle name to id
test_2 <- dupes_1 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(middle_name)))

test_2_dup_ids <- test_2 %>% 
  group_by(dedupe.ids,
           fiscal_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_2 <- test_2 %>% 
  filter(!dedupe.ids %in% test_2_dup_ids)

dupes_2 <- test_2 %>% 
  filter(dedupe.ids %in% test_2_dup_ids)
nrow(dupes_2) / nrow(mn_out_matches_df) * 100


# for remaining duplicates, append temporary id to dynamic id
test_3 <- dupes_2 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(temporary_id)))

test_3_dup_ids <- test_3 %>% 
  group_by(dedupe.ids,
           fiscal_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_3 <- test_3 %>% 
  filter(!dedupe.ids %in% test_3_dup_ids)

dupes_3 <- test_3 %>% 
  filter(dedupe.ids %in% test_3_dup_ids)
nrow(dupes_3) / nrow(mn_out_matches_df) * 100



mn_final <- rbind(unique_0,
                  unique_1,
                  unique_2,
                  unique_3)


mn_final <- mn_final %>% 
  select(fiscal_year,
         dedupe.ids,
         first_name, 
         middle_name, 
         last_name, 
         suffix, 
         original_hire_date,
         everything())
saveRDS(mn_final, 
        "../data/state-employee-data/MN/clean/mn-11-21-premerge.rds")















