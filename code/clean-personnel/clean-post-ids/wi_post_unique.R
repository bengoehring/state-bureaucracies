library(tidyverse)
library(lubridate)

# read in data outputted from the code that prob assigns ids 
wi_out <- readRDS("../data/state-employee-data/WI/clean/wi-fastlink-post.rds")

wi_out_fl_list <- wi_out[[1]]

wi_out_matches_df <- wi_out[[2]] %>% 
  select(year, 
         dedupe.ids,
         everything()) %>% 
  distinct()

# Need to remove dups so unique by person-period
remaining_dups <- wi_out_matches_df %>% 
  group_by(year, 
           dedupe.ids) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_0 <- wi_out_matches_df %>% 
  filter(!dedupe.ids %in% remaining_dups)
dupes_0 <- wi_out_matches_df %>% 
  filter(dedupe.ids %in% remaining_dups)
nrow(dupes_0) / nrow(wi_out_matches_df) * 100

# for remaining duplicates, append middle initial to id
test_1 <- dupes_0 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(middle_initial)))

test_1_dup_ids <- test_1 %>% 
  group_by(dedupe.ids,
           year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_1 <- test_1 %>% 
  filter(!dedupe.ids %in% test_1_dup_ids)

dupes_1 <- test_1 %>% 
  filter(dedupe.ids %in% test_1_dup_ids)
nrow(dupes_1) / nrow(wi_out_matches_df) * 100

# Nearly all of the start date NA values are from before 2016. Therefore, appending
#   iff there are no NA values in the entire set for the dedupe.id value. 
wi_out_matches_df %>% 
  group_by(year) %>% 
  summarise(sum(is.na(start_date)) / n())

test_1.1 <- dupes_1 %>% 
  group_by(dedupe.ids) %>% 
  mutate(dedupe.ids = case_when(
    sum(is.na(start_date)) == 0 & 
      n_distinct(start_date, na.rm = T) > 1 &
      sum(duplicated(year)) > 0 ~ str_c(dedupe.ids,
                                        start_date_join),
    TRUE ~ dedupe.ids
  )) %>% 
  ungroup()

test_1.1_dup_ids <- test_1.1 %>% 
  group_by(dedupe.ids,
           year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_1.1 <- test_1.1 %>% 
  filter(!dedupe.ids %in% test_1.1_dup_ids)

dupes_1.1 <- test_1.1 %>% 
  filter(dedupe.ids %in% test_1.1_dup_ids)
nrow(dupes_1.1) / nrow(wi_out_matches_df) * 100



# for remaining duplicates, append first name to id
test_2 <- dupes_1.1 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(first_name)))

test_2_dup_ids <- test_2 %>% 
  group_by(dedupe.ids,
           year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_2 <- test_2 %>% 
  filter(!dedupe.ids %in% test_2_dup_ids)

dupes_2 <- test_2 %>% 
  filter(dedupe.ids %in% test_2_dup_ids)
nrow(dupes_2) / nrow(wi_out_matches_df) * 100


# for remaining duplicates, append last name to id
test_3 <- dupes_2 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(last_name_join)))

test_3_dup_ids <- test_3 %>% 
  group_by(dedupe.ids,
           year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_3 <- test_3 %>% 
  filter(!dedupe.ids %in% test_3_dup_ids)

dupes_3 <- test_3 %>% 
  filter(dedupe.ids %in% test_3_dup_ids)
nrow(dupes_3) / nrow(wi_out_matches_df) * 100

# per the raw files, the gross payroll is per employee per agency -- summing
#   up the cases of people swithcing across agencies and taking the max row. 

test_4 <- dupes_3 %>% 
  group_by(dedupe.ids,
           year) %>% 
  mutate(max_row = case_when(
    n_distinct(agency) > 1 & payroll_gross == max(payroll_gross) ~ TRUE, 
    n_distinct(agency) > 1 & payroll_gross != max(payroll_gross) ~ FALSE, 
    TRUE ~ TRUE
  )) %>% 
  mutate(payroll_gross = case_when(
    n_distinct(agency) > 1 ~ sum(payroll_gross),
    TRUE ~ payroll_gross
  )) %>% 
  ungroup() %>% 
  filter(max_row) %>% 
  select(-max_row)

test_4_dup_ids <- test_4 %>% 
  group_by(dedupe.ids,
           year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_4 <- test_4 %>% 
  filter(!dedupe.ids %in% test_4_dup_ids)

dupes_4 <- test_4 %>% 
  filter(dedupe.ids %in% test_4_dup_ids)
nrow(dupes_4) / nrow(wi_out_matches_df) * 100


# this is arbitrary but it seems highly unlikely that two people would be working
#   in the same agency, have the exact same name, and have started on the exact
#   same day. So, if the gross payroll amounts are different, summing and taking
#   max. 

test_5 <- dupes_4 %>% 
  group_by(dedupe.ids,
           year) %>% 
  mutate(max_row = case_when(
    n_distinct(payroll_gross) > 1 & payroll_gross == max(payroll_gross) ~ TRUE, 
    n_distinct(payroll_gross) > 1 & payroll_gross != max(payroll_gross) ~ FALSE, 
    TRUE ~ TRUE
  )) %>% 
  mutate(payroll_gross = case_when(
    n_distinct(payroll_gross) > 1 ~ sum(payroll_gross),
    TRUE ~ payroll_gross
  )) %>% 
  ungroup() %>% 
  filter(max_row) %>% 
  select(-max_row)

test_5_dup_ids <- test_5 %>% 
  group_by(dedupe.ids,
           year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_5 <- test_5 %>% 
  filter(!dedupe.ids %in% test_5_dup_ids)

dupes_5 <- test_5 %>% 
  filter(dedupe.ids %in% test_5_dup_ids)
nrow(dupes_5) / nrow(wi_out_matches_df) * 100



# just a few left
test_6 <- dupes_5 %>% 
  group_by(dedupe.ids,
           year) %>% 
  arrange(job_code_title,
          .by_group = T) %>% 
  slice(1) %>% 
  ungroup()

test_6_dup_ids <- test_6 %>% 
  group_by(dedupe.ids,
           year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_6 <- test_6 %>% 
  filter(!dedupe.ids %in% test_6_dup_ids)

dupes_6 <- test_6 %>% 
  filter(dedupe.ids %in% test_6_dup_ids)
nrow(dupes_6) / nrow(wi_out_matches_df) * 100



wi_final <- rbind(unique_0,
                  unique_1,
                  unique_1.1,
                  unique_2,
                  unique_3,
                  unique_4,
                  unique_5,
                  unique_6)

saveRDS(wi_final, 
        "../data/state-employee-data/WI/clean/wi-10-22-premerge.rds")





