library(tidyverse)
library(lubridate)

# read in data outputted from the code that prob assigns 
ar_out <- readRDS("../data/state-employee-data/AR/clean/ar-fastlink-post.rds")

ar_out_matches_list <- map(1:length(ar_out), 
                      ~pluck(ar_out, ., 2))
ar_out_matches_df <- ar_out_matches_list %>% 
  bind_rows()

ar_out_fl_list <- map(1:length(ar_out), 
                           ~pluck(ar_out, ., 1))

ar_out_matches_df <- ar_out_matches_df %>% 
  mutate(month_year = my(month_year)) %>% 
  select(month_year, 
         dedupe.ids,
         everything())

# Need to remove dups so unique by person-period
remaining_dups <- ar_out_matches_df %>% 
  group_by(month_year, 
           dedupe.ids) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_0 <- ar_out_matches_df %>% 
  filter(!dedupe.ids %in% remaining_dups)
dupes_0 <- ar_out_matches_df %>% 
  filter(dedupe.ids %in% remaining_dups)
nrow(dupes_0) / nrow(ar_out_matches_df) * 100


# for remaining duplicates, append middle initial to id
test_1 <- dupes_0 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(middle_initial)))

test_1_dup_ids <- test_1 %>% 
  group_by(dedupe.ids,
           month_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_1 <- test_1 %>% 
  filter(!dedupe.ids %in% test_1_dup_ids)

dupes_1 <- test_1 %>% 
  filter(dedupe.ids %in% test_1_dup_ids)
nrow(dupes_1) / nrow(ar_out_matches_df) * 100


# for remaining duplicates, append last name to id
test_1.1 <- dupes_1 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(last_name))) 

test_1.1_dup_ids <- test_1.1 %>% 
  group_by(dedupe.ids,
           month_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_1.1 <- test_1.1 %>% 
  filter(!dedupe.ids %in% test_1.1_dup_ids)

dupes_1.1 <- test_1.1 %>% 
  filter(dedupe.ids %in% test_1.1_dup_ids)
nrow(dupes_1.1) / nrow(ar_out_matches_df) * 100

# for remaining duplicates, append first name to id
test_1.2 <- dupes_1.1 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(first_name))) 

test_1.2_dup_ids <- test_1.2 %>% 
  group_by(dedupe.ids,
           month_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_1.2 <- test_1.2 %>% 
  filter(!dedupe.ids %in% test_1.2_dup_ids)

dupes_1.2 <- test_1.2 %>% 
  filter(dedupe.ids %in% test_1.2_dup_ids)
nrow(dupes_1.2) / nrow(ar_out_matches_df) * 100


# for remaining duplicates, append agency to id
test_2 <- dupes_1.2 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            business_area_description))

test_2_dup_ids <- test_2 %>% 
  group_by(dedupe.ids,
           month_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_2 <- test_2 %>% 
  filter(!dedupe.ids %in% test_2_dup_ids)

dupes_2 <- test_2 %>% 
  filter(dedupe.ids %in% test_2_dup_ids)
nrow(dupes_2) / nrow(ar_out_matches_df) * 100


# for remaining duplicates, append further job info to id
test_3 <- dupes_2 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            personnel_area_description))

test_3_dup_ids <- test_3 %>% 
  group_by(dedupe.ids,
           month_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_3 <- test_3 %>% 
  filter(!dedupe.ids %in% test_3_dup_ids)

dupes_3 <- test_3 %>% 
  filter(dedupe.ids %in% test_3_dup_ids)
nrow(dupes_3) / nrow(ar_out_matches_df) * 100


# for remaining duplicates, append entry date to id
test_4 <- dupes_3 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            entry_date))

test_4_dup_ids <- test_4 %>% 
  group_by(dedupe.ids,
           month_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_4 <- test_4 %>% 
  filter(!dedupe.ids %in% test_4_dup_ids)

dupes_4 <- test_4 %>% 
  filter(dedupe.ids %in% test_4_dup_ids)
nrow(dupes_4) / nrow(ar_out_matches_df) * 100

# bind data back together
ar_all <- rbind(unique_0,
                unique_1,
                unique_1.1,
                unique_1.2,
                unique_2,
                unique_3,
                unique_4)

# adding in a couple of column codes that I forgot about. 
ar_all <- ar_all %>% 
  mutate(employment_status = case_when(
    employment_status == '3' ~ 'active employee',
    employment_status == '1' ~ 'lwop employee',
    employment_status == '0' ~ 'withdrawn employee'
  )) %>% 
  mutate(employee_group = case_when(
    employee_group == 1 ~ 'regular state employee',
    employee_group == 7 ~ 'extra help employee',
    TRUE ~ NA_character_
  ))


# arrange columns and save
# arrange columns and save ####
ar_final <- ar_all %>% 
  select(month_year, 
         dedupe.ids,
         first_name,
         middle_initial, 
         last_name, 
         suffix, 
         gender, 
         ethnic_origin,
         everything())

saveRDS(ar_final, 
        "../data/state-employee-data/AR/clean/ar-15-22-premerge.rds")



