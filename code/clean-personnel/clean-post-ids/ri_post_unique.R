library(tidyverse)
library(lubridate)

# read in data outputted from the code that prob assigns ids 
ri_out <- readRDS("../data/state-employee-data/RI/clean/ri-fastlink-post.rds")

ri_out_fl_list <- ri_out[[1]]

ri_out_matches_df <- ri_out[[2]] %>% 
  select(fiscal_year, 
         dedupe.ids,
         everything()) %>% 
  distinct()

# Need to remove dups so unique by person-period
remaining_dups <- ri_out_matches_df %>% 
  group_by(fiscal_year, 
           dedupe.ids) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_0 <- ri_out_matches_df %>% 
  filter(!dedupe.ids %in% remaining_dups)
dupes_0 <- ri_out_matches_df %>% 
  filter(dedupe.ids %in% remaining_dups)
nrow(dupes_0) / nrow(ri_out_matches_df) * 100


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
nrow(dupes_1) / nrow(ri_out_matches_df) * 100


# for remaining duplicates, append first name to id
test_1.2 <- dupes_1 %>% 
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
nrow(dupes_1.2) / nrow(ri_out_matches_df) * 100


# for remaining duplicates, append middle initial to id
test_2 <- dupes_1.2 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(middle_initial))) 

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
nrow(dupes_2) / nrow(ri_out_matches_df) * 100

# if termination date or job title is missing within an id fill it
test_2.1 <- dupes_2 %>% 
  group_by(dedupe.ids, 
           fiscal_year) %>% 
  fill(title,
       termination_date,
       .direction = "downup") %>% 
  ungroup() %>% 
  distinct()

test_2.1_dup_ids <- test_2.1 %>% 
  group_by(dedupe.ids,
           fiscal_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_2.1 <- test_2.1 %>% 
  filter(!dedupe.ids %in% test_2.1_dup_ids)

dupes_2.1 <- test_2.1 %>% 
  filter(dedupe.ids %in% test_2.1_dup_ids)
nrow(dupes_2.1) / nrow(ri_out_matches_df) * 100


# for remaining duplicates, append agency name to id
test_3 <- dupes_2.1 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(department))) 

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
nrow(dupes_3) / nrow(ri_out_matches_df) * 100


# if titles are the same, sum salary info and take one row
test_4 <- dupes_3 %>% 
  group_by(fiscal_year,
           dedupe.ids) %>% 
  mutate(same = case_when(
    n_distinct(title) == 1 & n() > 1 ~ 1,
    TRUE ~ 0
  )) %>% 
  mutate(across(regular:annual,
                ~case_when(
                  same == 1 ~ sum(as.numeric(.), 
                                  na.rm = T), 
                  TRUE ~ as.numeric(.))
                )
         ) %>% 
  ungroup() %>% 
  distinct() %>% 
  select(-same)

test_4_dup_ids <- test_4 %>% 
  group_by(dedupe.ids,
           fiscal_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_4 <- test_4 %>% 
  filter(!dedupe.ids %in% test_4_dup_ids)

dupes_4 <- test_4 %>% 
  filter(dedupe.ids %in% test_4_dup_ids)
nrow(dupes_4) / nrow(ri_out_matches_df) * 100


# handful left -- sum salaries and take max row
test_5 <- dupes_4 %>% 
  group_by(fiscal_year,
           dedupe.ids) %>% 
  mutate(max_row = case_when(
    regular == max(regular) ~ T,
    TRUE ~ F
  )) %>% 
  mutate(across(regular:annual,
                ~sum(as.numeric(.), 
                        na.rm = T))) %>%
  ungroup() %>% 
  filter(max_row) %>% 
  select(-max_row) %>% 
  filter(!(dedupe.ids == "3351_lovesky_benjamin_NA_dem" & 
           fiscal_year == 2019 & 
           termination_date == ymd("2018-01-31"))) %>% 
  filter(!(dedupe.ids == "20939_cornicelli_t_NA_board of elections" & 
             fiscal_year == 2017 & 
             termination_date == ymd("2016-09-13"))) %>% 
  distinct()
  
test_5_dup_ids <- test_5 %>% 
  group_by(dedupe.ids,
           fiscal_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_5 <- test_5 %>% 
  filter(!dedupe.ids %in% test_5_dup_ids)

dupes_5 <- test_5 %>% 
  filter(dedupe.ids %in% test_5_dup_ids)
nrow(dupes_5) / nrow(ri_out_matches_df) * 100  


ri_final <- rbind(unique_0,
                  unique_1,
                  unique_1.2,
                  unique_2,
                  unique_2.1,
                  unique_3,
                  unique_4,
                  unique_5)

saveRDS(ri_final, 
        "../data/state-employee-data/RI/clean/ri-11-22-premerge.rds")




