library(tidyverse)
library(lubridate)

# read in data outputted from the code that prob assigns ids 
sc_out <- readRDS("../data/state-employee-data/SC/clean/sc-fastlink-post.rds")

sc_out_matches_list <- map(1:length(sc_out), 
                           ~pluck(sc_out, 
                                  ., 
                                  2))
sc_out_fl_list <- map(1:length(sc_out), 
                      ~pluck(sc_out, 
                             ., 
                             1))

sc_out_matches_df <- sc_out_matches_list %>% 
  bind_rows() %>% 
  select(month_year, 
         dedupe.ids,
         everything()) %>% 
  distinct()


# Need to remove dups so unique by person-period
dups_ids <- sc_out_matches_df %>% 
  group_by(month_year,
           dedupe.ids) %>% 
  filter(n() > 1) %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_0 <- sc_out_matches_df %>% 
  filter(!dedupe.ids %in% dups_ids)

dups_0 <- sc_out_matches_df %>% 
  filter(dedupe.ids %in% dups_ids)

nrow(dups_0) / nrow(sc_out_matches_df) * 100


# for remaining duplicates, append last name to id
test_1 <- dups_0 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            last_name))

test_1_dup_ids <- test_1 %>% 
  group_by(dedupe.ids,
           month_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_1 <- test_1 %>% 
  filter(!dedupe.ids %in% test_1_dup_ids)

dups_1 <- test_1 %>% 
  filter(dedupe.ids %in% test_1_dup_ids)
nrow(dups_1) / nrow(sc_out_matches_df) * 100


# for remaining duplicates, append first name to id
test_1.1 <- dups_1 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            first_name))

test_1.1_dup_ids <- test_1.1 %>% 
  group_by(dedupe.ids,
           month_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_1.1 <- test_1.1 %>% 
  filter(!dedupe.ids %in% test_1.1_dup_ids)

dups_1.1 <- test_1.1 %>% 
  filter(dedupe.ids %in% test_1.1_dup_ids)
nrow(dups_1.1) / nrow(sc_out_matches_df) * 100


# for remaining duplicates, append middle name to id
test_2 <- dups_1.1 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(middle_name)))

test_2_dup_ids <- test_2 %>% 
  group_by(dedupe.ids,
           month_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_2 <- test_2 %>% 
  filter(!dedupe.ids %in% test_2_dup_ids)

dups_2 <- test_2 %>% 
  filter(dedupe.ids %in% test_2_dup_ids)
nrow(dups_2) / nrow(sc_out_matches_df) * 100

# for remaining duplicates, append suffix to id
test_3 <- dups_2 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(suffix)))

test_3_dup_ids <- test_3 %>% 
  group_by(dedupe.ids,
           month_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_3 <- test_3 %>% 
  filter(!dedupe.ids %in% test_3_dup_ids)

dups_3 <- test_3 %>% 
  filter(dedupe.ids %in% test_3_dup_ids)
nrow(dups_3) / nrow(sc_out_matches_df) * 100

## for remaining duplicates, append indicator for max salary if n == 2
#test_4 <- dups_3 %>% 
#  group_by(dedupe.ids, month_year) %>% 
#  mutate(n_obs = n()) %>% 
#  mutate(max_period_id = max(annual_salary)) %>% 
#  ungroup() %>% 
#  mutate(dedupe.ids = case_when(
#    n_obs == 2 & annual_salary == max_period_id ~ str_c(dedupe.ids,
#                                                        "_",
#                                                        1),
#    n_obs == 2 & annual_salary != max_period_id ~ str_c(dedupe.ids,
#                                                        "_",
#                                                        0),
#    TRUE ~ dedupe.ids
#  )) %>% 
#  select(-n_obs, 
#         -max_period_id)
#           
#test_4_dup_ids <- test_4 %>% 
#  group_by(dedupe.ids,
#           month_year) %>% 
#  filter(n() > 1) %>% 
#  ungroup() %>% 
#  pull(dedupe.ids) %>% 
#  unique()
#
#unique_4 <- test_4 %>% 
#  filter(!dedupe.ids %in% test_4_dup_ids)
#
#dups_4 <- test_4 %>% 
#  filter(dedupe.ids %in% test_4_dup_ids)
#nrow(dups_4) / nrow(sc_out_matches_df) * 100


# if two ids are the same at this point but one job title is NA, then drop the 
#   one that is NA
dups_4 <- dups_3 %>% 
  group_by(dedupe.ids, month_year) %>% 
  fill(position_title, .direction = "downup") %>% 
  distinct()


# for remaining duplicates, append agency to id
test_5 <- dups_4 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            agency))

test_5_dup_ids <- test_5 %>% 
  group_by(dedupe.ids,
           month_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_5 <- test_5 %>% 
  filter(!dedupe.ids %in% test_5_dup_ids)

dups_5 <- test_5 %>% 
  filter(dedupe.ids %in% test_5_dup_ids)
nrow(dups_5) / nrow(sc_out_matches_df) * 100


# for remaining duplicates, append job title to id
test_6 <- dups_5 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(job_class_title)))

test_6_dup_ids <- test_6 %>% 
  group_by(dedupe.ids,
           month_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_6 <- test_6 %>% 
  filter(!dedupe.ids %in% test_6_dup_ids)

dups_6 <- test_6 %>% 
  filter(dedupe.ids %in% test_6_dup_ids)
nrow(dups_6) / nrow(sc_out_matches_df) * 100

# for remaining duplicates, append position title to id
test_7 <- dups_6 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(position_title)))

test_7_dup_ids <- test_7 %>% 
  group_by(dedupe.ids,
           month_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_7 <- test_7 %>% 
  filter(!dedupe.ids %in% test_7_dup_ids)

dups_7 <- test_7 %>% 
  filter(dedupe.ids %in% test_7_dup_ids)
nrow(dups_7) / nrow(sc_out_matches_df) * 100

# At this point, only three people left. Saying Chang and Nancy Smith are 
#   the same person but there are two Mathew Millers
unique_8 <- dups_7 %>% 
  filter(!(month_year == ymd("2022-01-01") & first_name == "eunjung")) %>% 
  filter(!(month_year == ymd("2022-02-01") & first_name == "eunjung")) %>% 
  filter(!(month_year == ymd("2020-02-01") & first_name == "nancy")) %>% 
  group_by(month_year, 
           dedupe.ids) %>%
  mutate(max_salary = max(annual_salary)) %>% 
  ungroup() %>% 
  mutate(dedupe.ids = case_when(
    annual_salary == max_salary ~ str_c(dedupe.ids, 1),
    TRUE ~ str_c(dedupe.ids, 0)
  )) %>% 
  select(-max_salary)

unique_8 %>% 
  group_by(month_year,
           dedupe.ids) %>% 
  summarise(dups = duplicated(dedupe.ids)) %>% 
  pull(dups) %>% 
  sum()


# save deduped file
out <- rbind(unique_0,
             unique_1,
             unique_1.1,
             unique_2,
             unique_3,
             #unique_4,
             unique_5,
             unique_6,
             unique_7,
             unique_8)


saveRDS(out, 
        "../data/state-employee-data/SC/clean/sc-19-22-premerge.rds")

