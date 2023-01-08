library(tidyverse)
library(lubridate)

# read in data outputted from the code that prob assigns ids 
nd_out <- readRDS("../data/state-employee-data/ND/clean/nd-fastlink-post.rds")

nd_out_fl_list <- nd_out[[1]]

nd_out_matches_df <- nd_out[[2]] %>% 
  select(year, 
         dedupe.ids,
         everything()) %>% 
  distinct()

# Need to remove dups so unique by person-period
remaining_dups <- nd_out_matches_df %>% 
  group_by(year, 
           dedupe.ids) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_0 <- nd_out_matches_df %>% 
  filter(!dedupe.ids %in% remaining_dups)
dupes_0 <- nd_out_matches_df %>% 
  filter(dedupe.ids %in% remaining_dups)
nrow(dupes_0) / nrow(nd_out_matches_df) * 100


# for remaining duplicates, append last name to id
test_1 <- dupes_0 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(last_name))) 

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
nrow(dupes_1) / nrow(nd_out_matches_df) * 100

# for remaining duplicates, append first name to id
test_1.2 <- dupes_1 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(first_name))) 

test_1.2_dup_ids <- test_1.2 %>% 
  group_by(dedupe.ids,
           year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_1.2 <- test_1.2 %>% 
  filter(!dedupe.ids %in% test_1.2_dup_ids)

dupes_1.2 <- test_1.2 %>% 
  filter(dedupe.ids %in% test_1.2_dup_ids)
nrow(dupes_1.2) / nrow(nd_out_matches_df) * 100


# fold in temp job salaries into main position salary
test_2 <- dupes_1.2 %>% 
  group_by(dedupe.ids, 
           year) %>% 
  mutate(combine_row = case_when(
    n() > 1 & sum(fte) < 1.25 ~ 1,
    TRUE ~ 0
  )) %>% 
  mutate(max_row = case_when(
    combine_row == 1 & annual == max(annual) ~ 1,
    combine_row == 1 & annual != max(annual) ~ 0,
    TRUE ~ 1
  )) %>% 
  mutate(annual = case_when(
    combine_row == 1 ~ sum(annual),
    TRUE ~ annual
  )) %>%
  ungroup() %>% 
  filter(max_row == 1) %>% 
  select(-max_row) %>% 
  rename(salary_contains_temp_job = combine_row)

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
nrow(dupes_2) / nrow(nd_out_matches_df) * 100



# very few left -- dropping remaining instances of temp positions
test_3 <- dupes_2 %>% 
  mutate(temp_indic = case_when(
    str_detect(job_code_description, "temp") ~ 1, 
    TRUE ~ 0
  )) %>% 
  group_by(dedupe.ids,
           year) %>%
  mutate(drop_row = case_when(
    n() > 1 & temp_indic == 1 ~ 1,
    TRUE ~ 0
  )) %>% 
  ungroup() %>% 
  filter(drop_row == 0) %>% 
  select(-drop_row) %>% 
  select(-temp_indic)

test_3 <- test_3 %>% 
  filter(!(first_name == "shelly" & 
           last_name == "jones" & 
           job_code_description == "custodian"))

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
nrow(dupes_3) / nrow(nd_out_matches_df) * 100


unique_0$dedupe.ids <- as.character(unique_0$dedupe.ids)
# arrange and save
nd_final <- bind_rows(unique_0,
                      unique_1,
                      unique_1.2,
                      unique_2,
                      unique_3)

saveRDS(nd_final, 
        "../data/state-employee-data/ND/clean/nd-10-22-premerge.rds")





