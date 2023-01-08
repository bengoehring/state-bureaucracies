library(tidyverse)
library(lubridate)

# read in data outputted from the code that prob assigns ids 
id_out <- readRDS("../data/state-employee-data/ID/clean/id-fastlink-post.rds")

id_out_fl_list <- id_out[[1]]

id_out_matches_df <- id_out[[2]] %>% 
  select(dedupe.ids,
         everything()) %>% 
  distinct()


# I need to dedupe this file so it is unique by person period. But the added
#   wrinkle here is that I first need to expand the dataset so I have
#   the data by month-person rather than just person. 

# I am not grouping by dedupe.ids but rather by row id. This is because 
#   I want to allow for the potential of someone holding more than one job at once
#   or there being incorrectly assigned ids of different people under the same
#   id and working at the same time. 

# I am going to try this on aletered start and end dates. The start date is the first
#   day of the following month while the end date is the last day of the given month. 
#   This should help with mid-month job changes

id_complete <- id_out_matches_df %>% 
  mutate(row_id = row_number()) %>% 
  mutate(seperation_month = ceiling_date(seperation_date,
                                         "months") - period('1 day')) %>% 
  mutate(hire_month = floor_date(hire_date %m+% period("1 month"),
                                 "months")) %>% 
  mutate(month_year = hire_month) %>% 
  group_by(row_id) %>% 
  complete(month_year = seq.Date(min(hire_month),
                                 max(seperation_month),
                                 "month")) %>% 
  fill(everything()) %>% 
  mutate(test = min(month_year) == hire_month,
         test_2 = max(month_year) == floor_date(seperation_month, 
                                                "1 month")) %>% 
  ungroup() %>% 
  distinct()

id_complete %>% 
  filter(!test)
id_complete %>% 
  filter(!test_2)

id_complete <- id_complete %>% 
  select(-test,
         -test_2)

# OK NOW the data is set up like it has been in other states 
# Need to remove dups so unique by person-period
remaining_dups <- id_complete %>% 
  group_by(dedupe.ids,
           month_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_0 <- id_complete %>% 
  filter(!dedupe.ids %in% remaining_dups)
dupes_0 <- id_complete %>% 
  filter(dedupe.ids %in% remaining_dups)
nrow(dupes_0) / nrow(id_complete) * 100


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
nrow(dupes_1) / nrow(id_complete) * 100


# for remaining duplicates, append last name to id
test_2 <- dupes_1 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(last_name_join)))

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
nrow(dupes_2) / nrow(id_complete) * 100


# for remaining duplicates, append first name to id
test_3 <- dupes_2 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(first_name)))

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
nrow(dupes_3) / nrow(id_complete) * 100


# for remaining duplicates, append agency name to id
test_4 <- dupes_3 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(agency_name)))

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
nrow(dupes_4) / nrow(id_complete) * 100


# down to just 373 employees
length(unique(dupes_4$employee_name))


# For the remaining, going to take the one with the higher pay rate or full time
test_5 <- dupes_4 %>% 
  group_by(dedupe.ids, 
           month_year) %>% 
  mutate(keep = case_when(
    n() > 1 & pay_rate > max(pay_rate) ~ TRUE,
    n() > 1 & pay_rate < max(pay_rate) ~ FALSE,
    n() > 1 & full_time_part_time == "full-time" ~ TRUE,
    n() > 1 & full_time_part_time == "part-time" ~ FALSE,
    TRUE ~ TRUE
  )) %>% 
  filter(keep) %>% 
  select(-keep)

test_5_dup_ids <- test_5 %>% 
  group_by(dedupe.ids,
           month_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_5 <- test_5 %>% 
  filter(!dedupe.ids %in% test_5_dup_ids)

dupes_5 <- test_5 %>% 
  filter(dedupe.ids %in% test_5_dup_ids)
nrow(dupes_5) / nrow(id_complete) * 100




# bind and save data ####
id_all <- rbind(unique_0,
                unique_1,
                unique_2,
                unique_3,
                unique_4,
                unique_5)


id_final <- id_all %>% 
  select(-row_id,
         -seperation_month,
         -hire_month) %>% 
  select(month_year,
         dedupe.ids, 
         first_name, 
         middle_initial, 
         last_name, 
         last_name_join, 
         suffix, 
         everything())

saveRDS(id_final, 
        "../data/state-employee-data/ID/clean/id-99-22-premerge.rds")











