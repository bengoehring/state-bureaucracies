library(tidyverse)
library(lubridate)

# read in data outputted from the code that prob assigns ids 
va_out <- readRDS("../data/state-employee-data/VA/clean/va-fastlink-post.rds")

va_out_fl_list <- va_out[[1]]

va_out_matches_df <- va_out[[2]] %>% 
  select(data_source_date, 
         dedupe.ids,
         everything()) %>% 
  distinct()


# Need to remove dups so unique by person-period
dups_ids <- va_out_matches_df %>% 
  group_by(data_source_date,
           dedupe.ids) %>% 
  filter(n() > 1) %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_0 <- va_out_matches_df %>% 
  filter(!dedupe.ids %in% dups_ids)

dups_0 <- va_out_matches_df %>% 
  filter(dedupe.ids %in% dups_ids)

nrow(dups_0) / nrow(va_out_matches_df) * 100


# It appears that at least one sort of duplicates is from different missing
#   geo values across the pmis and cardinal personnel systems. Removing those
#   duplicates here: 
dups_1 <- dups_0 %>% 
  group_by(data_source_date, 
           agency_name, 
           first_name, 
           last_name, 
           suffix, 
           state_begin_date,
           position_number) %>% 
  mutate(n_rows = n()) %>% 
  ungroup() %>% 
  mutate(keep_row = case_when(
    n_rows == 1 ~ TRUE, 
    n_rows > 1 & is.na(agency_zip_code) ~ FALSE,
    n_rows > 1 & !is.na(agency_zip_code) ~ TRUE
  )) %>% 
  filter(keep_row) %>% 
  select(-n_rows, 
         -keep_row)

nrow(dups_1) / nrow(va_out_matches_df) * 100


# some false positives remaining due to different state begin dates but same 
#   state begin years
test_2 <- dups_1 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids, 
                            state_begin_date))

test_2_dup_ids <- test_2 %>% 
  group_by(dedupe.ids,
           data_source_date) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_2 <- test_2 %>% 
  filter(!dedupe.ids %in% test_2_dup_ids)

dups_2 <- test_2 %>% 
  filter(dedupe.ids %in% test_2_dup_ids)
nrow(dups_2) / nrow(va_out_matches_df) * 100
 

# for remaining ids, assign last names to ids
test_2.1 <- dups_2 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids, 
                            "_",
                            last_name))

test_2.1_dup_ids <- test_2.1 %>% 
  group_by(dedupe.ids,
           data_source_date) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_2.1 <- test_2.1 %>% 
  filter(!dedupe.ids %in% test_2.1_dup_ids)

dups_2.1 <- test_2.1 %>% 
  filter(dedupe.ids %in% test_2.1_dup_ids)
nrow(dups_2.1) / nrow(va_out_matches_df) * 100


# for remaining ids, assign first names to ids
test_2.2 <- dups_2.1 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids, 
                            "_",
                            first_name))

test_2.2_dup_ids <- test_2.2 %>% 
  group_by(dedupe.ids,
           data_source_date) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_2.2 <- test_2.2 %>% 
  filter(!dedupe.ids %in% test_2.2_dup_ids)

dups_2.2 <- test_2.2 %>% 
  filter(dedupe.ids %in% test_2.2_dup_ids)
nrow(dups_2.2) / nrow(va_out_matches_df) * 100


# for reamining dups, assigning new id based on first few characters of position 
test_3 <- dups_2.2 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids, 
                            str_sub(str_replace_na(employee_role_title), 
                                    end = 5)))

test_3_dup_ids <- test_3 %>% 
  group_by(dedupe.ids,
           data_source_date) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_3 <- test_3 %>% 
  filter(!dedupe.ids %in% test_3_dup_ids)

dups_3 <- test_3 %>% 
  filter(dedupe.ids %in% test_3_dup_ids)
nrow(dups_3) / nrow(va_out_matches_df) * 100


# dropping some by hand. These appear to be the same person who just shows
#   up more than once because they switched jobs or something like that. 

dups_3.1 <- dups_3 %>% 
  filter(!(data_source_date == ymd("2018-12-31") & agency_name == 'old dominion university' & last_name == "watsonpapelis")) %>% 
  filter(!(data_source_date == ymd("2022-03-31") & agency_name == 'dept of motor vehicles' & last_name == "davis")) %>% 
  filter(!(data_source_date == ymd("2022-06-30") & agency_name == 'virginia commonwealth univ' & last_name == "wayne")) %>% 
  filter(!(data_source_date == ymd("2021-12-31") & agency_name == 'old dominion university' & last_name == "fitzpatrick")) %>%   
  filter(!(data_source_date == ymd("2021-12-31") & agency_name == 'christopher newport university' & last_name == "kelly")) %>%   
  filter(!(data_source_date == ymd("2021-12-31") & agency_name == 'christopher newport university' & last_name == "allen")) %>% 
  filter(!(data_source_date == ymd("2022-06-30") & agency_name == 'va alcoholic bev control auth' & last_name == "enroughty")) %>% 
  filter(!(data_source_date == ymd("2022-03-31") & agency_name == 'dept of taxation' & last_name == "kazelskis")) %>% 
  group_by(data_source_date, 
           dedupe.ids) %>% 
  filter(n() > 1)
  
# at this point only two ids left
test_4 <- dups_3.1 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids, 
                            position_number)) %>% 
  mutate(dedupe.ids = str_replace(dedupe.ids, 
                                  "01653$",
                                  "03628$")) %>% 
  ungroup()

test_4_dup_ids <- test_4 %>% 
  group_by(dedupe.ids,
           data_source_date) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_4 <- test_4 %>% 
  filter(!dedupe.ids %in% test_4_dup_ids)

dups_4 <- test_4 %>% 
  filter(dedupe.ids %in% test_4_dup_ids)
nrow(dups_4) / nrow(va_out_matches_df) * 100



va_deduped_final <- rbind(unique_0,
                          unique_2,
                          unique_2.1,
                          unique_2.2,
                          unique_3,
                          unique_4)


# arrange columns and save
va_final <- va_deduped_final %>% 
  select(data_source_date,
         dedupe.ids,
         first_name,
         last_name,
         suffix,
         state_begin_date,
         -state_begin_year,
         everything())

saveRDS(va_final,
        "../data/state-employee-data/VA/clean/va-19-22-premerge.rds")



