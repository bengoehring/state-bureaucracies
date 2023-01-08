library(tidyverse)
library(lubridate)

# read in data outputted from the code that prob assigns ids 
nv_out <- readRDS("../data/state-employee-data/NV/clean/nv-fastlink-post.rds")

nv_out_fl_list <- nv_out[[1]]

nv_out_matches_df <- nv_out[[2]] %>% 
  select(cal_yr, 
         dedupe.ids,
         everything()) %>% 
  distinct()


# Need to remove dups so unique by person-period
dups_ids <- nv_out_matches_df %>% 
  group_by(cal_yr,
           dedupe.ids) %>% 
  filter(n() > 1) %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_0 <- nv_out_matches_df %>% 
  filter(!dedupe.ids %in% dups_ids)

dups_0 <- nv_out_matches_df %>% 
  filter(dedupe.ids %in% dups_ids)

nrow(dups_0) / nrow(nv_out_matches_df) * 100

# for remaining dups, assign different ids based on middle initial
test_1 <- dups_0 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids, 
                            "_",
                            str_replace_na(middle_initial)))

test_1_dup_ids <- test_1 %>% 
  group_by(dedupe.ids,
           cal_yr) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_1 <- test_1 %>% 
  filter(!dedupe.ids %in% test_1_dup_ids)

dups_1 <- test_1 %>% 
  filter(dedupe.ids %in% test_1_dup_ids)
nrow(dups_1) / nrow(nv_out_matches_df) * 100


# for remaining dups, assign different ids based on last name
test_2 <- dups_1 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids, 
                            "_",
                            str_replace_na(last_name)))

test_2_dup_ids <- test_2 %>% 
  group_by(dedupe.ids,
           cal_yr) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_2 <- test_2 %>% 
  filter(!dedupe.ids %in% test_2_dup_ids)

dups_2 <- test_2 %>% 
  filter(dedupe.ids %in% test_2_dup_ids)
nrow(dups_2) / nrow(nv_out_matches_df) * 100


# for remaining dups, assign different ids based on first name
test_3 <- dups_2 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids, 
                            "_",
                            str_replace_na(first_name)))

test_3_dup_ids <- test_3 %>% 
  group_by(dedupe.ids,
           cal_yr) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_3 <- test_3 %>% 
  filter(!dedupe.ids %in% test_3_dup_ids)

dups_3 <- test_3 %>% 
  filter(dedupe.ids %in% test_3_dup_ids)
nrow(dups_3) / nrow(nv_out_matches_df) * 100


# for remaining dups, assign different ids based on general title category
test_4 <- dups_3 %>% 
  mutate(dedupe.ids = case_when(
    str_detect(title, "\\.") & 
      !str_detect(title, "\\w") ~ str_c(dedupe.ids, 
                                        "_",
                                        str_extract(title, 
                                                    "^.*\\.")),
    str_detect(title, "^\\w") ~ str_c(dedupe.ids, 
                                      "_",
                                      str_sub(title, end = 2)),
    TRUE ~ str_c(dedupe.ids, 
                 "_",
                 title)
  ))

test_4_dup_ids <- test_4 %>% 
  group_by(dedupe.ids,
           cal_yr) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_4 <- test_4 %>% 
  filter(!dedupe.ids %in% test_4_dup_ids)

dups_4 <- test_4 %>% 
  filter(dedupe.ids %in% test_4_dup_ids)
nrow(dups_4) / nrow(nv_out_matches_df) * 100


# for remaining dups, assign different ids based on agency name 
test_5 <- dups_4 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids, 
                            "_",
                            str_replace_na(agency_name)))

test_5_dup_ids <- test_5 %>% 
  group_by(dedupe.ids,
           cal_yr) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_5 <- test_5 %>% 
  filter(!dedupe.ids %in% test_5_dup_ids)

dups_5 <- test_5 %>% 
  filter(dedupe.ids %in% test_5_dup_ids)
nrow(dups_5) / nrow(nv_out_matches_df) * 100


# just a handful of ids left 
test_6 <- dups_5 %>% 
  filter(!(cal_yr == 2019 & first_name == 'kevin' & hire_date_parse == ymd("2018-12-17"))) %>% 
  filter(!(cal_yr == 2019 & first_name == 'brandon' & total == 248.18)) %>% 
  filter(!(cal_yr == 2015 & first_name == 'juan' & total == 19329.79)) %>% 
  mutate(dedupe.ids = case_when(
    cal_yr == 2019 & empl_id == 70813 ~ str_c(dedupe.ids, 1),
    TRUE ~ dedupe.ids
  )) %>% 
  group_by(dedupe.ids, 
           cal_yr) %>% 
  mutate(max_salary = max(total)) %>% 
  ungroup() %>% 
  mutate(dedupe.ids = case_when(
    dedupe.ids == "12306_r_thompson_devlin_13_department of corrections" & 
      empl_id == 32745 ~ str_c(dedupe.ids, 
                               "_", 
                               1),
    dedupe.ids == "12306_r_thompson_devlin_13_department of corrections" & 
      empl_id == 58408 ~ str_c(dedupe.ids, 
                               "_", 
                               0),
    dedupe.ids == "12306_r_thompson_devlin_13_department of corrections" & 
      total == max_salary ~ str_c(dedupe.ids, 
                                  "_", 
                                  1),
    dedupe.ids == "12306_r_thompson_devlin_13_department of corrections" & 
      total != max_salary ~ str_c(dedupe.ids, 
                                  "_", 
                                  0),
    TRUE ~ dedupe.ids
  )) %>% 
  mutate(dedupe.ids = case_when(
    dedupe.ids == "10770_NA_miller_ross_13_highway patrol" & 
      empl_id == 78358 ~ str_c(dedupe.ids, 
                               "_",
                               1), 
    TRUE ~ dedupe.ids
  )) %>% 
  select(-max_salary) %>% 
  mutate(dedupe.ids = case_when(
    dedupe.ids == "6279_l_garcia_maria_12_employment security division" & 
      (str_sub(title_description, -1) %in% c("3", "4", "5") 
       | (cal_yr == 2016 & 
            str_sub(title_description, -1) == "1")) ~ str_c(dedupe.ids, "_", 1),
    dedupe.ids == "35300_NA_price_dennis_9._department of transportation" & 
      str_detect(title_description, 
                 "worker") ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "26209_v_jackson_ren_9._department of transportation" & 
      str_detect(title_description, 
                 "worker") ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "13704_NA_rose_anthony_13_department of corrections" & 
      cal_yr %in% c(2015, 2016) ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "13704_NA_rose_anthony_13_department of corrections" & 
      cal_yr %in% c(2018, 2019, 2020, 2021) ~ str_c(dedupe.ids, "_0"),
    dedupe.ids == "13704_NA_rose_anthony_13_department of corrections" & 
      cal_yr == 2017 & !str_detect(title_description, 
                                  "trainee") ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "13704_NA_rose_anthony_13_department of corrections" & 
      cal_yr == 2017 & str_detect(title_description, 
                                   "trainee") ~ str_c(dedupe.ids, "_0"),
    dedupe.ids == "1357_d_straughter_andre_10_aging and disability services" &
      title_description == "developmental support tech 3" ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "12946_a_betterly_terrill_13_department of corrections" & 
      hire_date_parse == ymd("2020-04-06") ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "12367_r_hill_perry_13_department of corrections" & 
      str_detect(title_description, 
                 "train") ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "12111_k_erekson_jackie_13_department of corrections" & 
      hire_date_parse == ymd("2001-06-04") ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "12111_k_erekson_jackie_13_department of corrections" & 
      hire_date_parse == ymd("2014-09-29") ~ str_c(dedupe.ids, "_0"),
    dedupe.ids == "12111_k_erekson_jackie_13_department of corrections" & 
      cal_yr == 2017 & 
      pay_rate == 27.21 ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "12111_k_erekson_jackie_13_department of corrections" & 
      cal_yr == 2017 & 
      pay_rate == 20.02 ~ str_c(dedupe.ids, "_0"),
    dedupe.ids == "12111_k_erekson_jackie_13_department of corrections" &
      title_description == "correctional officer" ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "12111_k_erekson_jackie_13_department of corrections" ~ str_c(dedupe.ids, "_0"),
    dedupe.ids == "42894_NA_ramirez_alfredo_9._department of transportation" & 
      empl_id == 80575 ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "28877_e_wimer_robert_6._environmental protection" ~ str_c(dedupe.ids,
                                                                             str_sub(title_description, 
                                                                                     end = 1)),
    dedupe.ids == "20063_r_johnson_timothy_9._department of transportation" & 
      empl_id == 71927 ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "42992_j_banks_gregory_9._department of transportation" & 
      empl_id == 65607 ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "42993_d_smith_kenneth_9._department of transportation" ~ str_c(dedupe.ids,
                                                                                  str_sub(title_description, 
                                                                                          end = 7)),
    dedupe.ids == "43761_NA_salas_martin_9._department of transportation" ~ str_c(dedupe.ids,
                                                                                  str_sub(title_description, 
                                                                                          end = 1)),
    TRUE ~ dedupe.ids
  )) %>% 
  filter(!(dedupe.ids == "32100_NA_ramirez_marcelino_7._department of transportation" &
             total == .61))

test_6_dup_ids <- test_6 %>% 
  group_by(dedupe.ids,
           cal_yr) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_6 <- test_6 %>% 
  filter(!dedupe.ids %in% test_6_dup_ids)

dups_6 <- test_6 %>% 
  filter(dedupe.ids %in% test_6_dup_ids)
nrow(dups_6) / nrow(nv_out_matches_df) * 100


out <- rbind(unique_0,
             unique_1,
             unique_2,
             unique_3,
             unique_4,
             unique_5,
             unique_6)

# check for any differing employee ids within each of the imputed ids. 
#   note that these employee ids are only available in a handful of years
boop <- out %>% 
  group_by(dedupe.ids) %>% 
  filter(!is.na(empl_id)) %>% 
  summarise(n = n_distinct(empl_id)) %>% 
  filter(n > 1)

out_2 <- out %>% 
  mutate(dedupe.ids = case_when(
    dedupe.ids == "17800" & empl_id == 65346 ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "21462_a" & empl_id == 69620 ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "29825" & cal_yr <= 2018 ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "30279_j" & empl_id == 69484 ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "37024" & empl_id == 72055 ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "40714_c_harris_brandon_13_department of corrections" & 
      empl_id == "65914" ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "40968" & empl_id == 64236 ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "32951" & empl_id == 52367 ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "36417_NA_villanueva_patricia_12" & 
      empl_id == 71596 ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "46172" & empl_id == "72067" ~ str_c(dedupe.ids, "_1"),
    dedupe.ids == "7120" & empl_id == "70027" ~ str_c(dedupe.ids, "_1"),
    TRUE ~ dedupe.ids
  ))

saveRDS(out_2, 
        "../data/state-employee-data/NV/clean/nv-10-21-premerge.rds")









