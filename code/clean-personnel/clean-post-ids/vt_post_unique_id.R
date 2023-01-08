# This file does some additional cleaning of the ids generated
#   by vt-prob-ids.R. 

library(tidyverse)

# read in data outputted from the code that prob assigns ids 
vt_out <- readRDS("../data/state-employee-data/VT/clean/vt-fastlink-post.rds")

vt_out_fl_list <- vt_out[[1]]

vt_out_matches_df <- vt_out[[2]] %>% 
  select(fiscal_year, 
         dedupe.ids,
         everything()) %>% 
  distinct()

vt_out_matches_df <- vt_out_matches_df %>% 
  mutate(department = case_when(
    department == "administration, agency of" ~ "administration agency",
    department == "agriculture, food & mrkts agency" ~ "agriculture, food & markets",
    department == "attorney general's office" ~ "attorney general",
    department == "auditor of accounts' office" ~ "auditor of accounts",
    department == "corrections, department of" ~ "corrections",
    department == "criminal justice trng council" ~ "criminal justice training coun",
    department == "commerce & community developme" ~ "commerce & community dev agency",
    department == "defender general's office" ~ "defender general",
    department == "children and families" ~ "dept for children & families",
    department == "finance & management, dept of" ~ "finance & management",
    department == "financial regulation, dept of" ~ "financial regulation",
    department == "fish & wildlife, dept of" ~ "fish & wildlife",
    department == "human resources" ~ "department of human resources",
    department == "human services, agency of" ~ "human services agency",
    department == "labor, dept of" ~ "labor",
    department == "lt governor" ~ "lieutenant governor's office",
    department == "military, dept of" ~ "military",
    department == "natural resources, agency of" ~ "natural resources agency",
    department == "public safety, dept of" ~ "public safety",
    department == "public service, dept of" ~ "public service department",
    department == "secretary of state's office" ~ "secretary of state",
    department == "state treasurer's office" ~ "state treasurer",
    department == "taxes, dept of" ~ "taxes",
    department == "transportation, agency of" ~ "transportation agency",
    department == "vermont health access, dept of" ~ "vermont health access",
    department == "veteran's home" ~ "vermont veterans' home",
    department == "veterans' home" ~ "vermont veterans' home",
    department == "state's attorneys, dept of" ~ "state's attorneys",
    department == "sergeant at arms' office" ~ "sgt-at-arms",
    department == "liquor control, dept of" ~ "liquor control",
    department == "libraries, dept of" ~ "libraries",
    department == "education, dept of" ~ "education agency",
    department == "education, agency of" ~ "education agency",
    department == "office of vt health access" ~ "vermont health access",
    department == "dept of financial regulation" ~ "financial regulation",
    department == "dept of financial regulation" ~ "financial regulation",
    department == "dept. of mental health" ~ "mental health",
    TRUE ~ department
  )) %>% 
  mutate(agency = case_when(
    agency == "agency of agriculture, food & mrkts" ~ "agency of agriculture",
    agency == "education, agency of" ~ "agency of education",
    agency == "judicial" ~ "judiciary",
    agency == "depts not affiliated w agency" ~ "independent",
    TRUE ~ agency
  ))

# Need to remove dups so unique by person-period
dups_ids <- vt_out_matches_df %>% 
  group_by(fiscal_year,
           dedupe.ids) %>% 
  filter(n() > 1) %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_0 <- vt_out_matches_df %>% 
  filter(!dedupe.ids %in% dups_ids)

dups_0 <- vt_out_matches_df %>% 
  filter(dedupe.ids %in% dups_ids)

nrow(dups_0) / nrow(vt_out_matches_df) * 100

# for remaining ids, assign last names to ids
test_1 <- dups_0 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids, 
                            "_",
                            last_name_join))

test_1_dup_ids <- test_1 %>% 
  group_by(dedupe.ids,
           fiscal_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_1 <- test_1 %>% 
  filter(!dedupe.ids %in% test_1_dup_ids)

dups_1 <- test_1 %>% 
  filter(dedupe.ids %in% test_1_dup_ids)
nrow(dups_1) / nrow(vt_out_matches_df) * 100


# for remaining ids, assign first names to ids
test_2 <- dups_1 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids, 
                            "_",
                            first_name))

test_2_dup_ids <- test_2 %>% 
  group_by(dedupe.ids,
           fiscal_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_2 <- test_2 %>% 
  filter(!dedupe.ids %in% test_2_dup_ids)

dups_2 <- test_2 %>% 
  filter(dedupe.ids %in% test_2_dup_ids)
nrow(dups_2) / nrow(vt_out_matches_df) * 100


# for remaining ids, assign middle names to ids
test_3 <- dups_2 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids, 
                            "_",
                            str_replace_na(middle_name_initial)))

test_3_dup_ids <- test_3 %>% 
  group_by(dedupe.ids,
           fiscal_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_3 <- test_3 %>% 
  filter(!dedupe.ids %in% test_3_dup_ids)

dups_3 <- test_3 %>% 
  filter(dedupe.ids %in% test_3_dup_ids)
nrow(dups_3) / nrow(vt_out_matches_df) * 100


# for remaining ids, assign agency name to ids
test_4 <- dups_3 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids, 
                            "_",
                            agency))

test_4_dup_ids <- test_4 %>% 
  group_by(dedupe.ids,
           fiscal_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_4 <- test_4 %>% 
  filter(!dedupe.ids %in% test_4_dup_ids)

dups_4 <- test_4 %>% 
  filter(dedupe.ids %in% test_4_dup_ids)
nrow(dups_4) / nrow(vt_out_matches_df) * 100


# just seven remaining ids left
test_5 <- dups_4 %>% 
  mutate(dedupe.ids = case_when(
    dedupe.ids == "5103_smith_stephanie_a_independent" &
      fiscal_year %in% c(2020, 2021) &
      department == "labor" ~ str_c(dedupe.ids, 
                                    "_",
                                    department),
    dedupe.ids == "1663_campbell_christopher_j_independent" ~ str_c(
      dedupe.ids, "_", department),
    dedupe.ids == "20302_davis_heather_NA_agency of human services" ~ str_c(dedupe.ids, 
                                                                            "_",
                                                                            department),
    dedupe.ids == "22219_stark_michael_NA_agency of human services" &
      fiscal_year == 2019 &
      department == "mental health" ~ str_c(dedupe.ids, 
                                            "_1"),
    dedupe.ids == "21469_manning_michael_j_independent" ~ str_c(dedupe.ids,
                                                                "_",
                                                                department),
    TRUE ~ dedupe.ids
  )) %>% 
  filter(!(fiscal_year == 2014 & 
             dedupe.ids == "19606_nitka_alice_w_legislative" & 
             expenses == 14834.83)) %>% 
  filter(!(fiscal_year == 2020 &
             dedupe.ids == "21871_bapp_christopher_j_agency of transportation" &
             job_type == "temporary seasonal")) %>% 
  filter(!(fiscal_year == 2011 & 
             dedupe.ids == "6646_davis_tamara_j_agency of administration" & 
             pay == 104.25)) %>%
  filter(!(fiscal_year == 2011 & 
             dedupe.ids == "20416_brown_kelly_l_independent" &
             department == "lieutenant governor's office"))
  

test_5_dup_ids <- test_5 %>% 
  group_by(dedupe.ids,
           fiscal_year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_5 <- test_5 %>% 
  filter(!dedupe.ids %in% test_5_dup_ids)

dups_5 <- test_5 %>% 
  filter(dedupe.ids %in% test_5_dup_ids)
nrow(dups_5) / nrow(vt_out_matches_df) * 100


vt_final <- rbind(unique_0,
                  unique_1,
                  unique_2,
                  unique_3,
                  unique_4,
                  unique_5) %>% 
  select(-multiple_record)

saveRDS(vt_final, 
        "../data/state-employee-data/VT/clean/vt-09-21-premerge.rds")


