library(tidyverse)
library(lubridate)

# read in data outputted from the code that prob assigns ids 
nj_out <- readRDS("../data/state-employee-data/NJ/clean/nj-fastlink-post.rds")

nj_out_fl_list <- nj_out[[1]]

nj_out_matches_df <- nj_out[[2]] %>% 
  select(as_of_date, 
         dedupe.ids,
         everything()) %>% 
  distinct() %>% 
  select(-payroll_id) %>% 
  mutate(dedupe.ids = str_c(dedupe.ids, "authority"))


# note, I only had to run fastlink on the NJ authority data because there is 
#   an employee id for the agency data. Merging them back together here. 
nj_agency_data <- readRDS("../data/state-employee-data/NJ/clean/nj-fastlink-pre.rds")
nj_agency_data <- nj_agency_data %>% 
  filter(agency_type == 'agency') %>% 
  rename(dedupe.ids = payroll_id) %>% 
  mutate(dedupe.ids = str_c(dedupe.ids, "agency"))


nj_all <- rbind(nj_agency_data,
                nj_out_matches_df)


# Need to remove dups so unique by person-period
remaining_dups <- nj_all %>% 
  group_by(as_of_date, 
           dedupe.ids) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_0 <- nj_all %>% 
  filter(!dedupe.ids %in% remaining_dups)
dupes_0 <- nj_all %>% 
  filter(dedupe.ids %in% remaining_dups)
nrow(dupes_0) / nrow(nj_all) * 100


# for remaining duplicates, append middle initial to id
test_1 <- dupes_0 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(middle_initial)))

test_1_dup_ids <- test_1 %>% 
  group_by(dedupe.ids,
           as_of_date) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_1 <- test_1 %>% 
  filter(!dedupe.ids %in% test_1_dup_ids)

dupes_1 <- test_1 %>% 
  filter(dedupe.ids %in% test_1_dup_ids)
nrow(dupes_1) / nrow(nj_all) * 100


# for remaining duplicates, append last name to id
test_1.1 <- dupes_1 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(last_name))) 

test_1.1_dup_ids <- test_1.1 %>% 
  group_by(dedupe.ids,
           as_of_date) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_1.1 <- test_1.1 %>% 
  filter(!dedupe.ids %in% test_1.1_dup_ids)

dupes_1.1 <- test_1.1 %>% 
  filter(dedupe.ids %in% test_1.1_dup_ids)
nrow(dupes_1.1) / nrow(nj_all) * 100

# for remaining duplicates, append first name to id
test_1.2 <- dupes_1.1 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(first_name))) 

test_1.2_dup_ids <- test_1.2 %>% 
  group_by(dedupe.ids,
           as_of_date) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_1.2 <- test_1.2 %>% 
  filter(!dedupe.ids %in% test_1.2_dup_ids)

dupes_1.2 <- test_1.2 %>% 
  filter(dedupe.ids %in% test_1.2_dup_ids)
nrow(dupes_1.2) / nrow(nj_all) * 100


# for remaining duplicates, append agency to id
test_1.3 <- dupes_1.2 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(department_agency_desc))) 

test_1.3_dup_ids <- test_1.3 %>% 
  group_by(dedupe.ids,
           as_of_date) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_1.3 <- test_1.3 %>% 
  filter(!dedupe.ids %in% test_1.3_dup_ids)

dupes_1.3 <- test_1.3 %>% 
  filter(dedupe.ids %in% test_1.3_dup_ids)
nrow(dupes_1.3) / nrow(nj_all) * 100


# for remaining duplicates, append further agency info to id
test_1.5 <- dupes_1.3 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_sub(str_replace_na(section_desc),
                                    1,
                                    4))) 

test_1.5_dup_ids <- test_1.5 %>% 
  group_by(dedupe.ids,
           as_of_date) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_1.5 <- test_1.5 %>% 
  filter(!dedupe.ids %in% test_1.5_dup_ids)

dupes_1.5 <- test_1.5 %>% 
  filter(dedupe.ids %in% test_1.5_dup_ids)
nrow(dupes_1.5) / nrow(nj_all) * 100


# for remaining duplicates, append union info to id
test_1.6 <- dupes_1.5 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_sub(str_replace_na(employee_relations_group),
                                    1,
                                    4))) 

test_1.6_dup_ids <- test_1.6 %>% 
  group_by(dedupe.ids,
           as_of_date) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_1.6 <- test_1.6 %>% 
  filter(!dedupe.ids %in% test_1.6_dup_ids)

dupes_1.6 <- test_1.6 %>% 
  filter(dedupe.ids %in% test_1.6_dup_ids)
nrow(dupes_1.6) / nrow(nj_all) * 100

# few left -- taking whomever had the higher salary
test_1.7 <- dupes_1.6 %>% 
  group_by(as_of_date, 
           dedupe.ids) %>% 
  mutate(keep_row = case_when(
    n() > 1 & ytd_earnings == max(ytd_earnings) ~ 1,
    n() > 1 & ytd_earnings != max(ytd_earnings) ~ 0,
    TRUE ~ 1
  )) %>% 
  ungroup() %>% 
  filter(keep_row == 1) %>% 
  select(-keep_row)

test_1.7 <- test_1.7 %>% 
  filter(!(first_name == 'none' & last_name == 'none')) %>% 
  filter(!(first_name == 'george' & last_name == 'hewitt' & salary_hourly_rate == 0)) %>% 
  slice(1)

test_1.7_dup_ids <- test_1.7 %>% 
  group_by(dedupe.ids,
           as_of_date) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_1.7 <- test_1.7 %>% 
  filter(!dedupe.ids %in% test_1.7_dup_ids)

dupes_1.7 <- test_1.7 %>% 
  filter(dedupe.ids %in% test_1.7_dup_ids)
nrow(dupes_1.7) / nrow(nj_all) * 100


# bind together and save ####
nj_deduped <- rbind(unique_0,
                    unique_1,
                    unique_1.2,
                    unique_1.3,
                    unique_1.5,
                    unique_1.6,
                    unique_1.7,
                    unique_1.1)


# add in job classification information. This was scraped from:
# https://info.csc.state.nj.us/TItleList/StateList.aspx

# This provides the title, code, and classification status of jobs in new jersey
nj_job_class <- readRDS("../data/state-employee-data/NJ/raw/job_class_codes.rds")

nj_job_class_clean <- nj_job_class %>% 
  mutate(across(where(is.character),
                ~str_trim(str_squish(str_to_lower(.))))) %>% 
  mutate(class_of_service = case_when(
    class_of_service == "c" ~ "career - competitive",
    class_of_service == 'n' ~ "career - non-competitive",
    class_of_service == 'u' ~ 'unclassified',
    class_of_service == 'm' ~ 'senior executive service',
    TRUE ~ NA_character_
  )) 

# based on looking at some job postings, "special services" seems to be a catch all 
#   term for temporary employment. 
nj_job_class_clean <- nj_job_class_clean %>% 
  mutate(title_code = as.numeric(str_remove(title_code,
                                            "[:alpha:]"))) %>% 
  select(title_code,
         class_of_service) %>% 
  distinct() %>% 
  rbind(data.frame(title_code = 98988,
                   class_of_service = "temporary")) %>% 
  group_by(title_code) %>% 
  mutate(class_of_service = case_when(
    n() > 1 ~ "ambiguous",
    TRUE ~ class_of_service
  )) %>% 
  distinct()


# It links together very poorly with the full payroll dataset. So, the state
#   provided me with a crosswalk between the codes in nj_job_class and the 
#   job titles in nj_deduped. 
code_title_xwalk <- read_csv("../data/state-employee-data/NJ/raw/Agency Payroll Title list.csv")
code_title_xwalk <- code_title_xwalk %>% 
  rename_with(tolower) %>% 
  mutate(master_title_desc = str_trim(str_squish(str_to_lower(master_title_desc)))) %>% 
  mutate(master_title_desc = case_when(
    master_title_cde == 0 ~ NA_character_,
    TRUE ~ master_title_desc
  )) %>% 
  distinct() %>% 
  filter(!str_detect(master_title_desc,
                    "assembly|legislat|senator"))


# In order to merge the job titles in the main payroll dataset with the 
#   classified status information: 
#   1) merge the scraped classification info with the crosswalk by code
#   2) merge the result from 1 with the main payroll dataset by title name

# merge 1 
merge_1 <- left_join(code_title_xwalk, 
                     nj_job_class_clean,
                     by = c("master_title_cde" = "title_code"))
nrow(merge_1) == nrow(code_title_xwalk)

merge_1 <- merge_1 %>% 
  select(-master_title_cde) %>% 
  distinct() %>% 
  group_by(master_title_desc) %>% 
  mutate(class_of_service = case_when(
    n() > 1 ~ "ambiguous",
    TRUE ~ class_of_service
  )) %>% 
  ungroup() %>% 
  distinct()


# merge 2
# it appears these just apply to agencies, so going to just leave out authority 
#   employees. 
nj_deduped_agency <- nj_deduped %>% 
  filter(agency_type == "agency")
nj_deduped_authority <- nj_deduped %>% 
  filter(agency_type == "authority") %>% 
  mutate(class_of_service = "authority")


merge_2 <- left_join(nj_deduped_agency, 
                     merge_1,
                     by = c("title_desc" = "master_title_desc"))
nrow(merge_2) == nrow(nj_deduped_agency)

n_hits <- nrow(semi_join(nj_deduped_agency, 
                         merge_1,
                         by = c("title_desc" = "master_title_desc")))
n_hits / nrow(nj_deduped_agency)

misses <- anti_join(nj_deduped_agency, 
                    merge_1,
                    by = c("title_desc" = "master_title_desc"))


# clean everything up and save 
nj_final <- rbind(merge_2,
                  nj_deduped_authority)
saveRDS(nj_final,
        "../data/state-employee-data/NJ/clean/nj-10-22-premerge.rds")

