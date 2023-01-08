# ms_clean.R
# Description: This file reads in and cleans the MS personnel data. 
# Notes: 
##########################################################################

# preamble ####
library(tidyverse)
library(lubridate)
library(readxl)

# read data ####
read_ms <- function(path) {
  out <- read_xlsx(path, 
                   sheet = 1)
  
  colnames(out) <- tolower(colnames(out))
  out <- out %>% 
    mutate(personid = as.character(personid))
    
  if("occuname" %in% colnames(out)) {
    out <- out %>% 
      rename(curocc_occuname = occuname)
  }
  if("fte" %in% colnames(out)) {
    out <- out %>% 
      rename(fteperc = fte)
  }
  if("county" %in% colnames(out)) {
    out <- out %>% 
      rename(cnty = county)
  }
  
  # getting some odd values with this variable and it is not in the
  #   other dataset, so removing
  if("hiredate" %in% colnames(out)) {
    out <- out %>% 
      select(-hiredate)
  }
  
  return(out)
}

ms_raw_19_21 <- read_ms("../data/state-employee-data/MS/raw/Muckrock_7.2022.xlsx")
ms_raw_15_18 <- read_ms("../data/state-employee-data/MS/raw/Muckrock_11.2022.xlsx")
ms_raw_xwalk <- read_xlsx("../data/state-employee-data/MS/raw/Muckrock_7.2022.xlsx",
                          sheet = 2,
                          col_names = c("status_code_value",
                                        "status_code_name"))

ms_raw <- bind_rows(ms_raw_19_21, 
                    ms_raw_15_18)


# basic cleaning ####
ms_vars <- ms_raw %>% 
  rename(month_observed = lastdate,
         first_name = fname,
         middle_name = mname,
         last_name = lname,
         person_id = personid,
         job_title = curocc_occuname,
         agency_name = agencyname,
         fte_percent = fteperc,
         part_time = parttime,
         county = cnty,
         status_code = statuscde,
         current_salary = curyrsal)

ms_clean <- ms_vars %>% 
  mutate(across(where(is.character),
                ~str_trim(str_squish(tolower(.))))) %>% 
  mutate(person_id = as.numeric(person_id)) %>% 
  mutate(month_observed = ymd(month_observed))

# name parsing
ms_clean <- ms_clean %>% 
  mutate(across(c(first_name, 
                  middle_name,
                  last_name),
                ~str_trim(str_remove_all(., 
                               "[:punct:]"))))

# add suffix column
suffixes <- "\\ssr$|\\sjr$|\\sii$|\\siii$|\\siv$|\\sv$|\\siiii$"

ms_names <- ms_clean %>% 
  mutate(suffix = case_when(
    str_detect(last_name, 
               suffixes) ~ str_extract(last_name, 
                                       suffixes),
    str_detect(middle_name, 
               suffixes) ~ str_extract(middle_name, 
                                       suffixes), 
    TRUE ~ NA_character_
  )) %>% 
  mutate(across(c(last_name,
                  middle_name), 
                ~str_trim(str_remove(., 
                            suffixes)))) %>% 
  mutate(suffix = str_trim(suffix)) %>% 
  distinct()

# most obs in middle_name are actually middle initials
ms_names <- ms_names %>% 
  mutate(middle_initial = str_sub(middle_name, 
                                  start = 1, 
                                  end = 1)) %>% 
  select(-middle_name) %>% 
  distinct()

# add values from included crosswalk
ms_status <- left_join(ms_names, 
                       ms_raw_xwalk,
                       by = c("status_code" = "status_code_value"))
ms_status_misses <- anti_join(ms_names, 
                              ms_raw_xwalk,
                              by = c("status_code" = "status_code_value"))

ms_status <- ms_status %>% 
  select(-status_code) %>% 
  mutate(status_code_name = str_to_lower(status_code_name))


# check to be sure that the state-provided person_id variables are the same
#   across the 19-21 and 15-18 datasets. 
check_ids_x <- ms_status %>% 
  filter(month_observed == ymd("2018-12-31"))
check_ids_y <- ms_status %>% 
  filter(month_observed == ymd("2019-01-31"))

check_ids <- left_join(check_ids_x, 
                       check_ids_y, 
                       by = "person_id") %>% 
  filter(!is.na(last_name.y))
sum(check_ids$last_name.x == check_ids$last_name.y, na.rm = T) / nrow(check_ids)
# looks good enough



# fix any missing values by group 
ms_status <- ms_status %>% 
  group_by(person_id) %>% 
  fill(last_name,
       .direction = 'downup') %>% 
  fill(first_name,
       .direction = 'downup') %>% 
  fill(middle_initial,
       .direction = 'downup') %>% 
  fill(race, 
       .direction = 'downup') %>% 
  fill(sex, 
       .direction = 'downup') %>% 
  fill(suffix, 
       .direction = 'downup') %>% 
  ungroup() %>% 
  distinct()




# check for duplicates in period-person ####
remaining_dups <- ms_status %>% 
  group_by(month_observed, 
           person_id) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(person_id) %>% 
  unique()

unique_0 <- ms_status %>% 
  filter(!person_id %in% remaining_dups)
dupes_0 <- ms_status %>% 
  filter(person_id %in% remaining_dups)
nrow(dupes_0) / nrow(ms_status) * 100


# some of these seem to be instances of women changing their last names but
#   showing up in the system under both married and maiden names. I am unsure
#   which one is more accurate, so taking the name that comes up first
#   alphabetically
test_1 <- dupes_0 %>% 
  group_by(person_id) %>% 
  mutate(maiden_name_change = case_when(
    n_distinct(last_name) > 1 & sex == 'female' ~ 1, 
    TRUE ~ 0
    )) %>% 
  ungroup() %>% 
  group_by(person_id, 
           month_observed) %>% 
  arrange(last_name, 
          .by_group = T) %>% 
  filter((maiden_name_change == 1 & cur_group_rows() == 1) | maiden_name_change == 0) %>% 
  select(-maiden_name_change)

test_1_dup_ids <- test_1 %>% 
  group_by(person_id,
           month_observed) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(person_id) %>% 
  unique()

unique_1 <- test_1 %>% 
  filter(!person_id %in% test_1_dup_ids)
dupes_1 <- test_1 %>% 
  filter(person_id %in% test_1_dup_ids)

nrow(dupes_1) / nrow(ms_status) * 100


# a couple of seeming small data issues 
test_2 <- dupes_1 %>% 
  mutate(sex = case_when(
    person_id == 577067 ~ "male",
    TRUE ~ sex
    )) %>% 
  mutate(race = case_when(
    person_id == 317916 ~ "african american",
    TRUE ~ race
  )) %>% 
  mutate(last_name = case_when(
    person_id == 576948 ~ "kent stricklan", 
    TRUE ~ last_name
  ) ) %>% 
  mutate(middle_initial = case_when(
    person_id == 577454 ~ NA_character_,
    TRUE ~ middle_initial
    )) %>% 
  mutate(middle_initial = case_when(
    person_id == 571181 ~ NA_character_,
    TRUE ~ middle_initial
  )) %>% 
  mutate(first_name = case_when(
    person_id == 571181 ~ "kevin",
    TRUE ~ first_name
  )) %>% 
  distinct()

test_2_dup_ids <- test_2 %>% 
  group_by(person_id,
           month_observed) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(person_id) %>% 
  unique()

unique_2 <- test_2 %>% 
  filter(!person_id %in% test_2_dup_ids)
dupes_2 <- test_2 %>% 
  filter(person_id %in% test_2_dup_ids)

nrow(dupes_2) / nrow(ms_status) * 100


# for the remaining, most of these appear to be cases of somebody being 
#   on the rolls for a single month while they are transitioning from 
#   one job to another. As a result, taking the row with the higher salary
test_3 <- dupes_2 %>% 
  group_by(person_id, 
           month_observed) %>% 
  mutate(higher_sal = case_when(
    n() > 1 & n_distinct(current_salary) == 1 ~ as.numeric(sample(c(1, #this is for ties
                                                                    rep(0, 
                                                                        n() - 1)))),
    n() > 1 & current_salary == max(current_salary) ~ 1,
    n() > 1 & current_salary != max(current_salary) ~ 0,
    TRUE ~ 1
  )) %>% 
  ungroup() %>% 
  filter(higher_sal == 1) %>% 
  select(-higher_sal)

test_3_dup_ids <- test_3 %>% 
  group_by(person_id,
           month_observed) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(person_id) %>% 
  unique()

unique_3 <- test_3 %>% 
  filter(!person_id %in% test_3_dup_ids)
dupes_3 <- test_3 %>% 
  filter(person_id %in% test_3_dup_ids)

nrow(dupes_3) / nrow(ms_status) * 100



# bind datasets together to create deduped dataset
ms_deduped <- rbind(unique_0,
                    unique_1,
                    unique_2,
                    unique_3)

# arrange columns and save ####
ms_final <- ms_deduped %>% 
  select(month_observed,
          person_id,
          first_name,
          middle_initial, 
          last_name,
          suffix,
          race,
          sex,
          everything()
          )

saveRDS(ms_final, 
        file = "../data/state-employee-data/MS/clean/ms-15-22-premerge.rds")



