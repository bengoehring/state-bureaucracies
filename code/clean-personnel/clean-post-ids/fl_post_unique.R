# As it stands, I am not using fastlink to assign ids in Florida. It is 
#   too computationally intensive. So, I am assigning ids deterministically
#   using sex, race, and names. 

library(tidyverse)
library(lubridate)

fl_no_id <- readRDS("../data/state-employee-data/FL/clean/fl-fastlink-pre-full.rds")

fl_no_id <- fl_no_id %>% 
  select(-temp_id)


fl_id <- fl_no_id %>% 
  group_by(first_name,
           last_name,
           suffix,
           race_descr,
           gender) %>% 
  mutate(person_id = cur_group_id()) %>% 
  select(run_date, 
         person_id, 
         everything()) %>% 
  ungroup()

# Need to remove dups so unique by person-period
remaining_dups <- fl_id %>% 
  group_by(run_date, 
           person_id) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(person_id) %>% 
  unique()

unique_0 <- fl_id %>% 
  filter(!person_id %in% remaining_dups)
dupes_0 <- fl_id %>% 
  filter(person_id %in% remaining_dups)
nrow(dupes_0) / nrow(fl_id) * 100


# a number of these seem to be people in temp jobs alongside full time
#   positions. If that is the case, noting and removing temp position. 
test_1 <- dupes_0 %>% 
  mutate(test_ops = case_when(
    emp_type == 'ops' ~ 1,
    TRUE ~ 0
  )) %>% 
  group_by(person_id,
           run_date) %>% 
  mutate(also_has_temp_job = case_when(
    n_distinct(emp_type) > 1 & sum(test_ops, 
                                   na.rm = T) > 0 ~ 1,
    TRUE ~ 0
  )) %>% 
  ungroup() %>% 
  filter(!(test_ops == 1 & also_has_temp_job == 1)) %>% 
  select(-test_ops)
  
test_1_dup_ids <- test_1 %>% 
  group_by(person_id,
           run_date) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(person_id) %>% 
  unique()

unique_1 <- test_1 %>% 
  filter(!person_id %in% test_1_dup_ids)

dupes_1 <- test_1 %>% 
  filter(person_id %in% test_1_dup_ids)
nrow(dupes_1) / nrow(fl_id) * 100


# we also have the state hire date for the employee. Adding that in now and not 
#   earlier because they are NA for all temp positions -- which is tricky 
#   for the ppl holding both temp and salaried positions at the same time.
#   In case someone transitioned from salried to ops or vice versa, not assigning
#   hire date to those observations 

test_2 <- dupes_1 %>% 
  group_by(person_id) %>% 
  mutate(person_id = case_when(
    n_distinct(emp_type) > 1 ~ as.character(person_id),
    TRUE ~ str_c(person_id,
                 str_replace_na(as.character(state_hire_date)))
  ))

test_2_dup_ids <- test_2 %>% 
  group_by(person_id,
           run_date) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(person_id) %>% 
  unique()

unique_2 <- test_2 %>% 
  filter(!person_id %in% test_2_dup_ids)

dupes_2 <- test_2 %>% 
  filter(person_id %in% test_2_dup_ids)
nrow(dupes_2) / nrow(fl_id) * 100


# take higher paying row for remaining dups -- feeling fine about this given 
#   the state hire date variable -- highly unlikely for two people with all the same
#   variables
test_3 <- dupes_2 %>% 
  group_by(person_id,
           run_date) %>% 
  mutate(max_row = case_when(
    base_rate_or_ops_hourly_rate == max(base_rate_or_ops_hourly_rate) ~ 1,
    TRUE ~ 0
  )) %>% 
  ungroup() %>% 
  filter(max_row == 1) %>% 
  group_by(person_id,
            run_date) %>% 
  slice_sample(n = 1) %>% 
  ungroup() %>% 
  select(-max_row)

test_3_dup_ids <- test_3 %>% 
  group_by(person_id,
           run_date) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(person_id) %>% 
  unique()

unique_3 <- test_3 %>% 
  filter(!person_id %in% test_3_dup_ids)

dupes_3 <- test_3 %>% 
  filter(person_id %in% test_3_dup_ids)
nrow(dupes_3) / nrow(fl_id) * 100



unique_0 <- mutate(unique_0, 
                   person_id = as.character(person_id),
                   also_has_temp_job = 0)
unique_1 <- mutate(unique_1, person_id = as.character(person_id))
unique_2 <- mutate(unique_2, person_id = as.character(person_id))
unique_3 <- mutate(unique_3, person_id = as.character(person_id))

fl_deduped <- rbind(unique_0,
                    unique_1,
                    unique_2,
                    unique_3)


# check for different state hire dates for the same person
fix_hire_dates <- fl_deduped %>% 
  filter(!is.na(state_hire_date)) %>% 
  group_by(person_id) %>% 
  filter(n_distinct(state_hire_date) > 1)

# return the ids of employees who have >1 hire date seemingly incorrectly. 
#   Basically, these are the instances where the hire date change but nothing
#   else really changes. 
bad_date_ids <- fix_hire_dates %>% 
  group_by(person_id) %>% 
  arrange(run_date, 
          .by_group = T) %>% 
  mutate(across(c(class_code,
                  class_title,
                  state_hire_date,
                  agency_name,
                  pos_num),
         ~. == lead(.) & lead(run_date) == run_date + period('1 month'),
         .names = "{.col}_bool")) %>% 
  ungroup() %>% 
  filter(!state_hire_date_bool & 
           class_code_bool & 
           class_title_bool &
           agency_name_bool & 
           pos_num_bool) %>% 
  pull(person_id)

fl_deduped_2 <- fl_deduped %>% 
  group_by(person_id) %>% 
  mutate(state_hire_date = case_when(
    person_id %in% bad_date_ids ~ min(state_hire_date),
    TRUE ~ state_hire_date
  )) %>% 
  mutate(person_id = case_when(
    person_id %in% bad_date_ids ~ person_id,
    TRUE ~ str_c(person_id, state_hire_date)
  )) %>% 
  ungroup()
  

saveRDS(fl_deduped_2,
        "../data/state-employee-data/FL/clean/fl-10-22-premerge.rds")


