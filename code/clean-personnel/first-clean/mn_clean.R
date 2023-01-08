# mn_clean.R
# Description: This file reads in and cleans the MN personnel data. 
# Notes: Currently, it is just reading in 2016-2021. 
##########################################################################

# preamble ####
library(tidyverse)
library(readxl)
library(lubridate)

# read data ####
# read_mn() reads in the MN data for each fiscal year. The MN data is 
#   structured in excel files with separate sheets for earnings data
#   and the rest of the personnel data. 

read_mn <- function(fy_data_path, 
                    earnings) {
  
  if(!is.logical(earnings)) {
    stop("earnings need to be T/F")
  }
  
  fy_year <- as.numeric(str_extract(fy_data_path, "[:digit:]+"))
  
  # files follow different formats depending on year
  if(fy_year > 2015) {
    if(!earnings) {
      fy_data <- read_xlsx(fy_data_path,
                           sheet = 2,
                           col_types = c("text", #TEMPORARY_ID
                                         "numeric", #RECORD_NBR
                                         "text", #EMPLOYEE_NAME
                                         "text", #AGENCY_NBR
                                         "text", #AGENCY_NAME
                                         "text", #DEPARTMENT_NBR
                                         "text", #DEPARTMENT_NAME
                                         "text", #BRANCH_CODE
                                         "text", #BRANCH_NAME
                                         "text", #JOB_CODE
                                         "text", #JOB_TITLE
                                         "text", #LOCATION_NBR
                                         "text", #LOCATION_NAME
                                         "text", #LOCATION_POSTAL_CODE
                                         "text", #REG_TEMP_CODE
                                         "text", #REG_TEMP_DESC
                                         "text", #CLASSIFIED_CODE
                                         "text", #CLASSIFIED_DESC
                                         "date", #ORIGINAL_HIRE_DATE
                                         "date", #LAST_HIRE_DATE
                                         "date", #JOB_ENTRY_DATE
                                         "text", #FULL_PART_TIME_CODE
                                         "text", #FULL_PART_TIME_DESC
                                         "text", #SALARY_PLAN_GRID
                                         "numeric", #SALARY_GRADE_RANGE
                                         "numeric", #MAX_SALARY_STEP
                                         "numeric", #COMPENSATION_RATE
                                         "text", #COMP_FREQUENCY_CODE
                                         "text", #COMP_FREQUENCY_DESC
                                         "numeric", #POSITION_FTE
                                         "numeric", #BARGAINING_UNIT_NBR
                                         "text", #BARGAINING_UNIT_NAME
                                         "text" #ACTIVE_ON_FY
                           ),
                           na = c("", "-"))
      
      fy_data <- fy_data %>% 
        rename_with(tolower) %>% 
        rename("active_on_fy" = starts_with("active_on")) %>% 
        mutate(fiscal_year = fy_year)
      
      return(fy_data)
      
    } else {
      fy_data <- read_xlsx(fy_data_path, 
                           sheet = 3,
                           na = c("", "-"))
      
      fy_data <- fy_data %>% 
        rename_with(tolower) %>% 
        mutate(fiscal_year = fy_year)
      
      return(fy_data)
    }
  } else {
    # this covers data from 2015 and earlier
    if(!earnings) {
      fy_data <- read_xlsx(fy_data_path,
                           sheet = 2,
                           na = c("", "-"))
      
      fy_data <- fy_data %>% 
        rename_with(tolower) %>% 
        rename(temporary_id = temporary_employee_id,
               record_nbr = empl_rec_nbr,
               employee_name = empl_nm,
               agency_nbr = emplt_agency_nbr,
               agency_name = emplt_agency_nm,
               department_nbr = dept_nbr,
               department_name = dept_nm,
               branch_code = dept_branch_cd,
               branch_name = dept_branch_nm,
               job_code = job_cd,
               job_title = job_desc,
               location_nbr = locn_cd,
               location_name = locn_nm,
               location_postal_code = zip_cd,
               reg_temp_code = rglr_tmpry_cd,
               reg_temp_desc = rglr_tmpry_desc,
               classified_code = classified_cd,
               classified_desc = classified_desc,
               original_hire_date = orig_hire_dt,
               last_hire_date = last_hire_dt,
               job_entry_date = job_entry_dt,
               full_part_time_code = full_part_tm_cd,
               full_part_time_desc = full_part_tm_desc,
               salary_plan_grid = slry_admin_plan_nbr,
               salary_grade_range = slry_grade_nbr,
               max_salary_step = max_slry_step_nbr,
               compensation_rate = cmptn_rt,
               comp_frequency_code = cmptn_freq_cd,
               comp_frequency_desc = cmptn_freq_desc,
               position_fte = posn_fte_prct_rt,
               bargaining_unit_nbr = brgng_unit_nbr,
               bargaining_unit_name = brgng_unit_nm) %>% 
        rename(active_on_fy = starts_with("active_")) %>% 
        mutate(fiscal_year = fy_year) %>% 
        mutate(salary_grade_range = as.numeric(salary_grade_range)) %>% 
        mutate(bargaining_unit_nbr = as.numeric(bargaining_unit_nbr))
      
      return(fy_data)
    } else {
      fy_data <- read_xlsx(fy_data_path, 
                           sheet = 3,
                           na = c("", "-")) %>% 
        rename_with(tolower) %>% 
        mutate(fiscal_year = fy_year) %>% 
        rename(temporary_id = temporary_employee_id)
      
      if(fy_year == 2011){
        fy_data <- fy_data %>% 
          rename(regular_wages = salary,
                 overtime_wages = overtime,
                 other_wages = other_pay) %>% 
          rowwise() %>% 
          mutate(total_wages = sum(regular_wages,
                                   overtime_wages,
                                   differentials_etc,
                                   other_wages,
                                   na.rm = T)) %>% 
          ungroup()
      } else if(fy_year %in% c(2012, 2014, 2015)) {
        fy_data <- fy_data %>% 
          rename(regular_wages = regwages,
                 overtime_wages = otwages,
                 other_wages = otherwages) %>% 
          rowwise() %>% 
          mutate(total_wages = sum(regular_wages,
                                   overtime_wages,
                                   other_wages,
                                   na.rm = T)) %>% 
          ungroup()
      } else if(fy_year == 2013) {
        fy_data <- fy_data %>% 
          select(-empl_nbr) %>% 
          rename(regular_wages = regwages,
                 overtime_wages = otwages,
                 other_wages = otherwages) %>% 
          rowwise() %>% 
          mutate(total_wages = sum(regular_wages,
                                   overtime_wages,
                                   other_wages,
                                   na.rm = T)) %>% 
          ungroup()
      }
      
      return(fy_data)
    }
  }
}    
   
# read in earnings and non-earnings data
mn_files <- list.files("../data/state-employee-data/MN/raw/")
mn_paths <- str_c("../data/state-employee-data/MN/raw/",
                  mn_files)


# read in non-earnings data
#   note that location_postal_code available 2011:2019 and then 
#   location_county_name availbale after
mn_hr_list <- map(mn_paths,
                        ~read_mn(., 
                                 earnings = F))

mn_hr_df <- mn_hr_list %>% 
  bind_rows()

# read in earnings data
mn_earnings_list <- map(mn_paths,
                        ~read_mn(., 
                                 earnings = T))

mn_earnings_df <- mn_earnings_list %>% 
  bind_rows()

mn_earnings_df %>% 
  group_by(temporary_id,
           fiscal_year) %>% 
  filter(n() > 1)

# basic cleaning ####
# first merging data -- cleaning up the very few number of duplicate ids in the 
# earnings data. Very few, so aggregating those 8 cases by year and id. 
mn_earnings_df <- mn_earnings_df %>% 
  group_by(fiscal_year, 
           temporary_id) %>% 
  summarise(total_wages = sum(total_wages, 
                              na.rm = T),
            regular_wages = sum(regular_wages, 
                                na.rm = T),
            overtime_wages = sum(overtime_wages, 
                                 na.rm = T),
            differentials_etc = sum(differentials_etc,
                                    na.rm = T),
            other_wages = sum(other_wages, 
                              na.rm = T)) %>% 
  ungroup()


mn_all <- left_join(mn_hr_df,
                    mn_earnings_df,
                    by = c("temporary_id", 
                           "fiscal_year"))
mn_all_misses <- anti_join(mn_hr_df,
                           mn_earnings_df,
                           by = c("temporary_id", 
                                  "fiscal_year"))
# very few misses -- ignoring


# dropping number variables for their corresponding descriptions
mn_all_clean <- mn_all %>% 
  mutate(across(where(is.character),
                ~str_trim(str_squish(tolower(.))))) %>% 
  select(-c(agency_nbr,
            department_nbr,
            branch_code,
            job_code,
            location_nbr,
            reg_temp_code,
            classified_code
           )) %>% 
  select(fiscal_year, 
         temporary_id, 
         record_nbr, 
         everything())

# the temporary id variable is not dynamic and provides at most a way of 
#   differentiating people in the same year. 13% of people worked more than one
#   job in a given year. When this happens, selecting the row with the highest 
#   salary and aggregating income.

# some checks regarding the temp id
# just a few instances of the same id going to two different people
# differentiating those cases here. 
mn_all_clean_2 <- mn_all_clean %>% 
  group_by(temporary_id,
           fiscal_year) %>% 
  mutate(temporary_id = case_when(
    n_distinct(employee_name) > 1 ~str_c(temporary_id, 
                                         "_",
                                         str_c(employee_name,
                                               ",\\d{2}")),
    TRUE ~ temporary_id
  )) %>% 
  mutate(dup_id = case_when(
    n() > 1 ~ 1,
    TRUE ~ 0
  )) %>% 
  ungroup()

dup_ids <- mn_all_clean_2 %>% 
  filter(dup_id == 1) %>% 
  pull(temporary_id) %>% 
  unique()

mn_multiple_jobs <- mn_all_clean_2 %>% 
  filter(temporary_id %in% dup_ids)
mn_unique <- mn_all_clean_2 %>% 
  filter(!temporary_id %in% dup_ids)

# note that even though there are multiple rows if someone has multiple
#   jobs the salary and wage info is all already aggregated to the person level.

# Now select a single row for each temporary id - year observation. 
#   When duplicates exist, taking active, full-time, highest-paying roles. 

mn_multiple_jobs_2 <- mn_multiple_jobs %>% 
  group_by(fiscal_year,
           temporary_id) %>% 
  mutate(keep_row = case_when(
    compensation_rate == max(compensation_rate) ~ 1, 
    TRUE ~ 0
  )) %>% 
  ungroup() %>% 
  filter(keep_row == 1) %>% 
  select(-keep_row) %>% 
  group_by(fiscal_year,
           temporary_id) %>% 
  mutate(keep_row = case_when(
    position_fte == max(position_fte) ~ 1, 
    TRUE ~ 0
  )) %>% 
  ungroup() %>% 
  filter(keep_row == 1) %>% 
  select(-keep_row) %>% 
  group_by(fiscal_year,
           temporary_id) %>% 
  mutate(drop_row = case_when(
    n() > 1 & 
      n_distinct(active_on_fy) > 1 &
      active_on_fy == "no" ~ 1, 
    TRUE ~ 0
  )) %>% 
  ungroup() %>% 
  filter(drop_row == 0) %>% 
  select(-drop_row)
  
# few dups remaining -- just taking a random row
mn_multiple_jobs_2  <- mn_multiple_jobs_2 %>% 
  group_by(fiscal_year,
           temporary_id) %>% 
  slice_sample(n = 1) %>% 
  ungroup()


mn_deduped_in_period <- rbind(mn_multiple_jobs_2, 
                              mn_unique)


# parse names ####
suffixes <- "\\ssr$|\\sjr$|\\sii$|\\siii$|\\siv$|\\sv$|\\siiii$"

mn_names <- mn_deduped_in_period %>% 
  mutate(last_name = str_extract(employee_name, 
                                 ".+(?=,)")) %>% 
  mutate(first_name = str_extract(employee_name, 
                                  "(?<=,)[:graph:]+")) %>% 
  mutate(middle_name = str_remove(employee_name, 
                                  str_c(last_name, 
                                        ",",
                                        first_name))) %>% 
  mutate(middle_name = case_when(
    str_length(middle_name) == 0 ~ NA_character_,
    TRUE ~ middle_name
  )) %>% 
  mutate(across(c(first_name, 
                  middle_name,
                  last_name), 
                ~ str_trim(str_remove_all(., 
                                          "[:punct:]")))) %>% 
  mutate(suffix = str_extract(last_name, 
                              suffixes)) %>% 
  mutate(last_name = str_remove(last_name, 
                                suffixes))

# some additional cleaning of some different name formats
mn_names_2 <- mn_names %>% 
  mutate(first_name_temp = case_when(
    str_length(first_name) == 1 & !is.na(middle_name) ~ middle_name,
    TRUE ~ NA_character_
  )) %>% 
  mutate(middle_name_temp = case_when(
    str_length(first_name) == 1 & !is.na(middle_name) ~ first_name,
    TRUE ~ NA_character_
  )) %>% 
  mutate(middle_name = case_when(
    !is.na(middle_name_temp) ~ middle_name_temp,
    TRUE ~ middle_name
  )) %>% 
  mutate(first_name = case_when(
    !is.na(first_name_temp) ~ first_name_temp,
    TRUE ~ first_name
  )) %>% 
  select(-first_name_temp,
         -middle_name_temp,
         -dup_id,
         -employee_name)

mn_dates <- mn_names_2 %>% 
  mutate(across(c(original_hire_date,
                  job_entry_date,
                  last_hire_date),
                ymd))

# save for fastlink ####
# create a hire date year variable for blocking
mn_fastlink <- mn_dates %>% 
  mutate(original_hire_year = as.integer(year(original_hire_date))) %>% 
  select(fiscal_year,
         temporary_id,
         first_name,
         middle_name,
         last_name,
         suffix,
         everything())

saveRDS(mn_fastlink,
        "../data/state-employee-data/MN/clean/mn-fastlink-pre.rds")










  
  



