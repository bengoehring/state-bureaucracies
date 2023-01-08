library(tidyverse)
library(vroom)
library(lubridate)

nc_files <- list.files("../data/state-employee-data/NC/raw/")
nc_paths <- str_c("../data/state-employee-data/NC/raw/",
                  nc_files)

read_nc <- function(path) {
    out <- vroom_fwf(path,
                     col_positions = fwf_cols(personnel_area = c(1,
                                                                 40),
                                              employee_name = c(41,
                                                                80),
                                              appointment_type = c(81,
                                                                   110),
                                              age = c(111,
                                                      112),
                                              original_hire_date = c(113,
                                                                     122),
                                              agency_hire_date = c(123,
                                                                   132),
                                              current_position_number = c(133,
                                                                          140),
                                              current_position_title = c(141,
                                                                         180),
                                              current_job_title = c(181,
                                                                    220),
                                              last_public_info_action_date = c(221,
                                                                               230),
                                              last_public_info_action = c(231,
                                                                          290),
                                              last_salary_increase_date = c(291,
                                                                            300),
                                              last_salary_increase_action = c(301,
                                                                              360),
                                              last_salary_change_amount = c(361,
                                                                            375),
                                              current_salary_last_salary = c(376,
                                                                             390),
                                              position_county = c(391,
                                                                  393)),
                     col_types = cols(age = col_character(),
                                      current_position_number = col_character(),
                                      current_salary_last_salary = col_character()),
                     guess_max = 50000)
    
    return(out)
}

nc_list_raw <- map(nc_paths,
                   read_nc)

file_dates <- str_remove(nc_files,
                         "Pub Info | Pub_Info|_Pub_Info|^Pub_Info|PUBINFO|Public_Info_|PublicInfo|Public_info|_Public_Info")
file_dates <- str_remove(file_dates,
                          "\\.zip$")
file_dates <- str_remove(file_dates, 
                          "^_")
file_dates <- str_replace_all(file_dates,
                              "_",
                              "-")
file_dates <- case_when(
  str_detect(file_dates, 
             "^\\d{8}$") ~ str_c(str_sub(file_dates,
                                         5,
                                         6),
                                 "-",
                                 str_sub(file_dates,
                                         7,
                                         8),
                                 "-",
                                 str_sub(file_dates,
                                         3,
                                         4)),
  TRUE ~ file_dates
)

names(nc_list_raw) <- file_dates

nc_df_raw <- nc_list_raw %>% 
  bind_rows(.id = "data_date")  %>% 
  distinct()


#test_sample <- nc_df_raw %>% 
#  slice_sample(prop = .05)

# initial cleaning 
nc_df_dates <- nc_df_raw %>% 
  mutate(data_date = mdy(data_date)) %>% 
  mutate(original_hire_date = case_when(
    str_detect(original_hire_date, "2217$") ~ str_replace(original_hire_date, 
                                                          "2217$",
                                                          "2017$"),
    str_detect(original_hire_date, "^0*$") ~ NA_character_,
    TRUE ~ original_hire_date
  )) %>% 
  mutate(agency_hire_date = case_when(
    str_detect(agency_hire_date, "^0*$") ~ NA_character_,
    TRUE ~ agency_hire_date
  )) %>% 
  mutate(last_salary_increase_date = case_when(
    str_detect(last_salary_increase_date, 
               "2106$") ~ str_replace(last_salary_increase_date, 
                                      "2106$", 
                                      "2016$"),
    str_detect(last_salary_increase_date, 
               "2208$") ~ NA_character_,
    TRUE ~ last_salary_increase_date
  )) %>% 
  mutate(across(c(original_hire_date, 
                  agency_hire_date,
                  last_public_info_action_date,
                  last_salary_increase_date),
                ~mdy(str_c(str_sub(., 
                                   1, 
                                   2),
                           "-",
                           str_sub(., 
                                   3, 
                                   4),
                           "-",
                           str_sub(., 
                                   5, 
                                   8))))) %>% 
  mutate(across(where(is.character),
                ~str_squish(str_trim(str_to_lower(.))))) %>% 
  mutate(age = case_when(
    str_detect(age, 
               "^\\d*$", 
               negate = T) ~ NA_character_,
    TRUE ~ age
  )) %>% 
  mutate(age = as.numeric(age))

# impute DOB
nc_dob <- nc_df_dates %>% 
  mutate(employee_dob_year = case_when(
    data_date == ymd("2022-05-31") ~ as.numeric(year(data_date) - age),
    TRUE ~ NA_real_
  )) %>% 
  group_by(employee_name, 
           original_hire_date) %>% 
  fill(employee_dob_year, 
       .direction = "updown") %>% 
  ungroup()

# NC is odd in that it keeps formerly employed employees in the system. 
#   dropping these additional rows here. 

nc_df_last_obs <- nc_dob %>% 
  group_by(employee_name,
           employee_dob_year,
           original_hire_date) %>% 
  mutate(last_observation = case_when(
    last_public_info_action_date == max(last_public_info_action_date) ~ 1, 
    TRUE ~ 0
  )) %>% 
  ungroup()

not_last_obs_data <- nc_df_last_obs %>% 
  filter(last_observation == 0)

last_obs_data <- nc_df_last_obs %>% 
  filter(last_observation == 1)

last_obs_data_deduped <- last_obs_data %>% 
  group_by(employee_name,
           employee_dob_year,
           original_hire_date) %>% 
  slice_min(data_date) %>% 
  ungroup()

nc_deduped <- rbind(not_last_obs_data,
                    last_obs_data_deduped)

# there are still some cases here (e.g. MICHAEL BRANDON) who only worked before
#   data began -- if that is the case, dropping.

nc_deduped_2 <- nc_deduped %>% 
  group_by(employee_name,
           employee_dob_year,
           original_hire_date) %>% 
  mutate(last_date_observed = max(last_public_info_action_date)) %>% 
  ungroup() %>% 
  mutate(first_data_date = min(data_date)) %>% 
  filter(last_date_observed >= first_data_date)



boop <- nc_deduped %>% 
  group_by(data_date,
           age,
           original_hire_date) %>% 
  filter(n() > 1)









