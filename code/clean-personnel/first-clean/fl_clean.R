# fl_clean.R
# Description: This file reads in and cleans the FL personnel data. 
# Notes: 
##########################################################################

# preamble ####
library(tidyverse)
library(vroom)
library(lubridate)
library(readxl)

# read in data ####
fl_files <- list.files("../data/state-employee-data/FL/raw/")
fl_crosswalks <- str_subset(fl_files, 
                            "Race|Class")

fl_files <- str_subset(fl_files, 
                       "Race|Class",
                       negate = T)

fl_paths <- str_c("../data/state-employee-data/FL/raw/", 
                  fl_files)

# read in excel workbooks, some of which contain multiple sheets
read_sheets <- function(path) {
  sheet_names <- excel_sheets(path)
  
  all_sheets <- map(sheet_names, 
                    ~read_excel(path, 
                                sheet = .,
                                guess_max = 200000))
  names(all_sheets) <- map_chr(sheet_names,
                               ~str_c(path, "-", .))
  
  # for binding
  all_sheets_standard <- map(1:length(all_sheets), 
                                ~mutate(all_sheets[[.]],
                                       `Run Date` = as.character(`Run Date`),
                                       `Pay Plan` = as.character(`Pay Plan`)))
  
  all_sheets_df <- bind_rows(all_sheets_standard)
  
  return(all_sheets_df)
}

fl_raw_list <- map(fl_paths, 
                   read_sheets)

names(fl_raw_list) <- fl_files

# simple function to standardize dataframes for binding
clean_fl_dfs <- function(dataframe) {
  colnames(dataframe) <- str_replace_all(tolower(colnames(dataframe)),
                                         " ",
                                         "_")
  
  if(!"protected_identity" %in% colnames(dataframe) & !"restricted_relative" %in% colnames(dataframe)) {
    dataframe <- dataframe %>% 
      mutate(protected_identity = NA_character_,
             restricted_relative = NA_character_)
  } 
  if(is.numeric(dataframe$pay_plan)) {
    dataframe <- dataframe %>% 
      mutate(pay_plan = as.character(pay_plan))
  }
  if(is.numeric(dataframe$run_date)) {
    dataframe <- dataframe %>% 
      mutate(run_date = as.character(run_date))
  }
  
  return(dataframe)
}

fl_clean_list <- map(fl_raw_list, 
                     clean_fl_dfs)

fl_clean_df <- bind_rows(fl_clean_list)



# basic cleaning ####
suffixes <- "\\ssr$|\\sjr$|\\sii$|\\siii$|\\siv$|\\sv$|\\siiii$"


fl_clean_df_2 <- fl_clean_df %>% 
  mutate(across(where(is.character),
                ~str_trim(str_squish(tolower(.))))) %>% 
  mutate(run_date = ym(run_date))

fl_clean_df_parse <- fl_clean_df_2 %>% 
  mutate(last_name = str_extract(emp_name, 
                                  ".*(?=,)")) %>% 
  mutate(first_name = str_trim(str_extract(emp_name, 
                                           "(?<=,).*"))) %>% 
  mutate(suffix = str_trim(str_extract(last_name,
                              suffixes))) %>% 
  mutate(last_name = str_trim(str_remove(last_name, 
                                         suffixes))) %>% 
  mutate(across(c(first_name, 
                  last_name, 
                  suffix),
                ~str_squish(str_trim(str_remove_all(.,
                                                    "[:punct:]"))))) 
        

# merge in race and class codes
fl_code_paths <- str_c("../data/state-employee-data/FL/raw/", 
                       fl_crosswalks)

fl_race_codes <- read_excel(str_subset(fl_code_paths, "Race"))
fl_class_codes <- read_excel(str_subset(fl_code_paths, "Class"))

colnames(fl_race_codes) <- str_replace_all(tolower(colnames(fl_race_codes)),
                                           " ",
                                           "_")
colnames(fl_class_codes) <- str_replace_all(tolower(colnames(fl_class_codes)),
                                           " ",
                                           "_")

fl_race_codes <- fl_race_codes %>%
  select(-reference_type) %>% 
  mutate(across(.fns = tolower)) %>% 
  rename(race = reference_code,
         race_descr = reference_desc)

fl_class_codes <- fl_class_codes %>% 
  mutate(across(where(is.character),
                tolower)) %>% 
  mutate(pay_plan = as.numeric(pay_plan),
         class_code = as.numeric(class_code))


fl_clean_df_race <- left_join(fl_clean_df_parse,
                              fl_race_codes, 
                              by = "race")
fl_clean_df_race_misses <- anti_join(fl_clean_df_parse,
                                     fl_race_codes, 
                                     by = "race")

fl_clean_df_race <- fl_clean_df_race %>% 
  mutate(pay_plan = as.numeric(pay_plan),
         class_code = as.numeric(class_code))

fl_clean_df_class <- left_join(fl_clean_df_race,
                               fl_class_codes, 
                               by = c("class_code",
                                      "pay_plan"))
fl_clean_df_class_misses <- anti_join(fl_clean_df_race,
                                      fl_class_codes, 
                                      by = c("class_code",
                                             "pay_plan"))

# emailed FL about the class code misses. 


# adding in the CS, SES, and SMS codes
# the are from this page: 
# https://www.dms.myflorida.com/workforce_operations/human_resource_management/for_state_personnel_system_hr_practitioners/classification_and_compensation/classification_and_reorganization_requests
read_fl_codes <- function(path, 
                          sheet_name) {
  out <- read_xlsx(path, 
                   skip = 1, 
                   sheet = sheet_name)
  
  return(out)
}

fl_codes_list <- map(excel_sheets("../data/state-employee-data/FL/Class_Designations_06082017.xlsx"),
                     ~read_fl_codes("../data/state-employee-data/FL/Class_Designations_06082017.xlsx",
                                    sheet_name = .))
names(fl_codes_list) <- excel_sheets("../data/state-employee-data/FL/Class_Designations_06082017.xlsx")

fl_codes_df <- bind_rows(fl_codes_list,
                         .id = "class_type") %>% 
  rename_with(~str_to_lower(str_replace_all(., 
                                            " ",
                                            "_"))) %>% 
  mutate(across(where(is.character),
                ~str_trim(str_squish(str_to_lower(.))))) %>% 
  mutate(class_code = as.numeric(class_code)) %>% 
  mutate(pay_plan = as.numeric(pay_plan)) %>% 
  select(class_type, 
         class_code, 
         pay_plan) %>% 
  distinct()

# the codes do not appear to be unique without the pay plan variable
fl_codes_df %>% 
  group_by(class_code, 
           pay_plan) %>% 
  filter(n() > 1)

# merge class type with full dataset
fl_clean_df_class_merge <- left_join(fl_clean_df_class,
                                     fl_codes_df, 
                                     by = c("class_code", 
                                            "pay_plan"))

# a number of NAs remaining -- these are mostly OPS employees which are temps
# https://www.dms.myflorida.com/workforce_operations/human_resource_management/for_other_personal_services_employment_ops

fl_clean_df_class_merge <- fl_clean_df_class_merge %>% 
  mutate(class_type = case_when(
    emp_type == "ops" ~ "temporary",
    TRUE ~ class_type
  )) %>% 
  distinct()


# adding row_number variable and selecting only the merge variables due to 
#   memory issues
fl_clean_df_class_merge <- fl_clean_df_class_merge %>% 
  mutate(temp_id = row_number()) %>% 
  mutate(last_name_join = case_when(
    is.na(suffix) ~ last_name,
    !is.na(suffix) ~ str_c(last_name, 
                           " ", 
                           suffix)
  )) %>% 
  filter(!is.na(gender))


fl_clean_df_class_cluster <- fl_clean_df_class_merge %>% 
  select(temp_id, 
         first_name, 
         last_name_join,
         gender, 
         race_descr)


saveRDS(fl_clean_df_class_merge, 
        '../data/state-employee-data/FL/clean/fl-fastlink-pre-full.rds')
saveRDS(fl_clean_df_class_cluster, 
        '../data/state-employee-data/FL/clean/fl-fastlink-pre.rds')



