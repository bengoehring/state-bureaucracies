library(tidyverse)
library(lubridate)
library(readxl)

ca_files <- list.files("../data/state-employee-data/CA/raw/")
ca_files <- str_subset(ca_files, 
                       "xlsx$")
ca_paths <- str_c("../data/state-employee-data/CA/raw/",
                  ca_files)

read_ca <- function(path) {
  out <- read_xlsx(path, 
                   skip = 5,
                   guess_max = 10000)
  
  # extract data pull date
  data_date <- read_xlsx(path,
                    skip = 2,
                    n_max = 1,
                    col_names = "data_pull_date")
  
  out <- out %>%
    mutate(data_pull_date = data_date$data_pull_date[1]) %>% 
    select(data_pull_date, 
           everything())
  
  return(out)
}
ca_raw_list <- map(ca_paths, 
                   read_ca)

ca_raw_df <- ca_raw_list %>% 
  bind_rows()


# initial cleaning
ca_clean <- ca_raw_df %>% 
  rename_with(~str_to_lower(str_replace_all(.,
                                            " ",
                                            "_"))) %>% 
  mutate(across(where(is.character),
                ~str_trim(str_squish(str_to_lower(.)))))

ca_clean <- ca_clean %>% 
  mutate(data_pull_date = mdy(str_remove(data_pull_date, "data as of"))) %>% 
  select(data_pull_date,
         unique_identifier,
         everything()) %>% 
  distinct()

# there are some dup observations within an ID. These appear to be instances
#   of the same person having multiple jobs -- usually intermittent ones. Going
#   to take the full time/ permanent job. 
# Need to remove dups so unique by person-period
remaining_dups <- ca_clean %>% 
  group_by(data_pull_date, 
           unique_identifier) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(unique_identifier) %>% 
  unique()

unique_0 <- ca_clean %>% 
  filter(!unique_identifier %in% remaining_dups)
dupes_0 <- ca_clean %>% 
  filter(unique_identifier %in% remaining_dups)
nrow(dupes_0) / nrow(ca_clean) * 100


# take the permanent row for remaining dupes
test_1 <- dupes_0 %>% 
  group_by(data_pull_date,
           unique_identifier) %>% 
  mutate(keep = case_when(
    n_distinct(tenure) >= 1 & tenure == 'p' ~ T,
    n_distinct(tenure) > 1 & tenure != 'p' ~ F,
    TRUE ~ T
  )) %>% 
  ungroup() %>% 
  filter(keep)
  
test_1_dup_ids <- test_1 %>% 
  group_by(data_pull_date,
           unique_identifier) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(unique_identifier) %>% 
  unique()

unique_1 <- test_1 %>% 
  filter(!unique_identifier %in% test_1_dup_ids)

dupes_1 <- test_1 %>% 
  filter(unique_identifier %in% test_1_dup_ids)
nrow(dupes_1) / nrow(ca_clean) * 100

# take the non-intermittent row for remaining dupes
test_2 <- dupes_1 %>% 
  group_by(data_pull_date,
           unique_identifier) %>% 
  mutate(keep2 = case_when(
    n_distinct(int_indicator) > 1 & is.na(int_indicator) ~ T,
    n_distinct(int_indicator) > 1 & !is.na(int_indicator) ~ F,
    TRUE ~ T
  )) %>% 
  ungroup() %>% 
  filter(keep2) %>% 
  select(-keep,
         -keep2)

test_2_dup_ids <- test_2 %>% 
  group_by(data_pull_date,
           unique_identifier) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(unique_identifier) %>% 
  unique()

unique_2 <- test_2 %>% 
  filter(!unique_identifier %in% test_2_dup_ids)

dupes_2 <- test_2 %>% 
  filter(unique_identifier %in% test_2_dup_ids)
nrow(dupes_2) / nrow(ca_clean) * 100


# take the full time row for remaining dupes
test_3 <- dupes_2 %>% 
  group_by(data_pull_date,
           unique_identifier) %>% 
  mutate(keep = case_when(
    n_distinct(time_base) > 1 & time_base == 'ft' ~ T,
    n_distinct(time_base) > 1 & time_base != 'ft' ~ F,
    TRUE ~ T
  )) %>% 
  ungroup() %>% 
  filter(keep) %>% 
  select(-keep)

test_3_dup_ids <- test_3 %>% 
  group_by(data_pull_date,
           unique_identifier) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(unique_identifier) %>% 
  unique()

unique_3 <- test_3 %>% 
  filter(!unique_identifier %in% test_3_dup_ids)

dupes_3 <- test_3 %>% 
  filter(unique_identifier %in% test_3_dup_ids)
nrow(dupes_3) / nrow(ca_clean) * 100


# take higher salary for remaining dups
test_4 <- dupes_3 %>% 
  group_by(data_pull_date,
           unique_identifier) %>% 
  filter(base_salary == max(base_salary)) %>% 
  ungroup()

test_4_dup_ids <- test_4 %>% 
  group_by(data_pull_date,
           unique_identifier) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(unique_identifier) %>% 
  unique()

unique_4 <- test_4 %>% 
  filter(!unique_identifier %in% test_4_dup_ids)

dupes_4 <- test_4 %>% 
  filter(unique_identifier %in% test_4_dup_ids)
nrow(dupes_4) / nrow(ca_clean) * 100


# if someone works for two different agencies, take the first one alphabetically
test_5 <- dupes_4 %>% 
  group_by(data_pull_date,
           unique_identifier) %>% 
  arrange(department_name, 
          .by_group = T) %>% 
  mutate(keep = case_when(
    n_distinct(department_name) > 1 & row_number() == 1 ~ T,
    n_distinct(department_name) > 1 & row_number() != 1 ~ F,
    TRUE ~ T
  )) %>% 
  ungroup() %>% 
  filter(keep) %>% 
  select(-keep) %>% 
  mutate(first_name = case_when(
    unique_identifier == '0880231975024' ~ "juan a",
    TRUE ~ first_name
  )) %>% 
  distinct()

test_5_dup_ids <- test_5 %>% 
  group_by(data_pull_date,
           unique_identifier) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(unique_identifier) %>% 
  unique()

unique_5 <- test_5 %>% 
  filter(!unique_identifier %in% test_5_dup_ids)

dupes_5 <- test_5 %>% 
  filter(unique_identifier %in% test_5_dup_ids)
nrow(dupes_5) / nrow(ca_clean) * 100


# just take first row now 
test_6 <- dupes_5 %>% 
  group_by(data_pull_date,
           unique_identifier) %>% 
  arrange(department_name,
          facility_name,
          .by_group = T) %>% 
  slice(1) %>% 
  ungroup()

test_6_dup_ids <- test_6 %>% 
  group_by(data_pull_date,
           unique_identifier) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(unique_identifier) %>% 
  unique()

unique_6 <- test_6 %>% 
  filter(!unique_identifier %in% test_6_dup_ids)

dupes_6 <- test_6 %>% 
  filter(unique_identifier %in% test_6_dup_ids)
nrow(dupes_6) / nrow(ca_clean) * 100


ca_deduped <- rbind(unique_0,
                    select(unique_1, -keep),
                    unique_2,
                    unique_3,
                    unique_4,
                    unique_5,
                    unique_6)


# About 1% of the first names are different within an ID -- a number of these
#   appear to be due to the (non)inclusion of middle initials
check_first_names <- ca_deduped %>% 
  group_by(unique_identifier) %>% 
  filter(n_distinct(first_name) > 1)

# taking the dup rows where the first name is longer (due to inclusion of MI)
ca_deduped_2 <- ca_deduped %>% 
  group_by(unique_identifier) %>% 
  mutate(keep = case_when(
    n_distinct(first_name) > 1 & 
      str_length(first_name) == max(str_length(first_name)) ~ T,
    n_distinct(first_name) > 1 & 
      str_length(first_name) != max(str_length(first_name)) ~ F,
    TRUE ~ T
  )) %>% 
  ungroup() %>% 
  filter(keep)

# filter to the more frequent first names, within duplicates
more_frequent_first_name_dups <- ca_deduped_2 %>% 
  group_by(unique_identifier) %>% 
  filter(n_distinct(first_name) > 1) %>% 
  count(first_name) %>% 
  filter(n == max(n)) %>% 
  ungroup()

dup_first_name_ids <- ca_deduped_2 %>% 
  group_by(unique_identifier) %>% 
  filter(n_distinct(first_name) > 1) %>% 
  pull(unique_identifier)

ca_deduped_2_dup_firstnames <- ca_deduped_2 %>% 
  filter(unique_identifier %in% dup_first_name_ids)
ca_deduped_2_no_dup_firstnames <- ca_deduped_2 %>% 
  filter(!unique_identifier %in% dup_first_name_ids)

ca_deduped_2_dup_firstnames <- semi_join(ca_deduped_2_dup_firstnames,
                                         more_frequent_first_name_dups,
                                         by = c("unique_identifier",
                                                "first_name"))

ca_deduped_3 <- rbind(ca_deduped_2_dup_firstnames,
                      ca_deduped_2_no_dup_firstnames) %>% 
  select(-keep)

check_first_names_2 <- ca_deduped_3 %>% 
  group_by(unique_identifier) %>% 
  filter(n_distinct(first_name) > 1)
nrow(check_first_names_2) / nrow(ca_deduped_3)
# ok that is fine.


# parse names
suffixes <- "\\ssr$|\\sjr$|\\sii$|\\siii$|\\siv$|\\sv$|\\siiii$"

ca_names <- ca_deduped_3 %>% 
  mutate(across(c(first_name, 
                last_name),
                ~str_remove_all(.,
                                "[:punct:]"))) %>% 
  mutate(suffix = str_trim(str_extract(last_name, 
                              suffixes))) %>% 
  mutate(last_name = str_trim(str_remove(last_name, 
                                         suffixes))) %>% 
  mutate(middle_initial = case_when(
    str_detect(first_name, 
               "\\s[:alpha:]{1}$") ~ str_extract(first_name, 
                                                 "\\s[:alpha:]{1}$"),
    TRUE ~ NA_character_
  )) %>% 
  mutate(first_name = case_when(
    str_detect(first_name, 
               "\\s[:alpha:]{1}$") ~ str_remove(first_name, 
                                                "\\s[:alpha:]{1}$"),
    TRUE ~ first_name
  )) %>% 
  select(data_pull_date, 
         unique_identifier, 
         first_name, 
         middle_initial, 
         last_name,
         suffix,
         everything())



# pull in info from readme
ca_full <- ca_names %>% 
  mutate(int_indicator = case_when(
    int_indicator == 'x' ~ TRUE,
    TRUE ~ FALSE
  )) %>% 
  mutate(class_type_name = case_when(
    is.na(class_type) ~ "civil service",
    class_type == "c" ~ "California State University",
    class_type == "d" ~ "California Dep. of Human Resources Statutory",
    class_type == "e" ~ "California Conservation Corp",
    class_type == "f" ~ "California Dep. of Human Resources Exempt",
    class_type == "j" ~ "Judicial Council",
    class_type == "l" ~ "Statutory",
    class_type == "m" ~ "Maritime Academy",
    class_type == "p" ~ "Ceta Exempt",
    class_type == "s" ~ "State Personnel Board Exempt"
  )) %>% 
  mutate(career_exec_assignment = case_when(
    cea_indicator == "x" ~ TRUE,
    TRUE ~ FALSE
  )) %>% 
  select(-cea_indicator) %>% 
  mutate(time_base = case_when(
    time_base == 'ft' ~ "full time",
    time_base == 'int' ~ "intermittent",
    time_base == 'ind' ~ "indeterminate",
    is.na(time_base) ~ NA_character_,
    TRUE ~ as.character(round(as.numeric(str_sub(time_base,
                                   1,
                                   3)) / as.numeric(str_sub(time_base,
                                                            4,
                                                            6)),
                              2))
  )) %>% 
  mutate(tenure = case_when(
    tenure == "p" ~ "permanent",
    tenure == "l" ~ "limited term",
    tenure == "t" ~ "temporary authorization",
    tenure == "c" ~ "career executive assignment",
    tenure == "e" ~ "emergency",
    tenure == "r" ~ "retired annuitant"
  )) %>% 
  mutate(salary_per = case_when(
    salary_per == "m" ~ "Monthly",
    salary_per == "d" ~ "Daily",
    salary_per == "h" ~ "Hourly",
    salary_per == "y" ~ "Yearly",
    salary_per == "u" ~ "Unit",
    salary_per == "o" ~ "Other"
  )) %>% 
  mutate(safety_member = case_when(
    safety_member == "mis" ~ "miscellaneous",
    safety_member == "ind" ~ "industrial",
    safety_member == "pat" ~ "patrol",
    safety_member == "pof" ~ "peace officer or firefighter",
    safety_member == "saf" ~ "safety",
    safety_member == "no" ~ "none"
  )) %>% 
  mutate(oasdi = case_when(
    oasdi == "yes" ~ "social security and medicare",
    oasdi == "med" ~ "medicare only",
    oasdi == "no" ~ "neither social security nor medicare"
    )) %>% 
  mutate(on_leave = case_when(
    `on-leave_indicator` == 'x' ~ TRUE,
    TRUE ~ FALSE
  )) %>% 
  select(-`on-leave_indicator`)

# cant figure out that warning -- but seems irrelevant. time_base has no
#   additional NAs before and after mutate()
sum(is.na(ca_full$time_base))
  
# add in collective bargaining info from here: 
# https://www.calhr.ca.gov/Pay%20Scales%20Library/PS_Sec_02.pdf#search=CBID%20KEY
# more detail on the website and in the document in the CA data folder -- 
#   definitely necessary if you are looking at collective bargaining stuff
sort(unique(ca_full$cbid))
ca_bargaining <- ca_full %>% 
  mutate(cbid_name = case_when(
    str_sub(cbid, 1, 1) == "m" ~ "excluded, managerial",
    str_sub(cbid, 1, 1) == "s" ~ "excluded, supervisory",
    str_sub(cbid, 1, 1) == "r" ~ "excluded, rank and file",
    str_sub(cbid, 1, 1) == "c" ~ "confidential",
    str_sub(cbid, 1, 1) == "e" ~ "exempt/excluded",
  ))

# save data
saveRDS(ca_bargaining, 
        "../data/state-employee-data/CA/clean/ca-18-22-premerge.rds")


min(ca_full$data_pull_date)
max(ca_full$data_pull_date)



