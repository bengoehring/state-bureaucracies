library(tidyverse)
library(vroom)
library(readxl)

# read in data
ks_files <- list.files("../data/state-employee-data/KS/raw/")
ks_paths <- str_c("../data/state-employee-data/KS/raw/",
                  ks_files)

ks_raw_list <- map(ks_paths,
                   vroom)
names(ks_raw_list) <- ks_files


# note that fiscal year is only available in the 2021 data
ks_raw_df <- ks_raw_list %>% 
  bind_rows(.id = "id") %>% 
  select(-`...7`,
         -`...8`,
         -Fiscal_Year) %>% 
  rename_with(tolower)


# initial cleaning
ks_clean_df <- ks_raw_df %>% 
  mutate(across(where(is.character),
                ~str_trim(str_squish(tolower(.))))) %>% 
  mutate(fiscal_year = str_extract(id, 
                            "\\d{4}")) %>% 
  select(-id) %>% 
  select(fiscal_year, 
         everything())
# total gross pay only available after 2014.

# going to make my life a bit easier and remove legislature and universities here
ks_names <- ks_clean_df %>% 
  filter(!str_detect(agency_name,
                    "univ|legislat|judicial"))



# lets test out the job titles before running fastlink
# per email from the state: 
#Unfortunately, just looking at the title alone you would not be able to know if 
#it was classified or unclassified. However, I have attached a copy of our job 
#code table that includes unclassified and classified titles that can help. Any 
#job code that starts with a number except for ‘0’ is a classified job code. If 
#the Job Code starts with ‘0’ or an alpha-character, then it is an unclassified 
#job code. Hopefully, this will be helpful in going through the spreadsheets.

ks_codes <- read_xlsx("../data/state-employee-data/KS/Job_Code_Table_12.2.22.xlsx")
ks_codes <- ks_codes %>% 
  rename_with(~str_replace_all(tolower(.), 
                               " ",
                               "_")) %>% 
  mutate(job_code_title = str_trim(str_squish(str_to_lower(job_code_title))))

boop <- ks_names %>% 
  select(job_title) %>% 
  distinct()

booper <- anti_join(boop, 
                    ks_codes,
                    by = c("job_title" = "job_code_title"))
nrow(booper) / nrow(boop) * 100

# ask the state 1) if they keep old lists of crosswalks 
#2) if they can provide the OG data with the codes




