library(tidyverse)
library(lubridate)
library(readxl)

md_files <- list.files("../data/state-employee-data/MD/raw/")
md_paths <- str_c("../data/state-employee-data/MD/raw/",
                  md_files)

read_md <- function(path) {
  out <- read_csv(path, 
                  col_types = cols(class_code = col_character()),
                  guess_max = 100000,
                  na = c("1900-01-00",
                         "",
                         "na"))
  
  if("hire_date" %in% colnames(out)) {
    out <- out %>% 
      mutate(hire_date = as.character(hire_date))
  }
  if("term_date" %in% colnames(out)) {
    out <- out %>% 
      mutate(term_date = as.character(term_date))
  }
  
  return(out)
}


md_list_raw <- map(md_paths, 
                   read_md)
names(md_list_raw) <- str_extract(md_files, "\\d{4}")

md_df_raw <- md_list_raw %>% 
  bind_rows(.id = "year") %>% 
  select(-c(sheet, 
            id))

md_df_raw <- md_df_raw %>% 
  mutate(class_code = as.numeric(class_code))

# initial cleaning 
md_df_clean <- md_df_raw %>% 
  mutate(across(where(is.character),
                ~str_to_lower(str_trim(str_squish(.))))) %>% 
  mutate(hire_date = case_when(
    hire_date == "0" ~ NA_character_,
    TRUE ~ hire_date
  )) %>% 
  mutate(hire_date = parse_date_time(hire_date, 
                           c("ymd",
                             "mdy"))) %>% 
  mutate(term_date = case_when(
    str_detect(term_date, 
               "t00:00:00z") ~ str_remove(term_date, 
                                          "t00:00:00z"),
    term_date %in% c("true", 
                     "0", 
                      "na") ~ NA_character_,
    TRUE ~ term_date
    )) %>% 
  mutate(term_date = parse_date_time(term_date, 
                                     c("ymd",
                                       "mdy")))

# making life and the merge easier by removing judiciary, colleges, etc here. 
md_df_clean <- md_df_clean %>% 
  filter(!str_detect(organization, 
                     "judiciary|general assembly|university|college"))

# pull in class codes 
# source: https://dbm.maryland.gov/employees/pages/salaryplan.aspx
# match about 90% of codes with the DOT -- 99.9% without the DOT. Reached out to 
#   DOT on 12/14/22 to try and get the DOT-specific codes. 

md_codes <- read_xlsx("../data/state-employee-data/MD/md-job-codes.xlsx")
md_codes <- md_codes %>% 
  rename_with(~str_to_lower(str_replace_all(., 
                                            " ",
                                            "_"))) %>%
  filter(class_code != "Term") %>% 
  mutate(class_code = as.numeric(class_code)) %>% 
  mutate(across(where(is.character),
                ~str_trim(str_squish(str_to_lower(.)))))

md_df_codes <- left_join(md_df_clean, 
                         md_codes,
                         by = "class_code")
nrow(md_df_codes) == nrow(md_df_clean)
nrow(semi_join(md_df_clean, md_codes, by = "class_code")) / nrow(md_df_clean)

# test without the DOT
test_data <- md_df_clean %>% 
  filter(!str_detect(organization, "transportation"))
nrow(semi_join(test_data, md_codes, by = "class_code")) / nrow(test_data)

md_df_codes <- md_df_codes %>% 
  mutate(svc_name = case_when(
    svc == "ps" ~ "professional service",
    svc == "es" ~ "executive service",
    svc == "ss" ~ "skilled service",
    svc == "ms" ~ "managerial service"
  ))


# test hire date
# does not seem all that great for deduping -- lots of dups
boop <- md_df_codes %>% 
  group_by(first_name, 
           last_name,
           suffix) %>% 
  filter(n_distinct(hire_date, 
                    na.rm = T) > 1)


# save for fastlink 
md_fastlink <- md_df_codes %>% 
  mutate(last_name_join = case_when(
    !is.na(suffix) ~ str_c(last_name, 
                           " ",
                           suffix),
    TRUE ~ last_name
  )) %>% 
  select(year, 
         first_name, 
         middle_initial,
         last_name,
         last_name_join,
         suffix,
         everything())

saveRDS(md_fastlink, 
        "../data/state-employee-data/MD/clean/md-fastlink-pre.rds")




