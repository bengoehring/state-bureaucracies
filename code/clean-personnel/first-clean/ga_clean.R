# ga_clean.R
# Description: This file reads in and cleans the GA personnel data. 
# Notes: 
##########################################################################

library(tidyverse)

ga_raw <- read_csv("../data/state-employee-data/GA/raw/SalaryTravelDataExport_ALL_YEARS.txt")


# basic cleaning ####
colnames(ga_raw) <- tolower(colnames(ga_raw))

#for (i in 1:nrow(ga_raw)) {
#  if(BeRn::contains_multibyte_string(ga_raw$name[i])) {
#    print(ga_raw$name[i])
#  }
#}
ga_raw_2 <- ga_raw %>% 
  mutate(name = case_when(
    str_detect(name, 
               "OZ,JAVIER E") ~ "MUNOZ,JAVIER E",
    str_detect(name, 
               "EZ,ISAAC F") ~ "NUNEZ,ISAAC F",
    str_detect(name, 
               "DUKES") & str_detect(name, 
                                     ",MELANIE") ~ "DUKES,MELANIE",
    str_detect(name, 
               "DURDIN") & str_detect(name, 
                                     ",LEA") ~ "DURDIN,LEA",
    str_detect(name, 
               "LIZEK") & str_detect(name, 
                                      ",ALISON") ~ "LIZEK,ALISON",
    str_detect(name, 
               "^M") & str_detect(name, 
                                     ",JOS") ~ "MENDEZ,JOSE",
    str_detect(name, 
               "^DICKERSON,MAIYA GLENA") ~ "DICKERSON,MAIYA GLENAE",
    str_detect(name, 
               "^ROUSSELOT DE SAINT") ~ "ROUSSELOT DE SAINT CERAN,TINA M",
    str_detect(name, 
               "^RICH") & str_detect(name, 
                                     "KRISTEN") ~ "RICHARD,KRISTEN",
    TRUE ~ name
  ))

ga_clean <- ga_raw_2 %>% 
  mutate(across(where(is.character),
                ~str_trim(str_squish(tolower(.)))))

# parse names
suffixes <- "\\ssr$|\\sjr$|\\sii$|\\siii$|\\siv$|\\sv$|\\siiii$"

# some of the names include parantheses (some but not all are maiden names)
#   dropping here as it throws off the regex parsing 
ga_names <- ga_clean %>% 
  filter(name != "student employee") %>% 
  mutate(name = str_remove_all(name, 
                               "\\(.*\\)")) %>% 
  mutate(name = str_remove(name, 
                           "\\($")) %>% 
  mutate(name = str_remove(name, 
                           "\\(.*$")) %>% 
  mutate(name = str_squish(str_trim(name)))


ga_names <- ga_names %>% 
  mutate(last_name = str_extract(name, 
                                 "^.*(?=,)")) %>% 
  mutate(first_name = str_extract(name, 
                                  "(?<=,).*")) %>% 
  mutate(middle_name_initial = case_when(
    str_detect(first_name, 
               "\\s\\w{1}$") ~ str_extract(first_name, 
                                           "\\s\\w{1}$"),
    str_detect(first_name, 
               "\\s") ~ str_extract(first_name, 
                                    "\\s.*$"),
    TRUE ~ NA_character_
  )) %>% 
  mutate(first_name = case_when(
    !is.na(middle_name_initial) ~ str_remove(first_name, 
                                             middle_name_initial),
    TRUE ~ first_name
  ))%>% 
  mutate(across(c(first_name, 
                  middle_name_initial, 
                  last_name),
                ~str_trim(str_squish((str_remove_all(.,
                                                     "[:punct:]")))))) %>% 
  mutate(suffix = str_extract(last_name, 
                              suffixes)) %>% 
  mutate(last_name = str_remove(last_name, 
                                suffixes)) %>% 
  mutate(last_name_join = case_when(
    is.na(suffix) ~ last_name, 
    TRUE ~ str_c(last_name, 
                 " ", 
                 suffix)
  ))

# titles include some information (at least in some years) about the general 
#   level of an employee. 
# See for more information: https://open.ga.gov/glossary.html

# prepare for fastlink
ga_titles <- ga_names %>% 
  mutate(job_level = case_when(
    str_detect(title, "\\(\\w{2}\\)$") ~ str_extract(title, "\\(\\w{2}\\)$"),
    str_detect(title, "\\(\\w{2}$") ~ str_extract(title, "\\(\\w{2}$"),
    str_detect(title, "\\s\\w{2}$") ~ str_extract(title, "\\s\\w{2}$"),
    str_detect(title, "-\\w{2}$") ~ str_extract(title, "-\\w{2}$"),
  )) %>% 
  mutate(job_level_desc = case_when(
    str_detect(job_level, "wl") ~ "working level",
    str_detect(job_level, "el") ~ "entry level",
    str_detect(job_level, "al") ~ "advanced level",
    str_detect(job_level, "sp") ~ "supervisor",
    TRUE ~ NA_character_
  )) %>% 
  mutate(title = case_when(
    !is.na(job_level_desc) ~ str_remove(title, 
                                        "\\(\\w{2}\\)$|\\(\\w{2}$|\\s\\w{2}$|-\\w{2}$"),
    TRUE ~ title
  ))

# dropping county board of education employees 
# also dropping university system -- just to make the merges more manageable
ga_state_employees <- ga_titles %>% 
  filter(!str_detect(organization, 
                    "institute of technology|county board of education|school district|board of education|university|college"))


# save for fastlink
ga_fastlink <- ga_state_employees %>% 
  select(-name) %>% 
  select(fiscal_year, 
         first_name, 
         middle_name_initial, 
         last_name,
         last_name_join,
         suffix,
         everything())

saveRDS(ga_fastlink, 
        "../data/state-employee-data/GA/clean/ga-fastlink-pre.rds")





