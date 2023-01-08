library(tidyverse)
library(lubridate)
library(readxl)

# read in data outputted from the code that prob assigns 
co_out <- readRDS("../data/state-employee-data/CO/clean/co-fastlink-post.rds")

co_out_matches_list <- map(1:length(co_out), 
                           ~pluck(co_out, ., 2))
co_out_matches_df <- co_out_matches_list %>% 
  bind_rows()

co_out_fl_list <- map(1:length(co_out), 
                      ~pluck(co_out, ., 1))


# per correspondence from the state, any code not contained in the following
#   spreadsheet is non-classified. Assigning here so I dont have to rerun fastlink

job_codes <- read_xlsx("../data/state-employee-data/CO/raw/FY19-FY22_Job_Class_Tables.xlsx")
job_codes_vector <- job_codes %>% 
  mutate(across(.fns = ~str_trim(str_squish(str_to_lower(.))))) %>% 
  select(-contains("title")) %>% 
  pivot_longer(everything(),
               names_to = "code_year",
               values_to = "class") %>% 
  select(-code_year) %>% 
  pull(class) %>% 
  unique()

co_out_matches_df <- co_out_matches_df %>% 
  mutate(classification_status = case_when(
    str_detect(class, 
               "^p1a1") ~ "non-classified (temporary aide)",
    class %in% job_codes_vector ~ "classified",
    !class %in% job_codes_vector ~ "non-classified",
    TRUE ~ NA_character_
  )) %>% 
  distinct()
  


# Need to remove dups so unique by person-period
remaining_dups <- co_out_matches_df %>% 
  group_by(source_date, 
           dedupe.ids) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_0 <- co_out_matches_df %>% 
  filter(!dedupe.ids %in% remaining_dups)
dupes_0 <- co_out_matches_df %>% 
  filter(dedupe.ids %in% remaining_dups)
nrow(dupes_0) / nrow(co_out_matches_df) * 100


# for remaining duplicates, append middle initial to id
test_1 <- dupes_0 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(middle_initial)))

test_1_dup_ids <- test_1 %>% 
  group_by(dedupe.ids,
           source_date) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_1 <- test_1 %>% 
  filter(!dedupe.ids %in% test_1_dup_ids)

dupes_1 <- test_1 %>% 
  filter(dedupe.ids %in% test_1_dup_ids)
nrow(dupes_1) / nrow(co_out_matches_df) * 100

# for remaining duplicates, append first name to id
test_2 <- dupes_1 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(first_name)))

test_2_dup_ids <- test_2 %>% 
  group_by(dedupe.ids,
           source_date) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_2 <- test_2 %>% 
  filter(!dedupe.ids %in% test_2_dup_ids)

dupes_2 <- test_2 %>% 
  filter(dedupe.ids %in% test_2_dup_ids)
nrow(dupes_2) / nrow(co_out_matches_df) * 100

# for remaining duplicates, append last name to id
test_3 <- dupes_2 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(last_name_join)))

test_3_dup_ids <- test_3 %>% 
  group_by(dedupe.ids,
           source_date) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_3 <- test_3 %>% 
  filter(!dedupe.ids %in% test_3_dup_ids)

dupes_3 <- test_3 %>% 
  filter(dedupe.ids %in% test_3_dup_ids)
nrow(dupes_3) / nrow(co_out_matches_df) * 100


# for remaining duplicates, append agency info to id
test_4 <- dupes_3 %>% 
  mutate(dedupe.ids = str_c(dedupe.ids,
                            "_",
                            str_replace_na(department)))

test_4_dup_ids <- test_4 %>% 
  group_by(dedupe.ids,
           source_date) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_4 <- test_4 %>% 
  filter(!dedupe.ids %in% test_4_dup_ids)

dupes_4 <- test_4 %>% 
  filter(dedupe.ids %in% test_4_dup_ids)
nrow(dupes_4) / nrow(co_out_matches_df) * 100


# some weird instances of people having an fte of zero but a salary
test_4.1 <- dupes_4 %>% 
  group_by(dedupe.ids, 
           source_date) %>% 
  mutate(keep = case_when(
    n() > 1 & fte == 0 & salary == min(salary) ~ T,
    n() > 1 & fte == 0 & salary != min(salary) ~ F,
    TRUE ~ T
  )) %>% 
  filter(keep) %>% 
  select(-keep)

test_4.1_dup_ids <- test_4.1 %>% 
  group_by(dedupe.ids,
           source_date) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_4.1 <- test_4.1 %>% 
  filter(!dedupe.ids %in% test_4.1_dup_ids)

dupes_4.1 <- test_4.1 %>% 
  filter(dedupe.ids %in% test_4.1_dup_ids)
nrow(dupes_4.1) / nrow(co_out_matches_df) * 100

# given the number of variables I was able to match on I feel pretty confident
#   at this point that any dups are actually the same person being shown more
#   than once for one reason or another. It appears this is usually due to changing
#   jobs or someone holding multiple part-time positions

# test 5 below is going to 


#   IF FTE is less than 100 we are going to add up the salary figures and keep the 
#   row with the higher salary. Otherwise just selecting higher paying row 

test_5 <- dupes_4.1 %>% 
  group_by(source_date,
           dedupe.ids) %>% 
  mutate(keep_row = case_when(
    n() > 1 & sum(duplicated(fte)) > 0 & salary == max(salary) ~ 1,
    n() > 1 & sum(duplicated(fte)) > 0 & salary != max(salary) ~ 0,
    n() > 1 & fte == max(fte) ~ 1,
    n() > 1 & fte != max(fte) ~ 0,
    n() > 1 & salary == max(salary) ~ 1,
    n() > 1 & salary != max(salary) ~ 0,
    n() > 1 & fte_adjusted_annual_salary == max(fte_adjusted_annual_salary) ~ 1,
    n() > 1 & fte_adjusted_annual_salary != max(fte_adjusted_annual_salary) ~ 0,
    TRUE ~ 1
  )) %>%
  mutate(fte = case_when(
    n() > 1 & sum(fte, 
                  na.rm = T) < 100 ~ sum(fte, 
                                         na.rm = T),
    TRUE ~ fte
  )) %>% 
  mutate(salary = case_when(
    n() > 1 & sum(fte, 
                  na.rm = T) < 100 ~ sum(salary,
                                         na.rm = T),
    TRUE ~ salary
  )) %>% 
  mutate(fte_adjusted_annual_salary = case_when(
    n() > 1 & sum(fte, 
                  na.rm = T) < 100 ~ sum(fte_adjusted_annual_salary,
                                         na.rm = T),
    TRUE ~ fte_adjusted_annual_salary
  )) %>% 
  ungroup() %>% 
  filter(keep_row == 1) %>% 
  select(-keep_row)

test_5_dup_ids <- test_5 %>% 
  group_by(dedupe.ids,
           source_date) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_5 <- test_5 %>% 
  filter(!dedupe.ids %in% test_5_dup_ids)

dupes_5 <- test_5 %>% 
  filter(dedupe.ids %in% test_5_dup_ids)
nrow(dupes_5) / nrow(co_out_matches_df) * 100


# just a handful left 
test_6 <- dupes_5 %>% 
  filter(!(dedupe.ids == "2020_26901_s_kathie_gonzalez rodriguez_education" &
           class_title == "substitute")) %>% 
  filter(!(dedupe.ids == "2020_12989_b_alex_bolton_general assembly" & 
            address == "200 e colfax ave rm 271")) %>% 
  filter(!(dedupe.ids == "2020_21300_s_joshua_hammond_corrections" & 
             address == "p. o. box 1010")) %>% 
  mutate(dedupe.ids = case_when(
    dedupe.ids == "2020_10793_j_nicholas_neiman_judicial branch" ~ "2020_10794_j_nicholas_neiman_judicial branch",
    TRUE ~ dedupe.ids
  )) %>% 
  mutate(original_hiredate = case_when(
    dedupe.ids == "2020_10794_j_nicholas_neiman_judicial branch" ~ ymd("2018-12-03"),
    TRUE ~ original_hiredate
  )) %>% 
  mutate(fte = case_when(
    dedupe.ids == "2020_8958_e_shelby_roskop_judicial branch" & n() > 1 ~ sum(fte),
    dedupe.ids == "2020_9337_d_dawn_jose_judicial branch" & n() > 1 ~ sum(fte),
    dedupe.ids == "2020_9390_a_rachel_christophy_judicial branch" & n() > 1 ~ sum(fte),
    dedupe.ids == "2020_9959_a_kristy_berdzar_judicial branch" & n() > 1 ~ sum(fte),
    dedupe.ids == "2020_6081_w_aron_tetzlaff_judicial branch" & n() > 1 ~ sum(fte),
    dedupe.ids == "2020_10794_j_nicholas_neiman_judicial branch" & n() > 1 ~ sum(fte),
    dedupe.ids == "2010_11073_j_velvet_johnson_judicial branch" & n() > 1 ~ sum(fte),
    dedupe.ids == "2020_10961_d_stephanie_dinnocenzo_judicial branch" & n() > 1 ~ sum(fte),
    TRUE ~ fte
  )) %>% 
  mutate(fte_adjusted_annual_salary = case_when(
    dedupe.ids == "2020_8958_e_shelby_roskop_judicial branch" & n() > 1 ~ sum(fte_adjusted_annual_salary),
    dedupe.ids == "2020_9337_d_dawn_jose_judicial branch" & n() > 1 ~ sum(fte_adjusted_annual_salary),
    dedupe.ids == "2020_9390_a_rachel_christophy_judicial branch" & n() > 1 ~ sum(fte_adjusted_annual_salary),
    dedupe.ids == "2020_9959_a_kristy_berdzar_judicial branch" & n() > 1 ~ sum(fte_adjusted_annual_salary),
    dedupe.ids == "2020_6081_w_aron_tetzlaff_judicial branch" & n() > 1 ~ sum(fte_adjusted_annual_salary),
    dedupe.ids == "2020_10794_j_nicholas_neiman_judicial branch" & n() > 1 ~ sum(fte_adjusted_annual_salary),
    dedupe.ids == "2010_11073_j_velvet_johnson_judicial branch" & n() > 1 ~ sum(fte_adjusted_annual_salary),
    dedupe.ids == "2020_10961_d_stephanie_dinnocenzo_judicial branch" & n() > 1 ~ sum(fte_adjusted_annual_salary),
    TRUE ~ fte_adjusted_annual_salary
  )) %>% 
  filter(!(dedupe.ids == "2020_8958_e_shelby_roskop_judicial branch" & class_title == "specialist")) %>% 
  filter(!(dedupe.ids == "2020_9337_d_dawn_jose_judicial branch" & class_title == "protective proceeding mon")) %>% 
  filter(!(dedupe.ids == "2020_9390_a_rachel_christophy_judicial branch" & class_title == "court judicial assistant")) %>% 
  filter(!(dedupe.ids == "2020_9959_a_kristy_berdzar_judicial branch" & class_title == "protective proceeding mon")) %>% 
  filter(!(dedupe.ids == "2020_6081_w_aron_tetzlaff_judicial branch" & class_title == "temp-cntr prob officer")) %>% 
  filter(!(dedupe.ids == "2020_10794_j_nicholas_neiman_judicial branch" & class_title == "temp-cntr prob officer")) %>% 
  filter(!(dedupe.ids == "2010_11073_j_velvet_johnson_judicial branch" & address == "p o box 20000")) %>% 
  filter(!(dedupe.ids == "2020_10961_d_stephanie_dinnocenzo_judicial branch" & class_title == "water specialist"))
  
test_6_dup_ids <- test_6 %>% 
  group_by(dedupe.ids,
           source_date) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids) %>% 
  unique()

unique_6 <- test_6 %>% 
  filter(!dedupe.ids %in% test_6_dup_ids)

dupes_6 <- test_6 %>% 
  filter(dedupe.ids %in% test_6_dup_ids)
nrow(dupes_6) / nrow(co_out_matches_df) * 100



co_final <- rbind(unique_0,
                  unique_1,
                  unique_2,
                  unique_3,
                  unique_4,
                  unique_4.1,
                  unique_5,
                  unique_6) %>% 
  select(-blocking_period) %>% 
  select(-original_hire_year) %>% 
  select(-original_hiredate_join) %>% 
  select(source_date,
         dedupe.ids, 
         first_name, 
         middle_initial, 
         middle_name, 
         last_name, 
         original_hire_date = original_hiredate, 
         everything())

saveRDS(co_final, 
        "../data/state-employee-data/CO/clean/co-19-22-premerge.rds")

  










