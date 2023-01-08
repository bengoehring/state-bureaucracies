library(tidyverse)

# read in data outputted from the code that prob assigns 
il_out <- readRDS("../data/state-employee-data/IL/clean/il-fastlink-post.rds")

il_out_matches_list <- map(1:length(il_out), 
                           ~pluck(il_out, ., 2))
il_out_matches_df <- il_out_matches_list %>% 
  bind_rows()

il_out_fl_list <- map(1:length(il_out), 
                      ~pluck(il_out, ., 1))


# remaining to dedupe
# Note that not all of these are true dupes. They also represent, in some cases, 
#   instances of the same person switching jobs during the course of the 
#   year. 

# some instances of back pay -- pulling out and adding to annual salary
back_pay <- il_out_matches_df %>% 
  filter(str_detect(agency_division, "back pay")) %>% 
  select(dedupe.ids, 
         year, 
         back_pay = ytd_gross)

il_out_matches_df <- il_out_matches_df %>% 
  filter(!str_detect(agency_division, "back pay"))

il_out_matches_backpay <- left_join(il_out_matches_df, 
                                    back_pay, 
                                    by = c("dedupe.ids", 
                                           "year"))

remaining_dupes <- il_out_matches_backpay %>% 
  group_by(dedupe.ids,
           year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  pull(dedupe.ids)

unique_il_data <- il_out_matches_df %>% 
  filter(!dedupe.ids %in% remaining_dupes)
remaining_dups_df <- il_out_matches_df %>% 
  filter(dedupe.ids %in% remaining_dupes)
nrow(remaining_dups_df) / nrow(il_out_matches_df) * 100


boop <- remaining_dups_df %>% 
  group_by(agency) %>% 
  summarise(n = n())
length(unique(remaining_dups_df$dedupe.ids)) / length(unique(il_out_matches_df$dedupe.ids))




# If an ID only switched jobs once, saying they are the same person. 
# If an ID works in different counties in back to back years -- different person
dup_il_data_2 <- remaining_dups_df %>%
  group_by(dedupe.ids) %>% 
  arrange(year, 
          work_county,
          .by_group = T) %>% 
  mutate(same_person = case_when(
    sum(duplicated(year)) == 1 ~ 1,
    n_distinct(agency_division) == 1 ~ 1,
    n_distinct(agency) == 1 ~ 1,
    !(year == lead(year) & 
      lead(year, n = 2) == lead(year, n = 3) & 
      work_county != lead(work_county) & 
      lead(work_county, n = 2) != lead(work_county, n = 3)) ~ 1,
    TRUE ~ 0
  )) %>% 
  ungroup()

switched_once <- dup_il_data_2 %>% 
  filter(same_person == 1)

unique_il_data <- bind_rows(unique_il_data,
                        switched_once)
remaining_dups_df <- dup_il_data_2 %>% 
  filter(same_person == 0)

# someone cannot hold two different jobs in different counties in back to back 
#   years. 
county_condition <- dup_il_data_2 %>% 
  filter(same_person == 0) %>% 
  group_by(dedupe.ids) %>% 
  arrange(year, 
          work_county,
          .by_group = T) %>% 
  mutate(different_person = case_when(
    year == lead(year) & 
      lead(year) == lead(year, n = 2) &
      work_county != lead(work_county) & 
      lead(work_county) != lead(work_county, n = 2) ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(different_person_sum = sum(different_person)) %>% 
  ungroup()

county_condition_misses <- county_condition %>% 
  filter(different_person_sum == 0)

county_condition_hits <- county_condition %>% 
  filter(different_person_sum >= 1)



boop <- dup_il_data_2 %>% 
  group_by(dedupe.ids, year) %>% 
  summarise(n = n())






# Now it is necessary to figure out whether the duplicate ids are actually different
#   people or the same person switching jobs within a year. Below I count, for each id, 
#   the number of years that contain more than one observation. If, for a given id, 
#   there are more than 3 instances of duplicates observations within a year, I say
#   that this is a true duplicate. If not, I say that the id
#   represents a person switching jobs. 

likely_real_dupes <- dup_il_data %>% 
  group_by(dedupe.ids) %>% 
  summarise(n_dup_obs = sum(duplicated(year))) %>% 
  ungroup() %>% 
  filter(n_dup_obs > 3) %>% 
  pull(dedupe.ids) %>% 
  unique()

dup_il_data_true_pos <- dup_il_data %>% 
  filter(dedupe.ids %in% likely_real_dupes)
dup_il_data_false_pos <- dup_il_data %>% 
  filter(!dedupe.ids %in% likely_real_dupes)

# for the ids where we observe more than 3 duplicate observations, I assign new
#   id codes using the agency or agency division. 
dup_il_data_true_pos <- dup_il_data_true_pos %>% 
  group_by(dedupe.ids) %>% 
  mutate(dedupe.ids = case_when(
    n_distinct(agency) > 1 ~ str_c(dedupe.ids, 
                                   "_", 
                                   agency),
    n_distinct(agency_division) > 1 ~ str_c(dedupe.ids,
                                            "_",
                                            agency_division), 
    TRUE ~ dedupe.ids
  )) %>% 
  ungroup()

# For the remaining dupes (~300 observations): This means that, for a given id, there are more than 3
#   instances of duplicates but all observations are within the same agency
#   and the same agency division. If this is the case, I am just going to say that the
#   id in fact represents one person. 

# bind everything back together
all_deduped_data <- rbind(unique_il_data,
                          dup_il_data_false_pos,
                          dup_il_data_true_pos
                          )
         


# any missing values by id?
all_deduped_data %>% 
  group_by(dedupe.ids) %>% 
  summarise(na_first = sum(is.na(first_name), na.rm = T) / n(),
            na_last = sum(is.na(last_name), na.rm = T) / n(),
            na_vet = sum(is.na(military_veteran), na.rm = T) / n(),
            na_suffix = sum(is.na(suffix), na.rm = T) / n()) %>% 
  filter(if_any(na_first:na_suffix,
             ~ . > 0 & . < 1))
# just one issue with suffix -- filling. 
all_deduped_data <- all_deduped_data %>% 
  group_by(dedupe.ids) %>% 
  fill(suffix, 
       .direction = 'downup') %>% 
  ungroup()


# arrange columns and save ####
il_final <- all_deduped_data %>% 
  select(year,
         person_id = dedupe.ids,
         first_name,
         last_name, 
         suffix, 
         military_veteran,
         everything()
  )

saveRDS(il_final, 
        file = "../data/state-employee-data/IL/clean/il-11-22-premerge.rds")


