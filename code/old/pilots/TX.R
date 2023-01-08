library(tidyverse)
library(readxl)
library(vroom)
library(lubridate)
library(furrr)
library(stargazer)
options(future.globals.maxSize = 850*1024^2)
plan(multisession, workers = 8)

devtools::install_github("benfromfargo/BeRn")
BeRn::set_my_theme()

# STATE PERSONNEL FILE ####
tx_raw <- read_xlsx("../data/state-employee-data/TX/Ben Goehring Employee Report - Executive Agencies_FINAL - Copy.xlsx")

# clean column names and create a suffix variable
colnames(tx_raw) <- tolower(colnames(tx_raw))
colnames(tx_raw) <- str_replace_all(colnames(tx_raw), 
                                    " ",
                                    "_")

tx_suffix <- tx_raw %>% 
  mutate(across(c(first_name, 
                  last_name, 
                  mi), 
                tolower)) %>% 
  mutate(suffix = case_when(
    str_detect(last_name, " jr$") ~ "jr",
    str_detect(last_name, " sr$") ~ "sr",
    str_detect(last_name, " ii$") ~ "ii",
    str_detect(last_name, " iii$") ~ "iii",
    str_detect(last_name, " iv$") ~ "iv",
    TRUE ~ NA_character_
  )) %>% 
  mutate(last_name = str_remove(last_name, 
                                " jr$| sr$| ii$| iii$| iv$"))

# statenum appears to uniquely identify individuals, except for a handful
# (268) of cases. 
tx_id <- tx_suffix %>% 
  group_by(statenum, 
           first_name, 
           last_name, 
           suffix,
           mi, 
           ethnicity, 
           gender) %>% 
  mutate(unique_id = cur_group_id()) %>% 
  ungroup()

# Get dataset of just names for merging with the voter file. 
tx_person_ids <- tx_id %>% 
  select(first_name, 
         last_name,
         mi, 
         suffix,
         unique_id) %>% 
  distinct() 

# dropping the 97 full names that appear more than once in the dataset. 
dup_full_names <- tx_person_ids %>% 
  group_by(first_name, 
           last_name, 
           suffix,
           mi) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n > 1) %>% 
  pull(unique_id)

tx_person_ids <- tx_person_ids %>% 
  filter(!unique_id %in% dup_full_names)



# VOTER FILE ####
tx_voter_raw <- vroom("../data/voter-data/TX_final.tsv") 
colnames(tx_voter_raw) <- tolower(colnames(tx_voter_raw))


tx_voter_clean <- tx_voter_raw %>% 
  select(voters_firstname,
         voters_middlename,
         voters_lastname,
         voters_namesuffix,
         parties_description,
         lalvoterid) %>% 
  mutate(voters_middlename = str_remove(voters_middlename, 
                                        "\xe2\xbf\xe2\xbf")) %>% 
  mutate(across(voters_firstname:voters_namesuffix,
                tolower)) %>% 
  mutate(voters_mi = str_sub(voters_middlename, 
                             end = 1))

# need to get distinct by name
# doing this by selecting the party id that is most likely among dup names
tx_voter_dedup <- tx_voter_clean %>% 
  mutate(dem = ifelse(parties_description == "Democratic",
                      1,
                      0)) %>% 
  mutate(rep = ifelse(parties_description == "Republican",
                      1,
                      0)) %>% 
  mutate(non = ifelse(parties_description == "Non-Partisan",
                      1,
                      0)) %>% 
  group_by(voters_firstname,
           voters_mi, 
           voters_lastname,
           voters_namesuffix) %>% 
  summarise(n_names = n(),
            share_dem = sum(dem, na.rm = T) / n(),
            share_rep = sum(rep, na.rm = T) / n(),
            share_non = sum(non, na.rm = T) / n()) 

# Using the probabilities that a given name is associated with a given
# party id, return each voter's party id. 
sample_party_id <- function(rep_prob, 
                            dem_prob, 
                            non_prob) {
  
  out <- sample(c("republican",
                  "democratic",
                  "non-partisan"), 
                size = 1,
                prob = c(rep_prob, 
                         dem_prob,
                         non_prob))
  
  return(out)
}

party_id_out <- map_chr(1:nrow(tx_voter_dedup), 
                        ~sample_party_id(
                          rep_prob = tx_voter_dedup$share_rep[.],
                          dem_prob = tx_voter_dedup$share_dem[.],
                          non_prob = tx_voter_dedup$share_non[.]
                        ))

tx_voter_dedup$party_id_final <- party_id_out





# MERGE PERSONNEL AND VOTER FILE ####
merge_1 <- left_join(tx_person_ids, 
                     tx_voter_dedup,
                     by = c("first_name" ="voters_firstname",
                            "last_name" = "voters_lastname",
                            "mi" = "voters_mi",
                            "suffix" = "voters_namesuffix"))
merge_1_misses <- anti_join(tx_person_ids, 
                            tx_voter_dedup,
                            by = c("first_name" = "voters_firstname",
                                   "last_name" = "voters_lastname",
                                   "mi" = "voters_mi",
                                   "suffix" = "voters_namesuffix"))

# merge back in with full tx personnel file
merge_1 <- merge_1 %>% 
  select(unique_id, 
         party_id_final,
         n_names,
         share_dem,
         share_rep,
         share_non)

merge_2 <- left_join(tx_id, 
                     merge_1, 
                     by = "unique_id")


# Find means and CIs ####
personnel_voters <- merge_2 %>% 
  filter(!is.na(party_id_final)) %>% #dropping misses
  mutate(party_numeric = case_when(
    party_id_final == "republican" ~ 1,
    party_id_final == "democratic" ~ -1,
    party_id_final == "non-partisan" ~ 0,
  )) %>% 
  mutate(norm_salary = (salary - min(salary, na.rm = T)) / (max(salary, na.rm = T) - min(salary, na.rm = T))) 


personnel_voter_means <- personnel_voters %>% 
  group_by(agency_name, 
           month) %>% 
  summarise(agency_pref = mean(party_numeric, 
                               na.rm = T),
            agency_pref_norm = weighted.mean(party_numeric,
                                             norm_salary,
                                             na.rm = T),
            agency_mean_salary = mean(salary, 
                                      na.rm = T),
            n_employees = n(),
            n_reps = sum(party_id_final == 'republican', na.rm = T),
            n_dems = sum(party_id_final == 'democratic', na.rm = T),
            n_nons = sum(party_id_final == 'non-partisan', na.rm = T),
            share_matched = sum(!is.na(party_id_final)) / n()) %>% 
  ungroup()
  

# CALCULATE BOOTSTRAP CIS
# create list of dataframes, where each list contains the personnel from a 
#  given agency in a given month
personnel_voters_grouped_df <- personnel_voters %>% 
  group_by(agency_name, month) %>% 
  nest()

personnel_voters_list <- personnel_voters_grouped_df$data
names(personnel_voters_list) <- str_c(personnel_voters_grouped_df$agency_name, 
                                      "___",
                                      personnel_voters_grouped_df$month)

# Create function to loop through this list, resample each df with replacement,
# and return the mean and weighted mean from each agency/month
my_boot <- function(list_of_dfs) {
  
  # return number of dfs, names of each df (these are the agency/months), 
  # and number of rows in each dataframe. 
  n_dfs <- length(list_of_dfs)
  names_dfs <- names(list_of_dfs)
  nrow_each_df <- map_dbl(list_of_dfs, nrow)
  
  # resample with replacement each of the dataframes
  samples_list <- map(1:n_dfs, 
                      ~ slice_sample(list_of_dfs[[.]],
                                     n = nrow_each_df[.],
                                     replace = T))
  
  # return means from each of the resampled dataframes
  means <- map(1:n_dfs, 
               ~ summarise(samples_list[[.]],
                 mean = mean(.data$party_numeric,
                             na.rm = T),
                 weighted_mean = weighted.mean(.data$party_numeric,
                                               .data$norm_salary,
                                               na.rm = T),
                 mean_salary = mean(.data$salary,
                                    na.rm = T)
               ))
  
  # add ID to each of the returned mean/weighted_mean dataframes
  means <- map(1:n_dfs,
               ~ mutate(means[[.]],
                        id = names_dfs[.]),
               seed = T)
  return(means)
}

ci_out_list <- future_map(1:5000, 
                          ~my_boot(personnel_voters_list),
                          .options = furrr_options(seed = T))

ci_out_df <- ci_out_list %>% 
  flatten() %>% 
  bind_rows()
      
ci_out_df <- ci_out_df %>% 
  mutate(agency_name = str_extract(id, 
                                   '.*___'),
         month = str_extract(id, 
                             "___.*")) %>% 
  mutate(across(c(agency_name,
                  month),
                ~str_remove(., 
                            "___")))

ci_quantiles <- ci_out_df %>% 
  mutate(month = ymd(month)) %>% 
  group_by(agency_name,
           month) %>% 
  summarise(mean_lower_95_bound = quantile(mean, 
                                           .025),
            mean_upper_95_bound = quantile(mean,
                                           .975),
            salary_mean_lower_95_bound = quantile(mean_salary, 
                                                  .025),
            salary_mean_upper_95_bound = quantile(mean_salary,
                                                  .975),
            w_mean_lower_95_bound = quantile(weighted_mean, 
                                             .025, 
                                             na.rm = T),
            w_mean_upper_95_bound = quantile(weighted_mean, 
                                             .975,
                                             na.rm = T))

# LOOK INTO THESE NA VALUES -- SHOULD NOT BE ANY






agency_level_ci <- left_join(personnel_voter_means, 
                             ci_quantiles, 
                             by = c("agency_name", 
                                    "month"))
agency_level_ci_misses <- anti_join(personnel_voter_means, 
                                    ci_quantiles,
                                    by = c("agency_name",
                                           "month"))


# unweighted plot
agency_level_ci %>% 
  filter(month == ymd('2022-07-31')) %>% 
  pivot_longer(cols = c(agency_pref,
                        agency_pref_norm),
               names_to = "value_type") %>% 
  filter(value_type == "agency_pref") %>% 
  ggplot() + 
  geom_point(aes(x = value,
                 y = as.factor(agency_name))) + 
  geom_errorbarh(aes(xmin = mean_lower_95_bound,
                     xmax = mean_upper_95_bound,
                     y = as.factor(agency_name))) + 
  geom_text(aes(x = 1.1, 
                y = as.factor(agency_name),
                label = n_employees)) +
  labs(y = NULL,
       x = "policy preferences",
       title = "Policy Preferences of Texas State Agencies",
       subtitle = "As of July 31, 2022",
       caption = "95% confidence intervals calculated via nonparametric bootstrap with N=5,000.")
ggsave("pilots/plots/tx_unweighted_byagency.pdf",
       height = 7,
       width = 12,
       units = "in",
       device = cairo_pdf)

# weighted plot
agency_level_ci %>% 
  filter(month == ymd('2022-07-31')) %>% 
  pivot_longer(cols = c(agency_pref,
                        agency_pref_norm),
               names_to = "value_type") %>% 
  filter(value_type == "agency_pref_norm") %>% 
  ggplot() + 
  geom_point(aes(x = value,
                 y = as.factor(agency_name))) + 
  geom_errorbarh(aes(xmin = w_mean_lower_95_bound,
                     xmax = w_mean_upper_95_bound,
                     y = as.factor(agency_name))) + 
  geom_text(aes(x = 1.1, 
                y = as.factor(agency_name),
                label = n_employees)) + 
  labs(y = NULL,
       x = "policy preferences",
       title = "Policy Preferences of Texas State Agencies",
       subtitle = "Weighted by Salary, as of July 31, 2022",
       caption = "95% confidence intervals calculated via nonparametric bootstrap with N=5,000.")
ggsave("pilots/plots/tx_weighted_byagency.pdf",
       height = 7,
       width = 12,
       units = "in",
       device = cairo_pdf)


agency_level_ci %>% 
  filter(month == ymd('2022-07-31')) %>% 
  pivot_longer(cols = c(agency_pref,
                        agency_pref_norm),
               names_to = "value_type") %>% 
  mutate(value_type = case_when(
    value_type == "agency_pref" ~ "Unweighted",
    value_type == "agency_pref_norm" ~ "Weighted by salary")) %>% 
  ggplot() +
  geom_point(aes(x = value,
                 y = as.factor(agency_name),
                 size = n_employees,
                 color = value_type)) + 
  scale_x_continuous(limits = c(-1, 1),
                     breaks = seq(-1, 1, .25)) +
  scale_size(breaks = seq(0,4000, 500),
             name = "Number of employees") + 
  scale_color_discrete(name = "Score type") + 
  labs(y = NULL,
       x = "policy preferences",
       title = "Policy Preferences of Texas State Agencies",
       subtitle = "As of July 31, 2022")
ggsave("pilots/plots/tx_weighted-and-unweighted_byagency.pdf",
       height = 7,
       width = 12,
       units = "in",
       device = cairo_pdf)






# CHECK FOR SALARY BIAS BY SEX, RACE, PARTY #####
tx_bias <- tx_id %>% 
  filter(month == ymd('2022-07-31')) %>% 
  select(unique_id,
         gender,
         ethnicity,
         salary,
         class_title,
         class_code,
         hire_date,
         agency_name, 
         agency
         ) %>% 
  mutate(across(c(gender, 
                  ethnicity, 
                  class_code,
                  agency),
                as.factor)) %>% 
  mutate(hire_date = ymd(hire_date)) %>% 
  mutate(months_on_job = round(time_length(ymd('2022-07-31') - hire_date,
                                     unit = 'months'),
                               0))

# holding constant position and years in agency, no evidence of
#   gender wage discrimination
lm_out <- lm(salary ~ gender*months_on_job + class_code,
   data = tx_bias)
  
stargazer(lm_out, 
          #type = 'text',
          out = "pilots/plots/tx_gender_discrimination.html",
          omit = c("class_code"))


# holding constant position and years in agency, no evidence of
#   ethnicity wage discrimination
lm_out_2 <- lm(salary ~ ethnicity*months_on_job + class_code,
             data = tx_bias)

stargazer(lm_out_2, 
          #type = 'text',
          out = "pilots/plots/tx_ethnicity_discrimination.html", 
          omit = c("class_code"))




# capacity plots
agency_level_ci %>% 
  filter(month == ymd('2022-07-31')) %>% 
  ggplot() + 
  geom_point(aes(x = agency_mean_salary,
                 y = as.factor(agency_name))) + 
  geom_errorbarh(aes(xmin = salary_mean_lower_95_bound,
                     xmax = salary_mean_upper_95_bound,
                     y = as.factor(agency_name))) + 
  geom_text(aes(x = 12000, 
                y = as.factor(agency_name),
                label = n_employees)) + 
  scale_x_continuous(limits = c(3000,
                                12000), 
                       breaks = seq(3000, 
                                    12000, 
                                    3000)) + 
  labs(y = NULL,
       x = "monthly mean salary",
       title = "Average Salaries in Texas State Agencies",
       subtitle = "As of July 31, 2022",
       caption = "95% confidence intervals calculated via nonparametric bootstrap with N=5,000.")
ggsave("pilots/plots/tx_monthlysalary_byagency.pdf",
       height = 7,
       width = 12,
       units = "in",
       device = cairo_pdf)  

  






