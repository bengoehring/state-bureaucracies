# This file shows the partisan composition of a number of different states
library(tidyverse)
library(lubridate)
library(BeRn)
set_my_theme()

# read in data
va_personnel_raw <- readRDS("../data/state-employee-data/VA/clean/va-19-22-premerge.rds")
ct_personnel_raw <- readRDS("../data/state-employee-data/CT/clean/ct-15-22-premerge.rds")
ar_personnel_raw <- readRDS("../data/state-employee-data/AR/clean/ar-19-22-premerge.rds")
ms_personnel_raw <- readRDS("../data/state-employee-data/MS/clean/ms-19-21-premerge.rds")
il_personnel_raw <- readRDS("../data/state-employee-data/IL/clean/il-11-22-premerge.rds")


# create dataset containing period, id, agency, title
va_personnel_clean <- va_personnel_raw %>% 
  filter(str_detect(agency_name, 
                    "court|legisl|college|univ",
                    negate = T)) %>% 
  mutate(employee_id = str_c("va_", dedupe.ids)) %>% 
  mutate(state = "VA") %>% 
  filter(!employee_category %in% c("faculty", 
                                   "university staff")) %>%
  select(period = data_source_date, 
         employee_id,
         state,
         agency_name,
         first_name,
         last_name, 
         suffix)

ct_personnel_clean <- ct_personnel_raw %>% 
  filter(str_detect(agency,
                    "ccc|univ|uconn|csu|judicial",
                    negate = T)) %>% 
  mutate(month_year = floor_date(check_dt, 
                                 unit = 'month')) %>% 
  group_by(month_year, 
           emplid_empl_rcd) %>% 
  arrange(check_dt, .by_group = T) %>% 
  slice_head() %>% 
  ungroup() %>% 
  mutate(employee_id = str_c("ct_", emplid_empl_rcd)) %>% 
  mutate(state = "CT") %>% 
  mutate(middle_initial = str_sub(middle_name, 1, 1)) %>% 
  select(month_year,
         employee_id,
         state,
         agency,
         first_name, 
         middle_initial, 
         last_name,
         suffix = name_suffix,
         sex) %>% 
  rename(period = month_year)


ar_personnel_clean <- ar_personnel_raw %>% 
  mutate(employee_id = str_c("ar_", dedupe.ids)) %>% 
  mutate(state = "AR") %>% 
  mutate(sex = str_sub(gender, 1, 1)) %>% 
  select(period = month_year,
         employee_id,
         state,
         agency = personnel_area_description,
         first_name, 
         middle_initial,
         last_name,
         suffix,
         sex)


ms_personnel_clean <- ms_personnel_raw %>% 
  filter(str_detect(agency_name,
                    "court",
                    negate = T)) %>% 
  mutate(employee_id = str_c("ms_", as.character(person_id))) %>% 
  mutate(state = "MS") %>%
  mutate(sex = str_sub(sex, 1, 1)) %>% 
  select(period = month_observed,
         employee_id,
         state,
         agency = agency_name,
         first_name, 
         middle_initial,
         last_name, 
         suffix, 
         sex)


il_personnel_clean <- il_personnel_raw %>% 
  mutate(employee_id = str_c("il_", person_id)) %>% 
  mutate(year_date = ymd(str_c(year, "-01-01"))) %>% # setting this to calendar year but need to check if FY
  mutate(state = "IL") %>% 
  mutate(agency = str_c(agency, "-", agency_division)) %>%
  select(period = year_date, 
         employee_id,
         state,
         agency,
         first_name,
         last_name,
         suffix)

# read in voter files ####
va_voter_raw <- readRDS("../data/voter-data/unmerged-voter-files/VA-unmerged.rds")
ct_voter_raw <- readRDS("../data/voter-data/unmerged-voter-files/CT-unmerged.rds")
ar_voter_raw <- readRDS("../data/voter-data/unmerged-voter-files/AR-unmerged.rds")
ms_voter_raw <- readRDS("../data/voter-data/unmerged-voter-files/MS-unmerged.rds")
il_voter_raw <- readRDS("../data/voter-data/unmerged-voter-files/IL-unmerged.rds")


# for checking multibyte
for (i in 1:nrow(il_voter_raw)) {
  if(contains_multibyte_string(il_voter_raw$Voters_LastName[i])) {
    print(il_voter_raw$Voters_LastName[i])
  }
}

# for filtering out the handful in IL
IL_multibytes <- vector("logical", 
                        length = nrow(il_voter_raw))
for (i in 1:nrow(il_voter_raw)) {
  IL_multibytes[i] <- contains_multibyte_string(il_voter_raw$Voters_LastName[i]) | 
    contains_multibyte_string(il_voter_raw$Voters_FirstName[i])
}

il_voter_raw <- il_voter_raw[!IL_multibytes, ]
         
# just a couple in other states
ct_voter_raw <- ct_voter_raw %>% 
  mutate(Voters_MiddleName = str_replace(Voters_MiddleName, 
                                         "\xfd", 
                                         ""))
ms_voter_raw <- ms_voter_raw %>% 
  mutate(Voters_MiddleName = str_replace(Voters_MiddleName, 
                                         "xe3\x8", 
                                         ""))  


# condense voter files to unique merge variables
# summarize dataset to just the variables I am merging on

summarize_voter_file <- function(dataset,
                                 ...) {
  out <- dataset %>% 
    select(..., Parties_Description) %>%
    mutate(across(where(is.character),
                  ~str_trim(str_squish(tolower(.))))) %>% 
    mutate(dem = if_else(Parties_Description == 'democratic', 
                         1, 
                         0)) %>% 
    mutate(rep = if_else(Parties_Description == 'republican',
                         1,
                         0)) %>% 
    mutate(oth = if_else(dem == 0 & rep == 0,
                         1,
                         0)) %>% 
    group_by(...) %>% 
    summarise(across(c(dem, 
                       rep,
                       oth),
                     ~sum(., 
                          na.rm = T) / n(),
                     .names = str_c("{.col}", 
                                    "_share"))) %>% 
    ungroup()
    
  return(out)
}

va_voter_sum <- summarize_voter_file(va_voter_raw, 
                                     Voters_FirstName, 
                                     Voters_LastName,
                                     Voters_NameSuffix)

ct_voter_sum <- summarize_voter_file(ct_voter_raw, 
                                     Voters_FirstName,
                                     Voters_MiddleName,
                                     Voters_LastName,
                                     Voters_NameSuffix,
                                     Voters_Gender) %>% 
  mutate(Voters_MiddleInitial = str_sub(Voters_MiddleName, 1, 1)) 

ar_voter_sum <- summarize_voter_file(ar_voter_raw, 
                                     Voters_FirstName,
                                     Voters_MiddleName,
                                     Voters_LastName,
                                     Voters_NameSuffix,
                                     Voters_Gender) %>% 
  mutate(Voters_MiddleInitial = str_sub(Voters_MiddleName, 1, 1))

ms_voter_sum <- summarize_voter_file(ms_voter_raw, 
                                     Voters_FirstName,
                                     Voters_MiddleName,
                                     Voters_LastName,
                                     Voters_NameSuffix,
                                     Voters_Gender) %>% 
  mutate(Voters_MiddleInitial = str_sub(Voters_MiddleName, 1, 1))

il_voter_sum <- summarize_voter_file(il_voter_raw, 
                                     Voters_FirstName, 
                                     Voters_LastName,
                                     Voters_NameSuffix)


# now need to merge these with summarized versions of the personnel files for merge
summarize_personnel_file <- function(dataset, 
                                     ...) {
  out <- dataset %>% 
    select(employee_id, 
           ...) %>% 
    distinct()
  return(out)
}

il_personnel_sum <- summarize_personnel_file(il_personnel_clean,
                                             first_name, 
                                             last_name,
                                             suffix)
ct_personnel_sum <- summarize_personnel_file(ct_personnel_clean,
                                             first_name, 
                                             middle_initial,
                                             last_name,
                                             suffix,
                                             sex)
ms_personnel_sum <- summarize_personnel_file(ms_personnel_clean,
                                             first_name, 
                                             middle_initial,
                                             last_name,
                                             suffix,
                                             sex)
va_personnel_sum <- summarize_personnel_file(va_personnel_clean,
                                             first_name, 
                                             last_name,
                                             suffix)
ar_personnel_sum <- summarize_personnel_file(ar_personnel_clean,
                                             first_name, 
                                             middle_initial,
                                             last_name,
                                             suffix,
                                             sex)


# now need to merge summarized datasets together and merge with full dataset
il_merge_1 <- left_join(il_personnel_sum,
                        il_voter_sum, 
                        by = c("first_name" = "Voters_FirstName",
                               "last_name" = "Voters_LastName",
                               "suffix" = "Voters_NameSuffix")) %>% 
  select(employee_id, 
         dem_share, 
         rep_share, 
         oth_share)



va_merge_1 <- left_join(va_personnel_sum,
                        va_voter_sum, 
                        by = c("first_name" = "Voters_FirstName",
                               "last_name" = "Voters_LastName",
                               "suffix" = "Voters_NameSuffix")) %>% 
  select(employee_id, 
         dem_share, 
         rep_share, 
         oth_share)


ms_merge_1 <- left_join(ms_personnel_sum,
                        ms_voter_sum, 
                        by = c("first_name" = "Voters_FirstName",
                               "middle_initial" = "Voters_MiddleInitial",
                               "last_name" = "Voters_LastName",
                               "suffix" = "Voters_NameSuffix",
                               "sex" = "Voters_Gender")) %>% 
  select(employee_id, 
         dem_share, 
         rep_share, 
         oth_share)


ar_merge_1 <- left_join(ar_personnel_sum,
                        ar_voter_sum, 
                        by = c("first_name" = "Voters_FirstName",
                               "middle_initial" = "Voters_MiddleInitial",
                               "last_name" = "Voters_LastName",
                               "suffix" = "Voters_NameSuffix",
                               "sex" = "Voters_Gender")) %>% 
  select(employee_id, 
         dem_share, 
         rep_share, 
         oth_share)

ct_merge_1 <- left_join(ct_personnel_sum,
                        ct_voter_sum, 
                        by = c("first_name" = "Voters_FirstName",
                               "middle_initial" = "Voters_MiddleInitial",
                               "last_name" = "Voters_LastName",
                               "suffix" = "Voters_NameSuffix",
                               "sex" = "Voters_Gender")) %>% 
  select(employee_id, 
         dem_share, 
         rep_share, 
         oth_share)

# Now merge the files back to the full personnel files
ct_merge_final <- left_join(ct_personnel_clean,
                            ct_merge_1,
                            by = "employee_id")
ar_merge_final <- left_join(ar_personnel_clean,
                            ar_merge_1,
                            by = "employee_id")
ms_merge_final <- left_join(ms_personnel_clean,
                            ms_merge_1,
                            by = "employee_id")
va_merge_final <- left_join(va_personnel_clean,
                            va_merge_1,
                            by = "employee_id")
il_merge_final <- left_join(il_personnel_clean,
                            il_merge_1,
                            by = "employee_id")

# now need to assign partisanship affiliation to each person based on prob
# This is necessary because of the existence of multiple names in the voter 
# file. This function assigns the partisan affiliation that is the most probable.
assign_partisanship <- function(dataset) {
  
  simple_cases <- dataset %>% 
    filter(rep_share == 1 | dem_share == 1 | oth_share == 1) %>% 
    mutate(max_share = 1)
  
  multiple_names <- dataset %>%
    filter(!employee_id %in% simple_cases$employee_id) %>% 
    rowwise() %>% 
    mutate(max_share = max(c_across(dem_share:oth_share))) %>% 
    ungroup()
  
  out <- rbind(simple_cases, 
               multiple_names)
  
  out_long <- out %>% 
    pivot_longer(c(dem_share, 
                   oth_share,
                   rep_share),
                 names_to = "party",
                 values_to = "share") %>% 
    mutate(party = str_remove(party, 
                              "_share")) %>% 
    filter(max_share == share)
  
  # deal with remaining dups
  # we see these people more than once bc they have multiple partisan identities
  dup_ids <- out_long %>% 
    group_by(period, employee_id) %>% 
    filter(n() > 1) %>% 
    pull(employee_id) %>% 
    unique()
    
  unique_df <- out_long %>% 
    filter(!employee_id %in% dup_ids)
  
  dup_df <- out_long %>% 
    filter(employee_id %in% dup_ids) %>% 
    group_by(employee_id) %>% 
    mutate(party_final = sample(party, size = 1)) %>% 
    ungroup() %>% 
    group_by(employee_id, 
             period) %>% 
    slice_sample(n = 1) %>% 
    select(-party,
           party = party_final) %>% 
    ungroup()
  
  final <- rbind(unique_df, dup_df)
    
  return(final)
}


il_merge_final_2 <- assign_partisanship(il_merge_final)
ct_merge_final_2 <- assign_partisanship(ct_merge_final)
va_merge_final_2 <- assign_partisanship(va_merge_final)
ar_merge_final_2 <- assign_partisanship(ar_merge_final)
ms_merge_final_2 <- assign_partisanship(ms_merge_final)



# combine state files
va_merge_final_2 <- rename(va_merge_final_2, 
                           agency = agency_name)

quick_fun <- function(datset) {
  out <- datset %>% 
    select(period, employee_id, agency, state, party)
  return(out)
}
all_partisans <-rbind(
  quick_fun(il_merge_final_2),
  quick_fun(ct_merge_final_2),
  quick_fun(va_merge_final_2),
  quick_fun(ar_merge_final_2),
  quick_fun(ms_merge_final_2)
)


# get n employees per state per period
ct_n_employees <- ct_personnel_clean %>% 
  group_by(period) %>% 
  summarise(n_employees = n()) %>% 
  mutate(state = "CT") %>% 
  ungroup()

il_n_employees <- il_personnel_clean %>% 
  group_by(period) %>% 
  summarise(n_employees = n()) %>% 
  mutate(state = "IL") %>% 
  ungroup()

ms_n_employees <- ms_personnel_clean %>% 
  group_by(period) %>% 
  summarise(n_employees = n()) %>% 
  mutate(state = "MS") %>% 
  ungroup()

va_n_employees <- va_personnel_clean %>% 
  group_by(period) %>% 
  summarise(n_employees = n()) %>% 
  mutate(state = "VA") %>% 
  ungroup()

ar_n_employees <- ar_personnel_clean %>% 
  group_by(period) %>% 
  summarise(n_employees = n()) %>% 
  mutate(state = "AR") %>% 
  ungroup()

n_employees <- rbind(ct_n_employees, 
                     il_n_employees, 
                     va_n_employees,
                     ar_n_employees,
                     ms_n_employees)

# number of partisans in each state
n_partisans <- all_partisans %>% 
  group_by(state, period, party) %>% 
  summarise(n = n()) %>% 
  ungroup()

# find last instance of id
all_partisans_departs <- all_partisans %>% 
  group_by(state, 
           employee_id) %>% 
  arrange(period, .by_group = T) %>% 
  slice_tail(n = 1) %>% 
  ungroup() %>% 
  group_by(state, 
           party,
           period) %>% 
  summarise(n_last_period = n()) %>% 
  ungroup()


final_departures <- left_join(n_partisans, 
                              all_partisans_departs, 
                              by = c("state", 
                                     "period", 
                                     "party"))
final_departures <- final_departures %>% 
  group_by(state) %>% 
  mutate(last_period = if_else(period == max(period), 1, 0)) %>% 
  ungroup()


# dropping a few periods that are wrong or have next to no data
final_departures <- final_departures %>% 
  filter(period != ymd("1920-12-01")) %>% 
  filter(!(state == "CT" & period == ymd("2014-10-01"))) %>% 
  filter(!(state == "CT" & period == ymd("2014-12-01"))) %>% 
  filter(!(state == "VA" & period == ymd("2019-04-04"))) %>% 
  filter(!(state == "VA" & period == ymd("2021-01-04")))



final_departures %>% 
  mutate(state = case_when(
    state == "AR" ~ "Arkansas",
    state == "MS" ~ "Mississippi",
    state == "VA" ~ "Virginia",
    state == "IL" ~ "Illinois",
    state == "CT" ~ "Connecticut"
  )) %>% 
  filter(party != "oth") %>% 
  filter(last_period == 0) %>% 
  filter(period > "2010-01-01") %>% 
  mutate(state_party = str_c(state, party)) %>% 
  ggplot() + 
  geom_line(aes(x = period, 
                y = n_last_period / n,
                color = party)) + 
  facet_wrap(~state, 
             nrow = 5) + 
  labs(x = NULL, 
       y = "Share of Employees that Depart") + 
  scale_y_continuous(breaks = seq(0, .1, .02),
                     labels = str_c(seq(0, 10, 2), "%")) + 
  scale_color_manual(values = c("blue", 
                                "red")) + 
  theme(legend.position = "none",
        strip.text.x = element_text(size = 5, face = "bold"),
        axis.text = element_text(size = 5),
        text = element_text(family = "Fira Sans"),
        axis.title.y = element_text(size = 6),
        panel.spacing = unit(.05, "lines"),
        strip.background = element_blank(),
        panel.spacing.x = unit(0,"line"),
        axis.line.x.bottom = element_blank(),
        axis.ticks.x = element_line(colour = "#dedddd")) 
  
ggsave("../presentations/exec-politics-mini/images/partisan-departures.pdf",
       device = cairo_pdf,
       height = 3.6,
       width = 4.2)

ct_personnel_clean %>% 
  filter(state == "CT") %>% 
  select(period) %>% 
  arrange(period)


