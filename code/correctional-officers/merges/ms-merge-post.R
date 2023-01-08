library(tidyverse)
library(fastLink)
BeRn::set_my_theme()

# read in data ####
fastlink_output <- readRDS("../data/correctional-officers/merge-outputs/ms-fastlink-output.rds")

# return the matches or diagnostic info for a given list of fl objects in 
#   fastlink_output
return_fl_info <- function(fl_name, 
                           type = c("matches",
                                    "diagnostics",
                                    "input_voter_data",
                                    "input_co_data"),
                           input = fastlink_output) {
  
  # extract distance from the name
  distance_km <- str_remove_all(str_extract(fl_name, 
                                            "_\\d+_"), 
                                "_")
  
  if(type == "matches") {
    matches_out <- map(1:length(pluck(input, 
                                      fl_name,
                                      "fl_obj")), 
                       ~pluck(input, 
                              fl_name, 
                              "fl_obj",
                              ., 
                              2))
    
    matches_out_df <- matches_out %>% 
      bind_rows() %>% 
      select(-starts_with("gamma"),
             -posterior) %>% 
      mutate(radius_around_prison = distance_km)

    return(matches_out_df)
  } else if(type == "diagnostics") {
    diagnostics_out <- map(1:length(pluck(input, 
                                          fl_name,
                                          "fl_obj")), 
                           ~pluck(input, 
                                  fl_name, 
                                  "fl_obj",
                                  ., 
                                  1))
    
    diagnostics_out_full <- aggregateEM(em.list = diagnostics_out)
    
    return(diagnostics_out_full)
  } else if(type == "input_voter_data") {
    voter_out <- pluck(input, 
                       fl_name,
                       "voter_data")
    
    return(voter_out)
  } else if(type == "input_co_data") {
    co_out <- pluck(input, 
                    "input_correctional_data")
    
    return(co_out)
  }
}

out_25.9_diagnostics <- return_fl_info("out_25_.9",
                                       "diagnostics")
out_25.9_matches <- return_fl_info("out_25_.9",
                                   "matches")
out_25.9_input_voter <- return_fl_info("out_25_.9",
                                       "input_voter_data")

out_50.9_diagnostics <- return_fl_info("out_50_.9",
                                       "diagnostics")
out_50.9_matches <- return_fl_info("out_50_.9",
                                   "matches")
out_50.9_input_voter <- return_fl_info("out_50_.9",
                                       "input_voter_data")

out_100.9_diagnostics <- return_fl_info("out_100_.9",
                                       "diagnostics")
out_100.9_matches <- return_fl_info("out_100_.9",
                                   "matches")
out_100.9_input_voter <- return_fl_info("out_100_.9",
                                        "input_voter_data")

input_correctional_data <- return_fl_info(NULL, 
                                          "input_co_data")



# Assign partisanship to COs that have multiple matches in voter file ####
#   I do this under different conditions. The main strategy is, if a given 
#   CO matches to more than one voter with different partisan IDs, to impute 
#   whichever partisanship is more common (e.g., if 75% of the voters who match
#   with a CO are Reps, then assign the person to Rep). If the proportions are 
#   equal -- then randomly assign partisanship across the potential, equally
#   likely options. 
# It is less important here given the number of key variables, but I also 
#   assign partisanship under alternative hard bounds where, if a given 
#   CO matches to more than one voter with different partisan IDs including X,
#   then assign it to X partisanship. 


assign_partisanship <- function(input_data, 
                                method = c("highest_probablity",
                                           "hard_code_republican",
                                           "hard_code_democrat",
                                           "hard_code_other",
                                           "hard_code_non_partisan")) {
  
  out <- input_data %>% 
    pivot_longer(cols = dem_share:non_share,
                 names_to = "partisanship",
                 values_to = "partisanship_prop") %>% 
    filter(partisanship_prop > 0)%>% 
    group_by(person_id)
  
  if(method == "highest_probablity") {
    out <- out %>% 
      mutate(keep = case_when(
        n() > 1 & n_distinct(partisanship_prop) == 1 ~ as.numeric(sample(c(1, 
                                                                           rep(0, 
                                                                               n() - 1)))),
        n() > 1 & partisanship_prop == max(partisanship_prop) ~ 1,
        n() > 1 & partisanship_prop != max(partisanship_prop) ~ 0,
        n() == 1 ~ 1,
      )) %>% 
      ungroup() %>% 
      filter(keep == 1)
    
  } else if(method == "hard_code_republican") {
    out <- out %>% 
      mutate(multi_row = case_when(
        n() > 1 & any(str_detect(partisanship, 
                             "rep")) ~ 1, # group includes a republican
        n() > 1 ~ 0, # group does not include a republican 
        TRUE ~ 0 # matched to a single voter
      )) %>% 
      ungroup() %>% 
      filter((multi_row == 1 & str_detect(partisanship, 
                                         "rep")) | multi_row == 0)
    
  } else if(method == "hard_code_democrat") {
    out <- out %>% 
      mutate(multi_row = case_when(
        n() > 1 & any(str_detect(partisanship, 
                             "dem")) ~ 1,
        n() > 1 ~ 0,
        TRUE ~ 0
      )) %>% 
      ungroup() %>% 
      filter((multi_row == 1 & str_detect(partisanship, 
                                          "dem")) | multi_row == 0)

  } else if(method == "hard_code_other") {
    out <- out %>% 
      mutate(multi_row = case_when(
        n() > 1 & any(str_detect(partisanship, 
                             "oth")) ~ 1,
        n() > 1 ~ 0,
        TRUE ~ 0
      )) %>% 
      ungroup() %>% 
      filter((multi_row == 1 & str_detect(partisanship, 
                                          "oth")) | multi_row == 0)
    
  } else if(method == "hard_code_non_partisan") {
    out <- out %>% 
      mutate(multi_row = case_when(
        n() > 1 & any(str_detect(partisanship, 
                             "non")) ~ 1,
        n() > 1 ~ 0,
        TRUE ~ 0
      )) %>% 
      ungroup() %>% 
      filter((multi_row == 1 & str_detect(partisanship, 
                                          "non")) | multi_row == 0)
    
  }
  
  out <- out %>% 
    select(-any_of(c("keep", 
                     "multi_row")),
           -partisanship_prop) %>% 
    mutate(partisanship = str_remove(partisanship, 
                                     "_share"))
  
  return(out)
}

# assign partisanship for 25k radius
out_25.9_matches_main <- assign_partisanship(out_25.9_matches,
                                             "highest_probablity")
out_25.9_matches_dem <- assign_partisanship(out_25.9_matches,
                                            "hard_code_democrat")
out_25.9_matches_rep <- assign_partisanship(out_25.9_matches,
                                            "hard_code_republican")
out_25.9_matches_oth <- assign_partisanship(out_25.9_matches,
                                            "hard_code_other")
out_25.9_matches_non <- assign_partisanship(out_25.9_matches,
                                            "hard_code_non_partisan")

# assign partisanship for 50k radius
out_50.9_matches_main <- assign_partisanship(out_50.9_matches,
                                             "highest_probablity")
out_50.9_matches_dem <- assign_partisanship(out_50.9_matches,
                                            "hard_code_democrat")
out_50.9_matches_rep <- assign_partisanship(out_50.9_matches,
                                            "hard_code_republican")
out_50.9_matches_oth <- assign_partisanship(out_50.9_matches,
                                            "hard_code_other")
out_50.9_matches_non <- assign_partisanship(out_50.9_matches,
                                            "hard_code_non_partisan")

# assign partisanship for 100k radius
out_100.9_matches_main <- assign_partisanship(out_100.9_matches,
                                             "highest_probablity")
out_100.9_matches_dem <- assign_partisanship(out_100.9_matches,
                                            "hard_code_democrat")
out_100.9_matches_rep <- assign_partisanship(out_100.9_matches,
                                            "hard_code_republican")
out_100.9_matches_oth <- assign_partisanship(out_100.9_matches,
                                            "hard_code_other")
out_100.9_matches_non <- assign_partisanship(out_100.9_matches,
                                            "hard_code_non_partisan")




# Combine merged and unmerged data #####
# This will return the same thing as a fastlink-version of 
#   left_join(co_data, voter_data)
bind_un_merged <- function(merged_data, 
                           full_input_data = input_correctional_data) {
  misses <- anti_join(full_input_data, 
                      merged_data)
  
  full_data <- bind_rows(merged_data, 
                         misses) %>% 
    fill(radius_around_prison)
  
  if(nrow(misses) + nrow(merged_data) != nrow(full_data)) {
    stop("something went wrong with the merges")
  }
  
  return(full_data)
}

out_25.9_full <- bind_un_merged(out_25.9_matches_main)
out_25.9_full_dem <- bind_un_merged(out_25.9_matches_dem)
out_25.9_full_rep <- bind_un_merged(out_25.9_matches_rep)
out_25.9_full_oth <- bind_un_merged(out_25.9_matches_oth)
out_25.9_full_non <- bind_un_merged(out_25.9_matches_non)

out_50.9_full <- bind_un_merged(out_50.9_matches_main)
out_50.9_full_dem <- bind_un_merged(out_50.9_matches_dem)
out_50.9_full_rep <- bind_un_merged(out_50.9_matches_rep)
out_50.9_full_oth <- bind_un_merged(out_50.9_matches_oth)
out_50.9_full_non <- bind_un_merged(out_50.9_matches_non)

out_100.9_full <- bind_un_merged(out_100.9_matches_main)
out_100.9_full_dem <- bind_un_merged(out_100.9_matches_dem)
out_100.9_full_rep <- bind_un_merged(out_100.9_matches_rep)
out_100.9_full_oth <- bind_un_merged(out_100.9_matches_oth)
out_100.9_full_non <- bind_un_merged(out_100.9_matches_non)





# impute worst case bounds ####
# All unmatched COs are either republicans, democrats, etc. 
impute_worst_case_bounds <- function(data, 
                                     bounding_party = c("rep",
                                                        "dem",
                                                        "oth",
                                                        "non",
                                                        "no_bounds")) {
  
  if(bounding_party == "no_bounds") { 
    out <- data %>% 
      mutate(worst_case_bounds = bounding_party)
  } else {
    out <- data %>% 
      mutate(partisanship = case_when(
        is.na(partisanship) ~ bounding_party,
        TRUE ~ partisanship
      )) %>% 
      mutate(worst_case_bounds = bounding_party)
    }
  return(out)
}

out_25.9_full <- impute_worst_case_bounds(out_25.9_full, 
                                          "no_bounds")
out_25.9_full_dem_worst <- impute_worst_case_bounds(out_25.9_full_dem,
                                                    "dem")
out_25.9_full_rep_worst <- impute_worst_case_bounds(out_25.9_full_rep,
                                                    "rep")
out_25.9_full_oth_worst <- impute_worst_case_bounds(out_25.9_full_oth,
                                                    "oth")
out_25.9_full_non_worst <- impute_worst_case_bounds(out_25.9_full_non,
                                                    "non")

out_50.9_full <- impute_worst_case_bounds(out_50.9_full, 
                                          "no_bounds")
out_50.9_full_dem_worst <- impute_worst_case_bounds(out_50.9_full_dem,
                                                    "dem")
out_50.9_full_rep_worst <- impute_worst_case_bounds(out_50.9_full_rep,
                                                    "rep")
out_50.9_full_oth_worst <- impute_worst_case_bounds(out_50.9_full_oth,
                                                    "oth")
out_50.9_full_non_worst <- impute_worst_case_bounds(out_50.9_full_non,
                                                    "non")

out_100.9_full <- impute_worst_case_bounds(out_100.9_full, 
                                          "no_bounds")
out_100.9_full_dem_worst <- impute_worst_case_bounds(out_100.9_full_dem,
                                                     "dem")
out_100.9_full_rep_worst <- impute_worst_case_bounds(out_100.9_full_rep,
                                                     "rep")
out_100.9_full_oth_worst <- impute_worst_case_bounds(out_100.9_full_oth,
                                                     "oth")
out_100.9_full_non_worst <- impute_worst_case_bounds(out_100.9_full_non,
                                                     "non")


# save data for UROP ####
# pull in full data and merge to get to get longitudinal data with partisanship
ms_personnel <- readRDS("../data/state-employee-data/MS/clean/ms-15-22-premerge.rds")
ms_correctional <- ms_personnel %>% 
  filter(agency_name %in% c("corr-central ms correctional",
                            "corr-parchman",
                            "corr-south ms correctional")) %>% 
  mutate(agency_name = str_replace_all(agency_name, 
                                       "-|\\s",
                                       "_"))

out_100.9_full_party <- out_100.9_full %>% 
  select(person_id, 
         facility,
         partisanship)

ms_correctional_full_100 <- left_join(ms_correctional, 
                                      out_100.9_full_party,
                                      by = c("person_id", 
                                             "agency_name" = "facility"))
ms_correctional_full_100_misses <- anti_join(ms_correctional, 
                                             out_100.9_full_party,
                                             by = c("person_id", 
                                                    "agency_name" = "facility"))
saveRDS(ms_correctional_full_100, 
        "../../urop/urop-shared/analysis/data/raw/ms-15-22-COs-merged.rds")

# Plots ####
# summary of diagnostic information
sum_25 <- summary(out_25.9_diagnostics,
                  thresholds = seq(.85, .99, .01)) %>% 
                    as_tibble(.name_repair = 'unique') %>% 
  mutate(distance = 25)
sum_50 <- summary(out_50.9_diagnostics,
                  thresholds = seq(.85, .99, .01)) %>% 
  as_tibble(.name_repair = 'unique') %>% 
  mutate(distance = 50)
sum_100 <- summary(out_100.9_diagnostics,
                  thresholds = seq(.85, .99, .01)) %>% 
  as_tibble(.name_repair = 'unique') %>% 
  mutate(distance = 100)

sum_long <- rbind(sum_25, 
                  sum_50,
                  sum_100) %>% 
  pivot_longer(cols = `85%`:Exact,
               names_to = "threshold") %>% 
  mutate(across(c(threshold, 
                  value),
                ~str_remove(., 
                            "%"))) %>% 
  mutate(value = case_when(
    value == "" ~ NA_character_,
    TRUE ~ value
  )) %>% 
  mutate(threshold = case_when(
    threshold == 'Exact' ~ '100',
    TRUE ~ threshold
  )) %>% 
  mutate(across(c(value, 
                  threshold), 
                as.numeric)) %>% 
  rename(diagnostic = `...1`) %>% 
  mutate(across(where(is.character),
                tolower))
    
sum_long %>% 
  filter(diagnostic != 'match count') %>% 
  mutate(diagnostic = case_when(
    diagnostic == "fnr" ~ "False Negative Rate",
    diagnostic == "fdr" ~ "False Discovery Rate",
    diagnostic == "match rate" ~ "Match Rate"
  )) %>% 
  ggplot() + 
  geom_line(aes(x = threshold, 
                y = value,
                color = as.factor(distance))) + 
  geom_vline(aes(xintercept = 90),
             linetype = 'twodash') +
  facet_wrap(~diagnostic,
             nrow = 3) + 
  labs(x = "Matching Threshold",
       y = "Percent",
       color = "Radial Distance Around Prison (km):") + 
  theme(legend.position = "bottom") + 
  scale_x_continuous(labels = c(seq(.85, .95, .05), "Exact")) + 
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, 20))
ggsave("../writing/initial-merge-writeup/plots/merge-diagnostics.pdf",
       device = cairo_pdf,
       height = 8, 
       width = 7,
       units = "in") 

# Partisan Composition of facilities under different bounding assumptions

all_final <- rbind(out_100.9_full,
                   out_100.9_full_dem_worst,
                   out_100.9_full_non_worst,
                   out_100.9_full_oth_worst,
                   out_100.9_full_rep_worst,
                   out_50.9_full,
                   out_50.9_full_dem_worst,
                   out_50.9_full_non_worst,
                   out_50.9_full_oth_worst,
                   out_50.9_full_rep_worst,
                   out_25.9_full,
                   out_25.9_full_dem_worst,
                   out_25.9_full_non_worst,
                   out_25.9_full_oth_worst,
                   out_25.9_full_rep_worst)

bounding_plot_data <- all_final %>% 
  group_by(facility, 
           radius_around_prison, 
           worst_case_bounds) %>% 
  summarise(share_rep = sum(partisanship == "rep", na.rm = T) / n(),
            share_dem = sum(partisanship == "dem", na.rm = T) / n(),
            share_non = sum(partisanship == "non", na.rm = T) / n(),
            share_oth = sum(partisanship == "oth", na.rm = T) / n(),
            share_missing = sum(is.na(partisanship)) / n()) %>% 
  ungroup() %>% 
  pivot_longer(share_rep:share_missing,
               names_to = "partisanship",
               values_to = "share") %>% 
  mutate(worst_case_bounds = case_when(
    worst_case_bounds == "rep" ~ "Republican hard bounds",
    worst_case_bounds == "dem" ~ "Democratic hard bounds",
    worst_case_bounds == "oth" ~ "Other party hard bounds",
    worst_case_bounds == "non" ~ "Non-partisan hard bounds",
    worst_case_bounds == "no_bounds" ~ "No imputation of missing data"
  )) 

bounding_plot_data %>% 
  filter(radius_around_prison == 100) %>% 
  ggplot() + 
  geom_col(aes(x = facility, 
               y = share,
               fill = partisanship),
           position = "dodge",
           width = .5) + 
    facet_wrap(~worst_case_bounds,
               nrow = 5) + 
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("share_dem" = "blue",
                                "share_rep" = "red",
                                "share_missing" = "grey",
                                "share_non" = "purple",
                                "share_oth" = "green"),
                    labels = c("share_dem" = "Democrats",
                               "share_rep" = "Republicans",
                               "share_missing" = "No match",
                               "share_non" = "Non-partisans",
                               "share_oth" = "Other party")) + 
  scale_x_discrete(labels = c("corr_central_ms_correctional" = "Central Mississippi Correctional Facility",
                              "corr_parchman" = "Mississippi State Penitentiary",
                              "corr_south_ms_correctional" = "South Mississippi Correctional Institution")) +
  labs(x = NULL,
       y = "Share of Correctional Officers at Facility",
       fill = NULL)
ggsave("../writing/initial-merge-writeup/plots/bound-results-100km.pdf",
       device = cairo_pdf,
       height = 8, 
       width = 7,
       units = "in") 


bounding_plot_data %>% 
  filter(radius_around_prison == 50) %>% 
  ggplot() + 
  geom_col(aes(x = facility, 
               y = share,
               fill = partisanship),
           position = "dodge",
           width = .5) + 
  facet_wrap(~worst_case_bounds,
             nrow = 5) + 
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("share_dem" = "blue",
                               "share_rep" = "red",
                               "share_missing" = "grey",
                               "share_non" = "purple",
                               "share_oth" = "green"),
                    labels = c("share_dem" = "Democrats",
                               "share_rep" = "Republicans",
                               "share_missing" = "No match",
                               "share_non" = "Non-partisans",
                               "share_oth" = "Other party")) + 
  scale_x_discrete(labels = c("corr_central_ms_correctional" = "Central Mississippi Correctional Facility",
                              "corr_parchman" = "Mississippi State Penitentiary",
                              "corr_south_ms_correctional" = "South Mississippi Correctional Institution")) +
  labs(x = NULL,
       y = "Share of Correctional Officers at Facility",
       fill = NULL)
ggsave("../writing/initial-merge-writeup/plots/bound-results-50km.pdf",
       device = cairo_pdf,
       height = 8, 
       width = 7,
       units = "in") 


bounding_plot_data %>% 
  filter(radius_around_prison == 25) %>% 
  ggplot() + 
  geom_col(aes(x = facility, 
               y = share,
               fill = partisanship),
           position = "dodge",
           width = .5) + 
  facet_wrap(~worst_case_bounds,
             nrow = 5) + 
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("share_dem" = "blue",
                               "share_rep" = "red",
                               "share_missing" = "grey",
                               "share_non" = "purple",
                               "share_oth" = "green"),
                    labels = c("share_dem" = "Democrats",
                               "share_rep" = "Republicans",
                               "share_missing" = "No match",
                               "share_non" = "Non-partisans",
                               "share_oth" = "Other party")) + 
  scale_x_discrete(labels = c("corr_central_ms_correctional" = "Central Mississippi Correctional Facility",
                              "corr_parchman" = "Mississippi State Penitentiary",
                              "corr_south_ms_correctional" = "South Mississippi Correctional Institution")) +
  labs(x = NULL,
       y = "Share of Correctional Officers at Facility",
       fill = NULL)
ggsave("../writing/initial-merge-writeup/plots/bound-results-25km.pdf",
       device = cairo_pdf,
       height = 8, 
       width = 7,
       units = "in")   


# compare results of main data across different distances
bounding_plot_data %>% 
  filter(worst_case_bounds == "No imputation of missing data") %>% 
  filter(partisanship == "share_missing") %>% 
  mutate(radius_around_prison = case_when(
    radius_around_prison == 25 ~ "a25",
    radius_around_prison == 50 ~ "b50",
    radius_around_prison == 100 ~ "c100"
  )) %>% 
  ggplot() + 
  geom_col(aes(x = facility, 
               fill = radius_around_prison,
               y = share),
           position = 'dodge') + 
    labs(fill = "Radial Distance Around Prison (km):",
         y = "Share of Correctional Officers Unable to be Merged",
         x = NULL)+ 
  theme(legend.position = "bottom") +
  scale_fill_discrete(labels = c("a25" = 25,
                                 "b50" = 50,
                                 "c100" = 100)) +
    scale_x_discrete(labels = c("corr_central_ms_correctional" = "Central Mississippi Correctional Facility",
                                "corr_parchman" = "Mississippi State Penitentiary",
                                "corr_south_ms_correctional" = "South Mississippi Correctional Institution")) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, .2))
ggsave("../writing/initial-merge-writeup/plots/share-missing-by-facility.pdf",
       device = cairo_pdf,
       height = 8, 
       width = 7,
       units = "in")    



# compare voter and correctional officer partisanship
co_100_share <- all_final %>% 
  filter(radius_around_prison == 100 & worst_case_bounds == 'no_bounds') %>% 
  filter(!is.na(partisanship)) %>% 
  group_by(facility) %>% 
  summarise(share_rep = sum(partisanship == "rep", na.rm = T) / n(),
            share_dem = sum(partisanship == "dem", na.rm = T) / n(),
            share_non = sum(partisanship == "non", na.rm = T) / n(),
            share_oth = sum(partisanship == "oth", na.rm = T) / n()) %>% 
  ungroup() %>% 
  pivot_longer(share_rep:share_oth,
               names_to = "partisanship",
               values_to = "share") %>% 
  filter(partisanship  != "share_oth") %>% 
  mutate(type = "correctional officers")
  
voter_100_share <- out_100.9_input_voter %>% 
  group_by(facility) %>% 
  summarise(share_rep = sum(rep_share) / n(),
            share_dem = sum(dem_share) / n(),
            share_non = sum(non_share) / n(),
            share_oth = sum(oth_share) / n()) %>% 
  ungroup() %>% 
  pivot_longer(share_rep:share_oth,
               names_to = "partisanship",
               values_to = "share") %>% 
  mutate(type = "voters") %>% 
  filter(partisanship != "share_oth")

share_plot_data <- rbind(voter_100_share, 
                         co_100_share)

share_plot_data %>% 
  mutate(partisanship = case_when(
    partisanship == "share_dem" ~ "Democratic",
    partisanship == "share_rep" ~ "Republican",
    partisanship == "share_non" ~ "Non-partisan"
  )) %>% 
  ggplot() + 
  geom_col(aes(x = facility, 
               y = share, 
               fill = type),
           position = 'dodge') + 
  facet_wrap(~partisanship,
             nrow = 3) + 
  scale_x_discrete(labels = c("corr_central_ms_correctional" = "Central Mississippi Correctional Facility",
                              "corr_parchman" = "Mississippi State Penitentiary",
                              "corr_south_ms_correctional" = "South Mississippi Correctional Institution")) +
  labs(x = NULL,
       y = NULL,
       fill = NULL) +
  theme(legend.position = 'bottom')
ggsave("../writing/jacob-memo/plots/party-facility.pdf",
       device = cairo_pdf,
       height = 8, 
       width = 7,
       units = "in")

# now compare composition of voters and COs by race
co_race_facility <- input_correctional_data %>% 
  group_by(facility, 
           race) %>% 
  summarise(n_race = n()) %>% 
  ungroup() %>% 
  group_by(facility) %>% 
  mutate(share = n_race / sum(n_race)) %>% 
  ungroup() %>% 
  filter(race %in% c("african american", 
                     "white, non-hispanic")) %>% 
  mutate(type = "correctional officers")

voter_race_facility <- out_100.9_input_voter %>% 
  filter(!is.na(race)) %>% 
  group_by(facility, 
           race) %>% 
  summarise(n_race = n()) %>% 
  ungroup() %>% 
  group_by(facility) %>% 
  mutate(share = n_race / sum(n_race)) %>% 
  ungroup()%>% 
  filter(race %in% c("african american", 
                     "white, non-hispanic")) %>% 
  mutate(type = "voters")

plot_race_facility_data <- rbind(voter_race_facility,
                                 co_race_facility)

plot_race_facility_data %>% 
  ggplot() + 
  geom_col(aes(x = facility, 
               y = share, 
               fill = type),
           position = 'dodge') + 
  facet_wrap(~race,
             nrow = 2) + 
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, .25)) +
  labs(x = NULL, 
       y = NULL,
       fill = NULL) +
  scale_x_discrete(labels = c("corr_central_ms_correctional" = "Central Mississippi Correctional Facility",
                              "corr_parchman" = "Mississippi State Penitentiary",
                              "corr_south_ms_correctional" = "South Mississippi Correctional Institution")) +
  theme(legend.position = 'bottom')
ggsave("../writing/jacob-memo/plots/race-facility.pdf",
       device = cairo_pdf,
       height = 8, 
       width = 7,
       units = "in")

# small differences in Ns here due to handful of different 
#   merge variables for a given id in the personnel file. 
# remember this has not been merged back to dynamic model yet
