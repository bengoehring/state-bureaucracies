library(tidyverse)
library(sf)
library(tigris)


# read in data ####
ar_personnel <- readRDS("../data/state-employee-data/AR/clean/ar-15-22-premerge.rds")
ar_voters <- readRDS("../data/voter-data/unmerged-voter-files/AR-unmerged.rds")

ar_prison_locations <- read_csv("../data/correctional-officers/prison-geocodings/ar-prisons.csv")
ar_county_map <- counties(state = "AR")


# initial cleaning ####
# filter down to just correctional officers
ar_correctional <- ar_personnel %>% 
  filter(personnel_area_description %in% ar_prison_locations$facility)


# some small cleaning of voter file
ar_voters_clean <- ar_voters %>% 
  filter(!is.na(residence_addresses_longitude)) %>%
  st_as_sf(coords = c("residence_addresses_longitude",
                      "residence_addresses_latitude"),
           crs = 4326) %>% 
  st_transform(3814) %>% 
  mutate(race = case_when(
    ethnicgroups_ethnicgroup1desc == "likely african-american" ~ "black/not hispanic origin",
    ethnicgroups_ethnicgroup1desc == "east and south asian" ~ "asian or pacific islander",
    ethnicgroups_ethnicgroup1desc == "european" ~ "white/not hispanic origin",
    ethnicgroups_ethnicgroup1desc == "hispanic and portuguese" ~ "hispanic",
    TRUE ~ NA_character_
  )) %>% 
  mutate(parties_description = case_when(
    parties_description %in% c('green', 
                               'reform',
                               'libertarian') ~ "other",
    TRUE ~ parties_description
  ))


ar_prison_locations <- ar_prison_locations %>% 
  st_as_sf(coords = c("lon", 
                      "lat"),
           crs = 4326) %>% 
  st_transform(3814)


# find voters within X km of a facility 
find_voters <- function(kilometers,
                        prison_location_data = ar_prison_locations,
                        voter_data = ar_voters_clean) {
  
  radius_data <- st_buffer(prison_location_data,
                           dist = kilometers * 1000)
  
  # matrix of booleans indicating whether a voter is in the buffer
  voters_in_radius_bools <- st_intersects(voter_data, 
                                          radius_data,
                                          sparse = F)
  
  # assign columns of booleans to a dataframe indicating whether the 
  #   voter is within the radius of the given facility
  voters_in_radius_bools <- voters_in_radius_bools %>% 
    as_tibble(.name_repair = "unique")
  
  colnames(voters_in_radius_bools) <- radius_data$facility
  colnames(voters_in_radius_bools) <- str_replace_all(colnames(voters_in_radius_bools), 
                                                      " |-",
                                                      "_")
  
  # bind it back to voter data
  voters_in_radius_full <- cbind(voter_data, 
                                 voters_in_radius_bools)
  
  # pivot data longer and only select hits 
  voters_in_radius_long <- voters_in_radius_full %>% 
    pivot_longer(cols = all_of(colnames(voters_in_radius_bools)),
                 names_to = "facility",
                 values_to = "in_radius") %>% 
    filter(in_radius) %>% 
    select(-in_radius) %>% 
    mutate(radius_distance_km = kilometers) %>% 
    st_drop_geometry()
  
  
  return(voters_in_radius_long)
}

voters_within_100km <- find_voters(100)
voters_within_50km <- find_voters(50)
voters_within_25km <- find_voters(25)


# set up correctinal data to be merged ####
ar_correctional_join <- ar_correctional %>% 
  mutate(personnel_area_description = str_replace_all(personnel_area_description,
                                       " |-",
                                       "_")) %>% 
  mutate(gender = str_sub(gender, 
                          1, 
                          1))
  
# simplifying the data to only the unique values of the merging variables
ar_correctional_join_unique <- ar_correctional_join %>% 
  select(dedupe.ids, 
         first_name, 
         middle_initial, 
         last_name_join, 
         facility = personnel_area_description, 
         gender, 
         race = ethnic_origin) %>% 
  distinct()


summarize_voter_file <- function(dataset,
                                 ...) {
  out <- dataset %>% 
    select(..., 
           parties_description) %>%
    mutate(dem = if_else(parties_description == 'democratic', 
                         1, 
                         0)) %>% 
    mutate(rep = if_else(parties_description == 'republican',
                         1,
                         0)) %>% 
    mutate(non = if_else(parties_description == 'non-partisan',
                         1,
                         0)) %>% 
    mutate(oth = if_else(parties_description == 'other',
                         1,
                         0)) %>%
    group_by(...) %>% 
    summarise(across(c(dem, 
                       rep,
                       oth, 
                       non),
                     ~sum(., 
                          na.rm = T) / n(),
                     .names = str_c("{.col}", 
                                    "_share"))) %>% 
    ungroup()
  
  return(out)
}

voters_within_100km_unique <- summarize_voter_file(voters_within_100km,
                                                   first_name, 
                                                   middle_initial, 
                                                   last_name_join, 
                                                   facility, 
                                                   gender, 
                                                   race)
voters_within_50km_unique <- summarize_voter_file(voters_within_50km,
                                                  first_name, 
                                                  middle_initial, 
                                                  last_name_join, 
                                                  facility, 
                                                  gender, 
                                                  race)
voters_within_25km_unique <- summarize_voter_file(voters_within_25km,
                                                  first_name, 
                                                  middle_initial, 
                                                  last_name_join, 
                                                  facility, 
                                                  gender, 
                                                  race) 

fastlink_input <- list("ar_correctional_join" = ar_correctional_join_unique, 
                       "voters_within_100km" = voters_within_100km_unique, 
                       "voters_within_50km" = voters_within_50km_unique,
                       "voters_within_25km" = voters_within_25km_unique)

saveRDS(fastlink_input, 
        "../data/correctional-officers/merge-inputs/ar-fastlink-input.rds")






