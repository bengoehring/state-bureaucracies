library(tidyverse)
library(readxl)

# this dataset is the 2020 file from here: 
# https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html

csa_raw <- read_xls("../data/other/cbsa-csa-raw.xls", 
                    skip = 2)

colnames(csa_raw) <- tolower(colnames(csa_raw))
colnames(csa_raw) <- str_replace_all(colnames(csa_raw), 
                                     "/| ",
                                     "_")

csa_clean <- csa_raw %>% 
  mutate(across(where(is.character),
                tolower)) %>% 
  mutate(county_county_equivalent = str_remove(county_county_equivalent,
                                               " county"))

write_rds(csa_clean, 
          "../data/other/cbsa-csa-clean.rds")

