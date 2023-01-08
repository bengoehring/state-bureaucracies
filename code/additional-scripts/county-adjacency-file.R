library(tidyverse)

adjacency_raw <- read_tsv("https://www2.census.gov/geo/docs/reference/county_adjacency.txt",
                          col_names = c("county_1", "fips_1", "county_2", "fips_2"))

adjacency_clean <- adjacency_raw %>% 
  fill(county_1, fips_1, 
       .direction = 'down') %>% 
  mutate(state_1 = str_extract(county_1, ", \\w{2}$")) %>% 
  mutate(state_2 = str_extract(county_2, ", \\w{2}$")) %>% 
  mutate(county_1 = str_remove(county_1, state_1)) %>% 
  mutate(county_2 = str_remove(county_2, state_2)) %>% 
  mutate(across(c(state_1, state_2),
                ~str_remove(., ", "))) %>% 
  mutate(across(c(county_1, county_2), 
                ~str_remove(tolower(.), " county"))) %>% 
  select(state_1, county_1, fips_1, state_2, county_2, fips_2)

write_rds(adjacency_clean, 
          "../data/other/county-adjacency-file.rds")

