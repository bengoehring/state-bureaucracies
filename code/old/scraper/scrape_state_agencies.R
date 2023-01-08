# This file retrieves every state agency for each state listed on openthebooks.com

pacman::p_load(tidyverse,
               rvest,
               xml2,
               urltools)


# retrieve state agencies
state_agency_scraper <- function(state) {
  # retrieve agencies for state
  agency_webpage <- str_c("https://www.openthebooks.com/",
                          state,
                          "-state-employees/?pg=1")
  
  agency_webpage_html <- read_html(agency_webpage)
   
  state_agencies <- agency_webpage_html %>% 
    html_node("body") %>%
    xml_find_all("//*[(@id = 'CT_Main_0_ctl03_drpEmployer')]") %>% 
    html_children() %>% 
    html_text2() 
  
  Sys.sleep(10)
  print(state)
  return(state_agencies)
}

state_names <- str_replace(str_to_lower(state.name), 
                           " ", 
                           "-")
# no data on hawaii or south dakota ?
state_names <- str_subset(state_names, 'hawaii|south-dakota', negate = T)


state_agencies <- map(state_names, state_agency_scraper)
names(state_agencies) <- state_names

state_agencies <- map(1:length(state_agencies),
                      ~str_subset(state_agencies[[.]],
                                  "All",
                                  negate = T))

# Turn this list into a long dataframe with state and agency columns

all_agencies <- flatten_chr(state_agencies)

n_agencies_by_state <- map(state_agencies, length)
all_states <- map2(names(n_agencies_by_state), 
                   n_agencies_by_state,
                   ~rep(.x,
                        times = .y
                   ))
all_states <- flatten_chr(all_states)

state_agencies_tbl <- tibble(state = all_states, 
                             agency = all_agencies)

# Now add year columns for 2017:2021
state_agencies_year <- state_agencies_tbl %>% 
  mutate(`2017` = 2017,
         `2018` = 2018,
         `2019` = 2019,
         `2020` = 2020,
         `2021` = 2021,
  ) %>% 
  pivot_longer(cols = `2017`:`2021`) %>% 
  rename(year = value) %>% 
  select(-name)

save(state_agencies_year,
     file = "../data/state-employees/state_agencies_year.Rdata")


