# Scrape employees by state, year, and agency

pacman::p_load(tidyverse,
               rvest,
               xml2,
               urltools)


# Number of state/agency/years to run
n_runs <- 200


# load agencies for each state
load("../data/state-employees/state_agencies_n_employees.Rdata")

# load the employee file in progress
load("../data/state-employees/full_state_employee_file.Rdata")


# Check if any state/agency/years contain more than 10000 employees. IF so, 
#   note it and set n_results to 10,000. Openthebooks caps results at 10,000

check_n_employees <- state_agencies_n_employees %>% 
  filter(n_results > 10000)

if(nrow(check_n_employees) > 0) {
  state_agencies_n_employees <- state_agencies_n_employees %>% 
    mutate(n_results = case_when(
      n_results > 10000 ~ 10000, 
      TRUE ~ n_results
    ))
  
  warning("Some state/agency/years have more than 10000 employees.")
}


# Openthebooks lists 100 employees per page, so creating the value for iterating
#   through the pages
state_agencies_n_employees <- state_agencies_n_employees %>%
  mutate(n_pages = case_when(
    n_results < 100 ~ 1,
    n_results %% 100 == 0 ~ n_results / 100,
    is.na(n_results) ~ NA_real_,
    TRUE ~ (n_results %/% 100) + 1
  ))



# function to scrape a given state's employees
state_employee_scraper <- function(state,
                                   agency,
                                   year,
                                   n_pages) {
    
    # Now loop through all of the pages for the agency/year, scraping 100 
    #   employees at a time.
    tryCatch({
      agency_output <- vector('list', 
                              length = n_pages)
      for(i in 1:n_pages) {
        
        Sys.sleep(sample(15:45, 1))
        
        webpage <- str_c("https://www.openthebooks.com/",
                         state,
                         "-state-employees/?",
                         "Year_S=",
                         year,
                         "&Emp_S=",
                         url_encode(agency),
                         "&pg=",
                         i)
        
        webpage_html <- read_html(webpage)
        
        # extract table from webpage
        output_table <- webpage_html %>% 
          html_node("body") %>% 
          html_children() %>% 
          xml_find_all("//table[@class='employer-detail-table']") %>% 
          html_table() %>% 
          pluck(1)
        
        # append data to output list
        output_table <- output_table %>% 
          mutate(state = state, 
                 function_year = year, 
                 agency = agency, 
                 condition = NA_character_,
                 notes = NA_character_)
        
        agency_output[[i]] <- output_table
        
        print(str_c(state, 
                    " ",
                    year, 
                    ": ",
                    agency,
                    " -- ",
                    "completed page: ", 
                    i))
      }
      
      return(agency_output)
    },
    # Warning, Error, and Message handling ####
    
    warning = function(w) {
      
      print(conditionMessage(w))
      
      return(tibble(Year = NA_real_,
                    Employer = NA_character_,
                    Name = NA_character_,
                    Title = NA_character_,
                    `Annual Wages` = NA_character_,
                    Source = NA_character_,
                    state = state,
                    function_year = year, 
                    agency = agency, 
                    condition = 'warning',
                    notes = conditionMessage(w)))
    },
    error = function(e) {
      
      print(conditionMessage(e))
      
      return(tibble(Year = NA_real_,
                    Employer = NA_character_,
                    Name = NA_character_,
                    Title = NA_character_,
                    `Annual Wages` = NA_character_,
                    Source = NA_character_,
                    state = state,
                    function_year = year, 
                    agency = agency, 
                    condition = 'warning',
                    notes = conditionMessage(e)))
    },
    message = function(m) {
      
      print(conditionMessage(m))
      
      return(tibble(Year = NA_real_,
                    Employer = NA_character_,
                    Name = NA_character_,
                    Title = NA_character_,
                    `Annual Wages` = NA_character_,
                    Source = NA_character_,
                    state = state,
                    function_year = year, 
                    agency = agency, 
                    condition = 'warning',
                    notes = conditionMessage(m)))
    }
    )
}


# only run the function on the state/agency/years that have not yet been 
#   completed.
data_for_function <- anti_join(state_agencies_n_employees, 
                               full_state_employee_file,
                               by = c("state",
                                      "year" = "function_year",
                                      "agency"))

out_list <- map(1:n_runs, 
                ~state_employee_scraper(data_for_function$state[.],
                                        data_for_function$agency[.],
                                        data_for_function$year[.],
                                        data_for_function$n_pages[.]))


full_state_employee_file <- out_list %>% 
  bind_rows()

save(full_state_employee_file,
     file = "../data/state-employees/full_state_employee_file.Rdata")


