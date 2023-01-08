# This file retrieves the number of employees per state/agency/year

pacman::p_load(tidyverse,
               rvest,
               xml2,
               urltools)

# Number of state agency years to scrape for n employees
n_to_scrape <- 2000



# load data containing all state/agency/years
load("../data/state-employees/state_agencies_year.Rdata")

# load data containing number of employee data collected up to this point
load("../data/state-employees/state_agencies_n_employees.Rdata")




# scrape_n_employees() collects the number of employees listed in openthebooks
#   for a given state, year, and agency. It returns a tibble containing this
#   information

scrape_n_employees <- function(state,
                               year,
                               agency) {
  
  Sys.sleep(sample(15:45, 1))
  
  # define the webpage 
  tryCatch(
    {
    n_results_page <- str_c("https://www.openthebooks.com/",
                            state,
                            "-state-employees/?",
                            "Year_S=",
                            year,
                            "&Emp_S=",
                            url_encode(agency),
                            "&pg=1")
    
    n_results_page_html <- read_html(n_results_page)
    
    # extract number of results from webpage
    n_results <- n_results_page_html %>% 
      html_node("body") %>% 
      xml_find_all('//*[contains(concat( " ", @class, " " ), concat( " ", "sort-status", " " ))]') %>% 
      html_text() 
    
    n_results <- n_results %>% 
      str_c(collapse = "") %>%
      str_extract_all("[:digit:]*") %>%
      as_vector() %>% 
      str_c(collapse = "") %>% 
      as.numeric()
    
    print(str_c("The number of results for ",
                state,
                "'s ",
                agency,
                " in ",
                year,
                ": ",
                n_results))
    
    return(tibble(state = state, 
                  year = year, 
                  agency = agency, 
                  n_results = n_results,
                  condition = NA_character_,
                  notes = NA_character_))
    },
    
    # Warning, Error, and Message handling ####
    
    warning = function(w) {
      
      print(conditionMessage(w))
      
      return(tibble(state = state, 
                    year = year, 
                    agency = agency, 
                    n_results = NA_real_,
                    condition = 'warning',
                    notes = conditionMessage(w)))
    },
    error = function(e) {
      
      print(conditionMessage(e))
      
      return(tibble(state = state, 
                    year = year, 
                    agency = agency, 
                    n_results = NA_real_,
                    condition = 'error',
                    notes = conditionMessage(e)))
    },
    message = function(m) {
      
      print(conditionMessage(m))
      
      return(tibble(state = state, 
                    year = year, 
                    agency = agency, 
                    n_results = NA_real_,
                    condition = 'message',
                    notes = conditionMessage(m)))
    }
  )
}
  

# This function needs to be run on the data that has not already been collected
#   and saved in such a way that it does not write over the already-completed
#   work. 

data_for_function <- anti_join(state_agencies_year,
                               state_agencies_n_employees,
                               by = c("state",
                                      "agency",
                                      "year"))

out_list <- map(1:n_to_scrape, 
                ~scrape_n_employees(data_for_function$state[.],
                                    data_for_function$year[.],
                                    data_for_function$agency[.]))

out_df <- out_list %>% 
  bind_rows()



state_agencies_n_employees <- rbind(state_agencies_n_employees,
                                    out_df)

save(state_agencies_n_employees,
     file = "../data/state-employees/state_agencies_n_employees.Rdata")







  
  
  
  
  
  
  
  