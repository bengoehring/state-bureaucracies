pacman::p_load(tidyverse, 
               RSelenium, 
               rvest)

# set port for selenium 
my_port <- 1678L

# Start a new Selenium Browser (If you're using IOS, this only works through Firefox)
# "port" can be any numeric value in the format below - 
# i.e. "****L" - select any four digit number until it runs successfully

# running on older version of geckover because the most recent update 
#   does not work on more recent Mac OS: https://github.com/mozilla/geckodriver/releases
rD <- rsDriver(browser="firefox",
               geckover = "0.29.1",
               port=my_port, 
               verbose=T)

remDr <- rD[["client"]]




scrape_HI_salaries <- function(year) {
  
  link<-"https://www.civilbeat.org/projects/public-employee-salaries/"
  remDr$navigate(link)
  
  # This is the number of pages when each page displays 250 employees
  #   And the Department == 'State of Hawaii'
  if(year == 2022) {
    n_pages <- 57
  } else if(year == 2020) {
    n_pages <- 61
  } else if(year == 2018) {
    n_pages <- 58
  } else if(year == 2016) {
    n_pages <- 60
  } else if(year == 2013) {
    n_pages <- 59
  } else if(year == 2012) {
    n_pages <- 57
  } else if(year == 2011) {
    n_pages <- 57
  }
  
  # Select only employees with the State of Hawaii
  Sys.sleep(sample(7:15, 1))
  state_hi <- remDr$findElement('xpath', 
                               "//*[(@name = 'Value1_1')]/option[@value = 'State of Hawaii']")
  state_hi$clickElement()
  
  # Select fiscal year
  Sys.sleep(sample(7:15, 1))
  fy_year_path <- str_glue("//*[(@id = 'Value8_1')]/option[@value = '{year}']")
  fy_year <- remDr$findElement('xpath', 
                               fy_year_path)
  fy_year$clickElement()
  
  # Search for employee records
  Sys.sleep(sample(7:15, 1))
  search <- remDr$findElement('xpath', 
                              '//*[contains(concat( " ", @class, " " ), concat( " ", "cbSearchButton", " " ))]')
  search$clickElement()
  
  # Select 250 observations per page
  Sys.sleep(sample(7:15, 1))
  select_n_page <- remDr$findElement("xpath",
                                     "//select[@class = 'cbResultSetRecordPerPageControl']/option[@value = '250']")
  select_n_page$clickElement()
  
  # loop through each of the n_pages and save the output
  output_list <- vector('list', 
                        length = n_pages)
  
  for(i in 1:n_pages) {
    Sys.sleep(sample(7:15, 1))
    
    tryCatch({
      table_output <- remDr$getPageSource()[[1]] %>% 
        read_html() %>% 
        html_table()
      
      output_list[[i]] <- table_output
      
      if(i %% 10 == 0) {
        print(str_c("The scraper is ",
                    round(i / n_pages * 100, 0),
                    "% complete."))
      }
      
      # Go to next page 
      Sys.sleep(sample(7:15, 1))
      next_page <- remDr$findElement('xpath', 
                                     "//ul/li/*[@class='cbResultSetJumpToTextField']")
      
      next_page$clearElement()
      next_page$sendKeysToElement(list(as.character(i + 1), 
                                       key = "enter"))
    },
    warning = function(w) {
      print(str_c("Warning on page ", 
                  i, 
                  ": ", 
                  conditionMessage(w)))
      
      output_list[[i]] <- conditionMessage(w)
      
      # Go to next page 
      Sys.sleep(sample(7:15, 1))
      next_page <- remDr$findElement('xpath', 
                                     "//ul/li/*[@class='cbResultSetJumpToTextField']")
      
      next_page$clearElement()
      next_page$sendKeysToElement(list(as.character(i + 1), 
                                       key = "enter"))
    },
    error = function(e) {
      print(str_c("Error on page ", 
                  i, 
                  ": ", 
                  conditionMessage(e)))
      
      output_list[[i]] <- conditionMessage(e)
      
      # Go to next page 
      Sys.sleep(sample(7:15, 1))
      next_page <- remDr$findElement('xpath', 
                                     "//ul/li/*[@class='cbResultSetJumpToTextField']")
      next_page$clearElement()
      next_page$sendKeysToElement(list(as.character(i + 1), 
                                       key = "enter"))
    })
  }
  return(output_list)
}


HI_out_2022 <- scrape_HI_salaries(2022)
HI_out_2020 <- scrape_HI_salaries(2020)
HI_out_2018 <- scrape_HI_salaries(2018)
HI_out_2016 <- scrape_HI_salaries(2016)
HI_out_2013 <- scrape_HI_salaries(2013)
HI_out_2012 <- scrape_HI_salaries(2012)
HI_out_2011 <- scrape_HI_salaries(2011)


# check for any messages indicating error/warning
all(map_lgl(1:length(HI_out_2022), ~is_tibble(HI_out_2022[[.]][[1]])))
all(map_lgl(1:length(HI_out_2020), ~is_tibble(HI_out_2020[[.]][[1]])))
all(map_lgl(1:length(HI_out_2018), ~is_tibble(HI_out_2018[[.]][[1]])))
all(map_lgl(1:length(HI_out_2016), ~is_tibble(HI_out_2016[[.]][[1]])))
all(map_lgl(1:length(HI_out_2013), ~is_tibble(HI_out_2013[[.]][[1]])))
all(map_lgl(1:length(HI_out_2012), ~is_tibble(HI_out_2012[[.]][[1]])))
all(map_lgl(1:length(HI_out_2011), ~is_tibble(HI_out_2011[[.]][[1]])))




# Clean scraped data and bind together. 
clean_scraped_list <- function(list) {
  out <- list %>% 
    bind_rows() %>% 
    distinct() %>% 
    rename(first_name = Name,
           last_name = ...4,
           salary_range_lwr_bnd = `Salary Range`,
           salary_range_upr_bnd = ...7,
           fiscal_year = FY,
           hourly_employee = Hourly) %>% 
    select(-...10) %>% 
    mutate(hourly_employee = 0)
  
  return(out)
}


HI_out_2022_df <- clean_scraped_list(HI_out_2022)
HI_out_2020_df <- clean_scraped_list(HI_out_2020)
HI_out_2018_df <- clean_scraped_list(HI_out_2018)
HI_out_2016_df <- clean_scraped_list(HI_out_2016)
HI_out_2013_df <- clean_scraped_list(HI_out_2013)
HI_out_2012_df <- clean_scraped_list(HI_out_2012)
HI_out_2011_df <- clean_scraped_list(HI_out_2011)


HI_out_final <- rbind(HI_out_2022_df,
                      HI_out_2020_df,
                      HI_out_2018_df,
                      HI_out_2016_df,
                      HI_out_2013_df,
                      HI_out_2012_df,
                      HI_out_2011_df)

save(HI_out_final,
     file = "../data/state-employee-data/HI/HI_out_final.Rdata")

