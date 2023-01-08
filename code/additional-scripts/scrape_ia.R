library(tidyverse)
library(RSelenium)
library(rvest)


# set port for selenium 
my_port <- 1685L

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


# define inputs
page_url <- "https://eservices.iowa.gov/icpp/index.faces"

all_classes <- vector("list",
                      length = 10)

# go to page
remDr$navigate(page_url)

all_classes[[1]] <- remDr$getPageSource()[[1]] %>% 
  read_html() %>% 
  html_nodes(xpath= '//*[@id="vp:objs"]') %>% 
  html_table(header = T) %>% 
  flatten_df() %>% 
  rename_with(~str_to_lower(str_replace_all(., 
                                            " ", 
                                            "_")))

for (i in 2:10) {
  Sys.sleep(sample(9:15, 1))
  
  # click to next page
  elem <- remDr$findElement(using = "id",
                              value = str_c('vp:scroller2idx',
                                            i))
  elem$clickElement()
  
  
  all_classes[[i]] <- remDr$getPageSource()[[1]] %>% 
    read_html() %>% 
    html_nodes(xpath= '//*[@id="vp:objs"]') %>% 
    html_table(header = T) %>% 
    flatten_df() %>% 
    rename_with(~str_to_lower(str_replace_all(., 
                                              " ", 
                                              "_")))
}

all_classes_bind <- map(all_classes, 
                        ~mutate(., 
                                pay_plan = as.character(pay_plan)))

all_classes_df <- all_classes_bind %>% 
  bind_rows()


# clean and pull in infor from this crosswalk
#https://das.iowa.gov/sites/default/files/hr/documents/class_and_pay/PayPlans/class_explanation.pdf

all_classes_df_clean <- all_classes_df %>% 
  










