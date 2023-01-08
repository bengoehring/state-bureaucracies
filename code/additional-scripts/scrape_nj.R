library(tidyverse)
library(RSelenium)
library(rvest)


# set port for selenium 
my_port <- 1691L

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

page_url <- "https://info.csc.state.nj.us/TItleList/StateList.aspx"

# these are the ids to the various pages on the website (job titles that begin
#   with different letters)
all_letter_header_ids <- str_c("LB",
                               LETTERS) %>% 
  str_replace("LBA",
              "LbA")
all_letter_header_ids <- c("LBSA",
                           "LBSU",
                           all_letter_header_ids)
all_letter_header_ids <- str_subset(all_letter_header_ids, 
                                    "^LBS$|^LBK$|^LBZ$",
                                    negate = T) %>% 
  sort()


scrape_nj_classes <- function(webpage_url, 
                              header_letter) {
  
  Sys.sleep(sample(1:6, 1))
  
  # navigate to page and select the subpage with job titles beginning with the 
  #   header_letter letter. 
  remDr$navigate(page_url)
  elem <- remDr$findElement(using = "id",
                            value = header_letter)
  elem$clickElement()
  
  # retrieve the full list of job title names from the webpage
  all_job_titles <- remDr$getPageSource()[[1]] %>% 
    read_html() %>% 
    html_nodes(xpath= '//*[@id="Panel1"]') %>% 
    html_table(header = T) %>% 
    flatten_df() %>% 
    rename_with(~str_to_lower(str_replace_all(., " ", "_")))
  
  # now need to loop through all of these job titles, click on each one 
  #   and extract the class information. 
  
  out_list <- vector('list',
                     length = nrow(all_job_titles))
  
  for (i in 1:nrow(all_job_titles)) { 
    tryCatch(
      {
        elem <- remDr$findElement(using = 'xpath',
                                  value = str_c('//*[@id="DgGroup"]/tbody/tr[',
                                                i + 1,
                                                ']/td/a'))
        elem$clickElement()                           
        
        out_list[[i]] <- remDr$getPageSource()[[1]] %>% 
          read_html() %>% 
          html_nodes(xpath= '//*[@id="Panel2"]') %>% 
          html_table(header = T) %>% 
          flatten_df() %>% 
          rename_with(~str_to_lower(str_replace_all(., " ", "_"))) %>% 
          mutate(job_title = all_job_titles$title_group_names[i]) %>% 
          select(job_title, 
                 everything())
        
        Sys.sleep(sample(9:16, 1))
        print(str_c("Completed job title: ", 
                    all_job_titles$title_group_names[i]))
      },
        warning = function(c) {
          print(paste0("Warning for page ", header_letter))
          print(conditionMessage(c))
          
          out_list[[i]] <- NULL
        },
        message = function(c) {
          print(paste0("Message for page ", header_letter))
          print(conditionMessage(c))
          
          out_list[[i]] <- NULL
        },
        error = function(c) {
          print(paste0("Error for page ", header_letter))
          print(conditionMessage(c))
          
          out_list[[i]] <- NULL
        }
    )
  }
  
  names(out_list) <- all_job_titles$title_group_names
  return(out_list)
}

all_titles_list <- map(all_letter_header_ids,
                       ~ scrape_nj_classes(page_url, 
                                           .))
names(all_titles_list) <- all_letter_header_ids




all_titles_list_bind <- all_titles_list %>% 
  flatten()
  
all_titles_list_bind <- map(all_titles_list_bind, 
                            ~mutate(., 
                                    across(.fns = as.character)))
all_titles_df <- all_titles_list_bind %>% 
  bind_rows()

saveRDS(all_titles_df, 
        "../data/state-employee-data/NJ/raw/job_class_codes.rds")

boop <- all_titles_df %>% 
  group_by(job_title) %>% 
  filter(n_distinct(class_of_service) > 1)

