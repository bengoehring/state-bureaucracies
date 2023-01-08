# This function returns the voting and demographic information from the L2
# dataset for a given state. 

# INPUTS
##  state: the abbreviation of the state
##  voter_path: the path to the voter file 
##  voter_file_cols: the columns of interest from the voter file
##  dem_file_cols: the columns of interest from the demographic file
##  working_directory: the absolute path to the external drive where the L2
##   data lives. 

## OUTPUT
#   A dataframe containing the requested information
################################################################################
################################################################################
library(tidyverse)
library(vroom)

read_state_file <- function(state,
                            voter_path = NULL,
                            voter_file_cols = NULL,
                            dem_file_cols = c("LALVOTERID",
                                              "Voters_Active",
                                              "Voters_FirstName",
                                              "Voters_MiddleName",
                                              "Voters_LastName",
                                              "Voters_NameSuffix",
                                              "Voters_Gender",
                                              "Voters_BirthDate",
                                              "DateConfidence_Description",
                                              "Parties_Description",
                                              "VoterParties_Change_Changed_Party",
                                              "County",
                                              "Residence_Addresses_Longitude",
                                              "Residence_Addresses_Latitude",
                                              "Residence_Addresses_LatLongAccuracy",
                                              "Ethnic_Description",
                                              "EthnicGroups_EthnicGroup1Desc"
                                              ),
                            working_directory = "/Volumes/My Passport for Mac/L2/VMFiles/"
                            ){
  # tests
  if(!is.null(voter_path) && is.null(voter_file_cols)) {
    stop("If voting data is requested, then columns from the voter file must be specified.")
  }
  if(!is.null(voter_file_cols) && !"LALVOTERID" %in% voter_file_cols) {
    stop("If columns from the voter file are requested, the voter id column (LALVOTERID) must be included.")
  }
  if(!"LALVOTERID" %in% dem_file_cols) {
    stop("The voter id column (LALVOTERID) must be included in the vector of column names from the demographic file.")
  }
  
  # retrieve file paths based on state 
  state_directory <- str_subset(list.dirs(), str_c("--",
                                                   state, 
                                                   "--"))
  state_demo_file <- str_subset(list.files(state_directory),
                                "DEMOGRAPHIC.tab")
  state_vote_file <- str_subset(list.files(state_directory),
                                "VOTEHISTORY.tab")
  
  state_demo_path <- str_c(state_directory, 
                           "/",
                           state_demo_file)
  state_vote_path <- str_c(state_directory, 
                           "/",
                           state_vote_file)
  
  # For some reason, reading from the external drive does not work unless 
  # the absolute path is specified using setwd(). Setting it and then resetting
  # it at the end of the function.
  orginal_working_directory <- getwd()
  setwd(working_directory)
        
  
  # read in state voter demographic file
  state_demos <- vroom(state_demo_path,
                       col_select = all_of(dem_file_cols))                   

  # if path to voter file is provided, then read in voter data and merge it with 
  #   the demographic data. 
  if(!is.null(voter_path)) {
    
    cat("Reading in the left join of the following two files from ",
        state, 
        ": ",
        state_demo_path,
        " and ",
        state_vote_path)
    
    state_voters <- vroom(state_vote_path, 
                          col_select = all_of(voter_file_cols))
    
    state_final <- left_join(state_demos, 
                             state_voters, 
                             by = "LALVOTERID")
    
    # cleaning 
    state_final <- state_final %>% 
      rename_all(.funs = tolower) %>% 
      mutate(across(where(is.character),
                    ~str_trim(str_squish(str_to_lower(.))))) %>% 
      mutate(across(c(voters_firstname,
                      voters_middlename,
                      voters_lastname,
                      voters_namesuffix), 
                    ~str_remove_all(., 
                                    "[:punct:]"))) %>% 
      rename(first_name = voters_firstname, 
             middle_name = voters_middlename,
             last_name = voters_lastname,
             suffix = voters_namesuffix,
             gender = voters_gender,
             birth_date = voters_birthdate) %>% 
      mutate(last_name_join = case_when(
        is.na(suffix) ~ last_name, 
        TRUE ~ str_c(last_name, 
                     " ",
                     suffix)
      )) %>% 
      mutate(middle_initial = str_sub(middle_name,
                                      1,
                                      1)) %>% 
      select(lalvoterid, 
             first_name,
             middle_name, 
             middle_initial,
             last_name,
             last_name_join,
             suffix,
             gender, 
             birth_date,
             everything()
             )
      
    saveRDS(state_final,
            file = str_c("../../../Macintosh HD/Users/goehring/Dropbox (University of Michigan)/study/projects/state-bureaucracies/data/voter-data/unmerged-voter-files/",
                         state,
                         "-unmerged-voter-history.rds"))
    setwd(orginal_working_directory)
    
  } else {
    # just write the requested demographic data
    cat("Reading in the following file from ", 
        state,
        ": ",
        state_demo_path)
    
    
    # cleaning 
    state_demos <- state_demos %>% 
      rename_all(.funs = tolower) %>% 
      mutate(across(where(is.character),
                    ~str_trim(str_squish(str_to_lower(.))))) %>% 
      mutate(across(c(voters_firstname,
                      voters_middlename,
                      voters_lastname,
                      voters_namesuffix), 
                    ~str_remove_all(., 
                                    "[:punct:]")))%>% 
      rename(first_name = voters_firstname, 
             middle_name = voters_middlename,
             last_name = voters_lastname,
             suffix = voters_namesuffix,
             gender = voters_gender,
             birth_date = voters_birthdate) %>% 
      mutate(last_name_join = case_when(
        is.na(suffix) ~ last_name, 
        TRUE ~ str_c(last_name, 
                     " ",
                     suffix)
      )) %>% 
      mutate(middle_initial = str_sub(middle_name,
                                      1,
                                      1)) %>% 
      select(lalvoterid, 
             first_name,
             middle_name, 
             middle_initial,
             last_name,
             last_name_join,
             suffix,
             gender, 
             birth_date,
             everything()
      )
    
    
    saveRDS(state_demos,
            file = str_c("../../../Macintosh HD/Users/goehring/Dropbox (University of Michigan)/study/projects/state-bureaucracies/data/voter-data/unmerged-voter-files/",
                         state,
                         "-unmerged.rds"))
    setwd(orginal_working_directory)
  }
}

read_state_file("AR")


