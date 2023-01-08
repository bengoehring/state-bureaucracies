# to do 
# clean AR, IL, MS, and CT data from L2 -- save in cluster folder

# set up basic code for merging 
# how to deal with different names within an id in the merge? Might not matter--
#   create a dataset distinct by id and the vars that you are going to merge 
#   with the voter file. only an issue if you get different partisan identities
#   within a given id. 

# standardize middle name / middle initial, gender/sex, race/ethnicity variables 
#   if applicable. 

# preamble ####
library(tidyverse)
library(fastLink)
library(furrr)

# read in data ####
ct_voter_raw <- readRDS("input-data/voter-files/CT-unmerged.rds")
ct_personnel_raw <- readRDS("input-data/personnel-data/ct-15-22-premerge.rds")

# links to local data
#ct_voter_raw <- readRDS("../data/voter-data/unmerged-voter-files/CT-unmerged.rds")
#ct_personnel_raw <- readRDS("../data/state-employee-data/CT/clean/ct-15-22-premerge.rds")


# basic cleaning -- voter data ####
# one instance of invalid encoding 
ct_voter_raw$Voters_MiddleName[!validEnc(ct_voter_raw$Voters_MiddleName)] <- "Joseph"

ct_voter_clean <- ct_voter_raw %>% 
  rename_with(tolower) %>% 
  mutate(across(where(is.character),
                ~str_trim(str_squish(tolower(.))))) %>% 
  mutate(across(c(voters_firstname,
                  voters_lastname,
                  voters_namesuffix,
                  voters_middlename,
                  voters_gender),
                ~str_trim(str_squish(str_remove_all(., "[:punct:]"))))) %>% 
  mutate(voters_lastname_join = case_when(
    !is.na(voters_namesuffix) ~ str_c(voters_lastname,
                                      " ",
                                      voters_namesuffix),
    TRUE ~ voters_lastname)) %>% 
  mutate(voters_middleinitial = str_sub(voters_middlename,
                                        1,
                                        1)) %>% 
  rename(first_name = voters_firstname,
         last_name = voters_lastname,
         middle_initial = voters_middleinitial,
         suffix = voters_namesuffix,
         last_name_join = voters_lastname_join,
         gender = voters_gender) %>% 
  select(-voters_middlename)
  

# basic cleaning -- personnel data ####
ct_personnel_clean <- ct_personnel_raw %>% 
  select(emplid_empl_rcd,
         first_name, 
         middle_name, 
         last_name,
         name_suffix,
         sex) %>% 
  distinct() %>% 
  mutate(sex = case_when(
    sex == 'u' ~ NA_character_,
    TRUE ~ sex
  )) %>% 
  mutate(last_name_join = case_when(
    !is.na(name_suffix) ~ str_c(last_name, 
                                " ",
                                name_suffix),
    TRUE ~ last_name
  )) %>% 
  mutate(middle_initial = str_sub(middle_name,
                                  1,
                                  1)) %>% 
  rename(gender = sex, 
         suffix = name_suffix)


ct_personnel_clean %>% 
  summarise(across(.fns = ~sum(is.na(.), na.rm = T) / n() * 100)) %>% 
  t()
ct_voter_clean %>% 
  summarise(across(.fns = ~sum(is.na(.), na.rm = T) / n() * 100)) %>% 
  t()

# set this up to block on gender
ct_blocking <- blockData(ct_personnel_clean,
                         ct_voter_clean,
                         varnames = c('gender', 
                                      'first_name'),
                         kmeans.block = "first_name",
                         nclusters = 10)


link_by_block <- function(df_a, 
                          df_b, 
                          blocking_object,
                          block_i,
                          var_names,
                          vars_stringdist,
                          cond_indep) {
  
  df_a_blocked <- df_a[blocking_object[[block_i]][["dfA.inds"]],]
  df_b_blocked <- df_b[blocking_object[[block_i]][["dfB.inds"]],]
  
  fl_out <- fastLink(df_a_blocked,
                     df_b_blocked,
                     varnames = var_names,
                     stringdist.match = vars_stringdist,
                     cond.indep = cond_indep,
                     n.cores = 36)
  
  fl_matches <- getMatches(df_a_blocked,
                           df_b_blocked,
                           fl_out)
  
  fl_out_name <- str_c("fl_object", 
                       "_", 
                       "block_", 
                       block_i)
  matches_out_name <- str_c("matches", 
                            "_", 
                            "block_", 
                            block_i)
  warnings_name <- str_c("warnings_", 
                         block_i)
  
  out <- list(fl_out, 
              fl_matches, 
              warnings())
  names(out) <- c(fl_out_name, 
                  matches_out_name, 
                  warnings_name)
  
  return(out)
}


# direct left merge --

# first dedup the voter file so you only have the most likely partisan id
#   per name pair. 
ct_voter_clean_condense <- ct_voter_clean %>% 
  select(first_name, 
         middle_initial,
         last_name, 
         suffix, 
         gender, 
         parties_description) %>% 
  mutate(parties_description = case_when(
    parties_description == 'democratic' ~ 'democratic',
    parties_description == 'republican' ~ 'republican',
    parties_description == 'non-partisan' ~ 'non-partisan',
    parties_description == 'registered independent' ~ 'registered independent',
    is.na(parties_description) ~ NA_character_,
    TRUE ~ 'other'
  )) %>% 
  mutate(dem = if_else(parties_description == 'democratic', 1, 0)) %>% 
  mutate(rep = if_else(parties_description == 'republican', 1, 0)) %>% 
  mutate(non = if_else(parties_description == 'non-partisan', 1, 0)) %>% 
  mutate(ind = if_else(parties_description == 'registered independent', 1, 0)) %>% 
  group_by(first_name, 
         middle_initial,
         last_name, 
         suffix, 
         gender) %>% 
  mutate(across(dem:ind,
                   ~sum(.) / n())) %>% 
  mutate(n_voters_group = n()) %>% 
  
  ungroup()



ct_out <- left_join(ct_personnel_clean,
                    ct_voter_clean, 
                    by = c("first_name",
                           "last_name",
                           "suffix",
                           "gender",
                           "middle_initial"))

ct_out_clean <- ct_out %>% 
  select(emplid_empl_rcd,
         first_name,
         last_name,
         suffix,
         gender,
         middle_initial,
         parties_description
         ) %>% 
  group_by(first_name, 
           last_name,
           suffix,
           gender, 
           middle_initial,
           parties_description) %>% 
  mutate(n = n())
  


summarise(boop = count(parties_description))



# first ####
# blocking: gender, first_name (k means clusters = 10)
# conditional independence: false
# exact matching: middle initial
# string matching: first and last name
ct_fl_merge_out <- future_map(1:length(ct_blocking), 
                   ~link_by_block(ct_personnel_clean,
                                  ct_voter_clean, 
                                  ct_blocking,
                                  ., 
                                  var_names = c("first_name",
                                                "middle_initial",
                                                "last_name_join"),
                                  vars_stringdist = c("first_name",
                                                      "last_name_join"),
                                  cond_indep = FALSE),
                   .options = furrr_options(seed = TRUE))

saveRDS(ct_fl_merge_out, 
        "output-data/ct_condindpF.rds")

## second ####
## blocking: gender
## conditional independence: true
## exact matching: middle initial
## string matching: first and last name
#ct_fl_merge_out2 <- map(1:length(ct_blocking), 
#                       ~link_by_block(ct_personnel_clean,
#                                      ct_voter_clean, 
#                                      ct_blocking,
#                                      ., 
#                                      var_names = c("first_name",
#                                                    "middle_initial",
#                                                    "last_name_join"),
#                                      vars_stringdist = c("first_name",
#                                                          "last_name_join"),
#                                      cond_indep = TRUE))
#
#saveRDS(ct_fl_merge_out2, 
#        "output-data/ct_condindpT.rds")
#rm(ct_fl_merge_out2)
#
## third ####
## blocking: gender
## conditional independence: false
## exact matching: NA
## string matching: first and last name
#ct_fl_merge_out3 <- map(1:length(ct_blocking), 
#                        ~link_by_block(ct_personnel_clean,
#                                       ct_voter_clean, 
#                                       ct_blocking,
#                                       ., 
#                                       var_names = c("first_name",
#                                                     "last_name_join"),
#                                       vars_stringdist = c("first_name",
#                                                           "last_name_join"),
#                                       cond_indep = FALSE))
#
#saveRDS(ct_fl_merge_out3, 
#        "output-data/ct_condindpF_nomiddle.rds")
#rm(ct_fl_merge_out3)
#
## fourth ####
## blocking: gender
## conditional independence: true
## exact matching: NA
## string matching: first and last name
#ct_fl_merge_out4 <- map(1:length(ct_blocking), 
#                        ~link_by_block(ct_personnel_clean,
#                                       ct_voter_clean, 
#                                       ct_blocking,
#                                       ., 
#                                       var_names = c("first_name",
#                                                     "last_name_join"),
#                                       vars_stringdist = c("first_name",
#                                                           "last_name_join"),
#                                       cond_indep = TRUE))
#
#saveRDS(ct_fl_merge_out4, 
#        "output-data/ct_condindpT_nomiddle.rds")
#rm(ct_fl_merge_out4)
