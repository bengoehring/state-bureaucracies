# Test pilot for Virginia

# what is salary variable? 
# what is position location code? 
# middle name available?
# fill in missing zip code info -- see va_data_dedup_zip_misses

# run full merge without deduping and then dedup with checking for adjacent
#   county


library(tidyverse)
library(readxl)
library(lubridate)
library(vroom)

va_files <- list.files("../data/state-employee-data/VA/")
va_paths <- str_c("../data/state-employee-data/VA/",
                  va_files)

va_data_raw_list <- map(va_paths,
                    ~read_xlsx(.,
                               col_types = "text"))
va_data_raw_df <- va_data_raw_list %>% 
  bind_rows()


# basic cleaning and renaming 
colnames(va_data_raw_df) <- tolower(colnames(va_data_raw_df))
colnames(va_data_raw_df) <- str_replace_all(colnames(va_data_raw_df),
                                        " ",
                                        "_")

va_data_raw_clean <- va_data_raw_df %>% 
  mutate(across(c(data_source_date,
                  state_begin_date,
                  position_separation_date),
                ~as_date(as.numeric(.),
                         origin = "1899-12-30"))) # apparently this is the origin for excel dates

va_data_raw_clean <- va_data_raw_clean %>% 
  mutate(across(c(first_name, last_name),
                tolower)) %>% 
  mutate(across(c(position_number,
                  agency_zip_code,
                  state_salary_or_wage_hourly_rate)))



# DEDUP ####
# note the multiple, sometimes overlapping data systems (PMIS and cardinal)

# Remove any duplicate people shown twice in the two systems. 
#   There are cardinal system entries for the following dates: 
#   03-31-2022
#   06-30-2022
#   12-31-2021
va_data_dedup <- va_data_raw_clean %>% 
  group_by(data_source_date) %>% 
  distinct() %>% 
  ungroup()

# many registered voters in VA and in the state bureaucracy share a first and 
# last name. In order to improve matching, I am going to only merge on 
# first and last names within adjacent counties. In order to do that, I need
# to first impute county via zip codes. That data comes from HUD:
# https://www.huduser.gov/portal/datasets/usps_crosswalk.html

# pull in zip code data for virginia 
# # if zip code is repeated, return the one with the highest
zip_county_xwalk <- read_xlsx("../data/voter-data/ZIP_COUNTY_032020.xlsx")
colnames(zip_county_xwalk) <- tolower(colnames(zip_county_xwalk))

zip_county_xwalk <- zip_county_xwalk %>% 
  group_by(zip) %>% 
  mutate(keeper = max(tot_ratio)) %>% 
  filter(keeper == tot_ratio) %>% 
  ungroup()

# still have some where the share of addresses is equally split across counties
zip_county_xwalk <- zip_county_xwalk %>% 
  group_by(zip) %>% 
  slice_sample(n = 1) %>% 
  ungroup()

zip_county_xwalk <- zip_county_xwalk %>% 
  select(zip,
         county)

va_data_dedup_zip <- left_join(va_data_dedup, 
                               zip_county_xwalk,
                               by = c("agency_zip_code" = "zip"))

va_data_dedup_zip_misses <- anti_join(va_data_dedup, 
                                      zip_county_xwalk,
                                      by = c("agency_zip_code" = "zip"))
# misses seem to be due to missing zip codes or PO-box only zip codes
# imputing the latter counties based on city information

va_data_dedup_zip_city <- va_data_dedup_zip %>% 
  mutate(agency_city = tolower(agency_city)) %>% 
  mutate(county = case_when(
    is.na(county) & agency_city == "martinsville" ~ "51089", 
    is.na(county) & agency_city == "norfolk" ~ "51710",
    is.na(county) & agency_city == "manassas" ~ "51683",
    is.na(county) & agency_city == "va state univ" ~ "51041",
    is.na(county) & agency_city == "richmond" ~ "51760",
    is.na(county) & agency_city == "lynchburg" ~ "51680",
    is.na(county) & agency_city == "bristol" ~ "51520",
    is.na(county) & agency_city == "petersburg" ~ "51730",
    is.na(county) & agency_city == "alberta" ~ "51025",
    is.na(county) & agency_city == "oyster" ~ "51131",
    is.na(county) & agency_city == "villamont" ~ "51019",
    is.na(county) & agency_city == "tazewell" ~ "51185",
    is.na(county) & agency_city == "reston" ~ "51059",
    is.na(county) & agency_city == "herndon" ~ "51059",
    is.na(county) & agency_city == "woodbridge" ~ "51153",
    is.na(county) & agency_city == "lunenburg" ~ "51111",
    is.na(county) & agency_city == "chesterfield" ~ "51041",
    TRUE ~ county
  ))

# Most of the remaining missing values seem to be due to a change in data 
#   strucutre. In 2021-12-31, 2022-03-31, and 2022-06-30 it looks like position_location code
#   starts to be used to capture location information instead of the agency zip 
#   and city variables. 
va_data_dedup_zip_city_2 <- va_data_dedup_zip_city %>% 
  mutate(position_location_code = tolower(position_location_code)) %>% 
  mutate(county = case_when(
    is.na(county) & position_location_code == "richmond (city)" ~ "51760",
    is.na(county) & position_location_code == "chesterfield" ~ "51041",     
    is.na(county) & position_location_code == "culpeper" ~ "51047",
    is.na(county) & position_location_code == "prince william" ~ "51153",  
    is.na(county) & position_location_code == "wise" ~ "51195",             
    is.na(county) & position_location_code == "pulaski" ~ "51155",          
    is.na(county) & position_location_code == "suffolk" ~ "51800",          
    is.na(county) & position_location_code == "roanoke (city)" ~ "51770",   
    is.na(county) & position_location_code == "fairfax county" ~ "51059",  
    is.na(county) & position_location_code == "prince edward" ~ "51147",    
    is.na(county) & position_location_code == "smyth" ~ "51173",            
    is.na(county) & position_location_code == "portsmouth" ~ "51740",       
    is.na(county) & position_location_code == "botetourt" ~ "51023",        
    is.na(county) & position_location_code == "henrico" ~ "51087",         
    is.na(county) & position_location_code == "998" ~ NA_character_,              
    is.na(county) & position_location_code == "tazewell" ~ "51185",         
    is.na(county) & position_location_code == "montgomery" ~ "51121",       
    is.na(county) & position_location_code == "lynchburg" ~ "51680",        
    is.na(county) & position_location_code == "475" ~ NA_character_,             
    is.na(county) & position_location_code == "norfolk" ~ "51710",          
    is.na(county) & position_location_code == "williamsburg" ~ "51830",     
    is.na(county) & position_location_code == "petersburg" ~ "51730",       
    is.na(county) & position_location_code == "washington" ~ "51191",       
    is.na(county) & position_location_code == "newport news" ~ "51700",    
    is.na(county) & position_location_code == "outside of state" ~ "out-of-state", 
    is.na(county) & position_location_code == "franklin (city)" ~ "51620",  
    is.na(county) & position_location_code == "manassas" ~ "51683",         
    is.na(county) & position_location_code == "nc" ~ "out-of-state",               
    is.na(county) & position_location_code == "virginia beach" ~ "51810",  
    is.na(county) & position_location_code == "augusta" ~ "51015",          
    is.na(county) & position_location_code == "salem" ~ "51775",            
    is.na(county) & position_location_code == "alexandria" ~ "51510",       
    is.na(county) & position_location_code == "alleghany" ~ "51005",        
    is.na(county) & position_location_code == "halifax" ~ "51083",         
    is.na(county) & position_location_code == "campbell" ~ "51031",         
    is.na(county) & position_location_code == "harrisonburg" ~ "51660",     
    is.na(county) & position_location_code == "gloucester" ~ "51073",       
    is.na(county) & position_location_code == "arlington" ~ "51013",        
    is.na(county) & position_location_code == "chesapeake" ~ "51550",      
    is.na(county) & position_location_code == "bedford county" ~ "51019",   
    is.na(county) & position_location_code == "norton" ~ "51720",           
    is.na(county) & position_location_code == "spotsylvania" ~ "51177",     
    is.na(county) & position_location_code == "loudoun" ~ "51107",          
    is.na(county) & position_location_code == "buchanan" ~ "51027",        
    is.na(county) & position_location_code == "scott" ~ "51169",            
    is.na(county) & position_location_code == "roanoke county" ~ "51161",   
    is.na(county) & position_location_code == "prince george" ~ "51149",    
    is.na(county) & position_location_code == "russell" ~ "51167",          
    is.na(county) & position_location_code == "danville" ~ "51590",        
    is.na(county) & position_location_code == "martinsville" ~ "51690",     
    is.na(county) & position_location_code == "franklin county" ~ "51067",  
    is.na(county) & position_location_code == "warren" ~ "51187",           
    is.na(county) & position_location_code == "shenandoah" ~ "51171",       
    is.na(county) & position_location_code == "albemarle" ~ "51003",       
    is.na(county) & position_location_code == "lexington" ~ "51678",        
    is.na(county) & position_location_code == "accomack" ~ "51001",         
    is.na(county) & position_location_code == "frederick" ~ "51069",        
    is.na(county) & position_location_code == "waynesboro" ~ "51820",       
    is.na(county) & position_location_code == "lancaster" ~ "51103",       
    is.na(county) & position_location_code == "fauquier" ~ "51061",         
    is.na(county) & position_location_code == "hampton" ~ "51650",          
    is.na(county) & position_location_code == "wythe" ~ "51197",            
    is.na(county) & position_location_code == "dickenson" ~ "51051",        
    is.na(county) & position_location_code == "southampton" ~ "51175",     
    is.na(county) & position_location_code == "lee" ~ "51105",              
    is.na(county) & position_location_code == "carroll" ~ "51035",          
    is.na(county) & position_location_code == "stafford" ~ "51179",         
    is.na(county) & position_location_code == "greensville" ~ "51081",      
    is.na(county) & position_location_code == "mecklenburg" ~ "51117",     
    is.na(county) & position_location_code == "manassas park" ~ "51685",    
    is.na(county) & position_location_code == "essex" ~ "51057",            
    is.na(county) & position_location_code == "henry" ~ "51089",            
    is.na(county) & position_location_code == "isle of wight" ~ "51093",    
    is.na(county) & position_location_code == "fredericksburg" ~ "51630",  
    is.na(county) & position_location_code == "bland" ~ "51021",            
    is.na(county) & position_location_code == "brunswick" ~ "51025",        
    is.na(county) & position_location_code == "king george" ~ "51099",      
    is.na(county) & position_location_code == "amherst" ~ "51009",          
    is.na(county) & position_location_code == "bristol" ~ "51520",         
    is.na(county) & position_location_code == "me" ~ "out-of-state",               
    is.na(county) & position_location_code == "nj" ~ "out-of-state",               
    is.na(county) & position_location_code == "rockbridge" ~ "51163",       
    is.na(county) & position_location_code == "fairfax (city)" ~ "51600",   
    is.na(county) & position_location_code == "giles" ~ "51071",           
    is.na(county) & position_location_code == "falls church" ~ "51610",     
    is.na(county) & position_location_code == "orange" ~ "51137",           
    is.na(county) & position_location_code == "louisa" ~ "51109",           
    is.na(county) & position_location_code == "patrick" ~ "51141",          
    is.na(county) & position_location_code == "fluvanna" ~ "51065",        
    is.na(county) & position_location_code == "caroline" ~ "51033",         
    is.na(county) & position_location_code == "charlottesville" ~ "51540",  
    is.na(county) & position_location_code == "richmond county" ~ "51159",  
    is.na(county) & position_location_code == "charlotte" ~ "51037",        
    is.na(county) & position_location_code == "nottoway" ~ "51135",        
    is.na(county) & position_location_code == "fl" ~ "out-of-state",               
    is.na(county) & position_location_code == "md" ~ "out-of-state",               
    is.na(county) & position_location_code == "pa" ~ "out-of-state",               
    is.na(county) & position_location_code == "radford" ~ "51750",          
    is.na(county) & position_location_code == "middlesex" ~ "51119",       
    is.na(county) & position_location_code == "rockingham" ~ "51165",       
    is.na(county) & position_location_code == "appomattox" ~ "51011",       
    is.na(county) & position_location_code == "pittsylvania" ~ "51143",     
    is.na(county) & position_location_code == "new kent" ~ "51127",         
    is.na(county) & position_location_code == "floyd" ~ "51063",           
    is.na(county) & position_location_code == "mathews" ~ "51115",          
    is.na(county) & position_location_code == "rappahannock" ~ "51157",     
    is.na(county) & position_location_code == "buckingham" ~ "51029",       
    is.na(county) & position_location_code == "northumberland" ~ "51133",   
    is.na(county) & position_location_code == "highland" ~ "51091",        
    is.na(county) & position_location_code == "goochland" ~ "51075",        
    is.na(county) & position_location_code == "nelson" ~ "51125",           
    is.na(county) & position_location_code == "charles city" ~ "51036",     
    is.na(county) & position_location_code == "page" ~ "51139",             
    is.na(county) & position_location_code == "clarke" ~ "51043",          
    is.na(county) & position_location_code == "king & queen" ~ "51097",     
    is.na(county) & position_location_code == "hanover" ~ "51085",          
    is.na(county) & position_location_code == "winchester" ~ "51840",       
    is.na(county) & position_location_code == "dinwiddie" ~ "51053",        
    is.na(county) & position_location_code == "cumberland" ~ "51049",      
    is.na(county) & position_location_code == "greene" ~ "51079",           
    is.na(county) & position_location_code == "james city" ~ "51095",       
    is.na(county) & position_location_code == "powhatan" ~ "51145",         
    is.na(county) & position_location_code == "westmoreland" ~ "51193",     
    is.na(county) & position_location_code == "surry" ~ "37171",           
    is.na(county) & position_location_code == "madison" ~ "51113",          
    is.na(county) & position_location_code == "northampton" ~ "37131",      
    is.na(county) & position_location_code == "craig" ~ "51045",            
    is.na(county) & position_location_code == "grayson" ~ "51077",          
    is.na(county) & position_location_code == "amelia" ~ "51007",          
    is.na(county) & position_location_code == "staunton" ~ "51790",         
    is.na(county) & position_location_code == "336" ~ NA_character_,              
    is.na(county) & position_location_code == "york" ~ "51199",             
    is.na(county) & position_location_code == "colonial heights" ~ "51570", 
    is.na(county) & position_location_code == "covington" ~ "51580",       
    is.na(county) & position_location_code == "king william" ~ "51101",     
    is.na(county) & position_location_code == "lunenburg" ~ "51111",        
    is.na(county) & position_location_code == "bath" ~ "51017",             
    is.na(county) & position_location_code == "sussex" ~ "51183",           
    is.na(county) & position_location_code == "478" ~ NA_character_,             
    is.na(county) & position_location_code == "338" ~ NA_character_,              
    is.na(county) & position_location_code == "488" ~ NA_character_,              
    is.na(county) & position_location_code == "de" ~ "out-of-state",               
    is.na(county) & position_location_code == "300" ~ NA_character_,              
    is.na(county) & position_location_code == "hopewell" ~ "51670",        
    is.na(county) & position_location_code == "emporia" ~ "51595",          
    is.na(county) & position_location_code == "galax" ~ "51640",            
    is.na(county) & position_location_code == "poquoson" ~ "51735",         
    is.na(county) & position_location_code == "402" ~ NA_character_,              
    is.na(county) & position_location_code == "369" ~ NA_character_,             
    is.na(county) & position_location_code == "432" ~ NA_character_,              
    is.na(county) & position_location_code == "444" ~ NA_character_,              
    is.na(county) & position_location_code == "494" ~ NA_character_,              
    is.na(county) & position_location_code == "buena vista" ~ "51530",      
    is.na(county) & position_location_code == "tx" ~ "out-of-state",              
    is.na(county) & position_location_code == "362" ~ NA_character_,              
    is.na(county) & position_location_code == "492" ~ NA_character_,              
    is.na(county) & position_location_code == "487" ~ NA_character_,              
    is.na(county) & position_location_code == "401" ~ NA_character_,              
    is.na(county) & position_location_code == "ne" ~ "out-of-state",              
    is.na(county) & position_location_code == "co" ~ "out-of-state",               
    is.na(county) & position_location_code == "ga" ~ "out-of-state",               
    is.na(county) & position_location_code == "wv" ~ "out-of-state",               
    is.na(county) & position_location_code == "nd" ~ "out-of-state",               
    is.na(county) & position_location_code == "mi" ~ "out-of-state",              
    is.na(county) & position_location_code == "411" ~ NA_character_,              
    is.na(county) & position_location_code == "476" ~ NA_character_,              
    is.na(county) & position_location_code == "dc" ~ "out-of-state",               
    is.na(county) & position_location_code == "370" ~ NA_character_,              
    is.na(county) & position_location_code == "tn" ~ "out-of-state",              
    is.na(county) & position_location_code == "md031" ~ "out-of-state",            
    is.na(county) & position_location_code == "ma" ~ "out-of-state",
    TRUE ~ county))


# for each data source date, there should be only one firstname-lastname-startdate-county
va_data_narrow <- va_data_dedup_zip_city_2 %>% 
  select(-c(agency_address_line_1,
            agency_address_line_2,
            agency_city,
            agency_state,
            agency_zip_code,
            position_location_code)) %>% 
  distinct()

# Only fifteen duplicate firstname-lastname-startdate-county within a given 
#   data source date. Dropping these. 
va_data_narrow_dedup <- va_data_narrow %>% 
  mutate(state_begin_date_test = str_replace_na(state_begin_date),
         county_test = str_replace_na(county)) %>% 
  mutate(test_id = str_c(first_name, 
                         last_name, 
                         state_begin_date_test, 
                         county_test)) %>% 
  group_by(data_source_date) %>% 
  mutate(dup = duplicated(test_id)) %>% 
  ungroup() %>% 
  filter(dup == FALSE) %>% 
  select(-state_begin_date_test, 
         -county_test,
         -dup,
         -test_id)

# now need to be able to trace an employee over time.
# try using county but take note of movers
boop <- va_data_narrow_dedup %>% 
  group_by(first_name,
           last_name,
           begin)


# how many people with the same start date, first, and last names chnaged
#   counties? 

# If >1 person works for the same agency with the same title, first name, 
# last name and start date, dropping
boop <- va_data_narrow_dedup %>% 
  group_by(agency_name, 
           data_source_date,
           employee_role_title,
           state_begin_date,
           first_name, 
           last_name) %>% 
  mutate(n_rows = n()) %>%  
  ungroup()

boop %>% 
  filter(n_rows > 1)


boop <- va_data_narrow_dedup %>% 
  group_by(first_name,
           last_name,
           state_begin_date) %>% 
  mutate(employee_id_1 = cur_group_id()) %>% 
  ungroup()

boop %>% 
  group_by(data_source_date) %>% 
  filter(duplicated(employee_id_1))

sum(duplicated(boop$employee_id_1))


# how many people 


# count how many dup names per year
# Create employee ID based on fixed employee-level variables
va_data_dedup <- va_data_dedup %>% 
  group_by(first_name,
           last_name,
           state_begin_date) %>% 
  mutate(employee_id = cur_group_id())

# few dups (n = 229) -- retrieve ids and drop 
duplicate_ids <- va_data_dedup %>% 
  group_by(data_source_date) %>% 
  mutate(dup = duplicated(employee_id)) %>% 
  filter(dup == T) %>% 
  pull(employee_id)

va_data_dedup_2 <- va_data_dedup %>% 
  filter(!employee_id %in% duplicate_ids)


# figure out seperation date
# It appears it is not exhaustive and some people are leaving the system 
#   w/0 an exit date (see zyeshia johnson). So, dropping cases where no_longer_employed == 1
va_data_term <- va_data_dedup_2 %>% 
  group_by(employee_id) %>% 
  fill(position_separation_date,
       .direction = "downup") %>% 
  ungroup() %>% 
  mutate(no_longer_employed = case_when(
    is.na(position_separation_date) ~ 0,
    position_separation_date < data_source_date ~ 1, 
    position_separation_date >= data_source_date ~ 0,
  )) %>% 
  filter(no_longer_employed == 0) %>% 
  select(-no_longer_employed)

# create first/last observed variable
va_data_last <- va_data_term %>% 
  group_by(employee_id) %>% 
  mutate(last_observed_date = max(data_source_date),
         first_observed_date = min(data_source_date)) %>% 
  ungroup()

# For merging with the voter file, only have first and last names as 
#   identifying IDs. So, need to randomly dedup first/last names
dup_va_employees <- va_data_last %>% 
  group_by(data_source_date) %>% 
  mutate(first_last = str_c(first_name,
                            "---",
                            last_name)) %>% 
  mutate(dup = duplicated(first_last)) %>% 
  ungroup() %>% 
  filter(dup == 1) %>% 
  pull(first_last)

va_employees_dedup <- va_data_last %>% 
  mutate(first_last = str_c(first_name,
                            "---",
                            last_name)) %>%
  group_by(first_last) %>% 
  slice_sample(n = 1) %>% 
  ungroup()




# VA voter file ####
va_voters <- vroom("../data/voter-data/VA_final.tsv",
                   delim = "\t")
colnames(va_voters) <- tolower(colnames(va_voters))

va_voters <- va_voters %>% 
  mutate(across(c(voters_firstname,
                voters_lastname),
                tolower))

# need to find everyone with same middle and last and randomly dedup
dup_voters <- va_voters %>% 
  mutate(first_last = str_c(voters_firstname,
                            "---",
                            voters_lastname)) %>% 
  mutate(dup = duplicated(first_last)) %>% 
  filter(dup == 1) %>% 
  pull(first_last)

# how many names are actually duplicated? 
length(unique(dup_voters))
length(dup_voters)
dup_table <- as.data.frame(table(dup_voters))



va_voters_dedup <- va_voters %>% 
  mutate(first_last = str_c(voters_firstname,
                            "---",
                            voters_lastname)) %>%
  group_by(first_last) %>% 
  slice_sample(n = 1) %>% 
  ungroup()



merge_1 <- left_join(va_employees_dedup,
                     va_voters_dedup,
                     by = c("first_name" = "voters_firstname",
                            "last_name" = "voters_lastname"))
merge_1_misses <- anti_join(va_employees_dedup,
                            va_voters_dedup,
                            by = c("first_name" = "voters_firstname",
                                   "last_name" = "voters_lastname"))


# read in aggregated agency file
#   created using this https://www.commonwealth.virginia.gov/va-government/orgchart22/
aggregated_va_agencies <- read_csv("../data/voter-data/va_agencies.csv")
aggregated_va_agencies <- aggregated_va_agencies %>% 
  select(agency_name, 
         umbrella_agency) %>% 
  mutate(umbrella_agency = case_when(
    is.na(umbrella_agency) ~ tolower(agency_name), 
    TRUE ~ umbrella_agency
  ))


merge_1_aggregate <- left_join(merge_1,
                               aggregated_va_agencies,
                               by = "agency_name")
merge_1_aggregate_misses <- anti_join(merge_1,
                                      aggregated_va_agencies,
                                      by = "agency_name")
merge_1_aggregate_misses2 <- anti_join(aggregated_va_agencies,
                                       merge_1,
                                       by = "agency_name")


# Run calculations for partisanship and seniority

merge_1_aggregate_vars <- merge_1_aggregate %>% 
  mutate(party_numeric = case_when(
    parties_description == "Republican" ~ 1,
    parties_description == "Democrat" ~ -1,
    parties_description == "Non-Partisan" ~ 0,
    TRUE ~ NA_real_
  )) %>% 
  mutate(numeric_salary = as.numeric(state_salary_or_wage_hourly_rate)) %>% 
  mutate(norm_salary = (numeric_salary - min(numeric_salary, na.rm = T)) / (max(numeric_salary, na.rm = T) - min(numeric_salary, na.rm = T))) %>% 
  mutate(norm_partisanship = party_numeric * norm_salary) 
  
agency_level_results <- merge_1_aggregate_vars %>% 
  group_by(umbrella_agency, 
           data_source_date) %>% 
  summarise(agency_partisanship = mean(norm_partisanship, na.rm = T),
            agency_n_employees = n())





