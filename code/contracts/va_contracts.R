library(tidyverse)
library(vroom)
library(lubridate)
library(readxl)
library(broom)
library(stargazer)

# read in files
files <- list.files("../data/contracts/VA/raw/")
file_paths <- str_c("../data/contracts/VA/raw/", 
                    files)

read_va_files <- function(path) {
  if(str_detect(path, "2019")) {
    out <- vroom(path, 
                 n_max = 1681460,
                 col_types = cols(.default = "c"))
  } else {
    out <- vroom(path, 
                 guess_max = 100000,
                 col_types = cols(.default = "c"))
  }
  return(out)
}

va_raw_list <- map(file_paths, read_va_files)
names(va_raw_list) <- files
# ignoring these problems as they seem to be from rows not read in (?)
map(va_raw_list, problems)



va_raw_df <- va_raw_list %>% 
  bind_rows(.id = "id")

colnames(va_raw_df) <- tolower(str_replace_all(colnames(va_raw_df), 
                                               " ", 
                                               "_"))





va_clean_df <- va_raw_df %>%
  mutate(requisition_approved_date = parse_date_time(requisition_approved_date,
                                                     c("%Y.%m.%d",
                                                       "%m.%d.%y",
                                                       "mdYHMS"))) %>% 
  mutate(requisition_approved_month = month(requisition_approved_date)) %>% 
  mutate(requisition_approved_year = year(requisition_approved_date)) %>% 
  mutate(across(where(is.character),
                tolower)) %>% 
  filter(vendor_address_state %in% c("va", 
                                     "virginia")) %>% 
  mutate(across(c(swam_minority:swam_micro_business,
                  line_total,
                  line_total_change),
                as.numeric)) %>% 
  mutate(after_21_election = case_when(
    requisition_approved_date > mdy("11-2-2021") ~ 1, 
    requisition_approved_date <= mdy("11-2-2021") ~ 0,
    TRUE ~ NA_real_
  )) %>% 
  mutate(after_21_inaug = case_when(
    requisition_approved_date > mdy("01-15-2022") ~ 1, 
    requisition_approved_date <= mdy("01-15-2022") ~ 0,
    TRUE ~ NA_real_
  )) %>% 
  mutate(zip_5_digit = as.numeric(str_extract(vendor_address_postal,
                                              "^\\d{5}")))

# add zip code data for merge with county data
zips <- read_xlsx("../data/other/ZIP_COUNTY_122021.xlsx") %>% 
  mutate(across(where(is.character),
                tolower)) %>% 
  filter(usps_zip_pref_state == "va") %>% 
  group_by(zip) %>% 
  slice_max(tot_ratio, 
            with_ties = F) %>% 
  ungroup()

county_returns <- read_csv("../data/other/countypres_2000-2020.csv") %>% 
  mutate(across(where(is.character),
                tolower)) %>% 
  filter(state == "virginia") %>% 
  filter(year == 2020) %>% 
  filter(str_detect(candidate, 'trump')) %>% 
  group_by(county_fips) %>% 
  mutate(trump_share = sum(candidatevotes) / totalvotes) %>% 
  ungroup() %>% 
  select(-candidatevotes, -mode) %>% 
  distinct()

zip_vote_share <- left_join(zips,
                            county_returns,
                            by = c("county" = "county_fips")) %>% 
  select(zip_code = zip,
         county_fips = county,
         county_name = usps_zip_pref_city,
         trump_share) %>% 
  mutate(zip_code = as.numeric(zip_code))


spending_by_county <- left_join(va_clean_df,
                                zip_vote_share,
                                by = c("zip_5_digit" = "zip_code")) %>% 
  filter(str_detect(entity_description, 
                    "department of")) %>% 
  mutate(three_months_before_after = case_when(
    requisition_approved_date >= mdy("03-01-2022") & requisition_approved_date < mdy("06-01-2022") ~ 'after',
    requisition_approved_date >= mdy("03-01-2021") & requisition_approved_date < mdy("06-01-2021") ~ 'before',
    TRUE ~ NA_character_
  )) %>% 
  filter(!is.na(three_months_before_after)) %>% 
  group_by(county_name,
           three_months_before_after) %>% 
  mutate(county_monthyear_spending = sum(line_total, na.rm = T)) %>% 
  select(county_name,
         three_months_before_after,
         trump_share,
         county_monthyear_spending) %>% 
  distinct()


lm_out <- lm(county_monthyear_spending ~ as.factor(three_months_before_after)*trump_share + as.factor(county_name),
             data = spending_by_county)
stargazer(lm_out, 
          type = 'text',
          omit = "county_name")







# look at spending by zip code of vendor 
spending_by_zip <- va_clean_df %>% 
  group_by(requisition_approved_month, 
           requisition_approved_year, 
           line_total,
           vendor) %>% 
  summ

sum(str_detect(va_clean_df$`order_#`, "\\w{2}\\d{7}"), na.rm = T)










# summary stats year before and after youngkin
boop <- va_clean_df %>% 
  filter(requisition_approved_date > mdy("01-01-2021") & requisition_approved_date < mdy("01-01-2023")) %>% 
  mutate(requisition_approved_month = month(requisition_approved_date)) %>% 
  mutate(requisition_approved_year = year(requisition_approved_date)) %>% 
  group_by(requisition_approved_month,
           requisition_approved_year) %>% 
  summarise(across(c(swam_minority,
                     swam_woman,
                     swam_small,
                     swam_micro_business),
                   ~sum(., na.rm = T))) %>% 
  ungroup()



boop2 <- va_clean_df %>%
  mutate(line_total = as.numeric(line_total)) %>% 
  filter(requisition_approved_date > mdy("01-01-2021") & requisition_approved_date < mdy("01-01-2023")) %>% 
  mutate(requisition_approved_month = month(requisition_approved_date)) %>% 
  mutate(requisition_approved_year = year(requisition_approved_date)) %>% 
  group_by(requisition_approved_month,
           requisition_approved_year) %>% 
  summarise(across(c(swam_minority,
                     swam_woman,
                     swam_small,
                     swam_micro_business),
                   ~sum(line_total, na.rm = T))) %>% 
  ungroup()

           
# create plot that shows the number and amount of contract to different swam
#   groups on average in year before and after election change by agency
boop2 <- va_clean_df %>%
  filter(vendor_address_state %in% c("va", 
                                     "virginia"))
  mutate(line_total = as.numeric(line_total)) %>% 
  mutate(three_months_before_after = case_when(
    requisition_approved_date >= mdy("03-01-2022") & requisition_approved_date < mdy("06-01-2022") ~ 'after',
    requisition_approved_date >= mdy("03-01-2021") & requisition_approved_date < mdy("06-01-2021") ~ 'before',
    TRUE ~ NA_character_
  )) %>% 
  filter(three_months_before_after %in% c("after", 'before')) %>% 
  group_by(three_months_before_after,
           entity_description) %>% 
  summarise(across(c(swam_minority,
                     swam_woman,
                     swam_small,
                     swam_micro_business),
                   ~sum(., na.rm = T),
                   .names = "{.col}_n")) %>% 
  ungroup() %>% 
  filter()



sort(table(va_clean_df$vendor_address_state))






sum(va_clean_df$swam_minority, na.rm = T)
sum(va_clean_df$swam_woman, na.rm = T)

unique(va_clean_df$requisition_submitted_date_lub)           
           
           case_when(
    str_detect(requisition_approved_date,
               "\\d{1,2}/\\d{1,2}/\\d{1,2}") ~ mdy(requisition_approved_date),
    str_detect(requisition_approved_date,
               "\\d{4}/\\d{2}/\\d{2}") ~ ymd(requisition_approved_date),
    TRUE ~ NA_Date_
  ))
           
sum(is.na(va_clean_df$requisition_approved_date_lub))

unique(guess_formats(va_raw_df$requisition_approved_date[1:10], c("mdy", "ymd")))

