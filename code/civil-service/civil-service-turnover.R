library(tidyverse)
library(lubridate)
library(stargazer)
library(broom)

BeRn::set_my_theme()

vt_raw <- readRDS("../data/state-employee-data/VT/clean/vt-09-21-premerge.rds")
nd_raw <- readRDS("../data/state-employee-data/ND/clean/nd-10-22-premerge.rds")
ms_raw <- readRDS("../data/state-employee-data/MS/clean/ms-15-22-premerge.rds")
tx_raw <- readRDS("../data/state-employee-data/TX/clean/tx-19-22-premerge.rds")
nj_raw <- readRDS("../data/state-employee-data/NJ/clean/nj-10-22-premerge.rds")
co_raw <- readRDS("../data/state-employee-data/CO/clean/co-19-22-premerge.rds")
mn_raw <- readRDS("../data/state-employee-data/MN/clean/mn-11-21-premerge.rds")
nv_raw <- readRDS("../data/state-employee-data/NV/clean/nv-10-21-premerge.rds")
va_raw <- readRDS("../data/state-employee-data/VA/clean/va-19-22-premerge.rds")

# clean civil service state data 
# remove anyone working in:
#   a state college/university
#   elected position
#   legislature or judiciary
#   temporary positions

# note that part time employees are still in here although temp employees are dropped

unique(nv_raw$agency_name)
out <- va_raw %>% 
  group_by(data_source_date) %>% 
  summarise(n = n())

# NEED TO CONDENSE VA DATA THAT IS REALLY CLOSE TOGETHER


clean_civil_service <- function(input_data, 
                                state) {
  if(state == "virginia") {
    out <- input_data %>% 
      filter(!employee_category %in% c("university staff",
                                       "faculty")) %>% 
      filter(!agency_name %in% c("house of delegates",
                                 "senate of virginia",
                                 "magistrate system",
                                 "virginia commonwealth univ",
                                 "new college institute",
                                 "virginia military institute")) %>% 
      filter(!str_detect(agency_name,
                         "court|legislat|univers|college|comm coll")) %>% 
      mutate(classified = case_when(
        employee_category == "classified" ~ 1,
        TRUE ~ 0
      ))
  }
  
  if(state == "nevada") {
    out <- input_data %>% 
      filter(agency_name != "judicial branch") %>% 
      filter(job_class != "elected") %>% 
      mutate(classified = case_when(
        job_class == "classified" ~ 1,
        TRUE ~ 0
      ))
    
    out <- out %>% 
      arrange(dedupe.ids,
              cal_yr) %>% 
      group_by(dedupe.ids) %>% 
      mutate(employed_next_period = case_when(
        cal_yr == 2021 ~ NA_real_,
        cal_yr + 1 == lead(cal_yr) ~ 1,
        TRUE ~ 0
      )) %>% 
      mutate(not_employed_next_period = case_when(
        employed_next_period == 1 ~ 0,
        employed_next_period == 0 ~ 1
      )) %>% 
      ungroup()
    
  }
  
  if(state == "minnesota") {
    out <- input_data %>% 
      filter(branch_name == "executive") %>% 
      filter(agency_name != "mn st colleges & universities") %>% 
      filter(classified_desc != "non-status") %>% 
      mutate(classified = case_when(
        classified_desc == "classified" ~ 1,
        TRUE ~ 0
      ))
    
    out <- out %>% 
      arrange(dedupe.ids,
              fiscal_year) %>% 
      group_by(dedupe.ids) %>% 
      mutate(employed_next_period = case_when(
        fiscal_year == 2021 ~ NA_real_,
        fiscal_year + 1 == lead(fiscal_year) ~ 1,
        TRUE ~ 0
      )) %>% 
      mutate(not_employed_next_period = case_when(
        employed_next_period == 1 ~ 0,
        employed_next_period == 0 ~ 1
      )) %>% 
      ungroup()
  } 
  
  if(state == "colorado") {
    out <- input_data %>% 
      filter(!agency_title %in% c("legislative council",
                                  "office of legislative legal services",
                                  "general assembly",
                                  "judicial administration",
                                  "supreme court grievance",
                                  "colorado school of mines",
                                  "auraria higher education center")) %>% 
      filter(!str_detect(agency_title, 
                        "cu - |college|university")) %>%
      filter(classification_status != "non-classified (temporary aide)") %>% 
      mutate(classified = case_when(
        classification_status == "classified" ~ 1,
        classification_status == "non-classified" ~ 0,
      ))
    
    out <- out %>% 
      arrange(dedupe.ids,
              source_date) %>% 
      group_by(dedupe.ids) %>% 
      mutate(employed_next_period = case_when(
        source_date == ymd("2022-05-01") ~ NA_real_,
        floor_date(source_date, 
                   unit = 'month') + period('1 month') == lead(floor_date(source_date, 
                                                                          unit = 'month')) ~ 1,
        TRUE ~ 0
      )) %>% 
      mutate(not_employed_next_period = case_when(
        employed_next_period == 1 ~ 0,
        employed_next_period == 0 ~ 1
      )) %>% 
      ungroup()
  }
  
  if(state == "new jersey") {
    out <- input_data %>% 
      filter(!department_agency_desc %in% c("legislature",
                                 "judiciary")) %>% 
      filter(!class_of_service %in% c("ambiguous",
                                      "authority",
                                      "temporary")) %>% 
      filter(as_of_date != ymd("2022-06-30")) %>% 
      filter(!is.na(class_of_service)) %>% 
      mutate(classified = case_when(
        str_detect(class_of_service, 
                   "career") ~ 1,
        TRUE ~ 0
      ))

      out <- out %>% 
        mutate(as_of_year = as.integer(year(as_of_date))) %>% 
        arrange(dedupe.ids,
                as_of_year) %>% 
        group_by(dedupe.ids) %>% 
        mutate(employed_next_period = case_when(
          as_of_year == 2022 ~ NA_real_,
          as_of_year + 1 == lead(as_of_year) ~ 1,
          TRUE ~ 0
        )) %>% 
        mutate(not_employed_next_period = case_when(
          employed_next_period == 1 ~ 0,
          employed_next_period == 0 ~ 1
        )) %>% 
        ungroup()
  }
  
  if(state == 'texas') {
    out <- input_data %>% 
      filter(!agency_name %in% c("senate",
                                 "ct crim appeals",
                                 "supreme court of texas",
                                 "house of representatives",
                                 "legislative reference library",
                                 "legislative budget board",
                                 "texas legislative council")) %>% 
      filter(!str_detect(agency_name, 
                         "court of appeals")) %>% 
      filter(!str_detect(emp_type, 
                         'temporary')) %>% 
      mutate(classified = case_when(
        str_extract(emp_type, 
                    "^[:alpha:]{1}") == 'c' ~ 1,
        TRUE ~ 0
      )) %>% 
      mutate(month = ymd(month))
    
    out <- out %>% 
      arrange(employee_id,
              month) %>% 
      group_by(employee_id) %>% 
      mutate(employed_next_period = case_when(
        month == ymd("2022-08-31") ~ NA_real_,
        floor_date(month, 
                   unit = 'month') + period('1 month') == lead(floor_date(month, 
                                                                          unit = 'month')) ~ 1,
        TRUE ~ 0
      ))%>% 
      mutate(not_employed_next_period = case_when(
        employed_next_period == 1 ~ 0,
        employed_next_period == 0 ~ 1
      )) %>% 
      ungroup()
  }
  
  if(state == 'vermont') {
    out <- input_data %>% 
      filter(!agency %in% c("legislative",
                            "judiciary")) %>% 
      filter(!job_title %in% c("governor",
                               "lieutenant governor",
                               "treasurer",
                               "secretary of state",
                               'auditor of accounts',
                               'attorney general')) %>% 
      mutate(job_type = case_when(
        str_detect(job_type, 
                   'temporary|contractual') ~ 'temporary',
        TRUE ~ job_type
      )) %>% 
      filter(job_type != 'temporary') %>% 
      mutate(classified = case_when(
        job_type == "classified" ~ 1,
        TRUE ~ 0
      ))
    
    out <- out %>% 
      arrange(dedupe.ids,
              fiscal_year) %>% 
      group_by(dedupe.ids) %>% 
      mutate(employed_next_period = case_when(
        fiscal_year == 2021 ~ NA_real_,
        fiscal_year + 1 == lead(fiscal_year) ~ 1,
        TRUE ~ 0
      )) %>% 
      mutate(not_employed_next_period = case_when(
        employed_next_period == 1 ~ 0,
        employed_next_period == 0 ~ 1
      )) %>% 
      ungroup()
  } 
  
  else if (state == 'north dakota') {
    out <- input_data %>% 
      filter(str_detect(job_code_description,
                        'elected',
                        negate = T)) %>% 
      filter(!agency == 'state judiciary') %>% 
      mutate(classified = case_when(
        classification_status == "classified" ~ 1,
        TRUE ~ 0
      ))
    
    out <- out %>% 
      arrange(dedupe.ids,
              year) %>% 
      group_by(dedupe.ids) %>% 
      mutate(employed_next_period = case_when(
        year == 2022 ~ NA_real_,
        year + 1 == lead(year) ~ 1,
        TRUE ~ 0
      )) %>% 
      mutate(not_employed_next_period = case_when(
        employed_next_period == 1 ~ 0,
        employed_next_period == 0 ~ 1
      )) %>% 
      ungroup()
  }
  
  else if (state == 'mississippi') {
    out <- input_data %>% 
      filter(!status_code_name == "ns - elected officials") %>% 
      filter(!status_code_name == "ns - justices") %>% 
      filter(!status_code_name == "ns - officers/enlisted, national guard") %>% 
      mutate(classified = case_when(
        status_code_name == 'state service' ~ 1,
        TRUE ~ 0
        ))
    
    out <- out %>% 
      arrange(person_id,
              month_observed) %>% 
      group_by(person_id) %>% 
      mutate(employed_next_period = case_when(
        month_observed == ymd("2022-06-30") ~ NA_real_,
        floor_date(month_observed, 
                   unit = 'month') + period('1 month') == lead(floor_date(month_observed, 
                                                                          unit = 'month')) ~ 1,
        TRUE ~ 0
      ))%>% 
      mutate(not_employed_next_period = case_when(
        employed_next_period == 1 ~ 0,
        employed_next_period == 0 ~ 1
      )) %>% 
      ungroup()
  }
  
  return(out)
}

ms_clean <- clean_civil_service(ms_raw,
                                'mississippi')
nd_clean <- clean_civil_service(nd_raw,
                                'north dakota')
vt_clean <- clean_civil_service(vt_raw,
                                'vermont')
tx_clean <- clean_civil_service(tx_raw,
                                'texas')
nj_clean <- clean_civil_service(nj_raw,
                                'new jersey')
co_clean <- clean_civil_service(co_raw,
                                "colorado")
mn_clean <- clean_civil_service(mn_raw,
                                "minnesota")
nv_clean <- clean_civil_service(nv_raw,
                                "nevada")

# analyses ####
# maybe plot differences in turnover rates across states and systems
# maybe do it again controlling for income 

run_turnover_regression <- function(data,
                                    state_abbv) {
  lm_out <- lm(not_employed_next_period ~ classified,
               data = data) %>% 
    augment(newdata = tibble(classified = c(1, 0)),
            se_fit = TRUE,
            interval = 'confidence') %>% 
    mutate(state = state_abbv)
  
  return(lm_out)
}

state_datasets <- list(nd_clean, 
                       vt_clean, 
                       ms_clean,
                       tx_clean,
                       nj_clean,
                       co_clean,
                       mn_clean,
                       nv_clean)
state_abbvs <- c("North Dakota", 
                 "Vermont", 
                 "Mississippi",
                 "Texas",
                 "New Jersey",
                 "Colorado",
                 "Minnesota",
                 "Nevada")

all_states_list <- map2(1:length(state_datasets),
                        state_abbvs,
                       ~run_turnover_regression(state_datasets[[.x]],
                                                .y))


all_states_df <- all_states_list %>% 
  map(~mutate(., 
              classified = as.character(classified))) %>% 
  bind_rows()


print_carets <- function(number) {
  if(str_length(number) == 4) {
    out <- str_c(str_sub(number, 1, 1),
                 ",",
                 str_sub(number, 2, 4))
  } else if(str_length(number) == 5) {
    out <- str_c(str_sub(number, 1, 2),
                 ",",
                 str_sub(number, 3, 5))
  } else if(str_length(number) == 6) {
    out <- str_c(str_sub(number, 1, 3),
                 ",",
                 str_sub(number, 4, 6))
  } else if(str_length(number) == 7) {
    out <- str_c(str_sub(number, 1, 1),
                 ",",
                 str_sub(number, 2, 4),
                 ",",
                 str_sub(number, 5, 7)
                 )
  }
  return(out)
}


all_states_df %>% 
  mutate(state = case_when(
    state == "Colorado" ~ str_c("Colorado\n", 
                                2019,
                                "-",
                                2022,
                                ", by month\n",
                                "N = ", 
                                print_carets(nrow(co_clean))),
    state == "Minnesota" ~ str_c("Minnesota\n", 
                                 2011,
                                "-",
                                2021,
                                ", by year\n",
                                "N = ", 
                                print_carets(nrow(mn_clean))),
    state == "Mississippi" ~ str_c("Mississippi\n", 
                                 2015,
                                 "-",
                                 2022,
                                 ", by month\n",
                                 "N = ", 
                                 print_carets(nrow(ms_clean))),
    state == "Nevada" ~ str_c("Nevada\n", 
                              2010,
                              "-",
                              2021,
                              ", by year\n",
                              "N = ", 
                              print_carets(nrow(nv_clean))),
    state == "New Jersey" ~ str_c("New Jersey\n", 
                                  2010,
                                  "-",
                                  2022,
                                  ", by year\n",
                                  "N = ", 
                                  print_carets(nrow(nj_clean))),
    state == "North Dakota" ~ str_c("North Dakota\n", 
                                    2010,
                                    "-",
                                    2022,
                                    ", by year\n",
                                    "N = ", 
                                    print_carets(nrow(nd_clean))),
    state == "Texas" ~ str_c("Texas\n", 
                             2019,
                             "-",
                             2022,
                             ", by month\n",
                             "N = ", 
                             print_carets(nrow(tx_clean))),
    state == "Vermont" ~ str_c("Vermont\n", 
                               2009,
                               "-",
                               2021,
                               ", by year\n",
                               "N = ", 
                               print_carets(nrow(vt_clean))),
  )) %>% 
  ggplot() + 
  geom_point(aes(x = as.factor(classified),
                 y = .fitted)) +
  geom_errorbar(aes(ymin = .lower,
                    ymax = .upper,
                    x = as.factor(classified))) +
  facet_wrap(~state,
             ncol = length(all_states_list)) + 
  scale_y_continuous(limits = c(0, .5),
                     breaks = seq(0, .5, .1)) + 
  scale_x_discrete(labels = c("0" = 'Exempt',
                              "1" = "Classified")) + 
  labs(x = NULL,
       y = "Probability of Departing in Next Period",
       title = "Turnover Rates by State and Classification Status")
ggsave("../figures/initial-turnover-class.pdf",
       width = 11,
       height = 7,
       units = "in",
       device = cairo_pdf)

