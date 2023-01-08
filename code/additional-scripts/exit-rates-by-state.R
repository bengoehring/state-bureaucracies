library(tidyverse)
library(lubridate)
library(gridExtra)
BeRn::set_my_theme()

# read in data
va_personnel_raw <- readRDS("../data/state-employee-data/VA/clean/va-19-22-premerge.rds")
ct_personnel_raw <- readRDS("../data/state-employee-data/CT/clean/ct-15-22-premerge.rds")
ar_personnel_raw <- readRDS("../data/state-employee-data/AR/clean/ar-19-22-premerge.rds")
ms_personnel_raw <- readRDS("../data/state-employee-data/MS/clean/ms-19-21-premerge.rds")
il_personnel_raw <- readRDS("../data/state-employee-data/IL/clean/il-11-22-premerge.rds")


# create dataset containing period, id, agency, title
va_personnel_clean <- va_personnel_raw %>% 
  mutate(employee_id = str_c("va_", dedupe.ids)) %>% 
  mutate(state = "VA") %>% 
  filter(!employee_category %in% c("faculty", 
                                   "university staff")) %>% 
  mutate(civil_service = if_else(employee_category == "classified", 1, 0)) %>% 
  group_by(employee_id) %>% 
  mutate(mean_salary = mean(as.numeric(state_salary_or_wage_hourly_rate),
                            na.rm = T)) %>% 
  ungroup() %>% 
  mutate(top_salary = case_when(
    mean_salary >= quantile(mean_salary, .90) ~ 1, 
    TRUE ~ 0
  )) %>% 
  select(period = data_source_date, 
         employee_id,
         state,
         agency = agency_name,
         job_title = employee_role_title,
         top_salary,
         civil_service,
         -mean_salary)

ct_personnel_clean <- ct_personnel_raw %>% 
  mutate(employee_id = str_c("ct_", emplid_empl_rcd)) %>% 
  mutate(state = "CT") %>% 
  mutate(period = floor_date(as_date(check_dt), 
                             "month")) %>% 
  mutate(civil_service = NA_real_) %>% 
  group_by(employee_id) %>% 
  mutate(mean_salary = mean(as.numeric(tot_gross),
                            na.rm = T)) %>% 
  ungroup() %>% 
  mutate(top_salary = case_when(
    mean_salary >= quantile(mean_salary, .90) ~ 1, 
    TRUE ~ 0
  )) %>%  
  select(period,
         employee_id,
         state,
         agency,
         job_title = job_cd_descr,
         top_salary,
         civil_service,
         -mean_salary) 
  

ar_personnel_clean <- ar_personnel_raw %>% 
  mutate(employee_id = str_c("ar_", dedupe.ids)) %>% 
  mutate(state = "AR") %>% 
  group_by(employee_id) %>% 
  mutate(civil_service = NA_real_) %>%
  mutate(mean_salary = mean(as.numeric(annual_salary),
                            na.rm = T)) %>% 
  ungroup() %>% 
  mutate(top_salary = case_when(
    mean_salary >= quantile(mean_salary, .90) ~ 1, 
    TRUE ~ 0
  )) %>%  
  select(period = month_year,
         employee_id,
         state,
         agency = personnel_area_description,
         job_title = position_title,
         top_salary,
         civil_service,
         -mean_salary)


ms_personnel_clean <- ms_personnel_raw %>% 
  mutate(employee_id = str_c("ms_", as.character(person_id))) %>% 
  mutate(state = "MS") %>% 
  group_by(employee_id) %>% 
  mutate(civil_service = NA_real_) %>% 
  mutate(mean_salary = mean(as.numeric(current_salary),
                            na.rm = T)) %>% 
  ungroup() %>% 
  mutate(top_salary = case_when(
    mean_salary >= quantile(mean_salary, .90) ~ 1, 
    TRUE ~ 0
  )) %>%
  select(period = month_observed,
         employee_id,
         state,
         agency = agency_name,
         job_title,
         top_salary,
         civil_service,
         -mean_salary)


il_personnel_clean <- il_personnel_raw %>% 
  mutate(employee_id = str_c("il_", person_id)) %>% 
  mutate(year_date = ymd(str_c(year, "-01-01"))) %>% # setting this to calendar year but need to check if FY
  mutate(state = "IL") %>% 
  mutate(agency = str_c(agency, "-", agency_division)) %>% 
  mutate(civil_service = case_when(
    rutan_exempt == "no" ~ 1, 
    TRUE ~ 0
  )) %>%
  group_by(employee_id) %>% 
  mutate(mean_salary = mean(as.numeric(ytd_gross),
                            na.rm = T)) %>% 
  ungroup() %>% 
  mutate(top_salary = case_when(
    mean_salary >= quantile(mean_salary, .90) ~ 1, 
    TRUE ~ 0
  )) %>%
  select(period = year_date, 
         employee_id,
         state,
         agency,
         job_title = position_title,
         top_salary,
         civil_service,
         -mean_salary)

# bind state files together
all_personnel <- rbind(va_personnel_clean,
                       ct_personnel_clean,
                       ar_personnel_clean,
                       ms_personnel_clean,
                       il_personnel_clean)

clean_personnel <- all_personnel %>% 
  filter(str_detect(agency, 
                    "ccc|csu|institute|uconn|college|univ|judicial|legislative|governor",
                    negate = T)) %>% 
  filter(str_detect(job_title, 
                    "college|univ|judicial|legislative|governor|professor|lecturer|governor|lieutenant governor|faculty",
                    negate = T))


# number of employees per period per state
n_employees_period <- clean_personnel %>% 
  group_by(state, 
           period,
           top_salary) %>% 
  summarise(n_employees = n())

# number of employees who left 
n_exits_period <- clean_personnel %>% 
  group_by(state, 
           employee_id) %>% 
  arrange(period, 
          .by_group = T) %>% 
  slice_tail() %>% 
  ungroup() %>% 
  group_by(state, 
           period,
           top_salary) %>% 
  summarise(n_last_period = n())


all_periods <- left_join(n_employees_period, 
                         n_exits_period,
                         by = c("state",
                                "period",
                                "top_salary")) %>% 
  mutate(share_exits = n_last_period / n_employees * 100)

# dropping data from the last period for each state 
all_periods <- all_periods %>% 
  filter(!(state == "AR" & period == ymd("2022-07-01"))) %>% 
  filter(!(state == "CT" & period == ymd("2022-04-01"))) %>%
  filter(!(state == "IL" & period == ymd("2022-01-01"))) %>%
  filter(!(state == "MS" & period == ymd("2022-06-30"))) %>%
  filter(!(state == "VA" & period == ymd("2022-06-30")))

# dropping a few periods that are wrong or have next to no data
all_periods_clean <- all_periods %>% 
  filter(period != ymd("1920-12-01")) %>% 
  filter(!(state == "CT" & period == ymd("2014-10-01"))) %>% 
  filter(!(state == "CT" & period == ymd("2014-12-01"))) %>% 
  filter(!(state == "VA" & period == ymd("2019-04-04"))) %>% 
  filter(!(state == "VA" & period == ymd("2021-01-04")))

all_periods_clean %>% 
  ggplot() + 
  geom_line(aes(x = period, 
                y = share_exits,
                linetype = as.factor(top_salary))) + 
  facet_wrap(~state,
             nrow = 5) + 
  geom_vline(aes(xintercept = ymd("2021-1-20")),
             linetype = 'dashed')

# get dates of each gubernatorial election in each state
# need to drop higher ed from all
# IL has civil service info with rutan variable
# VA has it too
# Maybe try just the highest paid -- like top 10% by state?
   

# number of employees per period per state
n_employees_period_civil <- clean_personnel %>% 
  filter(state %in% c("VA", "IL")) %>% 
  group_by(state, 
           period,
           civil_service) %>% 
  summarise(n_employees = n())

# number of employees who left 
n_exits_period_civil <- clean_personnel %>% 
  filter(state %in% c("VA", "IL")) %>% 
  group_by(state, 
           employee_id) %>% 
  arrange(period, 
          .by_group = T) %>% 
  slice_tail() %>% 
  ungroup() %>% 
  group_by(state, 
           period,
           civil_service) %>% 
  summarise(n_last_period = n())


all_periods_civil <- left_join(n_employees_period_civil, 
                         n_exits_period_civil,
                         by = c("state",
                                "period",
                                "civil_service")) %>% 
  mutate(share_exits = n_last_period / n_employees * 100)

# dropping data from the last period for each state 
all_periods_civil <- all_periods_civil %>% 
  filter(!(state == "AR" & period == ymd("2022-07-01"))) %>% 
  filter(!(state == "CT" & period == ymd("2022-04-01"))) %>%
  filter(!(state == "IL" & period == ymd("2022-01-01"))) %>%
  filter(!(state == "MS" & period == ymd("2022-06-30"))) %>%
  filter(!(state == "VA" & period == ymd("2022-06-30")))

# dropping a few periods that are wrong or have next to no data
all_periods_civil <- all_periods_civil %>% 
  filter(period != ymd("1920-12-01")) %>% 
  filter(!(state == "CT" & period == ymd("2014-10-01"))) %>% 
  filter(!(state == "CT" & period == ymd("2014-12-01"))) %>% 
  filter(!(state == "VA" & period == ymd("2019-04-04"))) %>% 
  filter(!(state == "VA" & period == ymd("2021-01-04")))

IL_plot <- all_periods_civil %>% 
  filter(state == "IL") %>% 
  ggplot() + 
  geom_line(aes(x = period, 
                y = share_exits,
                linetype = as.factor(civil_service))) +
  labs(y = "Share of Employees that Depart",
       x = NULL,
       linetype = NULL,
       title = "Illinois") +
  theme(legend.position = "none",
        text = element_text(family = "Fira Sans"),
        plot.title = element_text(face = "plain",
                                  size = 7),
        legend.text = element_text(size = 5),
        axis.text = element_text(size = 5),
        axis.title = element_text(size = 6)) +
  scale_y_continuous(breaks = seq(0, 25, 5),
                     labels = str_c(seq(0, 25, 5), "%")) +
  scale_x_continuous(limits = c(ymd('2011-01-01'),
                                ymd('2022-06-01')),
                     breaks = seq(ymd('2011-01-01'),
                                  ymd('2022-01-01'), 
                                  by = '1 year'),
                     labels = seq(2011, 2022, 1)) +
  scale_linetype_manual(values = c("dashed",
                                   "solid")) +
  annotate("text",
           x = ymd('2017-7-1'), 
           y = 22.5,
           label = "Not Covered by\nCivil Service",
           size = 2) +
  annotate("text",
           x = ymd('2019-07-1'), 
           y = 2.5,
           label = "Covered by Civil Service",
           size = 2) +
  annotate("rect", 
           xmin = ymd("2011-1-1"), 
           xmax = ymd("2014-11-4"),
           ymin = 0,
           ymax = 25,
           alpha = .1,
           fill = "blue") +
  annotate("rect", 
           xmin = ymd("2014-11-4"), 
           xmax = ymd("2015-1-12"),
           ymin = 0,
           ymax = 25,
           alpha = .1,
           fill = "purple") +
  annotate("rect", 
           xmin = ymd("2015-1-12"), 
           xmax = ymd("2018-11-6"),
           ymin = 0,
           ymax = 25,
           alpha = .1,
           fill = "red") +
  annotate("rect", 
           xmin = ymd("2018-11-6"), 
           xmax = ymd("2019-1-14"),
           ymin = 0,
           ymax = 25,
           alpha = .1,
           fill = "purple") +
  annotate("rect", 
           xmin = ymd("2019-1-14"), 
           xmax = ymd("2021-01-01"),
           ymin = 0,
           ymax = 25,
           alpha = .1,
           fill = "blue")

VA_plot <- all_periods_civil %>% 
  filter(state == "VA") %>% 
  ggplot() + 
  geom_line(aes(x = period, 
                y = share_exits,
                linetype = as.factor(civil_service))) + 
  labs(x = NULL,
       y = "Share of Employees that Depart",
       linetype = NULL,
       title = "Virginia") + 
  scale_y_continuous(limits = c(0, 25),
                     breaks = seq(0, 25, 5),
                     labels = str_c(seq(0, 25, 5), "%")) +
  scale_x_continuous(limits = c(ymd('2011-01-01'),
                                ymd('2022-06-01')),
                     breaks = seq(ymd('2011-01-01'),
                                  ymd('2022-01-01'), 
                                  by = '1 year'),
                     labels = seq(2011, 2022, 1)) +
  scale_linetype_manual(values = c("dashed",
                                   "solid")) +
  theme(legend.position = 'none',
        text = element_text(family = "Fira Sans"),
        plot.title = element_text(face = "plain",
                                  size = 7),
        legend.text = element_text(size = 5),
        axis.text = element_text(size = 5),
        axis.title = element_text(size = 6)) +
  annotate("rect", 
           xmin = ymd("2022-1-15"), 
           xmax = ymd("2022-3-31"),
           ymin = 0,
           ymax = 25,
           alpha = .1,
           fill = "red") +
  annotate("rect", 
           xmin = ymd("2021-11-2"), 
           xmax = ymd("2022-1-15"),
           ymin = 0,
           ymax = 25,
           alpha = .1,
           fill = "purple") +
  annotate("rect", 
           xmin = ymd("2018-12-31"), 
           xmax = ymd("2021-11-2"),
           ymin = 0,
           ymax = 25,
           alpha = .1,
           fill = "blue")


IL_and_VA_plot <- grid.arrange(IL_plot,
                               VA_plot,
                               nrow = 2)
ggsave(plot = IL_and_VA_plot,
       "../presentations/exec-politics-mini/images/il-and-va.pdf",
       device = cairo_pdf,
       height = 3.2,
       width = 4.2)
  
  


