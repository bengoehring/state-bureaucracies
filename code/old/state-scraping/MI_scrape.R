pacman::p_load(tidyverse,
               rvest,
               xml2,
               urltools)

MI_html <- read_html("https://www.mackinac.org/depts/policy/salary-table.aspx?report=state&sort=wage2020-desc&count=100000#report")

MI_html_table <- MI_html %>% 
  html_node("table") %>% 
  html_table()

write_csv(MI_html_table, 
          file = "../data/state-employee-data/MI/MI_employee_data.csv")


boop <- read_csv("../data/state-employee-data/MI/MI_employee_data.csv")


