pacman::p_load(tidyverse,
               readxl,
               kableExtra)

code_book <- read_xlsx("../codebook.xlsx")

progress_table <- code_book %>% 
  filter(`Next steps` %in% c('done', 'Confidential')) %>% 
  select(c(1:7)) %>% 
  mutate(across(Sex:`Union / civil service`, 
         ~case_when(is.na(.) ~ "", 
                    TRUE ~ .))) %>% 
  mutate(Frequency = case_when(
    str_detect(Frequency, 
               "\\(with hire date\\)") ~ str_replace(Frequency, 
                                                 "\\(with hire date\\)",
                                                 "$\\^1$"),
    TRUE ~ Frequency
  )) %>% 
  mutate(Frequency = case_when(
    str_detect(Frequency, 
               "\\(unsure\\)") ~ str_replace(Frequency, 
                                                     "\\(unsure\\)",
                                                     "$\\^2$"),
    TRUE ~ Frequency
  )) %>% 
  mutate(Frequency = case_when(
    Frequency == 'Confidential' ~ "NA $^3$",
    TRUE ~ Frequency
  )) %>% 
  mutate(Frequency = case_when(
    State == "NV" ~ "Day $^4$", 
    TRUE ~ Frequency
  ))

progress_table_kbl <- progress_table %>% 
  kable(format = 'latex', 
        escape = F,
        align = "lllcccc",
        booktabs = T,
        caption = "Data Collection Progress \\label{tab:progress}") %>%
  kable_styling(latex_options = c("scale_down")) %>%
  footnote(number = c("Includes the date an employee was hired.",
                      "I am working to confirm whether the data is published every calendar or fiscal year.",
                      "Payroll data not available.",
                      "Data only available by calendar year from 2010 - 2017."))

write_lines(progress_table_kbl,
            "../figures/prospectus/progress_table_kbl.txt")
  

# writing a similar one without a caption for beamer slides
progress_table_kbl_2 <- progress_table %>% 
  kable(format = 'latex', 
        escape = F,
        align = "lllcccc",
        booktabs = T,
        caption = NULL,
        linesep = "") %>%
  kable_styling(latex_options = c("scale_down"),
                font_size = 5) %>%
  footnote(number = c("Includes the date an employee was hired.",
                      "I am working to confirm whether the data is published every calendar or fiscal year.",
                      "Payroll data not available.",
                      "Data only available by calendar year from 2010 - 2017."))

write_lines(progress_table_kbl_2,
            "../figures/prospectus/progress_table_kbl_beamer.txt")



