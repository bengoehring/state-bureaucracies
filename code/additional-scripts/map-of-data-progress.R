library(tidyverse)
library(sf)
library(spData)
library(readxl)
BeRn::set_my_theme()



code_book <- read_xlsx("../codebook.xlsx")

state_data <- code_book %>% 
  select(state = State, timeframe = Timeframe) %>% 
  mutate(timeframe = str_remove_all(timeframe, 
                                    "\\s")) %>% 
  mutate(first_year = as.numeric(str_extract(timeframe, "^[:digit:]{4}"))) %>% 
  mutate(last_year = as.numeric(str_extract(timeframe, "[:digit:]{4}$"))) %>% 
  mutate(timespan = case_when(
    is.na(timeframe) ~ 0,
    !state %in% c("HI", "VA") ~ last_year - first_year + 1,
    state == "HI" ~ 7,
    state == "VA" ~ 7,
  )) %>% 
  filter(state %in% state.abb)

# shape files
states_sf <- urbnmapr::get_urbn_map("states", sf = TRUE)

states_sf_data <- left_join(state_data, 
                            states_sf, 
                            by = c("state" = "state_abbv")) %>% 
  mutate(timespan_buckets = case_when(
    timespan == 0 ~ 0,
    timespan %in% 1:5 ~ 1, 
    timespan %in% 6:10 ~ 2,
    timespan > 10 ~ 3
  ))

states_sf_data %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry,
              fill = as.factor(timespan_buckets)),
          color = NA) +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 5),
        legend.key.size = unit(.5, 'cm'),
        text = element_text(family="Fira Sans"),
        legend.position = "bottom") + 
  #labs(title = "Data Collection Progress") +
  scale_fill_manual(labels = c("In progress",
                                 "1-5 Years",
                                 "6-10 Years",
                                 ">10 Years",
                                 "Data Unavailable"),
                      values = c("#FFEE99",
                                 "#92C5DE",
                                 "#4393C3",
                                 "#2166AC",
                                 "#FFEE99"))
ggsave("../presentations/exec-politics-mini/data-progress.pdf",
       device = cairo_pdf,
       height = 3.2,
       width = 4.2)
  




         