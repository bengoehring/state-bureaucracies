# Merge Texas personnel file with the L2 data

library(tidyverse)
library(vroom)
source("additional-scripts/compile-voter-data.R")

tx_l2_data <- read_state_file("VM2--TX--2021-03-25/VM2--TX--2021-03-25-DEMOGRAPHIC.tab",
                              "TX")
