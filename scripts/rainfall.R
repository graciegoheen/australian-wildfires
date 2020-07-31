# Reads in rainfall data, gathered from the Australian Bureau of Meterology (BoM). 
# A number of weather stations were chosen, based on their proximity to major 
# Australian cities such as Sydney, Perth, Brisbane, Canberra, and Adelaide. 
# The South East region of Australia appears to be the most affected.

# Author: Gracie Goheen
# Version: 2020-02-02

# Libraries
library(tidyverse)

# Parameters
url_data <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv"
file_output_path <- here::here("c01-own/data/rainfall.rds")

#===============================================================================

url_data %>%
  read_csv() %>% 
  write_rds(file_output_path)

