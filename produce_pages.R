library(tidyverse)
library(zeallot)
source('scripts/utils.R')
input_file <- 'processed_data/team_information.rds'

c(team, fullname) %<-% input_file %>% 
  readr::read_rds(path = .) %>%
  select(abrv, name)

page_outputs <- make_f_name(fullname, 'html')

gen_team_output(team, fullname, page_outputs)

rm(list = ls())


