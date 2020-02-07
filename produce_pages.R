library(tidyverse)
library(zeallot)

source('scripts/utils.R')
input_file <- 'processed_data/team_information.rds'

team_info <- read_rds(input_file) %>% select(abrv,name)
c(team, fullname) %<-% team_info
page_outputs <- make_f_name(fullname, 'md')

gen_team_output(team, fullname, page_outputs)



