library(tidyverse)
library(zeallot)
library(heddlr)

source('scripts/utils.R')

input_file <- 'processed_data/team_information.rds'
team_info <- read_rds(input_file) %>% select(abrv,name)

c(team, fullname) %<-% team_info
page_outputs <- make_f_name(fullname, 'Rmd')

multi_team <- import_pattern('nfl_multi_team.Rmd')

team_reports <- team %>% 
  heddle(multi_team, 'TEAMNAME')

# generate rmarkdown files
map2(team_reports, page_outputs, ~make_template(.x) %>% 
       export_template(.y))

rm(list = ls())



