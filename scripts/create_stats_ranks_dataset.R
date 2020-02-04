library(dplyr)
library(purrr)
source('scripts/utils.R')
team_stats <- readr::read_rds('processed_data/stats_10y_seas.rds')
last_year <- max(team_stats$seas)
save_path <-
  paste0('processed_data/nfl-', 
         as.character(last_year), 
         '-stats-ranks.rds')

##########
team_sum_stats <- team_stats %>% 
  filter(seas == last_year) %>% 
  get_team_rank_stats()

get_ranks(team_sum_stats) %>% 
  left_join(team_sum_stats) %>% 
  readr::write_rds(save_path)

rm(list = ls())
gc()