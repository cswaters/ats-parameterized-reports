library(dplyr)

team_stats <- readr::read_rds('processed_data/stats_10y_seas.rds')
output_file <- 'processed_data/big_four_ranks.rds'
max_season <- max(team_stats$seas)

get_big_four_stats(team_stats, max_season) %>% 
  get_big_four_ranks(max_season) %>% 
  readr::write_rds(output_file)
