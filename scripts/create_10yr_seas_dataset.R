source('load_libs.R')
source('scripts/utils.R')

team <- vroom::vroom('raw_data/team.csv')
game <- vroom::vroom('raw_data/game.csv')
game_details <- vroom::vroom('raw_data/team_details.csv')

neutral_stadiums <- c(
  'Wembley Stadium',
  'Twickenham',
  'Twickenham Stadium',
  'Estadio Azteca',
  'Azteca Stadium'
)

league <- mk_last10_dataset(game, neutral_stadiums) %>%
  fix_teams() %>%
  add_ats_grades() %>% 
  add_game_details(game_details)

league %>%
  readr::write_rds('processed_data/ats_10y_seas.rds')

league %>%
  add_game_stats(team) %>%
  readr::write_rds('processed_data/stats_10y_seas.rds')

rm(list = ls())
gc()