source('load_libs.R')
source('scripts/utils.R')

f_path <- file.path('~','Dropbox','ArmchairAnalysis','nfl_00-19')
input_team <- paste0(f_path, '/TEAM.csv')
input_game <- paste0(f_path, '/GAME.csv')
team <- vroom::vroom(input_team)
game <- vroom::vroom(input_game)
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