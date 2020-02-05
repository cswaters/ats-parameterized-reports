library(dplyr)
library(stringr)
library(purrr)
library(teamcolors)

# data structure with team colors as list columns
tm_colors <- readr::read_rds('raw_data/nfl_colors.rds')
# original team details
team_details <- readr::read_csv('raw_data/team_details.csv')

filter(teamcolors, league == 'nfl') %>% 
  select(name, primary:quaternary) %>% 
  right_join(team_details, by=c('name' = 'fullname')) %>% 
  left_join(tm_colors, by = c('abrv' = 'team')) %>% 
  mutate(logo=make_img_path(tm_info$name)) %>% 
  readr::write_rds('processed_data/team_information.rds')

rm(team_details, tm_colors)