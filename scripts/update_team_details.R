library(dplyr)
library(stringr)
library(purrr)
library(teamcolors)
source('scripts/utils.R')

coaches <- read_csv('raw_data/head_coaches_19.csv')

# data structure with team colors as list columns
tm_colors <- readr::read_rds('raw_data/nfl_colors.rds')
# original team details
team_details <- readr::read_csv('raw_data/team_details.csv')

details <- filter(teamcolors, league == 'nfl') %>% 
  select(name, primary:quaternary) %>% 
  right_join(team_details, by=c('name' = 'fullname')) %>% 
  left_join(tm_colors, by = c('abrv' = 'team')) %>% 
  mutate(logo=make_img_path(team_details$fullname))
  
details  %>%
  select(-coach) %>%
  left_join(coaches %>%
              select(Team, coach = Coach),
            by = c('name' = 'Team')) %>% 
  readr::write_rds('processed_data/team_information.rds')

rm(list = ls())
