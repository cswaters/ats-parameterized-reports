####### Functions for create_10yr_seas_dataset.R ########
grade_game <- function(location, tm_pts, opp_pts, sprv) {
  new_tm_pts <- ifelse(location == 'v', tm_pts + sprv, tm_pts - sprv)
  new_tm_pts - opp_pts
}
grade_total <- function(tm_pts, opp_pts, ou) {
  (tm_pts + opp_pts) - ou
}
mk_last10_dataset <- function(df, stadiums, cutoff=2008) {
  long <- df %>%
    select(gid, seas, wk, v, h, ptsh, ptsv) %>%
    gather(location, team, -gid, -seas, -ptsh, -ptsv, -wk)
  
  long %>%
    left_join(long %>%
                select(gid, seas, wk, opp = team)) %>%
    filter(team != opp) %>%
    mutate(
      tm_pts = ifelse(location == 'v', ptsv, ptsh),
      opp_pts = ifelse(location == 'h', ptsv, ptsh)
    ) %>%
    select(-(ptsh:ptsv)) %>%
    left_join(df %>%
                select(gid,
                       stad,
                       temp,
                       humd,
                       cond,
                       surf,
                       sprv,
                       ou)) %>% 
    mutate(intl_game = as.numeric(stad %in% neutral_stadiums)) %>% 
    filter(seas >= cutoff)
}
add_ats_grades <- function(df){
  df %>% 
    mutate(
      mov = tm_pts - opp_pts,
      ats_w = as.integer(grade_game(location, tm_pts, opp_pts, sprv) > 0),
      ats_l = as.integer(grade_game(location, tm_pts, opp_pts, sprv) < 0),
      ats_p = as.integer(grade_game(location, tm_pts, opp_pts, sprv) == 0),
      ou_o = as.integer(grade_total(tm_pts, opp_pts, ou) > 0),
      ou_u = as.integer(grade_total(tm_pts, opp_pts, ou) < 0),
      ou_p = as.integer(grade_total(tm_pts, opp_pts, ou) == 0),
      su_w = as.integer(tm_pts > opp_pts),
      su_l = as.integer(tm_pts < opp_pts),
      su_p = as.integer(tm_pts == opp_pts)
    )
}
fix_teams <- function(df) {
  df[df$team == 'SD', 'team'] <- 'LAC'
  df[df$team == 'SD', 'opp'] <- 'LAC'
  df[df$team == 'STL', 'team'] <- 'LA'
  df[df$team == 'STL', 'opp'] <- 'LA'
  df
}
add_game_details <- function(df, details){
  df %>% 
    left_join(details %>% 
                select(
                  team = abrv,
                  conf_tm = conf,
                  div_tm = div
                )) %>% 
    left_join(details %>% 
                select(
                  opp = abrv,
                  conf_opp = conf,
                  div_opp = div
                )) %>% 
    mutate(
      div_game = as.numeric(div_tm == div_opp),
      conf_game = as.numeric(conf_tm == conf_opp),
      playoff_game = as.numeric(wk > 17)
    )
}
add_game_stats <- function(df, team) {
  tm_stats <- select(team, gid:fgy) %>%
    left_join(team %>%
                select(gid:fgy),
              by = 'gid',
              suffix = c('_tm', '_opp')) %>%
    filter(tname_tm != tname_opp) %>%
    select(-tname_opp) %>%
    rename(team = tname_tm)
  df %>%
    left_join(tm_stats,
              by = c('gid', 'team')) %>%
    mutate(
      ypp_tm = (ry_tm + py_tm) / tm_pts,
      ypp_opp = (ry_opp + py_opp) / opp_pts
    )
}