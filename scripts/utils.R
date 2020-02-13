source('load_libs.R')
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
  df[df$opp == 'SD', 'opp'] <- 'LAC'
  df[df$team == 'STL', 'team'] <- 'LA'
  df[df$opp == 'STL', 'opp'] <- 'LA'
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

####### Functions for create_stats_ranks_dataset.R ########
get_team_rank_stats <- function(df) {
  df %>%
    group_by(team) %>%
    transmute(
      f1d_tm = rfd_tm + pfd_tm + ifd_tm,
      f1d_opp = rfd_opp + pfd_opp + ifd_opp,
      tovrtds_tm = tdt_tm,
      tovrtds_opp = tdt_opp,
      top_tm = (top_tm / 60),
      top_opp = (top_opp / 60),
      fgm_tm,
      fgm_opp,
      td_tm,
      td_opp
    ) %>%
    summarise_all(mean) %>%
    ungroup()
}
# Helper function to turn stats to ranks
get_ranks <- function(df) {
  # helper function to flip ranking
  rev_rank <- function(x) {
    min_rank(desc(x))
  }
  tm_stats_rank <-
    c('f1d_tm', 'tovrtds_tm', 'top_tm', 'fgm_tm', 'td_tm')
  opp_stats_rank <-
    c('f1d_opp', 'tovrtds_opp', 'top_opp', 'fgm_opp', 'td_opp')
  
  df %>%
    mutate_at(.vars = all_of(tm_stats_rank), rev_rank) %>%
    mutate_at(.vars = all_of(opp_stats_rank), min_rank) %>%
    set_names(c('team', paste0(names(.)[-1], '_rank')))
}

####### Functions for create_big_four_stats_dataset #######
get_big_four_stats <- function(df, max_season){
  df %>% 
  filter(seas >= max_season - 5, wk < 18) %>% 
  group_by(team, seas) %>% 
    summarise_at(.vars = vars(pts_tm:fgy_opp, -opp),
                 .funs = mean) %>% 
    select(seas, team, 
           pts_tm, pts_opp, 
           py_tm, py_opp, 
           ry_tm, ry_opp) %>% 
    mutate(ypp_tm = (ry_tm + py_tm)/pts_tm,
           ypp_opp = (ry_opp + py_opp)/pts_opp)
  
}
get_big_four_ranks <- function(df, last_season){
  df %>% 
    filter(seas == last_season) %>% 
    left_join(
      df %>% 
        ungroup() %>% 
        filter(seas == last_season) %>% 
        mutate_at(.vars = vars(-team), .funs = min_rank) %>%
        select(-seas),
      by = 'team',
      suffix = c('_stat', '_rank')
    ) %>% 
    mutate(
      pts_tm_rank = 33 - pts_tm_rank,
      py_tm_rank = 33 - py_tm_rank,
      ry_tm_rank = 33 - ry_tm_rank,
      ypp_opp_rank = 33 - ypp_opp_rank
    ) %>% 
    ungroup()
}


####### Functions for update_team_details.R ######
make_img_path <- function(name){
  glue::glue(
    'nfl_logos/{tname}_logo.png',
    tname=str_replace_all(name, ' ', '_'))
}
####### Functions for nfl_template.Rmd
# get conference and division games
get_conf_games <- . %>% filter(conf_game == 1)
get_div_games <- . %>% filter(div_game == 1)
# helper function to calculate win percentage
win_pct <- function(w, p, games) {
  (sum(w) + (sum(p) * .5)) / games
}
ats_pct_summary <- partial(.f = summarise,
                           mov = mean(mov),
                           ou = mean(ou),
                           spread = mean(spread),
                           su_pct = win_pct(su_w, su_p, n()),
                           ats_pct = win_pct(ats_w, ats_p, n()),
                           ovr_pct = win_pct(ou_o, ou_p, n()))
# get margin/ats/su/ou records by h/v
get_location_records <- function(df, last_seas) {
  df <- df %>%
    filter(seas == last_seas) %>%
    mutate(
      spread = ifelse(location == 'h', sprv * -1, sprv),
      location = ifelse(intl_game == 1, 'n', location)
    )
  
  all_games <- df %>%
    ats_pct_summary() %>%
    mutate(location = 'all games')
  home_away_games <- df %>%
    group_by(location) %>%
    ats_pct_summary()
  
  bind_rows(all_games, home_away_games) %>%
    select(Location=location, 
           MOV=mov, 
           Spread=spread, 
           OU=ou, 
           SU_pct=su_pct, 
           ATS_pct=ats_pct,
           Ovr_pct=ovr_pct)
  
}
# Get ats summary table by year
make_ats_sum_tbl <- function(df) {
  df %>%
    group_by(team, seas) %>%
    sum_up_records() %>% 
    ungroup()
}
# Get ats summary table by year regular season only
make_regseas_ats_sum_tbl <- function(df) {
  df %>%
    filter(wk < 18) %>% 
    group_by(team, seas) %>%
    sum_up_records() %>% 
    ungroup()
}


# get ATS records by location
location_ats_tbl <- function(df) {
  df %>%
    mutate(location = ifelse(intl_game == 1, 'n', location)) %>%
    group_by(location) %>%
    sum_up_records() %>% 
    ungroup() %>%
    mutate(
      ATS = make_team_records(ats_w, ats_l, ats_p),
      OU = make_team_records(ou_o, ou_u, ou_p),
      SU = make_team_records(su_w, su_l, su_p),
      Location = factor(location, levels = c('h','n','v'),
                        labels = c('Home','Neutral','Away'))
    ) %>%
    select(Location, 
           SU, 
           ATS,
           OU)
}
# Make regular season schedule
get_schd <- function(df,last_seas) {
  df %>%
    filter(seas == last_seas) %>%
    mutate(
      score = paste0(tm_pts, '-', opp_pts),
      ats = ats_abrv(ats_w, ats_l, ats_p),
      spread = ifelse(location == 'h', sprv * -1, sprv),
      tot = ou,
      ou = ou_abrv(ou_o, ou_u, ou_p),
      location = ifelse(intl_game == 1, 'n', location),
      Location = factor(location, levels = c('h','v','n'),
                        labels = c('Home', 'Away', 'Neutral'))
    ) %>%
    select(Week=wk, 
           Location, 
           Opp=opp, 
           Score=score, 
           Spread=spread, 
           ATS=ats, 
           TOT=tot, 
           OU=ou) %>%
    arrange(Week)
}

mk_off_tbl <- function(big_four_team) {
  tibble(
    Stat = c('Rushing', 'Passing', 'Yards per Pt'),
    AvgPerGame = c(
      big_four_team$ry_tm_stat %>% round(1),
      big_four_team$py_tm_stat %>% round(1),
      big_four_team$ypp_tm_stat %>% round(1)
    ),
    Rank = c(
      big_four_team$ry_tm_rank,
      big_four_team$py_tm_rank,
      big_four_team$ypp_tm_rank
    )
  ) %>%
    kable()
}


# Text Generation: opening paragraph
get_team_rank_article <- function(tm_details, big_four_team) {
  # create opening paragraph
  glue(
    'The {full_name} averaged {team_pts} points a game last season, the',
    ' {team_pts_rank} ranked offense in the league.',
    'The defense gave up {opp_pts} points on average to opponents,',
    ' making them the {opp_pts_rank} ranked defense in the NFL. ',
    '{cityname} threw for an average of {team_py} yards per game giving them the',
    ' {team_py_rank} ranked passing game in the league. ',
    'The {teamname} defense ranked {opp_py_rank} giving up ',
    '{opp_py} yards a game to opposing quarterbacks.',
    'On the ground, the {teamname} picked up {team_ry} yards a game. Their rushing game ',
    'ranked {team_ry_rank} in the league. ',
    'The {cityname} defense gave up {opp_ry} yards on the ground per game. ',
    'The rushing defense ranked {opp_ry_rank} in the NFL.',
    full_name = tm_details$name,
    cityname = tm_details$cityname,
    teamname = tm_details$teamname,
    team_pts = big_four_team$pts_tm_stat %>% round(1),
    opp_pts = big_four_team$pts_opp_stat %>% round(1),
    team_pts_rank = big_four_team$pts_tm_rank %>% get_ending(),
    opp_pts_rank = big_four_team$pts_opp_rank %>% get_ending(),
    team_py = big_four_team$py_tm_stat %>% round(),
    team_py_rank = big_four_team$py_tm_rank %>% get_ending(),
    opp_py = big_four_team$py_opp_stat %>% round(),
    opp_py_rank = big_four_team$py_tm_rank %>% get_ending(),
    team_ry = big_four_team$ry_tm_stat %>% round(),
    team_ry_rank = big_four_team$ry_tm_rank %>% get_ending(),
    opp_ry = big_four_team$ry_opp_stat %>% round(),
    opp_ry_rank = big_four_team$ry_tm_rank %>% get_ending()
  )
}
# Text Generation: helper function
who_outscored_who <- function(df){
  df %>% 
    mutate(who=ifelse(MOV > 0, 'Outscored', 'Outscored by'))
}
# Text Generation: at a glance bullet points
season_bullet_points <- function(tm_details, seas_rslts) {
  glue(
    '- Won {w_pct} of their games\n',
    '- Won {h_w_pct} of their games at home \n',
    '- Won {v_w_pct} of their games on the road \n',
    '- Won {ats} of their games ATS \n',
    '- Won {h_ats} of their games at home ATS \n',
    '- Won {v_ats} of their games on the road ATS \n',
    '- {tot} of their games went over the total \n',
    '- Their average margin of victory was {mov_all} points \n',
    '- {who_h} opponents by {abs_mov_h} points at home \n',
    '- {who_v} opposing teams by {abs_mov_v} points on the road',
    teamname = tm_details$teamname,
    cityname = tm_details$cityname,
    w_pct = get_pcts(seas_rslts, 'all games', SU_pct),
    h_w_pct = get_pcts(seas_rslts, 'h', SU_pct),
    v_w_pct = get_pcts(seas_rslts, 'v', SU_pct),
    ats = get_pcts(seas_rslts, 'all games', ATS_pct),
    h_ats = get_pcts(seas_rslts, 'h', ATS_pct),
    v_ats = get_pcts(seas_rslts, 'v', ATS_pct),
    tot = get_pcts(seas_rslts, 'all games', Ovr_pct),
    who_h = who_outscored_who(filter(seas_rslts, Location == 'h')) %>%
      pull(who),
    who_v = who_outscored_who(filter(seas_rslts, Location == 'v')) %>%
      pull(who),
    mov_all = filter(seas_rslts, Location == 'all games') %>%
      pull(MOV) %>%
      round(1),
    mov_h = filter(seas_rslts, Location == 'h') %>%
      pull(MOV) %>%
      round(1),
    mov_v = filter(seas_rslts, Location == 'v') %>%
      pull(MOV) %>%
      round(1),
    abs_mov_h = abs(mov_h),
    abs_mov_v = abs(mov_v)
  )
}
# Text Generation: close games
in_close_games <- function(last_season, tm_details){
  glue(
    '- {close_games} of the {teamname} regular season games were decided',
    ' by 3 points or less. They won {close_wins} of those matchups.',
    teamname = tm_details$teamname,
    close_games = filter(last_season, abs(mov) <= 3) %>% nrow(),
    close_wins = filter(last_season, abs(mov) <= 3, mov > 0) %>% nrow()
  )
}
# # Text Generation: Calculate PF/PA and Pythag
pythag_proj_txt <- function(team, tm_details){
  glue(
    '- **The {tm_name} Pythagorean Projection for this season is {pyproj} wins**',
    tm_name = tm_details$teamname,
    pf = filter(team, seas == 2018) %>% pull(tm_pts) %>% sum(),
    pa = filter(team, seas == 2018) %>% pull(opp_pts) %>% sum(),
    pyproj = round(pythag_expect(pf, pa, 2.37) * 16, 1)
  )
  
}

# Text Generation: helper function
get_ending <- function(num) {
  # helper function to add st/nd/rd/th after number
  end_val <- substr(num, start = nchar(num), nchar(num))
  end_val <- as.integer(end_val)
  end_formal <- case_when(end_val == 1 ~ 'st',
                          end_val == 2 ~ 'nd',
                          end_val > 2 & end_val < 4 ~ 'rd',
                          TRUE ~ 'th')
  paste0(num, end_formal)
}

# Viz: Plot wins/loss/push
make_results_plot <- function(df, pal, grp_type, f_levels) {
  df %>% 
    gather(stat, val, -team, -seas) %>%
    separate(stat, into = c('grp', 'stat')) %>%
    mutate(stat = toupper(stat)) %>%
    filter(grp == grp_type) %>%
    mutate(stat = factor(stat, levels = f_levels)) %>%
    ggplot(aes(seas, val, fill = stat)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    scale_fill_manual(values = pal) +
    scale_y_continuous(breaks = seq(0, 20, 2)) +
    scale_x_continuous(breaks = seq(min(df$seas), max(df$seas), 1)) +
    labs(x = '', y = 'Games', fill = '') +
    theme_ipsum(grid=FALSE) +
    theme(legend.position = 'bottom')
}
# Viz: sprd vs mov plot
sprd_vs_mov_plot <- function(df, pal) {
  df %>%
    mutate(
      spread = ifelse(location == 'h', sprv * -1, sprv),
      location = factor(
        location,
        levels = c('h', 'v'),
        labels = c('Home', 'Away')
      )
    ) %>%
    ggplot(aes(spread, mov, color = location)) +
    geom_point(size = 3) +
    scale_color_manual(values = pal) +
    labs(x = 'Point Spread', y = 'Margin of Victory', col = '') +
    theme(legend.position = 'top')
  
}
# Viz: plot game total vs score
tot_vs_score_plot <- function(df, pal) {
  df %>%
    mutate(
      combined = tm_pts + opp_pts,
      location = factor(
        location,
        levels = c('h', 'v'),
        labels = c('Home', 'Away')
      ),
    ) %>%
    ggplot(aes(ou, combined, color = location)) +
    geom_point(size = 3) +
    scale_color_manual(values = pal) +
    labs(x = 'Game O/U', y = 'Combined Final Score', col = '') +
    theme(legend.position = 'top')
}
# Viz: plot week by week heatmap
week_heatmap_plot <- function(df, pal) {
  df %>%
    group_by(wk) %>%
    summarise(
      games = n(),
      su_w = sum(su_w),
      ats_w = sum(ats_w),
      ou_ovr = sum(ou_o)
    ) %>%
    ungroup() %>%
    select(-games) %>%
    gather(stat, val, -wk) %>%
    separate(stat, into = c('type', 'rslt'), sep = '_') %>%
    select(-rslt) %>%
    mutate(type = factor(
      type,
      levels = c('su', 'ats', 'ou'),
      labels = c('SU Win', 'ATS Win', 'Over Total')
    )) %>%
    ggplot(aes(factor(wk), type, fill = val)) +
    geom_tile() +
    scale_fill_gradient(low = '#ffffff', high = pal[2]) +
    theme_ipsum(grid=FALSE) +
    labs(x = 'Week', y = '', fill = 'Games')
}
# Viz: point per game plot
make_ppg_plot <- function(df, pal) {
  df %>% 
    group_by(team,seas) %>%
    summarise_at(.vars = vars(tm_pts, opp_pts), .funs = mean) %>%
    gather(team, pts, -team, -seas) %>%
    ungroup() %>%
    mutate(team = factor(
      team,
      levels = c('tm_pts', 'opp_pts'),
      labels = c('Team', 'Opp')
    )) %>%
    ggplot(aes(seas, pts, color = team)) +
    geom_line() +
    geom_point() +
    labs(x = 'Season', 
         y = 'Avg Points Per Game', 
         col = '',
         caption = 'Regular Season Only') +
    scale_color_manual(values = pal) +
    scale_x_continuous(breaks = seq(min(df$seas), max(df$seas)) 
                         ) +
    theme(legend.position = 'bottom')
}
# Viz: point qtr distribution
point_qtr_dist <- function(df, pal) {
  df %>%
    ggplot(aes(qtr, scr, fill = who)) +
    geom_bar(stat = 'identity', show.legend = FALSE) +
    scale_fill_manual(values = pal) + 
    theme_ipsum(grid="Y") +
    labs(x = ' ', y = 'Points per Qtr', caption = paste0(max(df$seas),' Season'))
}

sum_up_records <- partial(summarise_at, .vars=vars(ats_w:su_p),.funs=sum)
make_results_tbl <- function(df) {
  df %>%
    group_by(seas) %>%
    sum_up_records() %>% 
    ungroup() %>%
    mutate(
      ats = make_team_records(ats_w, ats_l, ats_p),
      ou = make_team_records(ou_o, ou_u, ou_p),
      su = make_team_records(su_w, su_l, su_p)
    ) %>%
    select(Season=seas, SU=su, ATS=ats, OU=ou)
}
# helper function make nice x-x or x-x-x records
make_team_records <- function(a, b, c) {
  ifelse(c == 0,
         paste0(a, '-', b),
         paste0(a, '-', b, '-', c))
}
# Get ATS results by division
div_results_tbl <- function(df) {
  df %>%
    mutate(
      ats = make_team_records(ats_w, ats_l, ats_p),
      ou = make_team_records(ou_o, ou_u, ou_p),
      su = make_team_records(su_w, su_l, su_p)
    ) %>%
    select(Opp=opp, 
           SU=su, 
           ATS=ats, 
           OU=ou)
}
# Text Generation: pull percents from table
get_pcts <- function(df, where, col) {
  #col <- enquo(col)
  
  x_chng <- function(x){
    paste0(as.character(x * 100),'%')
  }
  
  df %>%
    filter(Location == where) %>%
    pull({{col}}) %>%
    x_chng()
  
}
# Text Generation: select outscored/was outscored by using mov
who_outscored_who <- function(df){
  df %>% 
    mutate(who=ifelse(MOV > 0, 'Outscored', 'Outscored by'))
}
# Helper function for schedule ats column
ats_abrv <- function(a, b, c) {
  ifelse(a > 0, 'W',
         ifelse(b > 0, 'L',
                'P'))
}
# Helper function for schedule ou column
ou_abrv <- function(a, b, c) {
  ifelse(a > 0, 'O',
         ifelse(b > 0, 'U',
                'P'))
}
# Pythagorean projection
pythag_expect <- function(PS, PA, ex = 2.37) {
  PS ^ ex / (PS ^ ex + PA ^ ex)
}

# Make defense big four table
mk_def_tbl <- function(big_four_team) {
  tibble(
    Stat = c('Rushing', 'Passing', 'Yards per Pt'),
    AvgPerGame = c(
      big_four_team$ry_opp_stat %>% round(1),
      big_four_team$py_opp_stat %>% round(1),
      big_four_team$ypp_opp_stat %>% round(1)
    ),
    Rank = c(
      big_four_team$ry_opp_rank,
      big_four_team$py_opp_rank,
      big_four_team$ypp_opp_rank
    )
  ) %>%
    kable()
}

pt_diff_plot <- function(df, team_info, team_abrv, pal) {
  #
  df %>%
    select(seas, wk, team, q1p_tm:q4p_opp) %>%
    mutate(
      q1_diff = q1p_tm - q1p_opp,
      q2_diff = q2p_tm - q2p_opp,
      q3_diff = q3p_tm - q3p_opp,
      q4_diff = q4p_tm - q4p_opp
    ) %>%
    left_join(team_info, by = c('team' = 'abrv')) %>%
    group_by(team, seas) %>%
    summarise_at(vars(q1_diff:q4_diff), mean) %>%
    ungroup() %>%
    gather(qtr, scr, -team, -seas) %>%
    mutate(pos_neg = scr > 0,
           scr = round(scr, 2)) %>%
    separate(qtr, into = c('qtr', 'dif')) %>%
    mutate(qtr = toupper(qtr)) %>% 
    select(-dif) %>%
    filter(seas == max(df$seas)) %>%
    ggplot(aes(qtr, scr, fill = pos_neg, label = scr)) +
    geom_bar(stat = 'identity', show.legend = FALSE) +
    geom_label(show.legend = FALSE, 
               color = 'white',
               label.size=.25,) +
    labs(x = paste(max(df$seas), 'Season'),
         y = 'Point Differential per Quarter',
         #caption = 'Regular Season Games Only',
         fill = '') +
    scale_fill_manual(values = pal) +
    theme_ipsum(grid="Y")
  
}

clean_up_ranks <- function(df, tm){
  df %>% 
    filter(team == tm) %>% 
    select(-team) %>% 
    gather(stat, val) %>% 
    mutate(who = ifelse(grepl(x = stat,
                              pattern = '_tm'), 
                        'tm', 'opp'),
           is_rank = ifelse(grepl(x = stat, 
                                  pattern = '_rank'), 
                            'rank', 'stat'),
           stat = str_remove_all(stat, '_tm') %>%
             str_remove_all('_opp') %>%
             str_remove_all('_rank')
    )
}
make_rank_table <- function(df, stat_names, type){
  df %>% 
    left_join(stat_names) %>% 
    select(-stat) %>% 
    group_by(who) %>% 
    spread(is_rank, val) %>% 
    filter(who == type) %>% 
    ungroup() %>% 
    select(Statistic = statistic,
           AvgPerGame = stat,
           Rank = rank,
           -who) %>% 
    mutate(AvgPerGame = ifelse(
      Statistic == 'TimeOfPoss',
      paste0(round(AvgPerGame * 100), '%'),
      AvgPerGame %>% round(2)
    ))
}

make_f_name <- function(team, type) {
  glue::glue(
    'nfl-{tmname}.{ftype}',
    tmname = stringr::str_replace_all(team, ' ', '-'),
    ftype = type
  )
}

gen_team_output <-
  function(team, fullname, output, template = "nfl_template.Rmd") {
    for (i in seq_along(team)) {
      rmarkdown::render(
        template,
        params = list(team = team[i],
                      set_title = fullname[i]),
        output_file = output[i]
        
      )
    }
  }


