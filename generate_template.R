source('load_libs.R')

import_draft(
  "setup_pattern" = "rmrkdwn_patterns/setup_pattern.Rmd",
  "team_summary_pattern" = "rmrkdwn_patterns/team_summary_pattern.Rmd",
  "past_ats_pattern" = "rmrkdwn_patterns/past_ats_pattern.Rmd",
  "conf_div_pattern" = "rmrkdwn_patterns/conf_div_pattern.Rmd",
  "past_performance_pattern" = "rmrkdwn_patterns/past_performance_pattern.Rmd",
  "at_a_glance_pattern" = "rmrkdwn_patterns/at_a_glance_pattern.Rmd",
  "last_seas_pattern" = "rmrkdwn_patterns/last_seas_pattern.Rmd",
  "offensive_stats_pattern" = "rmrkdwn_patterns/offensive_stats_pattern.Rmd",
  "def_stats_pattern" = "rmrkdwn_patterns/def_stats_pattern.Rmd",
  "pred_tm_pattern" = "rmrkdwn_patterns/pred_tm_pattern.Rmd",
  "pred_opp_pattern" = "rmrkdwn_patterns/pred_opp_pattern.Rmd"
  ) %>% 
  make_template() %>% 
  export_template('nfl_template.Rmd')
  