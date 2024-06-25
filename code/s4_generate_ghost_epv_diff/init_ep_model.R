# PURPOSE: Fit multinomial logistic regression expected points model to use
#          for calculating the expected points given play-level information

library(tidyverse)

# Load data previously constructed for summer program ---------------------

nfl_ep_model_data <- read_rds(url("http://www.stat.cmu.edu/cmsac/sure/2021/materials/data/model_pbp_data.rds"))
nfl_ep_model_data <- nfl_ep_model_data %>%
  # Only use games prior to 2018:
  filter(str_detect(game_id, "2018_", negate = TRUE),
         str_detect(game_id, "2019_", negate = TRUE),
         str_detect(game_id, "2020_", negate = TRUE)) %>%
  mutate(Next_Score_Half = fct_relevel(Next_Score_Half, "No_Score"),
         # log transform of yards to go:
         log_ydstogo = log(ydstogo),
         # two minute warning:
         two_minute_warning = as.numeric(half_seconds_remaining < 120),
         # goal to go situation:
         goal_to_go = as.numeric(yardline_100 <= 10),
         # Changing down into a factor variable:
         down = factor(down))


# Fit the multinomial logistic regression model ---------------------------

library(nnet)
ep_model <-
  multinom(Next_Score_Half ~ half_seconds_remaining + yardline_100 + down +
             log_ydstogo + log_ydstogo*down + yardline_100*down + goal_to_go +
             goal_to_go*log_ydstogo + two_minute_warning,
           data = nfl_ep_model_data, maxit = 300)


# Save the model for use --------------------------------------------------

write_rds(ep_model, "data/ghost_ep_values/ep_model.rds")


