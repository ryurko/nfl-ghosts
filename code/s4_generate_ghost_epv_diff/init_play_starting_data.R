# PURPOSE: Intitialize the play-by-play dataset for the context to have in order
#          to compute the EP for predicted yardline

library(tidyverse)

# Load the model data -----------------------------------------------------

model_data <-
  read_rds("data/model_datasets/at_catch_yac_model_data.rds")

# Use nflreadr to get the 2018 plays --------------------------------------

pbp_2018 <- nflreadr::load_pbp(2018)

# Process data into format for model --------------------------------------

model_pbp_2018 <- pbp_2018 %>%
  dplyr::select(week, old_game_id, play_id, half_seconds_remaining,
                yardline_100, down, ydstogo) %>%
  unite("game_play_id", old_game_id:play_id, sep = "_", remove = TRUE) %>%
  rename(week_id = week) %>%
  filter(game_play_id %in% model_data$game_play_id) %>%
  mutate(two_minute_warning = as.numeric(half_seconds_remaining < 120),
         goal_to_go = as.numeric(yardline_100 <= 10))


# Save the data -----------------------------------------------------------

write_rds(model_pbp_2018,
          "data/ghost_ep_values/init_play_data.rds")
