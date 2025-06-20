# Purpose: Compute the estimate for the expected delta difference with ghost
#          distributions across every observed play

library(tidyverse)

# Proceed through each week to generate summary ---------------------------

play_level_summary <-
  map_dfr(1:17,
          function(week_i) {

            # First load CDE values
            week_cde_vals <-
              read_rds(paste0("data/ghost_location_model_output/at_catch_location_probs/week",
                              week_i, ".rds"))

            # Next the ghost EPV values:
            week_epv_vals <-
              read_rds(paste0("data/ghost_ep_values/play_grid_sample_ep/week",
                              week_i, ".rds"))

            # Now the observed EPV values
            week_obs_epv <-
              read_rds(paste0("data/ghost_ep_values/play_level_summary/observed_epv_new/week",
                              week_i, ".rds")) %>%
              rename(obs_ep = ep)

            week_epv_vals %>%
              left_join(week_obs_epv, by = c("week_id", "game_play_id")) %>%
              mutate(delta = obs_ep - ep) %>%
              group_by(game_play_id, grid_x, grid_y) %>%
              summarise(grid_delta = mean(delta),
                        .groups = "drop") %>%
              left_join(dplyr::select(week_cde_vals,
                                      game_play_id,
                                      grid_x, grid_y, pred_prob),
                        by = c("game_play_id", "grid_x", "grid_y")) %>%
              group_by(game_play_id) %>%
              summarize(epv_delta = sum(grid_delta * pred_prob),
                        .groups = "drop") %>%
              mutate(week_id = week_i)

          })

# Get info on the players in these plays ----------------------------------

# Want info on who were teams, receivers, and nearest defenders to then have
# some way of evaluating their performance:

yac_at_catch_data <-
  map_dfr(1:17,
          function(week_i) {

            read_rds(paste0("data/weekly_yac_bc_features/week",
                            week_i, ".rds")) %>%
              filter(is_start_bc == 1)
          })

play_level_summary <- play_level_summary %>%
  dplyr::left_join(dplyr::select(yac_at_catch_data,
                                 game_play_id, bc_displayName, bc_position,
                                 bc_nflId, defense_1_displayName,
                                 defense_1_position, defense_1_nflId),
                   by = "game_play_id")

# Read the play data to get the teams
play_info <- read_csv("data/nfl-big-data-bowl-2021/plays.csv")
game_info <- read_csv("data/nfl-big-data-bowl-2021/games.csv")
play_team_info <- play_info %>%
  dplyr::select(gameId, playId, possessionTeam) %>%
  dplyr::left_join(dplyr::select(game_info, gameId, homeTeamAbbr, visitorTeamAbbr),
                   by = "gameId") %>%
  rename(pos_team = possessionTeam) %>%
  mutate(def_team = ifelse(pos_team == homeTeamAbbr, visitorTeamAbbr, homeTeamAbbr)) %>%
  unite(game_play_id, gameId:playId, sep = "_", remove = TRUE) %>%
  dplyr::select(game_play_id, pos_team, def_team)

# Join over:
play_level_summary <- play_level_summary %>%
  dplyr::left_join(play_team_info, by = "game_play_id")


# Save this file:
write_csv(play_level_summary,
          "data/ghost_ep_values/play_level_summary/epv_diff_play_summary_table.csv")
