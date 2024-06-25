# PURPOSE: Compute difference between observed EPV with ghost EPV (negative is
#          good for the side of defense)

library(tidyverse)


# Proceed through each week to generate summary ---------------------------

play_level_summary <-
  map_dfr(1:17,
          function(week_i) {

            #week_i <- 1

            # First load the observed EPV summary:
            week_obs_epv <-
              read_rds(paste0("data/ghost_ep_values/play_level_summary/observed_epv/week",
                              week_i, ".rds")) %>%
              rename(obs_epv = ev_ep)

            # Load all of the play level files and compute the EPV:
            week_ghost_epv <-
              map_dfr(list.files(paste0("data/ghost_ep_values/clean_ghost_loc_distr/week",
                                        week_i), full.names = TRUE),
                      read_rds) %>%
              group_by(game_play_id) %>%
              summarize(ghost_epv = sum(ev_ep * cond_pred_prob),
                        .groups = "drop")

            # Join together and compute the difference to return:
            week_obs_epv %>%
              dplyr::left_join(week_ghost_epv, by = "game_play_id") %>%
              mutate(change_epv = obs_epv - ghost_epv)

          })

# There's one play with missing ghost right now... just ignore for ease

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
          "data/ghost_ep_values/play_level_summary/play_summary_table.csv")


