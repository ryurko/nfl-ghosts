# PURPOSE: Draw samples of the observed velocity vectors weighted by similarity
#          with the observed distance (ie conditioning on the distance but not
#          in a strict nearest neighbor sense, just using it for more informed
#          sampling of the features).

library(tidyverse)
library(parallel)

# Load the observed velo data ---------------------------------------------

defense_velo_data <-
  read_rds("data/ghost_location_model_output/obs_def1_velo_data.rds") %>%
  # Just keep the relevant columns for the sampling, since the obs deserved
  # for every grid location has already been computed:
  dplyr::select(-week_id, -game_play_id, -adj_bc_x, -adj_bc_y) %>%
  rename(obs_dist = defense_1_dist_to_bc)

# Get the position data to use:
def_pos_data <- read_rds("data/ghost_location_model_output/obs_def1_velo_data.rds") %>%
  # Just keep the relevant columns for the sampling, since the obs deserved
  # for every grid location has already been computed:
  dplyr::select(game_play_id, def_position) %>%
  rename(obs_def_position = def_position)

# Iterate through each week to sample rows of data ------------------------

# Global B to use for samples
N_VELO_SAMPLES <- 100

# Loop through each week:
walk(1:17,

     function(week_i) {

       # Load the location prob data:
       location_prob_data <-
         read_rds(paste0("data/ghost_location_model_output/at_catch_location_probs/week",
                         week_i, ".rds")) %>%
         left_join(def_pos_data, by = "game_play_id")

       # Loop through each play (in parallel) to create weighted sample
       play_sample_data <-
         mclapply(unique(location_prob_data$game_play_id), mc.cores = 12,
                  function(i) {

                    defense_velo_data %>%
                      mutate(obs_game_play_id = i) %>%
                      left_join(location_prob_data,
                                by = c("obs_game_play_id" = "game_play_id")) %>%
                      # Compute a probability based on the absolute
                      # difference from the distance:
                      mutate(inv_dist = 1 / (
                        pmax(abs(grid_dist_to_bc - obs_dist),
                             # Make sure we have no divide by 0s
                             .0005))) %>%
                      # Group by the grid location
                      group_by(grid_x, grid_y) %>%
                      sample_n(size = N_VELO_SAMPLES, replace = TRUE,
                               weight = inv_dist) %>%
                      group_by(grid_x, grid_y) %>%
                      mutate(b_i = 1:n()) %>%
                      ungroup() %>%
                      dplyr::select(-obs_dist, -inv_dist) %>%
                      rename(game_play_id = obs_game_play_id)

                  }) %>%
         bind_rows()

       write_rds(play_sample_data,
                 path = paste0("data/ghost_location_model_output/at_catch_location_velo_samples/week",
                               week_i, ".rds"),
                 compress = "gz")


     })
