# PURPOSE: Initialize table of velo vectors for nearest defender to sample
#          from in when constructing the full table of ghosts. Need to include
#          the ball carrier location to then compute an appropriate weight for
#          sampling from in a more careful way. 

library(tidyverse)

# Load the model data -----------------------------------------------------

model_data <-
  read_rds("data/model_datasets/at_catch_yac_model_data.rds")

# Set-up the table to only have nearest defense s, dir, o -----------------

defense_velo_data <- model_data %>%
  dplyr::select(week_id, game_play_id,
                adj_bc_x, adj_bc_y, defense_1_dist_to_bc,
                defense_1_s, defense_1_dir_target_endzone,
                defense_1_o_target_endzone) %>%
  filter(!is.na(defense_1_dist_to_bc),
         !is.na(defense_1_s), !is.na(defense_1_dir_target_endzone),
         !is.na(defense_1_o_target_endzone))

# Save this table ---------------------------------------------------------

write_rds(defense_velo_data,
          "data/ghost_location_model_output/obs_def1_velo_data.rds",
          compress = "gz")
