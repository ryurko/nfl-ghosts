# PURPOSE: Filter defender ghost locations to only nearest defender locations and
#          then rescale the CDEs to be probabilities

library(tidyverse)

# Load the modeling data with the play level info -------------------------

model_data <-
  read_rds("data/model_datasets/at_catch_yac_model_data.rds")

# Just grab the play's distance to closest defenders:
play_dist_bounds <- model_data %>%
  dplyr::select(week_id, game_play_id, adj_bc_x, adj_bc_y,
                defense_2_dist_to_bc)

# Iterate through each week to update the data ----------------------------

walk(1:17,
     function(week_i) {

       #week_i <- 1
       # Load the CDE data:
       cde_data <- read_rds(paste0("data/ghost_location_model_output/at_catch_cde/week",
                                   week_i, "_ghost_cde.rds")) %>%
         # Drop the previously created pred prob column
         dplyr::select(-pred_prob)

       # Join the play bound data:
       location_prob_data <- cde_data %>%
         dplyr::left_join(play_dist_bounds, by = c("week_id", "game_play_id")) %>%
         # Compute the distance with the receiver
         mutate(defense_1_dist_to_bc = sqrt((grid_x - adj_bc_x)^2 +
                                              (grid_y - adj_bc_y)^2)) %>%
         # Only consider locations that are the nearest defender
         filter(defense_1_dist_to_bc < defense_2_dist_to_bc) %>%
         # Rescale the CDE to be probabilities
         group_by(week_id, game_play_id) %>%
         mutate(pred_prob = cde_pred / sum(cde_pred)) %>%
         ungroup() %>%
         dplyr::select(defense_1_dist_to_bc, grid_x, grid_y, cde_pred, pred_prob,
                       week_id, game_play_id) %>%
         rename(grid_dist_to_bc = defense_1_dist_to_bc)

       write_rds(location_prob_data,
                 path = paste0("data/ghost_location_model_output/at_catch_location_probs/week",
                               week_i, ".rds"),
                 compress = "gz")


     })
