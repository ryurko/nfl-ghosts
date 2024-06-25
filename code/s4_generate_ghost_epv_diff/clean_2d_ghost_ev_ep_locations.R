# PURPOSE: Clean the play level ghost distribution files by iterating through,
#          and condition on being the closest defender by only considering values
#          within a radius of distance <= the 2nd closest defender

library(tidyverse)
library(parallel)

# Load the modeling data with the play level info -------------------------

model_data <-
  read_rds("data/model_datasets/at_catch_yac_model_data.rds")

# Just grab the play's distance to closest defenders:
play_dist_bounds <- model_data %>%
  dplyr::select(week_id, game_play_id, adj_bc_x, adj_bc_y,
                defense_2_dist_to_bc)


# Iterate through each week and play to update the data -------------------

walk(1:17,
     function(week_i) {

       play_files <-
         list.files(paste0("data/ghost_ep_values/ghost_location_distribution/week",
                           week_i))

       walk(play_files,
            function(play_name) {

              ghost_summary <-
                read_rds(paste0("data/ghost_ep_values/ghost_location_distribution/week",
                                week_i, "/", play_name))

              ghost_summary <- ghost_summary %>%
                # Join the ball carrier and distance for 2nd closest player:
                dplyr::left_join(play_dist_bounds,
                                 by = c("week_id", "game_play_id")) %>%
                mutate(defense_1_dist_to_bc = sqrt((grid_x - adj_bc_x)^2 +
                                                     (grid_y - adj_bc_y)^2)) %>%
                # Only consider locations within this bound:
                filter(defense_1_dist_to_bc <= defense_2_dist_to_bc) %>%
                # Rescale the predicted probs to sum to 1:
                mutate(cond_pred_prob = pred_prob / sum(pred_prob)) %>%
                dplyr::select(-cde_pred, -pred_prob, -adj_bc_x, -adj_bc_y)


              write_rds(ghost_summary,
                        path =
                          paste0("data/ghost_ep_values/clean_ghost_loc_distr/week",
                                 week_i, "/", play_name),
                        compress = "gz")



            })


     })
