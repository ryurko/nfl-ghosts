# PURPOSE: Create the dataset with the adjusted features to use for generating
#          YAC RFCDE estimates for, based on the 2D ghost defender locations

library(tidyverse)


# Load the original data to join info with --------------------------------

model_data <-
  read_rds("data/model_datasets/at_catch_yac_model_data.rds")

# Get the variable names for the features that are in the RFCDE and also
# conditioned on for the
bc_var_names <- c("adj_bc_x", "adj_bc_y", "bc_s",
                  "bc_dir_target_endzone_absval", "bc_o_target_endzone_absval",
                  "adj_bc_x_from_first_down",
                  "qb_adj_x_change", "qb_adj_y_change_absval",
                  "qb_dist_to_bc", "qb_s")

# The code below will compute the following updated features based on the
# ghost locations:
# "defense_1_dist_to_bc"
# "defense_1_adj_x"
# "defense_1_adj_y"
# "defense_1_adj_x_change"
# "defense_1_adj_y_change"
# "defense_1_adj_y_change_absval"

# Then, in the sampling script of speed, dir_target_endzone/o_target_endzone -
# we will have to compute the wrt_bc_diff variables based on the angle_with_bc
# since that's derived from the ghost locations

# Will work through each week and saving the output -----------------------

walk(unique(model_data$week_id),

     function(week_i) {

       # Load the CDE data:
       cde_data <- read_rds(paste0("data/ghost_location_model_output/at_catch_cde/week",
                                   week_i, "_ghost_cde.rds"))

       # Grab the week's plays in the CDE data:
       week_data <- model_data %>%
         filter(week_id == week_i, game_play_id %in% unique(cde_data$game_play_id)) %>%
         # Just grab the necessary columns:
         dplyr::select(week_id, game_play_id, all_of(bc_var_names))

       # Now left join to the CDE data, and proceed through the pipeline creating
       # the new versions of the variables in the YAC RFCDE model:
       upd_cde_data <- cde_data %>%
         left_join(week_data, by = c("week_id", "game_play_id")) %>%
         # Create the new columns
         mutate(defense_1_adj_x = grid_x,
                defense_1_adj_y = grid_y,
                defense_1_dist_to_bc = sqrt((defense_1_adj_x - adj_bc_x)^2 +
                                              (defense_1_adj_y - adj_bc_y)^2),
                defense_1_adj_x_change = adj_bc_x - defense_1_adj_x,
                defense_1_adj_y_change = adj_bc_y - defense_1_adj_y,
                defense_1_adj_y_change_absval = abs(defense_1_adj_y_change))

       write_rds(upd_cde_data,
                 path = paste0("data/ghost_location_model_output/ghost_yac_features/week",
                               week_i, ".rds"),
                 compress = "gz")


     })









