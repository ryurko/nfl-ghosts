# PURPOSE: Fit 2D ghosting model of nearest pass defender at the moment of catch,
#          then get the 2D CDE estimates for every play (just at the moment of
#          catch for now)

library(tidyverse)
library(RFCDE)

# Load the modeling data --------------------------------------------------

model_data <-
  read_rds("data/model_datasets/at_catch_yac_model_data.rds")

# Initialize the features to use ------------------------------------------

# Only use the ball carrier and QB related features:
bc_var_names <- c("adj_bc_x", "adj_bc_y", "bc_s",
                  "bc_dir_target_endzone_absval", "bc_o_target_endzone_absval",
                  "adj_bc_x_from_first_down",
                  "qb_adj_x_change", "qb_adj_y_change_absval",
                  "qb_dist_to_bc", "qb_s")


# Init the data for RFCDE training ----------------------------------------

model_train_data <- model_data %>%
  dplyr::select(defense_1_adj_x, defense_1_adj_y, all_of(bc_var_names))
complete_data_i <- complete.cases(model_train_data)
length(which(complete_data_i))
# [1] 10363 - lose some observations, keep these in mind for then the plays
#             which we'll use to generate estimates for

complete_train_data <- model_train_data[complete_data_i,]

# Split into x and response for model fitting:
train_data_resp <- complete_train_data %>%
  dplyr::select(defense_1_adj_x, defense_1_adj_y) %>%
  as.matrix()
train_data_x <- complete_train_data %>%
  dplyr::select(all_of(bc_var_names)) %>%
  as.matrix()


# Fit the model: ----------------------------------------------------------

ghost_rfcde <- RFCDE(train_data_x, train_data_resp, n_trees = 500, n_basis = 15)
# That took 3 minutes to run... why the hell does the python leave one week out
# take forever? Must be because of the way to generate predictions...


# Generate 2D CDE for every play - save week by week ----------------------

# Only grab the complete rows of the model data:
play_data <- model_data[complete_data_i,] %>%
  dplyr::select(week_id, game_play_id)

# Set-up grid to predict values - for now in 1 by 1 increments
field_grid <- expand.grid("grid_x" = -10:110, "grid_y" = -28:28)

# Walk through each week, and iteratively generate the predictions for every
# play - but store the week and play IDs so they can be joined with other data
# later on, specifically when thinking about how to get the updated features
# for the YAC RFCDE model

walk(unique(play_data$week_id),
     function(week_i) {

       # Get the indices for the week plays
       week_i_plays <- which(play_data$week_id == week_i)
       week_plays <- play_data[week_i_plays,]

       # Subset the train_data_x rows to these plays:
       week_data_x <- train_data_x[week_i_plays,]

       # Loop through each play and get the 2D CDE predictions across the grid:
       week_plays_cde <-
         map_dfr(1:nrow(week_data_x),
                 function(play_i) {

                   play_field_grid <- field_grid %>%
                     mutate(cde_pred =
                              as.numeric(predict(ghost_rfcde,
                                                 week_data_x[play_i,],
                                                 "CDE", field_grid))) %>%
                     # Convert to prob:
                     mutate(pred_prob = cde_pred / sum(cde_pred)) %>%
                     # Filter to remove any rows with predicted probs < 1e-10:
                     filter(pred_prob > 1e-10) %>%
                     # Add the play info:
                     bind_cols(week_plays[play_i,])

                 })

       # Save as rds file:
       write_rds(week_plays_cde,
                 path = paste0("data/ghost_location_model_output/at_catch_cde/week",
                               week_i, "_ghost_cde.rds"))


     })























