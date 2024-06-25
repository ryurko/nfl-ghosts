# PURPOSE: Generate LOWO CV RFCDE results for the 2D ghosting model

library(tidyverse)
library(RFCDE)
#library(cdetools)

source("code/utils/get_rfcde_means.R")

# Load the modeling data --------------------------------------------------

model_data <-
  read_rds("data/model_datasets/at_catch_yac_model_data.rds")

# Initialize different sets of features -----------------------------------

# The response variables for this dataset are adj_bc_x, adj_bc_y

# All of the features will then correspond to the bc, other def, and offense
# adj_x/y coordinates

# Start with bc and qb related variables:
bc_var_names <- c("adj_bc_x", "adj_bc_y", "bc_s",
                  "bc_dir_target_endzone_absval", "bc_o_target_endzone_absval",
                  "adj_bc_x_from_first_down",
                  "qb_adj_x_change", "qb_adj_y_change_absval",
                  "qb_dist_to_bc", "qb_s")

# Defensive player level info
def_var_name_list <-
  # only go from 2 to 4 since modeling information about
  lapply(2:4,
         function(def_i) {
           str_subset(colnames(model_data), paste0("defense_", def_i, "_")) %>%
             str_subset("(_dist_to_bc)|(_s)|(_adj_x)|(_adj_y$)|(_adj_y_change_absval)|(_dir_target_endzone_absval)|(_dir_wrt_bc_diff)|(_o_target_endzone_absval)|(_o_wrt_bc_diff)")
         })

# Repeat for offense:
off_var_name_list <- # for some reason there is up to 36 likely due to an
  # erroneous play...
  lapply(1:4,
         function(off_i) {
           str_subset(colnames(model_data), paste0("offense_", off_i, "_")) %>%
             str_subset("(_dist_to_bc)|(_s)|(_adj_x)|(_adj_y$)|(_adj_y_change_absval)|(_dir_target_endzone_absval)|(_dir_wrt_bc_diff)|(_o_target_endzone_absval)|(_o_wrt_bc_diff)")
         })

# Now create list of candidate covariates

candidate_var_list_with_abs_val <-
  list("bc_qb_only" = bc_var_names,
        "bc_qb_def2" = c(bc_var_names, def_var_name_list[[1]]),
        "bc_qb_def2_off1" = c(bc_var_names, def_var_name_list[[1]],
                              off_var_name_list[[1]]),
        "def2_off1" = c(def_var_name_list[[1]],
                        off_var_name_list[[1]]))


# Set-up field grid -------------------------------------------------------

# Set-up grid to predict values for (this will need to experiment on)
field_grid <-
  expand.grid("grid_x" = -10:110,
              "grid_y" = -28:28)

# Proceed to generate LOWO results ----------------------------------------

# Now generate the LOWO summary:
lowo_cv_play_level_summary <-
  map_dfr(1:length(candidate_var_list_with_abs_val),
          function(var_list_i) {

            # Generate the results holding out each week:
            map_dfr(1:17,
                    function(test_week_i) {

                      # Init the training data
                      train_data <- model_data %>%
                        filter(week_id != test_week_i) %>%
                        dplyr::select(defense_1_adj_x, defense_1_adj_y,
                                      candidate_var_list_with_abs_val[[var_list_i]])
                      # Get the indices of the complete cases to use
                      train_data_use_i <- complete.cases(train_data)
                      train_data <- train_data[train_data_use_i,]

                      # Now split into x and resp:
                      train_data_resp <- train_data %>%
                        dplyr::select(defense_1_adj_x, defense_1_adj_y) %>%
                        as.matrix()
                      train_data_matrix <- train_data %>%
                        dplyr::select(candidate_var_list_with_abs_val[[var_list_i]]) %>%                        # Convert to matrix
                        as.matrix()

                      # Do the same for test data:
                      test_data <- model_data %>%
                        filter(week_id == test_week_i) %>%
                        dplyr::select(defense_1_adj_x, defense_1_adj_y,
                                      candidate_var_list_with_abs_val[[var_list_i]])
                      # Get the indices of the complete cases to use
                      test_data_use_i <- complete.cases(test_data)
                      test_data <- test_data[test_data_use_i,]

                      # Now split into x and resp:
                      test_data_resp <- test_data %>%
                        dplyr::select(defense_1_adj_x, defense_1_adj_y) %>%
                        as.matrix()
                      test_data_matrix <- test_data %>%
                        dplyr::select(candidate_var_list_with_abs_val[[var_list_i]]) %>%                        # Convert to matrix
                        as.matrix()

                      # Create a round version of these responses to serve as
                      # the field grid assignments for the observed values:
                      round_test_data_resp <- test_data_resp %>%
                        as_tibble() %>%
                        mutate(defense_1_adj_x = round(defense_1_adj_x),
                               defense_1_adj_y = round(defense_1_adj_y)) %>%
                        # Provide necessary caps and floors based on the grid:
                        mutate(defense_1_adj_x = pmax(pmin(defense_1_adj_x, 110), -10),
                               defense_1_adj_y = pmax(pmin(defense_1_adj_y, 28), -28))

                      # Fit the model:
                      train_rfcde <- RFCDE(train_data_matrix, train_data_resp,
                                           n_trees = 500, n_basis = 15)

                      # Generate the predictions for the test data predictions:
                      test_data_pred_scores <-
                        map_dfr(1:nrow(test_data_matrix),
                                function(test_i) {

                                  # Generate the predictions over the grid:
                                  play_field_grid <- field_grid %>%
                                    mutate(cde_pred = as.numeric(
                                      predict(train_rfcde, test_data_matrix[test_i,],
                                              "CDE", field_grid)))
                                  # Mark the observed location:
                                  play_field_grid <- play_field_grid %>%
                                    dplyr::left_join(
                                      dplyr::mutate(round_test_data_resp[test_i,],
                                                    obs_grid = 1),
                                      by = c("grid_x" = "defense_1_adj_x",
                                             "grid_y" = "defense_1_adj_y")
                                    ) %>%
                                    mutate(obs_grid = ifelse(is.na(obs_grid), 0,
                                                             obs_grid))

                                  play_field_grid <- play_field_grid %>%
                                    # Remove rows that are negligible given the
                                    # grid size - unless it is the actual point
                                    filter((cde_pred > 1e-10) | (obs_grid == 1)) %>%
                                    mutate(cde_pred = pmax(cde_pred, 1e-10),
                                           # Compute normalized versions to sum to one:
                                           pred_prob = cde_pred / sum(cde_pred))

                                  # Get a few values - the -log(p) at the actual value:
                                  test_cross_entropy <-
                                    -log(play_field_grid$pred_prob[
                                      which(play_field_grid$obs_grid == 1)])

                                  # Distance from the most likely value based on the CDE grid:
                                  cde_pred_mode <- play_field_grid %>%
                                    dplyr::slice_max(pred_prob, with_ties = FALSE) %>%
                                    dplyr::select(grid_x, grid_y) %>%
                                    as.matrix()
                                  cde_pred_mode_dist <- dist(rbind(test_data_resp[test_i,],
                                                                   cde_pred_mode)) %>%
                                    as.numeric()

                                  # Get the distance for the expected location:
                                  cde_mean_dist <- dist(
                                    rbind(test_data_resp[test_i,],
                                          get_rfcde_means(train_rfcde,
                                                          test_data_matrix[test_i,]))) %>%
                                    as.numeric()

                                  # Get play level info for context to then return
                                  # the prediction results with:

                                  tibble(test_row_i = test_i,
                                         cross_entropy = test_cross_entropy,
                                         dist_cde_mode = cde_pred_mode_dist,
                                         dist_cde_mean = cde_mean_dist)
                                })

                      # Get the test data info to join:
                      test_data_info <- model_data %>%
                        filter(week_id == test_week_i) %>%
                        dplyr::select(week_id, game_play_id)
                      test_data_info %>%
                        dplyr::slice(c(1:nrow(test_data_info))[test_data_use_i]) %>%
                        bind_cols(test_data_pred_scores) %>%
                        mutate(test_week = test_week_i) %>%
                        return()
                    }) %>%
              mutate(features = names(candidate_var_list_with_abs_val)[var_list_i]) %>%
              return()
          })

write_csv(lowo_cv_play_level_summary,
          "data/ghost_location_model_output/lowo_validation/rfcde_summary.csv")
