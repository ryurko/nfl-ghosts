# PURPOSE: Generate the EP values for every play-grid-sample combination, by
#          first updating the features, generating the YAC CDEs, and then
#          computing the EP for the single play-grid-sample. The integration of
#          the differences will be done in a different script. This division
#          allows us to explore the EP values across the ghost grid and velo
#          samples with ease in future scripts.

library(tidyverse)
library(RFCDE)
library(parallel)

source("code/s4_ghosting_inference/ep_calculator_fn.R")

# Load the modeling data --------------------------------------------------

model_data <-
  read_rds("data/model_datasets/at_catch_yac_model_data.rds")

# Initialize the vectors of features --------------------------------------

bc_var_names <- c("adj_bc_x", "adj_bc_y", "bc_s",
                  "bc_dir_target_endzone_absval", "bc_o_target_endzone_absval",
                  "adj_bc_x_from_first_down",
                  "qb_adj_x_change", "qb_adj_y_change_absval",
                  "qb_dist_to_bc", "qb_s")

def1_var_names <- colnames(model_data) %>%
  str_subset("defense_1") %>%
  str_subset("(_dist_to_bc)|(_s)|(_adj_x)|(_adj_y$)|(_adj_y_change_absval)|(_dir_target_endzone_absval)|(_dir_wrt_bc_diff)|(_o_target_endzone_absval)|(_o_wrt_bc_diff)")

# Init the data for RFCDE training ----------------------------------------

model_train_data <- model_data %>%
  dplyr::select(end_x_change, all_of(bc_var_names), all_of(def1_var_names))
complete_data_i <- complete.cases(model_train_data)
#length(which(complete_data_i))
# [1] 10363
complete_train_data <- model_train_data[complete_data_i,]

# Split into x and response for model fitting:
train_data_resp <- complete_train_data %>%
  pull(end_x_change)
train_data_x <- complete_train_data %>%
  dplyr::select(-end_x_change) %>%
  as.matrix()

# Fit the model: ----------------------------------------------------------

yac_rfcde <- RFCDE(train_data_x, train_data_resp, n_trees = 500, n_basis = 15)

# Load EP model and datasets for process ----------------------------------

ep_model <- read_rds("data/ghost_ep_values/ep_model.rds")
play_data <- read_rds("data/ghost_ep_values/init_play_data.rds")

# Set-up global settings for process --------------------------------------

YAC_PADDING <- 2
DELTA_YARDS <- 1
MIN_YAC <- -10

# Generate the EP for each play-grid-sample weekly ------------------------

# To reduce memory load:
model_data <- model_data %>%
  dplyr::select(game_play_id, all_of(bc_var_names))

walk(1:17,
     function(week_i) {

       week_ghost_data <-
         read_rds(paste0("data/ghost_location_model_output/at_catch_location_velo_samples/week",
                         week_i, ".rds")) %>%
         rename(defense_1_dist_to_bc = grid_dist_to_bc) %>%
         # Create the updated features for the model:
         left_join(model_data, by = "game_play_id") %>%
         # Create the new columns:
         mutate(defense_1_adj_x = grid_x,
                defense_1_adj_y = grid_y,
                defense_1_adj_x_change = adj_bc_x - defense_1_adj_x,
                defense_1_adj_y_change = adj_bc_y - defense_1_adj_y,
                defense_1_adj_y_change_absval = abs(defense_1_adj_y_change),
                defense_1_dir_target_endzone_absval =
                  abs(defense_1_dir_target_endzone),
                defense_1_o_target_endzone_absval =
                  abs(defense_1_o_target_endzone),
                angle_with_bc =
                  (atan2(defense_1_adj_y_change,
                         -defense_1_adj_x_change) * 180) / pi,
                defense_1_dir_wrt_bc_diff = pmin(
                  pmin(abs(angle_with_bc - defense_1_dir_target_endzone),
                       abs(angle_with_bc - (defense_1_dir_target_endzone - 360))),
                  abs(angle_with_bc - (defense_1_dir_target_endzone + 360))),
                defense_1_o_wrt_bc_diff = pmin(
                  pmin(abs(angle_with_bc - defense_1_o_target_endzone),
                       abs(angle_with_bc - (defense_1_o_target_endzone - 360))),
                  abs(angle_with_bc - (defense_1_o_target_endzone + 360)))) %>%
         dplyr::select(-angle_with_bc, -defense_1_dir_target_endzone,
                       -defense_1_o_target_endzone)

       # To potentially speed things up, I can loop over play-grid:
       play_grid_nest <- week_ghost_data %>%
         group_by(game_play_id, grid_x, grid_y) %>%
         nest()

       # Loop through each row and compute the EP
       play_grid_sample_data <-
         mclapply(1:nrow(play_grid_nest), mc.cores = 12,
                  function(i) {

                    ghost_play_grid_sample <- play_grid_nest$data[[i]]
                    play_start <- play_data %>%
                      filter(game_play_id == play_grid_nest$game_play_id[i])

                    # Setup grid for play:
                    obs_max_x <- round(ghost_play_grid_sample$adj_bc_x[1]) +
                      YAC_PADDING
                    gain_pred_grid <- seq(MIN_YAC, obs_max_x, by = DELTA_YARDS)

                    # Generate the CDE estimates:
                    cde_pred_matrix <- predict(yac_rfcde,
                                               as.matrix(
                                                 dplyr::select(ghost_play_grid_sample,
                                                               all_of(bc_var_names),
                                                               all_of(def1_var_names))
                                               ),
                                               "CDE", gain_pred_grid)
                    colnames(cde_pred_matrix) <- gain_pred_grid
                    yac_pred_table <- cde_pred_matrix %>%
                      as_tibble() %>%
                      mutate(b_i = 1:n()) %>%
                      pivot_longer(cols = -b_i,
                                   names_to = "pred_yac",
                                   values_to = "cde") %>%
                      mutate(pred_yac = as.numeric(pred_yac)) %>%
                      mutate(pred_yac = pmin(pred_yac, obs_max_x)) %>%
                      group_by(b_i, pred_yac) %>%
                      summarize(cde = sum(cde, na.rm = TRUE),
                                .groups = "drop") %>%
                      # Convert to prob:
                      group_by(b_i) %>%
                      mutate(pred_prob = cde / sum(cde)) %>%
                      ungroup() %>%
                      # Filter to remove any rows with predicted probs < 1e-10:
                      filter(pred_prob > 1e-10) %>%
                      dplyr::select(-cde) %>%
                      # And get the receiver's predicted ending location
                      # (subtract yac since positive is closer to endzone)
                      mutate(pred_bc_x = obs_max_x - pred_yac)

                    # Now join the EP calculations given the initial
                    # play context and starting receiver location:
                    yac_pred_table %>%
                      mutate(ep = compute_ep(yac_pred_table$pred_bc_x,
                                             play_start,
                                             ep_model)) %>%
                      group_by(b_i) %>%
                      summarize(x_yac = sum(pred_prob * pred_yac),
                                ep = sum(pred_prob * ep)) %>%
                      mutate(game_play_id = play_grid_nest$game_play_id[i],
                             grid_x = play_grid_nest$grid_x[i],
                             grid_y = play_grid_nest$grid_y[i])

                  }) %>%
         bind_rows() %>%
         mutate(week_id = week_i)

       write_rds(play_grid_sample_data,
                 path = paste0("data/ghost_ep_values/play_grid_sample_ep/week",
                               week_i, ".rds"),
                 compress = "gz")




     })



