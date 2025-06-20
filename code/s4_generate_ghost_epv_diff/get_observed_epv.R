# PURPOSE: Generate the observed EPV based on player locations for the play

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

# Generate the EP for each play weekly ------------------------------------


walk(1:17,
     function(week_i) {

       week_data <- model_data %>%
         filter(week_id == week_i) %>%
         dplyr::select(game_play_id, end_x_change,
                       all_of(bc_var_names), all_of(def1_var_names))
       complete_week_data_i <- complete.cases(week_data)
       complete_week_data <- week_data[complete_week_data_i,]


       week_play_summary <-
         mclapply(1:nrow(complete_week_data), mc.cores = 12,
                  function(i) {

                    play_id <- complete_week_data$game_play_id[i]

                    play_loc_data <- complete_week_data %>%
                      filter(game_play_id == play_id)

                    play_start <- play_data %>%
                      filter(game_play_id == play_id)

                    # Setup grid for play
                    obs_max_x <- round(play_loc_data$adj_bc_x[1]) +
                      YAC_PADDING
                    gain_pred_grid <- seq(MIN_YAC, obs_max_x, by = DELTA_YARDS)

                    # Generate the CDE estimates:
                    cde_pred_matrix <- predict(yac_rfcde,
                                               as.matrix(
                                                 dplyr::select(play_loc_data,
                                                               all_of(bc_var_names),
                                                               all_of(def1_var_names))
                                               ),
                                               "CDE", gain_pred_grid)

                    yac_pred_table <- tibble(pred_yac = gain_pred_grid,
                                             cde = as.numeric(cde_pred_matrix)) %>%
                      mutate(pred_yac = pmin(pred_yac, obs_max_x)) %>%
                      # Sum the rows in the padding above:
                      group_by(pred_yac) %>%
                      summarize(cde = sum(cde, na.rm = TRUE),
                                .groups = "drop") %>%
                      # Convert to prob:
                      mutate(pred_prob = cde / sum(cde)) %>%
                      # Ungroup:
                      ungroup() %>%
                      # Filter to remove any rows with predicted probs < 1e-10:
                      filter(pred_prob > 1e-10) %>%
                      dplyr::select(-cde) %>%
                      # And get the receiver's predicted ending location
                      # (subtract yac since positive is closer to endzone)
                      mutate(pred_bc_x = obs_max_x - pred_yac)

                    # Now join the EP calculations given the initial
                    # play context and starting receiver location:
                    final_yac_pred_table <- yac_pred_table %>%
                      mutate(ep = compute_ep(yac_pred_table$pred_bc_x,
                                             play_start,
                                             ep_model)) %>%
                      bind_cols(dplyr::select(play_loc_data, game_play_id)) %>%
                      mutate(week_id = week_i)

                    write_rds(final_yac_pred_table,
                              path =
                                paste0("data/ghost_ep_values/play_level_summary/observed_yac_cde_new/week",
                                       week_i, "/play_", play_id, ".rds"))

                    # Integrate over and return the EPV ro:
                    final_yac_pred_table %>%
                      group_by(week_id, game_play_id) %>%
                      summarize(x_yac = sum(pred_prob * pred_yac),
                                ep = sum(pred_prob * ep),
                                .groups = "drop")

                  }) %>%
         bind_rows()

       # Save the week summary:
       write_rds(week_play_summary,
                 path = paste0("data/ghost_ep_values/play_level_summary/observed_epv_new/week",
                               week_i, ".rds"),
                 compress = "gz")

     })














