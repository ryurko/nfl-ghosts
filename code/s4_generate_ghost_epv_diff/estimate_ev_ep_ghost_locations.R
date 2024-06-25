# PURPOSE: Generate the EP values for each ghost grid - by first sampling velo
#          vectors, updating the features, generating the YAC CDEs, before computing
#          the EP for a single ghost loc + velo sample, then average the EPs over
#          the velo samples. Will then summarize the EP for each play by integrating
#          over the ghost loc in another script.

library(tidyverse)
library(RFCDE)
library(parallel)

#source("code/s4_ghosting_inference/ep_helper_functions.R")
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
def1_velo_data <- read_rds("data/ghost_ep_values/def1_velo_data.rds")


# Set-up global settings for process --------------------------------------

N_VELO_SAMPLES <- 100
YAC_PADDING <- 2
DELTA_YARDS <- 1
MIN_YAC <- -10

# Generate the EP for each ghost location weekly --------------------------

walk(1:17,
  #unique(model_data$week_id),
     function(week_i) {

      #week_i <- 1

       # Read in the weekly features to generate YAC distributions for:
       # (NOTE - only using the filtered ghost data locations to start with)
       week_ghost_data <-
         read_rds(paste0("data/ghost_location_model_output/small_ghost_features/week",
                         week_i, ".rds"))

       # Goal is to save a version of this dataset with just the week/play ids,
       # along with the grid_x and grid_y identifiers and the EP from averaging
       # over the values from sampling velo vectors

       # Now loop through each play:

       #ghost_ep_summary <-
         walk(unique(week_ghost_data$game_play_id),
                 function(play_i) {

                   #print(play_i)

                   #play_i <- "2018090600_1061"

                   # Get the play's data:
                   play_start <- play_data %>%
                     filter(game_play_id == play_i)

                   ghost_play <- week_ghost_data %>%
                     filter(game_play_id == play_i)

                   # What's the distance from the endzone:
                   obs_max_x <- ghost_play$adj_bc_x[1]

                   # Create a maximum possible gain with padding
                   max_possible_gain <- round(obs_max_x) + YAC_PADDING

                   # Now make a grid of values given the minimum observed in the whole
                   # data in increments of half yards to start:
                   gain_predict_grid <- seq(MIN_YAC, max_possible_gain,
                                            by = DELTA_YARDS)

                   # Slow way to do this, loop through each row of the play:
                   ghost_play_summary <-
                     mclapply(1:nrow(ghost_play), mc.cores = 12,
                     #map_dfr(1:nrow(ghost_play),
                             function(i) {
                               #i <- 1

                               ghost_play_loc <- ghost_play[i,]

                               # Create N_VELO_SAMPLES of each row with randomly
                               # sampled N_VELO_SAMPLES of the velo data:
                               ghost_play_velo_loc <-
                                 bind_rows(replicate(N_VELO_SAMPLES,
                                                     ghost_play_loc,
                                                     simplify = FALSE)) %>%
                                 bind_cols(sample_n(def1_velo_data,
                                                    N_VELO_SAMPLES)) %>%
                                 # Now compute the updated features:
                                 mutate(defense_1_dir_target_endzone_absval =
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

                               # Generate the CDE estimates:
                               cde_pred_matrix <- predict(yac_rfcde,
                                                   as.matrix(
                                                     dplyr::select(ghost_play_velo_loc,
                                                                   all_of(bc_var_names),
                                                                   all_of(def1_var_names))
                                                   ),
                                                   "CDE", gain_predict_grid)

                               yac_pred_table <- map_dfr(1:N_VELO_SAMPLES,
                                       function(b) {
                                         tibble(pred_yac = gain_predict_grid,
                                                cde = as.numeric(cde_pred_matrix[b,]),
                                                b_i = b)
                                       }) %>%
                                 mutate(pred_yac = pmin(pred_yac, obs_max_x)) %>%
                                 # Sum the rows in the padding above:
                                 group_by(b_i, pred_yac) %>%
                                 summarize(cde = sum(cde, na.rm = TRUE),
                                           .groups = "drop") %>%
                                 # Convert to prob:
                                 # Group by the b_i again: I FORGOT TO DO THIS
                                 # BEFORE HENCE WHY THE EPVS WERE SO SMALL!
                                 group_by(b_i) %>%
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
                               yac_pred_table %>%
                                 mutate(ep = compute_ep(yac_pred_table$pred_bc_x,
                                                        play_start,
                                                        ep_model)) %>%
                                 group_by(b_i) %>%
                                 summarize(ev_ep = sum(pred_prob * ep),
                                           .groups = "drop") %>%
                                 bind_cols(dplyr::select(ghost_play_loc,
                                                         grid_x, grid_y)) %>%
                                 group_by(grid_x, grid_y) %>%
                                 summarize(ev_ep = mean(ev_ep, na.rm = TRUE),
                                           .groups = "drop") %>%
                                 bind_cols(dplyr::select(ghost_play_loc,
                                                         week_id, game_play_id, cde_pred, pred_prob))

                             }) %>%
                     bind_rows()



                   # Save as rds file:
                   write_rds(ghost_play_summary,
                             path = paste0("data/ghost_ep_values/ghost_location_distribution/week",
                                           week_i, "/play_", play_i,".rds"),
                             compress = "gz")



                 })






     })










