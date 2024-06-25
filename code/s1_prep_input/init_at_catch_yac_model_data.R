# PURPOSE: Create YAC modeling dataset using information at moment of catch,
#          QB at release, and play-level context

library(tidyverse)


# Load the weekly YAC receiver features but limit to at-catch -------------

yac_at_catch_data <-
  map_dfr(1:17,
          function(week_i) {

            read_rds(paste0("data/weekly_yac_bc_features/week",
                            week_i, ".rds")) %>%
              filter(is_start_bc == 1)
          })

# Load weekly data corresponding to QB position at release ----------------

qb_release_data <-
  map_dfr(1:17,
          function(week_i) {

            read_rds(paste0("data/weekly_qb_at_release_features/week",
                            week_i, ".rds")) %>%
              dplyr::select(-frameId)
          })

# Load play-level data and join all together ------------------------------

play_level_data <- read_csv("data/nfl-big-data-bowl-2021/play_level_features.csv")

yac_model_data <- yac_at_catch_data %>%
  left_join(qb_release_data, by = c("gameId", "playId")) %>%
  left_join(play_level_data, by = c("gameId", "playId")) %>%
  # Create variable indicating where receiver is wrt first down line;
  # positive values indicate yards to first down / goal line while negative
  # indicates the ball-carrier is beyond the first down marker
  mutate(adj_bc_x_from_first_down = adj_bc_x - adj_x_first_down)

# Create list of covariates -----------------------------------------------

# Start with bc variables:
bc_var_names <- c("adj_bc_x", "adj_bc_y", "bc_s",
                  "bc_dir_target_endzone", "bc_o_target_endzone",
                  "adj_bc_x_from_first_down")

# Next the QB variables:
qb_var_names <- c("adj_qb_x", "adj_qb_y", "qb_s",
                  "qb_dir_target_endzone", "qb_o_target_endzone",
                  "qb_adj_x_change", "qb_adj_y_change",
                  "qb_dist_to_bc",
                  "qb_dir_wrt_bc_diff", "qb_o_wrt_bc_diff")

# Next make a list of all possible offense and defense player information
def_var_name_list <-
  lapply(1:4,
         function(def_i) {
           str_subset(colnames(yac_model_data), paste0("defense_", def_i, "_")) %>%
             str_subset("(_adj_x)|(_adj_y)|(_dist_to_bc)|(_s)|(_dir_target_endzone)|(_o_target_endzone)|(_wrt_bc_diff)")
         })

# Repeat for offense:
off_var_name_list <-
  lapply(1:4,
         function(off_i) {
           str_subset(colnames(yac_model_data), paste0("offense_", off_i, "_")) %>%
             str_subset("(_adj_x)|(_adj_y)|(_dist_to_bc)|(_s)|(_dir_target_endzone)|(_o_target_endzone)|(_wrt_bc_diff)")
         })

# Initial play-level context:
play_context_var_names <-
  setdiff(colnames(play_level_data),
          c("gameId", "playId", "adj_x_first_down", "quarter",
            "qtr_sec_remain", "game_sec_remain"))


# Select the necessary columns from the above lists -----------------------

yac_clean_model_data <- yac_model_data %>%
  dplyr::select(week_id, game_play_id, end_x_change,
                all_of(bc_var_names), all_of(qb_var_names),
                all_of(unlist(def_var_name_list)),
                all_of(unlist(off_var_name_list)),
                all_of(play_context_var_names)) %>%
  # Create absolute value versions of direction and y change variables:
  mutate_at(vars(contains("dir_target_endzone")),
            .funs = list(absval = ~abs(.))) %>%
  mutate_at(vars(contains("o_target_endzone")),
            .funs = list(absval = ~abs(.))) %>%
  mutate_at(vars(contains("adj_y_change")),
            .funs = list(absval = ~abs(.)))


# Save --------------------------------------------------------------------

write_rds(yac_clean_model_data,
          "data/model_datasets/at_catch_yac_model_data.rds")
