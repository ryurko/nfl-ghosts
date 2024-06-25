# PURPOSE: Set-up a table of just the velocity vectors that the nearest defender is facing
#          to sample from in the main script for computing ghost EV

library(tidyverse)


# Load the model data -----------------------------------------------------

model_data <-
  read_rds("data/model_datasets/at_catch_yac_model_data.rds")

# Set-up the table to only have nearest defense s, dir, o -----------------

defense_velo_data <- model_data %>%
  dplyr::select(defense_1_s, defense_1_dir_target_endzone,
                defense_1_o_target_endzone) %>%
  filter(!is.na(defense_1_s), !is.na(defense_1_dir_target_endzone),
         !is.na(defense_1_o_target_endzone))


# Save this table ---------------------------------------------------------

write_rds(defense_velo_data,
          "data/ghost_ep_values/def1_velo_data.rds",
          compress = "gz")
