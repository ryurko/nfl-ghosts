# PURPOSE: Save filtered versions of the 2D ghost CDE datasets to speed up the
#          computation time for computing ghost EV - basically tossing out
#          locations that in total sum up to < 0.0001 probability

library(tidyverse)


# Walk through each week and filter data ----------------------------------

PROB_FILTER <- 0.9999

walk(1:17,
     function(week_i) {

       # Read in the weekly features to generate YAC distributions for:
       week_ghost_data <-
         read_rds(paste0("data/ghost_location_model_output/ghost_yac_features/week",
                         week_i, ".rds"))

       clean_week_ghost_data <- week_ghost_data %>%
         arrange(game_play_id, desc(pred_prob)) %>%
         group_by(game_play_id) %>%
         mutate(cum_prob = cumsum(pred_prob)) %>%
         filter(cum_prob <= PROB_FILTER) %>%
         ungroup() %>%
         # Renormalize the probabilities:
         mutate(pred_prob = pred_prob / PROB_FILTER) %>%
         dplyr::select(-cum_prob)

       # Save as rds file:
       write_rds(clean_week_ghost_data,
                 path = paste0("data/ghost_location_model_output/small_ghost_features/week",
                               week_i, ".rds"),
                 compress = "gz")

     })
