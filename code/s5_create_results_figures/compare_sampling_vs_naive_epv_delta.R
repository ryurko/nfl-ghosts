# PURPOSE: Get the play-level results with the other two approaches for computing
#          the ghost EPVs based on the velo sampling strategy

library(tidyverse)

# Ignoring the velo vectors -----------------------------------------------

naive_play_level_summary <-
  map_dfr(1:17,
          function(week_i) {

            # First load CDE values
            week_cde_vals <-
              read_rds(paste0("data/ghost_location_model_output/at_catch_location_probs/week",
                              week_i, ".rds"))

            # Next the ghost EPV values:
            week_epv_vals <-
              read_rds(paste0("data/ghost_ep_values/play_grid_ep/week",
                              week_i, ".rds"))

            # Now the observed EPV values
            week_obs_epv <-
              read_rds(paste0("data/ghost_ep_values/play_level_summary/observed_epv_new/week",
                              week_i, ".rds")) %>%
              rename(obs_ep = ep)

            week_epv_vals %>%
              left_join(week_obs_epv, by = c("week_id", "game_play_id")) %>%
              mutate(delta = obs_ep - ep) %>%
              group_by(game_play_id, grid_x, grid_y) %>%
              summarise(grid_delta = mean(delta),
                        .groups = "drop") %>%
              left_join(dplyr::select(week_cde_vals,
                                      game_play_id,
                                      grid_x, grid_y, pred_prob),
                        by = c("game_play_id", "grid_x", "grid_y")) %>%
              group_by(game_play_id) %>%
              summarize(epv_delta = sum(grid_delta * pred_prob),
                        .groups = "drop") %>%
              mutate(week_id = week_i)

          })

write_csv(naive_play_level_summary,
          "data/ghost_ep_values/play_level_summary/naive_epv_diff_play_summary_table.csv")


# Compare against the informed version ------------------------------------

weighted_summary <- read_csv("data/ghost_ep_values/play_level_summary/epv_diff_play_summary_table.csv")

# Create new dataset to use for summarizing
comparison_summary <- weighted_summary %>%
  dplyr::select(game_play_id, epv_delta) %>%
  left_join(rename(naive_play_level_summary, naive_epv_delta = epv_delta),
            by = "game_play_id")
epv_delta_cor <- cor(comparison_summary$naive_epv_delta,
                     comparison_summary$epv_delta)
epv_delta_cor
# [1] 0.9042589


comp_plot <- comparison_summary %>%
  ggplot(aes(x = naive_epv_delta, y = epv_delta)) +
  geom_point(alpha = 0.5, size = .8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm") +
  coord_fixed() +
  labs(x = "Naive estimated average delta EPV",
       y = "Weighted sampling based estimated average delta EPV") +
  theme_bw() +
  theme(axis.title = element_text(size = 8))

save_plot("figures/suppl_delta_epv_comparison.jpeg",
          comp_plot, ncol = 1, nrow = 1)









