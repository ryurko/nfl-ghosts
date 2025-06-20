# PURPOSE: Create figures displaying the relevant EPV quantities for the example
#          play in the talks / paper

library(tidyverse)
library(cowplot)

# Explore this play: 2018111103_2796 in week 10

# Read in play data for context to figure out interesting example ---------

play_info <- read_csv("data/nfl-big-data-bowl-2021/plays.csv")
ex_play_info <- play_info %>%
  filter(gameId == 2018111103, playId == 2796)

# Load the actual tracking data to display --------------------------------

raw_play_tracking_data <-
  read_csv("data/nfl-big-data-bowl-2021/weekly_raw_tracking/week10.csv") %>%
  filter(gameId == 2018111103, playId == 2796)

# Filter to when the pass is caught:
play_at_catch_tracking_data <- raw_play_tracking_data %>%
  filter(event == "pass_outcome_caught") %>%
  # Create a column denoting special roles:
  mutate(player_role =
           case_when(nflId == 2552409 ~ "bc",
                     nflId == 2560952 ~ "def",
                     is.na(nflId) ~ "football",
                     TRUE ~ "other"),
         # Split between remaining offense and defense:
         # (in this play away is offense, home is defense)
         team_side = ifelse(team == "away", "offense", "defense"),
         player_role = ifelse(player_role == "other",
                              paste0(player_role, "_", team_side),
                              player_role))


#  velocity angle in radians (followed example here: https://github.com/asonty/ngs_highlights/blob/master/utils/scripts/plot_utils.R)
play_at_catch_tracking_data$dir_rad <- play_at_catch_tracking_data$dir * pi / 180

#  velocity components
play_at_catch_tracking_data$v_x <- sin(play_at_catch_tracking_data$dir_rad) *
  play_at_catch_tracking_data$s
play_at_catch_tracking_data$v_y <- cos(play_at_catch_tracking_data$dir_rad) *
  play_at_catch_tracking_data$s
# What are the receiver's coordinates:
receiver_tracking_data <- play_at_catch_tracking_data %>%
  filter(player_role == "bc")

def_tracking_data <- play_at_catch_tracking_data %>%
  filter(player_role == "def")

# Create the tracking data display ----------------------------------------

# General field boundaries
xmin <- 0
xmax <- 160/3
hash_right <- 38.35
hash_left <- 12
hash_width <- 3.3

# Specific boundaries for a given play
ymin <- 0
ymax <- 120
df_hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df_hash <- df_hash %>% filter(!(floor(y %% 5) == 0))
df_hash <- df_hash %>% filter(y < ymax, y > ymin)

# Use the play direction to determine the first down marker location:
x_first_down_marker <- ex_play_info$absoluteYardlineNumber -
  ex_play_info$yardsToGo

# Now create the plot:
at_catch_plot <- ggplot() +
  annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 160/3, alpha = 0.3,
           color = "gray", fill = "darkgreen") +
  annotate("text", y = df_hash$x[df_hash$x < 55/2], alpha = 0.75, color = "white",
           x = df_hash$y[df_hash$x < 55/2], label = "|", vjust = -0.3, hjust = 0.4) +
  annotate("text", y = df_hash$x[df_hash$x > 55/2], alpha = 0.75, color = "white",
           x = df_hash$y[df_hash$x > 55/2], label = "|", vjust = 1, hjust = 0.4) +
  annotate("segment", y = xmin,
           x = seq(max(10, ymin), min(ymax, 110), by = 5),
           yend =  xmax, color = "white",
           xend = seq(max(10, ymin), min(ymax, 110), by = 5), alpha = 0.75) +
  annotate("text", y = rep(hash_left, 11), x = seq(10, 110, by = 10), alpha = 0.75,
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "),
           angle = 0, size = 4, color = "white") +
  annotate("text", y = rep((xmax - hash_left), 11), x = seq(10, 110, by = 10),
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"),
           angle = 180, size = 4, alpha = 0.75, color = "white") +
  annotate("segment", y = c(xmin, xmin, xmax, xmax),
           x = c(ymin, ymax, ymax, ymin),
           yend = c(xmin, xmax, xmax, xmin),
           xend = c(ymax, ymax, ymin, ymin), colour = "white", alpha = 0.25) +
  # Line connecting ghost to player:
  annotate("segment", y = xmin, yend = xmax,
           x = x_first_down_marker,
           xend = x_first_down_marker,
           color = "gold", size = 2) +
  # annotate("segment", y = def_tracking_data$y, yend = def_ghost_y,
  #          x = def_tracking_data$x, xend = def_ghost_x,
  #          color = "red", size = 2) +
  geom_point(data = play_at_catch_tracking_data, #alpha = 0.75,
             aes(y = y, x = x, colour = player_role,
                 fill = player_role, alpha = player_role,
                 group = nflId, pch = player_role, size = player_role)) +
  geom_segment(data = play_at_catch_tracking_data, #alpha = 0.75,
               aes(y = y, x = x, xend = x + v_x, yend = y + v_y,
                   group = nflId, alpha = player_role),
               color = "black",
               alpha = 0.75,
               arrow = arrow(length = unit(0.01, "npc"))) +
  # annotate("point", x = def_ghost_x, y = def_ghost_y,
  #          fill = "gray25", size = 6, shape = 21, color = "black") +
  scale_size_manual(values = c(6, 6, 3, 5, 5), guide = FALSE) +
  scale_shape_manual(values = c(21, 21, 16, 21, 21), guide = FALSE) +
  scale_colour_manual(values = c("black", "black", "#654321", "white", "white"), guide = FALSE) +
  scale_fill_manual(values = c("blue", "red", "#654321", "darkorange", "blue")) +
  scale_alpha_manual(values = c(0.75, 1, 0.6, 0.6, 0.6), guide = FALSE) +
  xlim(ymin, ymax) +
  coord_fixed() +
  theme_nothing() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

save_plot("figures/ex_play_at_catch.pdf", at_catch_plot,
          base_asp = 2)
save_plot("figures/ex_play_at_catch.jpg", at_catch_plot,
          base_asp = 2)

# Display the YAC RFCDE underneath ----------------------------------------

# Load the play's YAC CDE observed estimates:

ex_play_obs_yac_cde <-
  read_rds("data/ghost_ep_values/play_level_summary/observed_yac_cde_new/week10/play_2018111103_2796.rds")

ex_rfcde_cde_curve <- ex_play_obs_yac_cde %>%
  mutate(pred_end = receiver_tracking_data$x - pred_yac) %>%
  ggplot() +
  geom_vline(xintercept = x_first_down_marker, color = "gold", size = 2) +
  geom_line(aes(x = pred_end, y = pred_prob), color = "red") +
  theme_minimal() +
  #theme(legend.position = "bottom") +
  #scale_color_manual(values = c("red", "gray25")) +
  labs(x = "Yard line",
       y = "Conditional density estimate",
       color = "Type:") +
  scale_x_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, by = 10),
                     labels = c("", as.character(seq(0, 50, by = 10)),
                                as.character(seq(40, 0, by = -10)), ""))

at_catch_yac_cde_plot <-
  plot_grid(at_catch_plot, ex_rfcde_cde_curve,
            ncol = 1, align = "v")

save_plot("figures/ex_play_at_catch_yac_cde.jpg", at_catch_yac_cde_plot,
          base_asp = 2, nrow = 2)


# Load the example play's ghost and sample values -------------------------

ex_play_ghost_cde_vals <-
  read_rds("data/ghost_location_model_output/at_catch_location_probs/week10.rds") %>%
  filter(game_play_id == "2018111103_2796")

ex_play_ghost_ep_vals <-
  read_rds("data/ghost_ep_values/play_grid_sample_ep/week10.rds") %>%
  #read_rds("data/ghost_ep_values/play_grid_ep/week10.rds") %>%
  filter(game_play_id == "2018111103_2796")


# Create the ghost heatmap for play ---------------------------------------

# Create grid to plot on top with raw positions:
ex_play_yac_grid <- ex_play_ghost_cde_vals %>%
  mutate(raw_x = grid_x + 10,
         raw_y = (160 / 6) - grid_y)

# First version without the defender:
no_def_plot <- ggplot() +
  annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 160/3, alpha = 0.3,
           color = "gray", fill = "darkgreen") +
  annotate("text", y = df_hash$x[df_hash$x < 55/2], alpha = 0.75, color = "white",
           x = df_hash$y[df_hash$x < 55/2], label = "|", vjust = -0.3, hjust = 0.4) +
  annotate("text", y = df_hash$x[df_hash$x > 55/2], alpha = 0.75, color = "white",
           x = df_hash$y[df_hash$x > 55/2], label = "|", vjust = 1, hjust = 0.4) +
  annotate("segment", y = xmin,
           x = seq(max(10, ymin), min(ymax, 110), by = 5),
           yend =  xmax, color = "white",
           xend = seq(max(10, ymin), min(ymax, 110), by = 5), alpha = 0.75) +
  annotate("text", y = rep(hash_left, 11), x = seq(10, 110, by = 10), alpha = 0.75,
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "),
           angle = 0, size = 4, color = "white") +
  annotate("text", y = rep((xmax - hash_left), 11), x = seq(10, 110, by = 10),
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"),
           angle = 180, size = 4, alpha = 0.75, color = "white") +
  annotate("segment", y = c(xmin, xmin, xmax, xmax),
           x = c(ymin, ymax, ymax, ymin),
           yend = c(xmin, xmax, xmax, xmin),
           xend = c(ymax, ymax, ymin, ymin), colour = "white", alpha = 0.25) +
  # Line connecting ghost to player:
  annotate("segment", y = xmin, yend = xmax,
           x = x_first_down_marker,
           xend = x_first_down_marker,
           color = "gold", size = 2) +
  geom_point(data = filter(play_at_catch_tracking_data,
                           player_role %in% c("bc", "other_offense")),
             aes(y = y, x = x), colour = "black",
             fill = "blue", alpha = 0.6,
             pch = 21, size = 4) +
  geom_segment(data = filter(play_at_catch_tracking_data,
                             player_role %in% c("bc", "other_offense")),
               aes(y = y, x = x, xend = x + v_x, yend = y + v_y),
               color = "black",
               alpha = 0.6,
               arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data = filter(play_at_catch_tracking_data,
                           player_role %in% c("other_defense")),
             aes(y = y, x = x), colour = "black",
             fill = "darkorange", alpha = 0.6,
             pch = 21, size = 4) +
  geom_segment(data = filter(play_at_catch_tracking_data,
                             player_role %in% c("other_defense")),
               aes(y = y, x = x, xend = x + v_x, yend = y + v_y),
               color = "black",
               alpha = 0.6,
               arrow = arrow(length = unit(0.01, "npc"))) +
  scale_fill_viridis_c() +
  xlim(ymin, ymax) +
  coord_fixed() +
  theme_nothing() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
save_plot("figures/no_def_ex_play_at_catch.jpg", no_def_plot,
          base_asp = 2)

def_heatmap_plot <- ggplot() +
  annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 160/3, alpha = 0.3,
           color = "gray", fill = "darkgreen") +
  annotate("text", y = df_hash$x[df_hash$x < 55/2], alpha = 0.75, color = "white",
           x = df_hash$y[df_hash$x < 55/2], label = "|", vjust = -0.3, hjust = 0.4) +
  annotate("text", y = df_hash$x[df_hash$x > 55/2], alpha = 0.75, color = "white",
           x = df_hash$y[df_hash$x > 55/2], label = "|", vjust = 1, hjust = 0.4) +
  annotate("segment", y = xmin,
           x = seq(max(10, ymin), min(ymax, 110), by = 5),
           yend =  xmax, color = "white",
           xend = seq(max(10, ymin), min(ymax, 110), by = 5), alpha = 0.75) +
  annotate("text", y = rep(hash_left, 11), x = seq(10, 110, by = 10), alpha = 0.75,
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "),
           angle = 0, size = 4, color = "white") +
  annotate("text", y = rep((xmax - hash_left), 11), x = seq(10, 110, by = 10),
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"),
           angle = 180, size = 4, alpha = 0.75, color = "white") +
  annotate("segment", y = c(xmin, xmin, xmax, xmax),
           x = c(ymin, ymax, ymax, ymin),
           yend = c(xmin, xmax, xmax, xmin),
           xend = c(ymax, ymax, ymin, ymin), colour = "white", alpha = 0.25) +
  # Line connecting ghost to player:
  annotate("segment", y = xmin, yend = xmax,
           x = x_first_down_marker,
           xend = x_first_down_marker,
           color = "gold", size = 2) +
  geom_point(data = filter(play_at_catch_tracking_data,
                           player_role %in% c("bc", "other_offense")),
             aes(y = y, x = x), colour = "black",
             fill = "blue", alpha = 0.6,
             pch = 21, size = 4) +
  geom_segment(data = filter(play_at_catch_tracking_data,
                             player_role %in% c("bc", "other_offense")),
               aes(y = y, x = x, xend = x + v_x, yend = y + v_y),
               color = "black",
               alpha = 0.6,
               arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data = filter(play_at_catch_tracking_data,
                           player_role %in% c("other_defense")),
             aes(y = y, x = x), colour = "black",
             fill = "darkorange", alpha = 0.6,
             pch = 21, size = 4) +
  geom_segment(data = filter(play_at_catch_tracking_data,
                             player_role %in% c("other_defense")),
               aes(y = y, x = x, xend = x + v_x, yend = y + v_y),
               color = "black",
               alpha = 0.6,
               arrow = arrow(length = unit(0.01, "npc"))) +
  geom_tile(data = ex_play_yac_grid,
            aes(x = raw_x, y = raw_y, fill = pred_prob),
            alpha = 0.95) +
  scale_fill_viridis_c() +
  xlim(ymin, ymax) +
  coord_fixed() +
  theme_nothing() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 6)) +
  labs(fill = "Estimated nearest defender location probability")

save_plot("figures/heatmap_ex_play_at_catch.jpg", def_heatmap_plot,
          base_asp = 2)


# Visualize distribution of EPV values with velo samples ------------------

# Compute the observed EPV value at catch:
obs_epv_at_catch <- ex_play_obs_yac_cde %>%
  summarize(epv = sum(ep * pred_prob)) %>%
  pull(epv)

# Compute the delta and integrate across:
mean_delta_ex_play <- ex_play_ghost_ep_vals %>%
  mutate(delta = obs_epv_at_catch - ep) %>%
  group_by(grid_x, grid_y) %>%
  summarise(grid_delta = mean(delta),
           .groups = "drop") %>%
  left_join(dplyr::select(ex_play_ghost_cde_vals,
                          grid_x, grid_y, pred_prob),
            by = c("grid_x", "grid_y")) %>%
  summarize(epv_delta = sum(grid_delta * pred_prob))
mean_delta_ex_play
# # A tibble: 1 x 1
#     epv_delta
#         <dbl>
#   1     -1.22

# Use sampling to generate a distribution of the EPVs for visualization:
epv_distr_plot <- ex_play_ghost_ep_vals %>%
  left_join(dplyr::select(ex_play_ghost_cde_vals,
                          grid_x, grid_y, pred_prob),
            by = c("grid_x", "grid_y")) %>%
  # Sample to roughly approximate the distribution
  sample_n(size = 1000000, replace = TRUE, weight = pred_prob) %>%
  ggplot(aes(x = ep)) +
  geom_density(adjust = 2.5) +
  geom_vline(xintercept = obs_epv_at_catch, color = "red", size = 2) +
  geom_vline(xintercept = 0,
             linetype = "dashed", color = "gray") +
  annotate(geom = "text", x = -1, y = 0.1, size = 4,
           color = "red", label = "Observed EPV") +
  labs(x = "Estimated ghost EPV", y = "Density") +
  theme_light()
save_plot("figures/ghost_epv_distr.jpg", epv_distr_plot,
          base_asp = 2)

# Visualize the distribution of differences:
epv_diff_plot <- ex_play_ghost_ep_vals %>%
  mutate(delta = obs_epv_at_catch - ep) %>%
  left_join(dplyr::select(ex_play_ghost_cde_vals,
                          grid_x, grid_y, pred_prob),
            by = c("grid_x", "grid_y")) %>%
  # Sample to roughly approximate the distribution
  sample_n(size = 1000000, replace = TRUE, weight = pred_prob) %>%
  ggplot(aes(x = delta)) +
  geom_density(adjust = 2.5) +
  geom_vline(xintercept = mean_delta_ex_play$epv_delta,
             color = "blue", size = 2) +
  geom_vline(xintercept = 0,
             linetype = "dashed", color = "gray") +
  annotate(geom = "text", x = -1.75, y = 0.6, size = 4,
           color = "blue", label = "Average delta EPV") +
  labs(x = "Estimated delta EPV", y = "Density") +
  theme_light()
save_plot("figures/ghost_epv_diff_distr.jpg", epv_diff_plot,
          base_asp = 2)


# View EPV grid plot ------------------------------------------------------

# Compute the average EPV and EPV diff at each location:
epv_grid_data <- ex_play_ghost_ep_vals %>%
  mutate(delta = obs_epv_at_catch - ep) %>%
  group_by(grid_x, grid_y) %>%
  summarise(grid_ep = mean(ep),
            grid_delta = mean(delta),
            .groups = "drop") %>%
  left_join(dplyr::select(ex_play_ghost_cde_vals,
                          grid_x, grid_y, pred_prob),
            by = c("grid_x", "grid_y")) %>%
  mutate(raw_x = grid_x + 10,
         raw_y = (160 / 6) - grid_y)


# Start with the EPV heatmap with no defender:
no_def_epv_grid_plot <- ggplot() +
  annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 160/3, alpha = 0.3,
           color = "gray", fill = "darkgreen") +
  annotate("text", y = df_hash$x[df_hash$x < 55/2], alpha = 0.75, color = "white",
           x = df_hash$y[df_hash$x < 55/2], label = "|", vjust = -0.3, hjust = 0.4) +
  annotate("text", y = df_hash$x[df_hash$x > 55/2], alpha = 0.75, color = "white",
           x = df_hash$y[df_hash$x > 55/2], label = "|", vjust = 1, hjust = 0.4) +
  annotate("segment", y = xmin,
           x = seq(max(10, ymin), min(ymax, 110), by = 5),
           yend =  xmax, color = "white",
           xend = seq(max(10, ymin), min(ymax, 110), by = 5), alpha = 0.75) +
  annotate("text", y = rep(hash_left, 11), x = seq(10, 110, by = 10), alpha = 0.75,
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "),
           angle = 0, size = 4, color = "white") +
  annotate("text", y = rep((xmax - hash_left), 11), x = seq(10, 110, by = 10),
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"),
           angle = 180, size = 4, alpha = 0.75, color = "white") +
  annotate("segment", y = c(xmin, xmin, xmax, xmax),
           x = c(ymin, ymax, ymax, ymin),
           yend = c(xmin, xmax, xmax, xmin),
           xend = c(ymax, ymax, ymin, ymin), colour = "white", alpha = 0.25) +
  # Line connecting ghost to player:
  annotate("segment", y = xmin, yend = xmax,
           x = x_first_down_marker,
           xend = x_first_down_marker,
           color = "gold", size = 2) +
  geom_point(data = filter(play_at_catch_tracking_data,
                           player_role %in% c("bc", "other_offense")),
             aes(y = y, x = x), colour = "black",
             fill = "blue", alpha = 0.6,
             pch = 21, size = 4) +
  geom_segment(data = filter(play_at_catch_tracking_data,
                             player_role %in% c("bc", "other_offense")),
               aes(y = y, x = x, xend = x + v_x, yend = y + v_y),
               color = "black",
               alpha = 0.6,
               arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data = filter(play_at_catch_tracking_data,
                           player_role %in% c("other_defense")),
             aes(y = y, x = x), colour = "black",
             fill = "darkorange", alpha = 0.6,
             pch = 21, size = 4) +
  geom_segment(data = filter(play_at_catch_tracking_data,
                             player_role %in% c("other_defense")),
               aes(y = y, x = x, xend = x + v_x, yend = y + v_y),
               color = "black",
               alpha = 0.6,
               arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data = epv_grid_data,
             aes(x = raw_x, y = raw_y, size = pred_prob,
                 color = grid_ep),
             alpha = 0.95) +
  scale_color_viridis_c(option = "plasma") +
  scale_size_continuous(range = c(0.2, 3), guide = FALSE) +
  xlim(ymin, ymax) +
  coord_fixed() +
  theme_nothing() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 6)) +
  labs(color = "Estimated ghost location EPV")

save_plot("figures/no_def_epv_grid.jpg", no_def_epv_grid_plot,
          base_asp = 2)

def_epv_grid_plot <- ggplot() +
  annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 160/3, alpha = 0.3,
           color = "gray", fill = "darkgreen") +
  annotate("text", y = df_hash$x[df_hash$x < 55/2], alpha = 0.75, color = "white",
           x = df_hash$y[df_hash$x < 55/2], label = "|", vjust = -0.3, hjust = 0.4) +
  annotate("text", y = df_hash$x[df_hash$x > 55/2], alpha = 0.75, color = "white",
           x = df_hash$y[df_hash$x > 55/2], label = "|", vjust = 1, hjust = 0.4) +
  annotate("segment", y = xmin,
           x = seq(max(10, ymin), min(ymax, 110), by = 5),
           yend =  xmax, color = "white",
           xend = seq(max(10, ymin), min(ymax, 110), by = 5), alpha = 0.75) +
  annotate("text", y = rep(hash_left, 11), x = seq(10, 110, by = 10), alpha = 0.75,
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "),
           angle = 0, size = 4, color = "white") +
  annotate("text", y = rep((xmax - hash_left), 11), x = seq(10, 110, by = 10),
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"),
           angle = 180, size = 4, alpha = 0.75, color = "white") +
  annotate("segment", y = c(xmin, xmin, xmax, xmax),
           x = c(ymin, ymax, ymax, ymin),
           yend = c(xmin, xmax, xmax, xmin),
           xend = c(ymax, ymax, ymin, ymin), colour = "white", alpha = 0.25) +
  # Line connecting ghost to player:
  annotate("segment", y = xmin, yend = xmax,
           x = x_first_down_marker,
           xend = x_first_down_marker,
           color = "gold", size = 2) +
  geom_point(data = filter(play_at_catch_tracking_data,
                           player_role %in% c("bc", "other_offense")),
             aes(y = y, x = x), colour = "black",
             fill = "blue", alpha = 0.6,
             pch = 21, size = 4) +
  geom_segment(data = filter(play_at_catch_tracking_data,
                             player_role %in% c("bc", "other_offense")),
               aes(y = y, x = x, xend = x + v_x, yend = y + v_y),
               color = "black",
               alpha = 0.6,
               arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data = filter(play_at_catch_tracking_data,
                           player_role %in% c("other_defense")),
             aes(y = y, x = x), colour = "black",
             fill = "darkorange", alpha = 0.6,
             pch = 21, size = 4) +
  geom_segment(data = filter(play_at_catch_tracking_data,
                             player_role %in% c("other_defense")),
               aes(y = y, x = x, xend = x + v_x, yend = y + v_y),
               color = "black",
               alpha = 0.6,
               arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data = epv_grid_data,
             aes(x = raw_x, y = raw_y, size = pred_prob,
                 color = grid_ep),
             alpha = 0.95) +
  geom_point(data = filter(play_at_catch_tracking_data,
                           player_role == "def"),
             aes(y = y, x = x), colour = "black",
             fill = "red", alpha = 1,
             pch = 21, size = 4) +
  geom_segment(data = filter(play_at_catch_tracking_data,
                             player_role %in% c("def")),
               aes(y = y, x = x, xend = x + v_x, yend = y + v_y),
               color = "red",
               alpha = 0.6,
               arrow = arrow(length = unit(0.01, "npc"))) +
  scale_color_viridis_c(option = "plasma") +
  scale_size_continuous(range = c(0.2, 3), guide = FALSE) +
  xlim(ymin, ymax) +
  coord_fixed() +
  theme_nothing() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 6)) +
  labs(color = "Estimated ghost location EPV")

save_plot("figures/def_epv_grid.jpg", def_epv_grid_plot,
          base_asp = 2)



diff_epv_grid_plot <- ggplot() +
  annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 160/3, alpha = 0.3,
           color = "gray", fill = "darkgreen") +
  annotate("text", y = df_hash$x[df_hash$x < 55/2], alpha = 0.75, color = "white",
           x = df_hash$y[df_hash$x < 55/2], label = "|", vjust = -0.3, hjust = 0.4) +
  annotate("text", y = df_hash$x[df_hash$x > 55/2], alpha = 0.75, color = "white",
           x = df_hash$y[df_hash$x > 55/2], label = "|", vjust = 1, hjust = 0.4) +
  annotate("segment", y = xmin,
           x = seq(max(10, ymin), min(ymax, 110), by = 5),
           yend =  xmax, color = "white",
           xend = seq(max(10, ymin), min(ymax, 110), by = 5), alpha = 0.75) +
  annotate("text", y = rep(hash_left, 11), x = seq(10, 110, by = 10), alpha = 0.75,
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "),
           angle = 0, size = 4, color = "white") +
  annotate("text", y = rep((xmax - hash_left), 11), x = seq(10, 110, by = 10),
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"),
           angle = 180, size = 4, alpha = 0.75, color = "white") +
  annotate("segment", y = c(xmin, xmin, xmax, xmax),
           x = c(ymin, ymax, ymax, ymin),
           yend = c(xmin, xmax, xmax, xmin),
           xend = c(ymax, ymax, ymin, ymin), colour = "white", alpha = 0.25) +
  # Line connecting ghost to player:
  annotate("segment", y = xmin, yend = xmax,
           x = x_first_down_marker,
           xend = x_first_down_marker,
           color = "gold", size = 2) +
  geom_point(data = filter(play_at_catch_tracking_data,
                           player_role %in% c("bc", "other_offense")),
             aes(y = y, x = x), colour = "black",
             fill = "blue", alpha = 0.6,
             pch = 21, size = 4) +
  geom_segment(data = filter(play_at_catch_tracking_data,
                             player_role %in% c("bc", "other_offense")),
               aes(y = y, x = x, xend = x + v_x, yend = y + v_y),
               color = "black",
               alpha = 0.6,
               arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data = filter(play_at_catch_tracking_data,
                           player_role %in% c("other_defense")),
             aes(y = y, x = x), colour = "black",
             fill = "darkorange", alpha = 0.6,
             pch = 21, size = 4) +
  geom_segment(data = filter(play_at_catch_tracking_data,
                             player_role %in% c("other_defense")),
               aes(y = y, x = x, xend = x + v_x, yend = y + v_y),
               color = "black",
               alpha = 0.6,
               arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data = epv_grid_data,
             aes(x = raw_x, y = raw_y, size = pred_prob,
                 color = grid_delta),
             alpha = 0.95) +
  # geom_point(data = filter(play_at_catch_tracking_data,
  #                          player_role == "def"),
  #            aes(y = y, x = x), colour = "black",
  #            fill = "red", alpha = 1,
  #            pch = 21, size = 6) +
  # geom_segment(data = filter(play_at_catch_tracking_data,
  #                            player_role %in% c("def")),
  #              aes(y = y, x = x, xend = x + v_x, yend = y + v_y),
  #              color = "red",
  #              alpha = 0.6,
  #              arrow = arrow(length = unit(0.01, "npc"))) +
  scale_color_gradient2(low = "red", mid = "gray",
                        high = "blue", midpoint = 0) +
  scale_size_continuous(range = c(0.2, 3), guide = FALSE) +
  xlim(ymin, ymax) +
  coord_fixed() +
  theme_nothing() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 6)) +
  labs(color = "Estimated difference in EPV")

save_plot("figures/diff_epv_grid.jpg", diff_epv_grid_plot,
          base_asp = 2)


def_diff_epv_grid_plot <- ggplot() +
  annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 160/3, alpha = 0.3,
           color = "gray", fill = "darkgreen") +
  annotate("text", y = df_hash$x[df_hash$x < 55/2], alpha = 0.75, color = "white",
           x = df_hash$y[df_hash$x < 55/2], label = "|", vjust = -0.3, hjust = 0.4) +
  annotate("text", y = df_hash$x[df_hash$x > 55/2], alpha = 0.75, color = "white",
           x = df_hash$y[df_hash$x > 55/2], label = "|", vjust = 1, hjust = 0.4) +
  annotate("segment", y = xmin,
           x = seq(max(10, ymin), min(ymax, 110), by = 5),
           yend =  xmax, color = "white",
           xend = seq(max(10, ymin), min(ymax, 110), by = 5), alpha = 0.75) +
  annotate("text", y = rep(hash_left, 11), x = seq(10, 110, by = 10), alpha = 0.75,
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "),
           angle = 0, size = 4, color = "white") +
  annotate("text", y = rep((xmax - hash_left), 11), x = seq(10, 110, by = 10),
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"),
           angle = 180, size = 4, alpha = 0.75, color = "white") +
  annotate("segment", y = c(xmin, xmin, xmax, xmax),
           x = c(ymin, ymax, ymax, ymin),
           yend = c(xmin, xmax, xmax, xmin),
           xend = c(ymax, ymax, ymin, ymin), colour = "white", alpha = 0.25) +
  # Line connecting ghost to player:
  annotate("segment", y = xmin, yend = xmax,
           x = x_first_down_marker,
           xend = x_first_down_marker,
           color = "gold", size = 2) +
  geom_point(data = filter(play_at_catch_tracking_data,
                           player_role %in% c("bc", "other_offense")),
             aes(y = y, x = x), colour = "black",
             fill = "blue", alpha = 0.6,
             pch = 21, size = 4) +
  geom_segment(data = filter(play_at_catch_tracking_data,
                             player_role %in% c("bc", "other_offense")),
               aes(y = y, x = x, xend = x + v_x, yend = y + v_y),
               color = "black",
               alpha = 0.6,
               arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data = filter(play_at_catch_tracking_data,
                           player_role %in% c("other_defense")),
             aes(y = y, x = x), colour = "black",
             fill = "darkorange", alpha = 0.6,
             pch = 21, size = 4) +
  geom_segment(data = filter(play_at_catch_tracking_data,
                             player_role %in% c("other_defense")),
               aes(y = y, x = x, xend = x + v_x, yend = y + v_y),
               color = "black",
               alpha = 0.6,
               arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data = epv_grid_data,
             aes(x = raw_x, y = raw_y, size = pred_prob,
                 color = grid_delta),
             alpha = 0.95) +
  geom_point(data = filter(play_at_catch_tracking_data,
                           player_role == "def"),
             aes(y = y, x = x), colour = "black",
             fill = "red", alpha = 1,
             pch = 21, size = 4) +
  geom_segment(data = filter(play_at_catch_tracking_data,
                             player_role %in% c("def")),
               aes(y = y, x = x, xend = x + v_x, yend = y + v_y),
               color = "red",
               alpha = 0.6,
               arrow = arrow(length = unit(0.01, "npc"))) +
scale_color_gradient2(low = "red", mid = "gray",
                      high = "blue", midpoint = 0) +
  scale_size_continuous(range = c(0.2, 3), guide = FALSE) +
  xlim(ymin, ymax) +
  coord_fixed() +
  theme_nothing() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 6)) +
  labs(color = "Estimated difference in EPV")

save_plot("figures/def_diff_epv_grid.jpg", def_diff_epv_grid_plot,
          base_asp = 2)









