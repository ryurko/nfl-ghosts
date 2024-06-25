# PURPOSE: Create example play figures for talks

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

# Repeat for ghost coordinates:
# def_ghost_x <- ifelse(receiver_tracking_data$playDirection == "right",
#                       ex_play_fuller_data$x_ghost,
#                       120 - ex_play_fuller_data$x_ghost)
# def_ghost_y <- ifelse(receiver_tracking_data$playDirection == "right",
#                       ex_play_fuller_data$y_ghost,
#                       (160 / 3) - ex_play_fuller_data$y_ghost)


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


# Display the YAC RFCDE underneath ----------------------------------------

# Load the play's YAC CDE observed estimates:

ex_play_obs_yac_cde <-
  read_rds("data/ghost_ep_values/play_level_summary/observed_yac_cde/week10/play_2018111103_2796.rds")

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


plot_grid(at_catch_plot, ex_rfcde_cde_curve,
          ncol = 1, align = "v")



# Create the what if visualizations with actual ghost estimates -----------

# load the play's ghost CDE data

ex_play_ghost_yac_cde <-
  read_rds("data/ghost_ep_values/clean_ghost_loc_distr/week10/play_2018111103_2796.rds")


# Create the ghost heatmap for play ---------------------------------------

# Create grid to plot on top with raw positions:
ex_play_yac_grid <- ex_play_ghost_yac_cde %>%
  mutate(raw_x = grid_x + 10,
         raw_y = (160 / 6) - grid_y)
ggplot() +
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
  # geom_point(data = filter(play_at_catch_tracking_data,
  #                          player_role == "def"),
  #            aes(y = y, x = x), colour = "black",
  #                fill = "red", alpha = 1,
  #            pch = 21, size = 6) +
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
            aes(x = raw_x, y = raw_y, fill = cond_pred_prob),
            alpha = 0.95) +
  # annotate("segment", y = def_tracking_data$y, yend = def_ghost_y,
  #          x = def_tracking_data$x, xend = def_ghost_x,
  #          color = "red", size = 2) +
  # geom_point(data = play_at_catch_tracking_data, #alpha = 0.75,
  #            aes(y = y, x = x, colour = player_role,
  #                fill = player_role, alpha = player_role,
  #                group = nflId, pch = player_role, size = player_role)) +
  # geom_segment(data = play_at_catch_tracking_data, #alpha = 0.75,
  #              aes(y = y, x = x, xend = x + v_x, yend = y + v_y,
  #                  group = nflId, alpha = player_role),
  #              color = "black",
  #              alpha = 0.75,
  #              arrow = arrow(length = unit(0.01, "npc"))) +
  # annotate("point", x = def_ghost_x, y = def_ghost_y,
  #          fill = "gray25", size = 6, shape = 21, color = "black") +
  #scale_size_manual(values = c(6, 6, 3, 5, 5), guide = FALSE) +
  #scale_shape_manual(values = c(21, 21, 16, 21, 21), guide = FALSE) +
  #scale_colour_manual(values = c("black", "black", "#654321", "white", "white"), guide = FALSE) +
    scale_fill_viridis_c() +
  #scale_fill_manual(values = c("blue", "red", "#654321", "darkorange", "blue")) +
  #scale_alpha_manual(values = c(0.75, 1, 0.6, 0.6, 0.6), guide = FALSE) +
  xlim(ymin, ymax) +
  coord_fixed() +
  theme_nothing() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))



# Make EPV grid plot ------------------------------------------------------


test_epv_grid_plot <- ggplot() +
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
             pch = 21, size = 6) +
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
             pch = 21, size = 6) +
  geom_segment(data = filter(play_at_catch_tracking_data,
                             player_role %in% c("other_defense")),
               aes(y = y, x = x, xend = x + v_x, yend = y + v_y),
               color = "black",
               alpha = 0.6,
               arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data = ex_play_yac_grid,
            aes(x = raw_x, y = raw_y, size = cond_pred_prob,
                color = ev_ep),
            alpha = 0.95) +
  geom_point(data = filter(play_at_catch_tracking_data,
                           player_role == "def"),
             aes(y = y, x = x), colour = "black",
             fill = "red", alpha = 1,
             pch = 21, size = 6) +
  geom_segment(data = filter(play_at_catch_tracking_data,
                             player_role %in% c("def")),
               aes(y = y, x = x, xend = x + v_x, yend = y + v_y),
               color = "red",
               alpha = 0.6,
               arrow = arrow(length = unit(0.01, "npc"))) +
  # annotate("segment", y = def_tracking_data$y, yend = def_ghost_y,
  #          x = def_tracking_data$x, xend = def_ghost_x,
  #          color = "red", size = 2) +
  # geom_point(data = play_at_catch_tracking_data, #alpha = 0.75,
  #            aes(y = y, x = x, colour = player_role,
  #                fill = player_role, alpha = player_role,
  #                group = nflId, pch = player_role, size = player_role)) +
  # geom_segment(data = play_at_catch_tracking_data, #alpha = 0.75,
  #              aes(y = y, x = x, xend = x + v_x, yend = y + v_y,
  #                  group = nflId, alpha = player_role),
  #              color = "black",
#              alpha = 0.75,
#              arrow = arrow(length = unit(0.01, "npc"))) +
# annotate("point", x = def_ghost_x, y = def_ghost_y,
#          fill = "gray25", size = 6, shape = 21, color = "black") +
#scale_size_manual(values = c(6, 6, 3, 5, 5), guide = FALSE) +
#scale_shape_manual(values = c(21, 21, 16, 21, 21), guide = FALSE) +
#scale_colour_manual(values = c("black", "black", "#654321", "white", "white"), guide = FALSE) +
scale_color_viridis_c(option = "plasma") +
  scale_size_continuous(range = c(1, 6)) +
  #scale_fill_manual(values = c("blue", "red", "#654321", "darkorange", "blue")) +
  #scale_alpha_manual(values = c(0.75, 1, 0.6, 0.6, 0.6), guide = FALSE) +
  xlim(ymin, ymax) +
  coord_fixed() +
  theme_nothing() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

plot_grid(get_legend(test_epv_grid_plot + scale_size_continuous(range = c(1, 6),
                                                                guide = FALSE) +
                       theme(legend.position = "bottom",
                             legend.text = element_text(size = 8)) +
                       labs(color = "EPV")))




