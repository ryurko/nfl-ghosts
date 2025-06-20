# PURPOSE: Generate player and team summaries from EPV diff quantities

library(tidyverse)
library(cowplot)

# Read in the play summary table ------------------------------------------

play_level_summary <-
  read_csv("data/ghost_ep_values/play_level_summary/epv_diff_play_summary_table.csv")

players_data <- read_csv("data/nfl-big-data-bowl-2021/players.csv")


# Load model data to compute YAC ------------------------------------------

model_data <-
  read_rds("data/model_datasets/at_catch_yac_model_data.rds")

yac_summary <- model_data %>%
  dplyr::select(game_play_id, end_x_change) %>%
  rename(yac = end_x_change)

play_level_summary <- play_level_summary %>%
  dplyr::left_join(yac_summary, by = "game_play_id")

# Summarize performance by players and teams ------------------------------

def_play_summary <- play_level_summary %>%
  group_by(defense_1_displayName, defense_1_nflId) %>%
  summarize(n_plays = n(),
            total_yac = sum(yac, na.rm = TRUE),
            total_delta_epv = sum(epv_delta, na.rm = TRUE),
            ave_delta_epv = mean(epv_delta, na.rm = TRUE),
            ave_yac = mean(yac, na.rm = TRUE),
            .groups = "drop") %>%
  dplyr::left_join(dplyr::select(players_data, nflId, position),
                   by = c("defense_1_nflId" = "nflId"))

players_focus <- def_play_summary |>
  filter(n_plays >= 10) |>
  pull(defense_1_nflId)

player_summary_plot <- def_play_summary |>
  filter(defense_1_nflId %in% players_focus) |> 
  filter(position != "DE") |> 
  mutate(pos_group = case_when(
    position %in% c("DT", "NT", "DE") ~ "Defensive line",
    position %in% c("MLB", "OLB", "ILB", "LB") ~ "Linebackers",
    position %in% c("DB", "SS", "FS", "CB", "S") ~ "Defensive backs"
  )) |> 
  ggplot(aes(x = ave_delta_epv, y = ave_yac)) +
  # geom_smooth(method = "lm", size = 1.5, color = "black", alpha = 0.5, se = FALSE) +
  geom_point(aes(color = pos_group), alpha = 0.7, size = 2) +
  scale_color_manual(values = c("#1E88E5", "#FFC107")) +
  labs(
    x = "Average delta EPV",
    y = "Average YAC allowed",
    color = "Position"
  ) +
  theme_light() +
  theme(legend.position = "bottom") 

save_plot("figs/def_player_summary.jpg", player_summary_plot,
          base_asp = 1.3, nrow = 1)

new_player_summary_plot <- def_play_summary |>
  filter(defense_1_nflId %in% players_focus) |> 
  filter(position != "DE") |> 
  mutate(pos_group = case_when(
    position %in% c("DT", "NT", "DE") ~ "Defensive line",
    position %in% c("MLB", "OLB", "ILB", "LB") ~ "Linebackers",
    position %in% c("DB", "CB") ~ "Cornerbacks",
    position %in% c("SS", "FS", "S") ~ "Safeties",
  )) |> 
  ggplot(aes(x = ave_delta_epv, y = ave_yac)) +
  # geom_smooth(method = "lm", size = 1.5, color = "black", alpha = 0.5, se = FALSE) +
  geom_point(aes(color = pos_group), alpha = 0.7, size = 2) +
  ggthemes::scale_color_colorblind() +
  labs(
    x = "Average delta EPV",
    y = "Average YAC allowed",
    color = "Position"
  ) +
  theme_light() +
  theme(legend.position = "bottom") 
save_plot("figs/new_def_player_summary.jpg", new_player_summary_plot,
          base_asp = 1.3, nrow = 1)

def_play_summary %>%
  ggplot(aes(x = total_delta_epv, y = total_yac)) +
  geom_point(alpha = 0.5) +
  theme_bw()

def_play_summary %>%
  filter(n_plays > 10) %>%
  ggplot(aes(x = ave_delta_epv, y = ave_yac)) +
  geom_point(alpha = 0.5) +
  theme_bw()
# Well that is correlated...

# Create table of top 10 players:
library(gt)

def_play_summary %>%
  filter(n_plays > 30) %>%
  arrange(total_delta_epv) %>%
  dplyr::slice(1:10) %>%
  dplyr::select(defense_1_displayName, position, n_plays,
                total_delta_epv, total_yac, ave_delta_epv, ave_yac) %>%
  mutate(total_delta_epv = round(total_delta_epv, digits = 2),
         total_yac = round(total_yac, digits = 2),
         ave_delta_epv = round(ave_delta_epv, digits = 2),
         ave_yac = round(ave_yac, digits = 2)) %>%
  rename(Player = defense_1_displayName,
         Position = position,
         `#receptions faced` = n_plays,
         `Sum delta EPV` = total_delta_epv,
         `Total YAC allowed` = total_yac,
         `Average delta EPV` = ave_delta_epv,
         `Average YAC allowed` = ave_yac) %>%
  gt() %>%
  tab_header(title = "Top ten defensive players based on overall reduction in EPV at moment of catch",
             subtitle = "Lower values indicate better defensive performance than ghosts") %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = vars(`#receptions faced`)
      )
    )
  ) %>%
  data_color(columns = vars(`Sum delta EPV`,
                            `Total YAC allowed`,
                            `Average delta EPV`,
                            `Average YAC allowed`),
             colors = scales::col_numeric(
               palette = c("darkorange", "darkblue"),
               domain = NULL
             ))


# Correlation? ------------------------------------------------------------



# Split the season in half:
def_play_first_half_summary <- play_level_summary %>%
  filter(week_id <= 8) %>%
  group_by(defense_1_displayName, defense_1_nflId) %>%
  summarize(first_n_plays = n(),
            first_total_yac = sum(yac, na.rm = TRUE),
            first_total_delta_epv = sum(epv_delta, na.rm = TRUE),
            first_ave_delta_epv = mean(epv_delta, na.rm = TRUE),
            first_ave_yac = mean(yac, na.rm = TRUE),
            .groups = "drop")

def_play_second_half_summary <- play_level_summary %>%
  filter(week_id > 8) %>%
  group_by(defense_1_displayName, defense_1_nflId) %>%
  summarize(n_plays = n(),
            second_total_yac = sum(yac, na.rm = TRUE),
            second_total_delta_epv = sum(epv_delta, na.rm = TRUE),
            second_ave_delta_epv = mean(epv_delta, na.rm = TRUE),
            second_ave_yac = mean(yac, na.rm = TRUE),
            .groups = "drop")


def_play_season_comp <- def_play_first_half_summary %>%
  dplyr::left_join(def_play_second_half_summary,
                   by = c("defense_1_displayName", "defense_1_nflId"))


def_play_season_comp %>%
  #filter(defense_1_nflId %in% players_focus) %>%
  #filter(first_n_plays >= 5, second_n_plays >= 5) %>%
  ggplot(aes(x = first_ave_delta_epv, y = second_ave_delta_epv)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_bw()
cor(def_play_season_comp$first_ave_delta_epv,
    def_play_season_comp$second_ave_delta_epv, use = "complete.obs")

def_play_season_comp %>%
  #filter(defense_1_nflId %in% players_focus) %>%
  #filter(first_n_plays >= 5, second_n_plays >= 5) %>%
  ggplot(aes(x = first_ave_yac, y = second_ave_yac)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_bw()
cor(def_play_season_comp$first_ave_yac,
    def_play_season_comp$second_ave_yac, use = "complete.obs")


# Team level summary ------------------------------------------------------

team_level_summary <- play_level_summary %>%
  group_by(def_team) %>%
  summarize(n_plays = n_distinct(game_play_id),
            total_yac = sum(yac, na.rm = TRUE),
            total_delta_epv = sum(epv_delta, na.rm = TRUE),
            ave_change_epv = total_delta_epv / n_plays,
            ave_yac = total_yac / n_plays,
            .groups = "drop")

team_level_summary %>%
  ggplot(aes(x = ave_change_epv, y = ave_yac)) +
  geom_text(aes(label = def_team)) +
  theme_bw()

pbp18 <- nflreadr::load_pbp(2018)

# average change
ave_team_plot <- pbp18 %>%
  filter(week <= 17, play_type == "pass", penalty == 0) %>%
  group_by(team_name = defteam) %>%
  summarise(avg_def_epa = mean(epa, na.rm = TRUE),
            total_def_epa = sum(epa, na.rm = TRUE)) %>%
  mutate(team_name = str_replace(team_name, "LV", "OAK")) %>%
  inner_join(team_level_summary, by = c("team_name" = "def_team")) %>%
  ggplot(aes(ave_change_epv, avg_def_epa)) +
  #geom_text(aes(label = team_name)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team_name), width = 0.08, alpha = 0.8) +
  labs(x = "Average delta EPV",
       y = "Average defensive EPA") +
  theme_light()

# total change
total_team_plot <- pbp18 %>%
  filter(week <= 17, play_type == "pass", penalty == 0) %>%
  group_by(team_name = defteam) %>%
  summarise(avg_def_epa = mean(epa, na.rm = TRUE),
            total_def_epa = sum(epa, na.rm = TRUE)) %>%
  mutate(team_name = str_replace(team_name, "LV", "OAK")) %>%
  inner_join(team_level_summary, by = c("team_name" = "def_team")) %>%
  ggplot(aes(total_delta_epv, total_def_epa)) +
  #geom_text(aes(label = team_name)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team_name), width = 0.08, alpha = 0.8) +
  labs(x = "Total delta EPV",
       y = "Total defensive EPA") +
  theme_light()

team_summary_plot <-
  plot_grid(total_team_plot, ave_team_plot,
            ncol = 2, align = "v")

save_plot("figs/team_summary.jpg", team_summary_plot,
          base_asp = 1, ncol = 2)
save_plot("figs/team_ave_summary.jpg", ave_team_plot,
          base_asp = 1.3, ncol = 1)

cor(team_level_summary$ave_change_epv, team_level_summary$ave_yac)
# [1] 0.3701862

