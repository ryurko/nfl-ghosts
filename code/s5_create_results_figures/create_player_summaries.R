# PURPOSE: Explore rankings of players and teams based on play level results

library(tidyverse)


# Read in the play summary table ------------------------------------------

play_level_summary <- read_csv("data/ghost_ep_values/play_level_summary/play_summary_table.csv")

players_data <- read_csv("data/nfl-big-data-bowl-2021/players.csv")

team_rosters <- nflreadr::load_rosters(2018)

team_rosters <- team_rosters %>%
  dplyr::select(teamPlayers.nflId, team.abbr) %>%
  rename(nfl_id = teamPlayers.nflId, team_abbr = team.abbr) %>%
  filter(nfl_id %in% ghost_yac_distr_summary$nfl_id)

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
            total_change_epv = sum(change_epv, na.rm = TRUE),
            ave_change_epv = mean(change_epv, na.rm = TRUE),
            ave_yac = mean(yac, na.rm = TRUE),
            .groups = "drop") %>%
  dplyr::left_join(dplyr::select(players_data, nflId, position),
                   by = c("defense_1_nflId" = "nflId"))

def_play_summary %>%
  ggplot(aes(x = total_change_epv, y = total_yac)) +
  geom_point(alpha = 0.5) +
  theme_bw()

# Create table of top 10 players:
library(gt)

def_play_summary %>%
  arrange(total_change_epv) %>%
  dplyr::slice(1:10) %>%
  dplyr::select(defense_1_displayName, position, n_plays,
                total_change_epv, total_yac, ave_change_epv, ave_yac) %>%
  mutate(total_change_epv = round(total_change_epv, digits = 2),
         total_yac = round(total_yac, digits = 2),
         ave_change_epv = round(ave_change_epv, digits = 2),
         ave_yac = round(ave_yac, digits = 2)) %>%
  rename(Player = defense_1_displayName,
         Position = position,
         `#receptions faced` = n_plays,
         `Sum change EPV` = total_change_epv,
         `Total YAC allowed` = total_yac,
         `Average change EPV` = ave_change_epv,
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
  data_color(columns = vars(`Sum change EPV`,
                            `Total YAC allowed`,
                            `Average change EPV`,
                            `Average YAC allowed`),
             colors = scales::col_numeric(
               palette = c("darkorange", "darkblue"),
               domain = NULL
             ))




players_focus <- def_play_summary %>%
  filter(n_plays > 10) %>%
  pull(defense_1_nflId)

# Split the season in half:
def_play_first_half_summary <- play_level_summary %>%
  filter(week_id <= 8) %>%
  group_by(defense_1_displayName, defense_1_nflId) %>%
  summarize(first_n_plays = n(),
            first_total_yac = sum(yac, na.rm = TRUE),
            first_total_change_epv = sum(change_epv, na.rm = TRUE),
            first_ave_change_epv = mean(change_epv, na.rm = TRUE),
            first_ave_yac = mean(yac, na.rm = TRUE),
            .groups = "drop")

def_play_second_half_summary <- play_level_summary %>%
  filter(week_id > 8) %>%
  group_by(defense_1_displayName, defense_1_nflId) %>%
  summarize(n_plays = n(),
            second_total_yac = sum(yac, na.rm = TRUE),
            second_total_change_epv = sum(change_epv, na.rm = TRUE),
            second_ave_change_epv = mean(change_epv, na.rm = TRUE),
            second_ave_yac = mean(yac, na.rm = TRUE),
            .groups = "drop")


def_play_season_comp <- def_play_first_half_summary %>%
  dplyr::left_join(def_play_second_half_summary,
                   by = c("defense_1_displayName", "defense_1_nflId"))


def_play_season_comp %>%
  #filter(defense_1_nflId %in% players_focus) %>%
  #filter(first_n_plays >= 5, second_n_plays >= 5) %>%
  ggplot(aes(x = first_ave_change_epv, y = second_ave_change_epv)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_bw()
cor(def_play_season_comp$first_ave_change_epv,
    def_play_season_comp$second_ave_change_epv, use = "complete.obs")

def_play_season_comp %>%
  #filter(defense_1_nflId %in% players_focus) %>%
  #filter(first_n_plays >= 5, second_n_plays >= 5) %>%
  ggplot(aes(x = first_ave_yac, y = second_ave_yac)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_bw()
cor(def_play_season_comp$first_ave_yac,
    def_play_season_comp$second_ave_yac, use = "complete.obs")

