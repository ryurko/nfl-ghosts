
# data prep ---------------------------------------------------------------

library(tidyverse)
theme_set(theme_light())
play_summary_table <- read_csv("play_summary_table.csv")
plays <- read_csv("data/plays.csv")
players <- read_csv("data/players.csv")
games <- read_csv("data/games.csv")

game_home_away <- games |> 
  select(gameId, home = homeTeamAbbr, away = visitorTeamAbbr) |> 
  pivot_longer(!gameId, names_to = "team", values_to = "team_name")

tracking <- here::here("data") |> 
  list.files() |> 
  str_subset("week") |> 
  str_c("data/", `...` = _) |> 
  map(read_csv) |> 
  list_rbind()

tracking <- tracking |> 
  left_join(game_home_away)

player_team <- tracking |> 
  distinct(gameId, playId, nflId, team_name) |> 
  transmute(game_play_id = str_c(gameId, playId, sep = "_"),
            defense_1_nflId = nflId,
            team_name)

pbp18 <- nflfastR::load_pbp(2018)
yac18 <- pbp18 |>
  transmute(game_play_id = str_c(old_game_id, play_id, sep = "_"),
            yac = yards_after_catch)


# player-level corr -------------------------------------------------------

play_level_summary <- play_summary_table |> 
  left_join(yac18)

def_play_summary <- play_level_summary |>
  group_by(defense_1_displayName, defense_1_nflId) |>
  summarize(n_plays = n(),
            total_yac = sum(yac, na.rm = TRUE),
            total_change_epv = sum(change_epv, na.rm = TRUE),
            ave_change_epv = mean(change_epv, na.rm = TRUE),
            ave_yac = mean(yac, na.rm = TRUE),
            .groups = "drop") |>
  dplyr::left_join(dplyr::select(players, nflId, position),
                   by = c("defense_1_nflId" = "nflId"))

players_focus <- def_play_summary |>
  filter(n_plays >= 10) |>
  pull(defense_1_nflId)

def_play_summary |>
  filter(defense_1_nflId %in% players_focus) |> 
  filter(position != "DE") |> 
  mutate(pos_group = case_when(
    position %in% c("DT", "NT", "DE") ~ "Defensive line",
    position %in% c("MLB", "OLB", "ILB", "LB") ~ "Linebackers",
    position %in% c("DB", "SS", "FS", "CB", "S") ~ "Defensive backs"
  )) |> 
  ggplot(aes(x = ave_change_epv, y = ave_yac)) +
  # geom_smooth(method = "lm", size = 1.5, color = "black", alpha = 0.5, se = FALSE) +
  geom_point(aes(color = pos_group), alpha = 0.7, size = 2) +
  scale_color_manual(values = c("#1E88E5", "#FFC107")) +
  labs(
    x = "Average Change in EPV",
    y = "Average YAC Allowed",
    color = "Position",
    title = "Average YAC Allowed vs. Average Change in EPV",
    subtitle = "For defenders facing at least 10 receptions"
  ) +
  theme(plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 10),
        axis.title = element_text(size = 10.5),
        axis.text = element_text(size = 9),
        legend.margin = unit(-2, "cm"),
        legend.box.margin = margin(0, 0, -1, -1),
        legend.text = element_text(size = 9.5),
        legend.title = element_text(size = 10),
        legend.position = "none",
        panel.grid.minor.x = element_blank()) 

# team-level corr ---------------------------------------------------------

team_level_summary <- play_level_summary |> 
  left_join(player_team) |> 
  group_by(team_name) |>
  summarize(n_plays = n_distinct(game_play_id),
            total_yac = sum(yac, na.rm = TRUE),
            total_change_epv = sum(change_epv, na.rm = TRUE),
            ave_change_epv = total_change_epv / n_plays,
            ave_yac = total_yac / n_plays,
            .groups = "drop")


pbp18 <- nflfastR::load_pbp(2018)

# average change
pbp18 |>
  filter(week <= 17, play_type == "pass", penalty == 0) |> 
  group_by(team_name = defteam) |>
  summarise(avg_def_epa = mean(epa, na.rm = TRUE),
            total_def_epa = sum(epa, na.rm = TRUE)) |>
  mutate(team_name = str_replace(team_name, "LV", "OAK")) |> 
  right_join(team_level_summary) |>
  ggplot(aes(ave_change_epv, avg_def_epa)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team_name), width = 0.08, alpha = 0.8) +
  labs(x = "Average change in EPV",
       y = "Average defensive EPA")

# total change
pbp18 |>
  filter(week <= 17, play_type == "pass", penalty == 0) |> 
  group_by(team_name = defteam) |>
  summarise(avg_def_epa = mean(epa, na.rm = TRUE),
            total_def_epa = sum(epa, na.rm = TRUE)) |>
  mutate(team_name = str_replace(team_name, "LV", "OAK")) |> 
  right_join(team_level_summary) |>
  ggplot(aes(total_change_epv, total_def_epa)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team_name), width = 0.08, alpha = 0.8) +
  labs(x = "Total change in EPV",
       y = "Total defensive EPA")
