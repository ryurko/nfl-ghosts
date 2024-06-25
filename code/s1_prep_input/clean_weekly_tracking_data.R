# PURPOSE: Clean weekly raw tracking data files to flip for play direction,
#          and only consider complete passes. This file also joins play and player
#          position information over for use later.

library(tidyverse)

# Load non-tracking input -------------------------------------------------

games_data <- read_csv("data/nfl-big-data-bowl-2021/games.csv")
plays_data <- read_csv("data/nfl-big-data-bowl-2021/plays.csv")
players_data <- read_csv("data/nfl-big-data-bowl-2021/players.csv")
targeted_receiver_data <- read_csv("data/nfl-big-data-bowl-2021/targetedReceiver.csv")

# Filter play-level data to just complete passes --------------------------

table(plays_data$passResult)
#     C     I    IN     R     S
# 11370  6135   420     4  1308
any(is.na(plays_data$passResult))
# [1] TRUE

# Will only keep the C plays in this dataset

# Proceed to process tracking data in weekly fashion ----------------------

walk(1:length(list.files("data/nfl-big-data-bowl-2021/weekly_raw_tracking")),
     function(week_i) {

       # First Load in the week's data and flip the x,y and angular variables
       # based on play direction:
       week_raw_data <- read_csv(paste0("data/nfl-big-data-bowl-2021/weekly_raw_tracking/week",
                                        week_i, ".csv")) %>%
         # Flip positional values
         mutate(x = ifelse(playDirection == "left", 120 - x, x),
                y = ifelse(playDirection == "left", 160 / 3 - y, y),
                # Flip directional information
                dir = ifelse(playDirection == "left", dir + 180, dir),
                dir = ifelse(dir > 360, dir - 360, dir),
                o = ifelse(playDirection == "left", o + 180, o),
                o = ifelse(o > 360, o - 360, o))

       # Join the play info and only grab the complete pass plays:
       complete_pass_data <- week_raw_data %>%
         left_join(plays_data,
                   by = c("gameId", "playId")) %>%
         filter(!is.na(passResult), passResult == "C",
                # Drop penalty plays:
                is.na(penaltyCodes)) %>%
         # Join game info:
         left_join(games_data, by = "gameId") %>%
         # Join target receiver info:
         left_join(targeted_receiver_data,
                   by = c("gameId", "playId")) %>%
         # Make target receiver indicator variable:
         mutate(is_target = as.numeric(nflId == targetNflId),
                # Side of ball the player is on:
                side_of_ball = ifelse(((team == "home") & (possessionTeam == homeTeamAbbr)) |
                                        ((team == "away") & (possessionTeam == visitorTeamAbbr)),
                                      "offense", "defense"),
                # Fix side of ball for football:
                side_of_ball = ifelse(displayName == "Football", "football", side_of_ball))


       # Save weekly complete pass tracking data
       write_rds(complete_pass_data,
                 paste0("data/weekly_complete_pass_tracking/week",
                        week_i, ".rds"), compress = "gz")

     })







