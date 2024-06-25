# PURPOSE: Construct datasets for all receptions with information about
#          the QB at release with info about where the receiver is relative
#          to the QB at this moment. Information about the other players on the
#          field is already stored and created across all frames in the script
#          s0_preprocess_input/create_weekly_yac_bc_features.R

# Access necessary packages -----------------------------------------------

library(tidyverse)

# Construct dataset for each week -----------------------------------------

# Walk through each week, load the week's respective file then save the
# processed version of the dataset with player information with respect to
# the ball carrier just at the moment of throw:

walk(1:length(list.files("data/weekly_complete_pass_tracking")),
     function(week_i) {

       week_data <-
         read_rds(paste0("data/weekly_complete_pass_tracking/week",
                         week_i, ".rds"))

       # Event for pass release: pass_forward

       # Get the QB position at the point of passing:
       qb_data <- week_data %>%
         filter(position == "QB", event == "pass_forward") %>%
         distinct() %>%
         group_by(gameId, playId) %>%
         arrange(frameId) %>%
         # Only keep the first instance...
         slice(1) %>%
         ungroup()

       # In case there are multiple QBs - find the one closest to the football
       # at the time of release:
       football_data <- week_data %>%
         filter(displayName == "Football", event == "pass_forward") %>%
         distinct() %>%
         dplyr::select(gameId, playId, frameId, x, y) %>%
         dplyr::rename(ball_x = x, ball_y = y) %>%
         distinct()

       # Join to the QB data, then only keep the QB closest to the ball:
       qb_data <- qb_data %>%
         dplyr::left_join(football_data, by = c("gameId", "playId", "frameId")) %>%
         mutate(dist_to_ball = sqrt((x - ball_x)^2 + (y - ball_y)^2)) %>%
         group_by(gameId, playId, frameId) %>%
         arrange(dist_to_ball) %>%
         slice(1) %>%
         ungroup() %>%
         dplyr::select(-ball_x, -ball_y, -dist_to_ball)

       # Get the receiver info at the release point:
       rec_data <- week_data %>%
         filter(is_target == 1, event == "pass_forward") %>%
         distinct() %>%
         group_by(gameId, playId) %>%
         arrange(frameId) %>%
         # Only keep the first instance...
         slice(1) %>%
         ungroup()

       # Convert the dataset with all of the information above into a more concise
       # dataset with just the necessary ball carrier info:
       rec_info <- rec_data %>%
         dplyr::select(gameId, playId, frameId, nflId,
                       event, displayName, position,
                       x, y) %>%
         # Rename columns:
         dplyr::rename(bc_nflId = nflId, bc_displayName = displayName, bc_position = position,
                       bc_x = x, bc_y = y) %>%
         dplyr::mutate(adj_bc_x = 110 - bc_x,
                       adj_bc_y = bc_y - (160 / 6))

       # Now create a dataset with all of the various QB info:
       qb_info <- qb_data %>%
         dplyr::select(gameId, playId, frameId, nflId, displayName,
                       x, y, s, a, dis, o, dir, playDirection) %>%
         # Rename columns:
         dplyr::rename(qb_nflId = nflId, qb_displayName = displayName,
                       qb_x = x, qb_y = y, qb_s = s, qb_a = a, qb_dis = dis,
                       qb_o = o, qb_dir = dir) %>%
         dplyr::mutate(adj_qb_x = 110 - qb_x,
                       adj_qb_y = qb_y - (160 / 6),
                       qb_dir_target_endzone =
                         case_when(
                           (qb_dir < 270) ~ 90 - qb_dir,
                           (qb_dir >= 270) ~ 450 - qb_dir,
                           TRUE ~ NA_real_),
                       qb_o_target_endzone =
                         case_when(
                           (qb_o < 270) ~ 90 - qb_o,
                           (qb_o >= 270) ~ 450 - qb_o,
                           TRUE ~ NA_real_)) %>%
         dplyr::select(-playDirection)


       # Join receiver data and QB info together:
       rec_info %>%
         left_join(qb_info, by = c("gameId", "playId", "frameId")) %>%
         # Compute distance to QB in two ways - change in x,y and then actual distance:
         mutate(qb_adj_x_change = adj_bc_x - adj_qb_x,
                qb_adj_y_change = adj_bc_y - adj_qb_y,
                qb_dist_to_bc = sqrt((bc_x - qb_x)^2 + (bc_y - qb_y)^2),
                # And now angle based features:
                # Compute the angle between the ball-carrier and the QB:
                qb_angle_with_bc = (atan2(qb_adj_y_change, -qb_adj_x_change) * 180) / pi,
                # Now compute the direction with respect to the ball-carrier as
                # simply the absolute minimum difference - this will be the minimum
                # across a few possible scenarios to deal 0 to 360 limits
                qb_dir_wrt_bc_diff = pmin(
                  pmin(abs(qb_angle_with_bc - qb_dir_target_endzone),
                       abs(qb_angle_with_bc - (qb_dir_target_endzone - 360))),
                  abs(qb_angle_with_bc - (qb_dir_target_endzone + 360))),
                qb_o_wrt_bc_diff = pmin(
                  pmin(abs(qb_angle_with_bc - qb_o_target_endzone),
                       abs(qb_angle_with_bc - (qb_o_target_endzone - 360))),
                  abs(qb_angle_with_bc - (qb_o_target_endzone + 360)))) %>%
         # drop the redundant receiver features stored elsewhere:
         dplyr::select(-c(bc_nflId:adj_bc_y)) %>%
         write_rds(paste0("data/weekly_qb_at_release_features/week",
                          week_i, ".rds"), compress = "gz")

     })
