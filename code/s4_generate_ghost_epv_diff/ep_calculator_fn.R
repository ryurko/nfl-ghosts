# PURPOSE: Define a faster functon for computing expected points given new
#          ending yardline for receiver (just ignoring time)

library(nnet)

compute_ep <- function(ending_yardline, init_context, model_ep) {
  #
  # ending_yardline <- yac_pred_table$pred_bc_x
  # model_ep <- ep_model
  # init_context <- play_start
  # time_adj_sec <- 0

  new_context <- bind_rows(replicate(length(ending_yardline),
                                     init_context,
                                     simplify = FALSE)) %>%
    # Now proceed to update the context based on this new yardline:
    mutate(pred_yardline_100 = ending_yardline,
           # First determine if TD or safety occur:
           is_td = as.numeric(pred_yardline_100 <= 0),
           is_safety = as.numeric(pred_yardline_100 >= 100),
           # Now cap the yardline at 1 and 99 to handle everything else,
           # will then just use indicators to fix these:
           pred_yardline_100 = pmin(pmax(pred_yardline_100, 1), 99),
           convert_ind = as.numeric(pred_yardline_100 <= (yardline_100 - ydstogo)),
           new_ydstogo = ifelse(convert_ind == 1,
                                10, ydstogo - (yardline_100 - pred_yardline_100)),
           new_down = ifelse(convert_ind == 1, 1, down + 1),
           is_turnover = as.numeric(new_down == 5),
           new_down = ifelse(is_turnover == 1, 1, new_down),
           pred_yardline_100 = ifelse(is_turnover == 1, 100 - pred_yardline_100,
                                      pred_yardline_100),
           new_goal_to_go = ifelse(new_down == 1 & (pred_yardline_100 <= 10),
                                   1, goal_to_go),
           new_ydstogo = ifelse(new_goal_to_go == 1,
                                pred_yardline_100, new_ydstogo),
           new_ydstogo = ifelse(is_turnover == 1 & pred_yardline_100 >= 10,
                                10, new_ydstogo)) %>%
    dplyr::select(is_td, is_safety, is_turnover, pred_yardline_100,
                  new_ydstogo, new_down, new_goal_to_go,
                  half_seconds_remaining, two_minute_warning) %>%
    dplyr::rename(yardline_100 = pred_yardline_100,
                  down = new_down, ydstogo = new_ydstogo,
                  goal_to_go = new_goal_to_go) %>%
    mutate(log_ydstogo = log(ydstogo),
           down = factor(down, levels = c("1", "2", "3", "4")))

  as_tibble(predict(model_ep, newdata = new_context, type = "probs")) %>%
    mutate(ep = (0 * No_Score) + (3 * Field_Goal) + (-3 * Opp_Field_Goal) +
             (-2 * Opp_Safety) + (-7 * Opp_Touchdown) + (2 * Safety) +
             (7 * Touchdown)) %>%
    dplyr::select(ep) %>%
    bind_cols(dplyr::select(new_context, is_td, is_safety, is_turnover)) %>%
    mutate(ep = ifelse(is_turnover == 1, -ep, ep),
           ep = ifelse(is_td == 1, 7, ep),
           ep = ifelse(is_safety == 1, -2, ep)) %>%
    dplyr::pull(ep) %>%
    return()

}
