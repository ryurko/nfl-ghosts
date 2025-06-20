# PURPOSE: Examine the YAC sensitivity analysis to number of training weeks

library(tidyverse)

# Load the holdout results ------------------------------------------------

holdout_results <-
  read_csv( "data/yac_sensitivity/rfcde_summary.csv")

# Display the test results ------------------------------------------------

yac_metric_plot <- holdout_results %>%
  pivot_longer(cde_loss_val:cde_test_mode_rmse,
               names_to = "metric", values_to = "value") %>%
  mutate(metric = fct_recode(metric,
                             `RMSE with mean` = "cde_test_mean_rmse",
                             `RMSE with mode` = "cde_test_mode_rmse",
                             `CDE loss` = "cde_loss_val")) %>%
  ggplot(aes(x = n_train_weeks)) +
  # geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
  #               color = "black") +
  geom_point(aes(y = value), size = 2) +
  labs(x = "Number of weeks for training YAC RFCDE",
       y = "Test week metric value") +
  facet_wrap(~metric, ncol = 1, scales = "free_y") +
  theme_bw() +
  theme(strip.background = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10, angle = 30),
        strip.text = element_text(size = 14))

cowplot::save_plot("figures/suppl_yac_week_metrics.jpeg",
                   yac_metric_plot, ncol = 1, nrow = 3)

