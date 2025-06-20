# PURPOSE: Examine the 2D ghosting sensitivity analysis to number of training weeks

library(tidyverse)

# Load the holdout results ------------------------------------------------

holdout_results <-
  read_csv( "data/ghost_2d_sensitivity/rfcde_summary.csv")

# Display the test results ------------------------------------------------

ghost_metric_plot <- holdout_results %>%
  pivot_longer(cross_entropy:dist_cde_mean,
               names_to = "metric", values_to = "value") %>%
  mutate(metric = fct_recode(metric,
                             `Distance with mean` = "dist_cde_mean",
                             `Distance with mode` = "dist_cde_mode",
                             `Cross entropy` = "cross_entropy")) %>%
  group_by(n_train_weeks, metric) %>%
  summarize(n_plays = n(),
            ave_val = mean(value),
            se_val = sd(value) / sqrt(n()),
            .groups = "drop") %>%
  mutate(lower_bound = ave_val - se_val,
         upper_bound = ave_val + se_val) %>%
  ggplot(aes(x = n_train_weeks)) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
                color = "black") +
  geom_point(aes(y = ave_val), size = 2) +
  labs(x = "Number of weeks for training 2D RFCDE",
       y = "Test week metric value") +
  facet_wrap(~metric, ncol = 1, scales = "free_y") +
  theme_bw() +
  theme(strip.background = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10, angle = 30),
        strip.text = element_text(size = 14))

cowplot::save_plot("figures/suppl_ghost_week_metrics.jpeg",
                   ghost_metric_plot, ncol = 1, nrow = 3)

