# PURPOSE: Examine the LOWO CV results for ghosting location of closest defender
library(tidyverse)

# Load the LOWO CV results ------------------------------------------------

lowo_cv_output <-
  read_csv("data/ghost_location_model_output/lowo_validation/rfcde_summary.csv")

# Display the test results ------------------------------------------------

ghost_metric_plot <- lowo_cv_output %>%
  pivot_longer(cross_entropy:dist_cde_mean,
               names_to = "metric", values_to = "value") %>%
  mutate(features = fct_relevel(features, "bc_qb_only",
                                "bc_qb_def2", "bc_qb_def2_off1",
                                "def2_off1"),
         features = fct_recode(features,
                               `qb,rec` = "bc_qb_only",
                               `qb,rec,def2` = "bc_qb_def2",
                               `qb,rec,def2,off1` = "bc_qb_def2_off1",
                               `def2,off2` = "def2_off1"),
         metric = fct_recode(metric,
                             `Distance with mean` = "dist_cde_mean",
                             `Distance with mode` = "dist_cde_mode",
                             `Cross entropy` = "cross_entropy")) %>%
  group_by(features, metric) %>%
  summarize(n_plays = n(),
            ave_val = mean(value),
            se_val = sd(value) / sqrt(n()),
            .groups = "drop") %>%
  mutate(lower_bound = ave_val - se_val,
         upper_bound = ave_val + se_val) %>%
  ggplot(aes(x = features)) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
                color = "black") +
  geom_point(aes(y = ave_val), size = 2) +
  labs(x = "Player features",
       y = "LOWO CV metric value") +
  facet_wrap(~metric, ncol = 1, scales = "free_y") +
  theme_bw() +
  theme(strip.background = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10, angle = 30),
        strip.text = element_text(size = 14))

cowplot::save_plot("figs/suppl_ghost_lowocv.jpeg",
                   ghost_metric_plot, ncol = 1, nrow = 3)


