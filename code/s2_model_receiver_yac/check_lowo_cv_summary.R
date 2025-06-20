# PURPOSE: Create a figure visualizing the LOWO-CV RFCDE YAC performance

library(tidyverse)
library(cowplot)

# Read in the RFCDE summary from python -----------------------------------

lowo_summary <-
  read_csv("data/yac_model_output/lowo_validation/rfcde_feature_comparison_revised.csv")

# Display the test results ------------------------------------------------

yac_metric_plot <- lowo_summary %>%
  pivot_longer(cols = cde_loss_val:cde_test_mode_rmse,
               names_to = "metric", values_to = "value") %>%
  mutate(features = fct_relevel(features, "bc_qb_only",
                                "bc_qb_def1", "bc_qb_def1_off1",
                                "bc_qb_def12", "bc_qb_def12_off12"),
         features = fct_recode(features,
                               `qb,rec` = "bc_qb_only",
                               `qb,rec,def1` = "bc_qb_def1",
                               `qb,rec,def1,off1` = "bc_qb_def1_off1",
                               `qb,rec,def1,def2` = "bc_qb_def12",
                               `qb,rec,def1,off1,def2,off2` = "bc_qb_def12_off12"),
         metric = fct_recode(metric,
                             `RMSE with mean` = "cde_test_mean_rmse",
                             `RMSE with mode` = "cde_test_mode_rmse",
                             `CDE loss` = "cde_loss_val")) %>%
  group_by(features, metric) %>%
  summarize(ave_val = mean(value),
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

save_plot("figures/suppl_yac_lowocv.jpeg",
          yac_metric_plot, ncol = 1, nrow = 3)


