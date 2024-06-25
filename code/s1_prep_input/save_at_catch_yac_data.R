# PURPOSE: Save model dataset to load into python

library(tidyverse)

model_data <-
  read_rds("data/model_datasets/at_catch_yac_model_data.rds")

write_csv(model_data,
          "data/model_datasets/at_catch_yac_model_data.csv.gz")