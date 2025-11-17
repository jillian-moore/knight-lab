# AI CHECKER

library(tidyverse)

# load data ----
load(here("data/ai_check_data.rda"))

# separate datasets of randomly assigned topics
eunice <- api_clean |> slice_sample(n = 10) |> 
  select(id, content.rendered)
keya <- api_clean |> slice_sample(n = 10) |> 
  select(id, content.rendered)
jillian <- api_clean |> slice_sample(n = 10) |> 
  select(id, content.rendered)
melissa <- api_clean |> slice_sample(n = 10) |> 
  select(id, content.rendered)
sophia <- api_clean |> slice_sample(n = 10) |> 
  select(id, content.rendered)

# write out blank csv
write_csv(eunice, here("ai_check/eunice.csv"))
write_csv(keya, here("ai_check/keya.csv"))
write_csv(jillian, here("ai_check/jillian.csv"))
write_csv(melissa, here("ai_check/melissa.csv"))
write_csv(sophia, here("ai_check/sophia.csv"))

# read in filled csv
jillian_new <- read_csv(here("ai_check/jillian_new.csv"))
keya_new <- read_csv(here("ai_check/keya_new.csv"))