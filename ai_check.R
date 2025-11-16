# AI CHECKER

library(tidyverse)

# load data ----
load(here("data/full_data.rda"))

# four datasets of randomly assigned topics
eunice <- full_data |> slice_sample(n = 10) |> 
  select(-topic_tag)
keya <- full_data |> slice_sample(n = 10) |> 
  select(-topic_tag)
jillian <- full_data |> slice_sample(n = 10) |> 
  select(-topic_tag)
melissa <- full_data |> slice_sample(n = 10) |> 
  select(-topic_tag)
sophia <- full_data |> slice_sample(n = 10) |> 
  select(-topic_tag)

write_csv(eunice, here("ai_check/eunice.csv"))
write_csv(keya, here("ai_check/keya.csv"))
write_csv(jillian, here("ai_check/jillian.csv"))
write_csv(melissa, here("ai_check/melissa.csv"))
write_csv(sophia, here("ai_check/sophia.csv"))
