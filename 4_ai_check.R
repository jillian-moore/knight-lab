# AI CHECKER ----

# load packages ----
library(tidyverse)

# load data ----
load(here("data/ai_check_data.rda"))

# separate datasets of randomly assigned topics ----
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

# read in filled csv ----
jillian_new <- read_csv(here("ai_check/jillian_new.csv")) |> 
  rename(human_topic_tag = my_topic)
keya_new <- read_csv(here("ai_check/keya_new.csv")) |> 
  rename(human_topic_tag = "Keya topic designation")
eunice_new <- read_csv(here("ai_check/eunice_new.csv")) |> 
  rename(human_topic_tag = ...3)
sophia_new <- read_csv(here("ai_check/sophia_new.csv")) |> 
  rename(human_topic_tag = ...3)
melissa_new <- read_csv(here("ai_check/melissa_new.csv")) |> 
  rename(human_topic_tag = ...3)

# stack all new CSVs together ----
stacked_new <- bind_rows(jillian_new, keya_new, sophia_new, eunice_new, melissa_new) |>
  mutate(id = as.character(id))

# merge with original data by ID ----
merged <- stacked_new |>
  left_join(api_clean |> 
              mutate(id = as.character(id)) |>
              select(id, topic_tag, topic_confidence), 
            by = "id")

# normalize
merged <- merged |>
  mutate(
    confidence_zscore = (topic_confidence - mean(topic_confidence, na.rm = TRUE)) / 
      sd(topic_confidence, na.rm = TRUE)
  )

# functions ----
first_word  <- function(x) str_split(x, "\\s+", simplify = TRUE)[,1]
second_word <- function(x) str_split(x, "\\s+", simplify = TRUE)[,2]

normalize_topic <- function(x) {
  x |>
    tolower() |>
    str_replace_all("\\band\\b", "&") |>
    str_to_title() |>
    trimws()
}

# standardize ----
merged <- merged |>
  mutate(
    # normalize both
    human_topic_tag = normalize_topic(human_topic_tag),
    topic_tag_norm  = normalize_topic(topic_tag),
    
    # extract first words
    human_first = first_word(human_topic_tag),
    topic_first = first_word(topic_tag_norm),
    
    # if human only wrote the first word, replace with full topic_tag_norm
    human_topic_tag = if_else(
      human_first == topic_first &
        str_count(human_topic_tag, "\\S+") == 1,
      topic_tag_norm,
      human_topic_tag
    ),
    
    # final match check
    match = if_else(human_topic_tag == topic_tag_norm, 1, 0)
  )

# add match column (1 if topics match, 0 if not)
merged <- merged |>
  mutate(match = if_else(
    tolower(trimws(human_topic_tag)) == tolower(trimws(topic_tag)),
    1,
    0
  ))
  
# calculate match rate (proportion of matches)
match_rate <- mean(merged$match, na.rm = TRUE)

# calculate relevant facts between match and ai_topic_confidence
summary(glm(match ~ confidence_zscore, data = merged, family = binomial))

# summary ----
cat("Match rate:", round(match_rate * 100, 2), "%\n")
cat("Total matches:", sum(merged$match, na.rm = TRUE), "\n")
cat("Total rows:", nrow(merged), "\n")
cat("Correlation (match vs ai_topic_confidence):", round(correlation, 3), "\n")

# helpful visualization ----
ggplot(merged, aes(x = as.factor(match), y = topic_confidence, fill = as.factor(match))) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(alpha = 0.5) +
  scale_fill_manual(values = c("0" = "#e74c3c", "1" = "#2ecc71"),
                    labels = c("No Match", "Match")) +
  labs(
    title = "AI Confidence by Match Status",
    x = "Match with Human Tag",
    y = "AI Topic Confidence",
    fill = "Result"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")