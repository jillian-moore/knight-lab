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
eunice_new <- read_csv(here("ai_check/eunice_new.csv"))
sophia_new <- read_csv(here("ai_check/sophia_new.csv"))
melissa_new <- read_csv(here("ai_check/melissa_new.csv"))

# Function to stack CSVs, merge with original data, add match column, and calculate correlation
merge_and_check_topics <- function(new_csv_paths, original_data, id_col = "id", 
                                   new_topic_col = "new_topic", orig_topic_col = "topic") {
  
stacked_new <- map_dfr(new_csv_paths, read_csv)
  
# merge with original data by ID
merged <- stacked_new |>
  left_join(ai_check_data |> select(all_of(c(id, topic_tag))),
            by = id)
  
# Add match column (1 if topics match, 0 if not)
merged <- merged |>
  mutate(match = if_else(
    tolower(trimws(!!sym(new_topic_col))) == tolower(trimws(!!sym(orig_topic_col))),
    1,
    0
    ))
  
  # Calculate correlation
  # Remove any NAs for correlation calculation
  valid_data <- merged |>
    filter(!is.na(!!sym(new_topic_col)), 
           !is.na(!!sym(orig_topic_col)),
           !is.na(match))
  
  # Calculate match rate (proportion of matches)
  match_rate <- mean(valid_data$match, na.rm = TRUE)
  
  # Print summary statistics
  cat("Summary Statistics:\n")
  cat("------------------\n")
  cat("Total rows:", nrow(merged), "\n")
  cat("Valid rows for correlation:", nrow(valid_data), "\n")
  cat("Matches:", sum(valid_data$match), "\n")
  cat("Mismatches:", sum(valid_data$match == 0), "\n")
  cat("Match rate (correlation):", round(match_rate * 100, 2), "%\n\n")
  
  # Return merged dataset with match column
  return(merged)
}

# Example usage:
# new_files <- c(
#   here("ai_check/jillian_new.csv"),
#   here("ai_check/keya_new.csv"),
#   here("ai_check/eunice_new.csv"),
#   here("ai_check/sophia_new.csv"),
#   here("ai_check/melissa_new.csv")
# )
# 
# result <- merge_and_check_topics(new_files, api_clean)
# 
# # View results
# result |> select(id, new_topic, topic, match) |> head()
# 
# # Write out if needed
# write_csv(result, here("ai_check/merged_with_match.csv"))

left_join()