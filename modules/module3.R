# DATA QUALITY CHECKS 

# load packages ----
library(here)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)
library(purrr)

# load data ----
load(here("data/full_data.rda"))

# create output directory
dir.create(here("output/quality_checks"), recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 1. EVENT TAG CONFIRMATION ----
# Average accuracy by each topic of the AI, in percent
# =============================================================================

cat("\n📊 TOPIC DISTRIBUTION ANALYSIS\n")
cat("=", rep("=", 50), "\n", sep = "")

# Count articles by topic
topic_counts <- article_data %>%
  count(topic_match, name = "article_count") %>%
  arrange(desc(article_count)) %>%
  mutate(
    percentage = (article_count / sum(article_count)) * 100,
    percentage_label = paste0(round(percentage, 1), "%")
  )

print(topic_counts)

# Plot topic distribution
p1 <- ggplot(topic_counts, aes(x = reorder(topic_match, article_count), y = article_count)) +
  geom_col(fill = "#667eea", alpha = 0.8) +
  geom_text(aes(label = paste0(article_count, "\n(", percentage_label, ")")), 
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(
    title = "Article Distribution by Topic",
    subtitle = paste0("Total Articles: ", format(sum(topic_counts$article_count), big.mark = ",")),
    x = NULL,
    y = "Number of Articles"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.text = element_text(size = 11)
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15)))

ggsave(here("output/quality_checks/01_topic_distribution.png"), p1, 
       width = 10, height = 8, dpi = 300)

cat("\n✅ Topic distribution plot saved to output/quality_checks/01_topic_distribution.png\n")

# =============================================================================
# 2. CENSUS DATA CHECK ----
# Total people by community and each demographic
# =============================================================================

cat("\n\n👥 CENSUS DEMOGRAPHICS ANALYSIS\n")
cat("=" , rep("=", 50), "\n", sep = "")

# Get total population by community
census_summary <- chi_boundaries_sf %>%
  st_drop_geometry() %>%
  select(community, total_population, 
         white, black_or_african_american, asian, hispanic_or_latino,
         age_0_17, age_18_24, age_25_34, age_35_49, age_50_64, age_65_plus,
         under_25_000, x25_000_to_49_999, x50_000_to_74_999, 
         x75_000_to_125_000, x125_000) %>%
  arrange(desc(total_population))

# Summary statistics
cat("\nPopulation Summary:\n")
cat("Total Population:", format(sum(census_summary$total_population, na.rm = TRUE), big.mark = ","), "\n")
cat("Communities:", nrow(census_summary), "\n")
cat("Avg Population per Community:", 
    format(round(mean(census_summary$total_population, na.rm = TRUE)), big.mark = ","), "\n")
cat("Median Population:", 
    format(round(median(census_summary$total_population, na.rm = TRUE)), big.mark = ","), "\n")

# Top 10 communities by population
cat("\nTop 10 Communities by Population:\n")
print(census_summary %>% 
        select(community, total_population) %>% 
        head(10))

# Race/Ethnicity breakdown
race_totals <- census_summary %>%
  summarise(
    White = sum(white, na.rm = TRUE),
    Black = sum(black_or_african_american, na.rm = TRUE),
    Asian = sum(asian, na.rm = TRUE),
    Hispanic = sum(hispanic_or_latino, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "race_ethnicity", values_to = "population") %>%
  mutate(
    percentage = (population / sum(population)) * 100,
    percentage_label = paste0(round(percentage, 1), "%")
  )

cat("\nRace/Ethnicity Distribution:\n")
print(race_totals)

# Plot race/ethnicity
p2 <- ggplot(race_totals, aes(x = reorder(race_ethnicity, population), y = population)) +
  geom_col(aes(fill = race_ethnicity), show.legend = FALSE) +
  geom_text(aes(label = paste0(comma(population), "\n(", percentage_label, ")")), 
            hjust = -0.1, size = 4) +
  coord_flip() +
  scale_fill_manual(values = c("#e74c3c", "#3498db", "#2ecc71", "#f39c12")) +
  labs(
    title = "Chicago Population by Race/Ethnicity",
    subtitle = "Based on ACS 2020-2024 Census Data",
    x = NULL,
    y = "Population"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.text = element_text(size = 11)
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15)))

ggsave(here("output/quality_checks/02_race_ethnicity.png"), p2, 
       width = 10, height = 6, dpi = 300)

# Age distribution
age_totals <- census_summary %>%
  summarise(
    `0-17` = sum(age_0_17, na.rm = TRUE),
    `18-24` = sum(age_18_24, na.rm = TRUE),
    `25-34` = sum(age_25_34, na.rm = TRUE),
    `35-49` = sum(age_35_49, na.rm = TRUE),
    `50-64` = sum(age_50_64, na.rm = TRUE),
    `65+` = sum(age_65_plus, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "age_group", values_to = "population") %>%
  mutate(
    percentage = (population / sum(population)) * 100,
    percentage_label = paste0(round(percentage, 1), "%")
  )

cat("\nAge Distribution:\n")
print(age_totals)

# Plot age distribution
p3 <- ggplot(age_totals, aes(x = age_group, y = population)) +
  geom_col(fill = "#9b59b6", alpha = 0.8) +
  geom_text(aes(label = paste0(comma(population), "\n(", percentage_label, ")")), 
            vjust = -0.5, size = 3.5) +
  labs(
    title = "Chicago Population by Age Group",
    subtitle = "Based on ACS 2020-2024 Census Data",
    x = "Age Group",
    y = "Population"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.text = element_text(size = 11)
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15)))

ggsave(here("output/quality_checks/03_age_distribution.png"), p3, 
       width = 10, height = 6, dpi = 300)

# Income distribution
income_totals <- census_summary %>%
  summarise(
    `Under $25k` = sum(under_25_000, na.rm = TRUE),
    `$25k-$50k` = sum(x25_000_to_49_999, na.rm = TRUE),
    `$50k-$75k` = sum(x50_000_to_74_999, na.rm = TRUE),
    `$75k-$125k` = sum(x75_000_to_125_000, na.rm = TRUE),
    `$125k+` = sum(x125_000, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "income_bracket", values_to = "households") %>%
  mutate(
    percentage = (households / sum(households)) * 100,
    percentage_label = paste0(round(percentage, 1), "%"),
    income_bracket = factor(income_bracket, 
                            levels = c("Under $25k", "$25k-$50k", "$50k-$75k", 
                                       "$75k-$125k", "$125k+"))
  )

cat("\nIncome Distribution:\n")
print(income_totals)

# Plot income distribution
p4 <- ggplot(income_totals, aes(x = income_bracket, y = households)) +
  geom_col(fill = "#16a085", alpha = 0.8) +
  geom_text(aes(label = paste0(comma(households), "\n(", percentage_label, ")")), 
            vjust = -0.5, size = 3.5) +
  labs(
    title = "Chicago Households by Income Bracket",
    subtitle = "Based on ACS 2020-2024 Census Data",
    x = "Income Bracket",
    y = "Number of Households"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.text.x = element_text(size = 10, angle = 15, hjust = 1)
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15)))

ggsave(here("output/quality_checks/04_income_distribution.png"), p4, 
       width = 10, height = 6, dpi = 300)

cat("\n✅ Census plots saved to output/quality_checks/\n")

# =============================================================================
# 3. API DATA CHECK ----
# Total articles per year
# =============================================================================

cat("\n\n📰 ARTICLE PUBLICATION TRENDS\n")
cat("=", rep("=", 50), "\n", sep = "")

# Articles by year
articles_by_year <- article_data %>%
  mutate(year = year(article_date)) %>%
  count(year, name = "article_count") %>%
  arrange(year)

cat("\nArticles by Year:\n")
print(articles_by_year)

# Articles by month
articles_by_month <- article_data %>%
  mutate(
    year_month = floor_date(article_date, "month"),
    year = year(article_date),
    month = month(article_date, label = TRUE)
  ) %>%
  count(year_month, year, month, name = "article_count") %>%
  arrange(year_month)

# Plot articles over time
p5 <- ggplot(articles_by_month, aes(x = year_month, y = article_count)) +
  geom_line(color = "#667eea", size = 1) +
  geom_point(color = "#667eea", size = 2) +
  labs(
    title = "Article Publication Trend Over Time",
    subtitle = paste0("Total Articles: ", format(nrow(article_data), big.mark = ",")),
    x = "Date",
    y = "Articles Published"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.text = element_text(size = 11)
  ) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y")

ggsave(here("output/quality_checks/05_publication_trend.png"), p5, 
       width = 12, height = 6, dpi = 300)

# Plot articles by year (bar chart)
p6 <- ggplot(articles_by_year, aes(x = factor(year), y = article_count)) +
  geom_col(fill = "#e67e22", alpha = 0.8) +
  geom_text(aes(label = comma(article_count)), vjust = -0.5, size = 4) +
  labs(
    title = "Articles Published by Year",
    subtitle = "Annual publication volume",
    x = "Year",
    y = "Number of Articles"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.text = element_text(size = 11)
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15)))

ggsave(here("output/quality_checks/06_articles_by_year.png"), p6, 
       width = 10, height = 6, dpi = 300)

cat("\n✅ Publication trend plots saved to output/quality_checks/\n")

# =============================================================================
# 4. NEIGHBORHOOD MAPPING CHECK ----
# Make pretty list of mapping and explain methodology
# =============================================================================

cat("\n\n🗺️  NEIGHBORHOOD MAPPING METHODOLOGY\n")
cat("=", rep("=", 50), "\n", sep = "")

# Create neighborhood mapping summary
mapping_summary <- article_data %>%
  group_by(community) %>%
  summarise(
    article_count = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(article_count))

cat("\nNeighborhood Coverage Summary:\n")
cat("Total Neighborhoods:", nrow(mapping_summary), "\n")
cat("Total Articles Mapped:", sum(mapping_summary$article_count), "\n")
cat("Avg Articles per Neighborhood:", 
    round(mean(mapping_summary$article_count)), "\n")

# Top neighborhoods by coverage
cat("\nTop 15 Neighborhoods by Article Coverage:\n")
print(mapping_summary %>% head(15))

# Neighborhoods with low coverage
cat("\nNeighborhoods with <10 Articles:\n")
low_coverage <- mapping_summary %>% filter(article_count < 10)
print(low_coverage)

# Plot neighborhood coverage
p7 <- mapping_summary %>%
  top_n(20, article_count) %>%
  ggplot(aes(x = reorder(community, article_count), y = article_count)) +
  geom_col(fill = "#27ae60", alpha = 0.8) +
  geom_text(aes(label = comma(article_count)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Top 20 Neighborhoods by Article Coverage",
    subtitle = "Most frequently covered communities",
    x = NULL,
    y = "Number of Articles"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.text = element_text(size = 10)
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15)))

ggsave(here("output/quality_checks/07_neighborhood_coverage.png"), p7, 
       width = 10, height = 8, dpi = 300)

cat("\n✅ Neighborhood mapping plot saved to output/quality_checks/\n")

# Create methodology explanation
methodology_text <- "
NEIGHBORHOOD MAPPING METHODOLOGY
=================================

This analysis maps Block Club Chicago articles to Chicago's 77 official 
community areas using a hierarchical matching system:

1. PRIMARY MATCHING (Sub-community field):
   - Extract the lead text before em-dash/hyphen in article content
   - Match against neighborhood mapping table
   - If match found, use ONLY this neighborhood

2. SECONDARY MATCHING (Article Sections):
   - If no primary match, parse parsely.meta.articleSection field
   - Check each section against mapping table
   - Collect ALL matching neighborhoods

3. TERTIARY MATCHING (Primary Category):
   - If still no match, parse slp_primary_category.name field
   - Check each category against mapping table
   - Collect ALL matching neighborhoods

4. DEFAULT ASSIGNMENT:
   - If no matches found, assign to 'chicago' (citywide)

NEIGHBORHOOD MAPPING TABLE:
- Maps sub-neighborhoods to official community areas
- Examples:
  * 'Wicker Park' → 'West Town'
  * 'Bronzeville' → 'Grand Boulevard', 'Kenwood', 'Washington Park'
  * 'Chinatown' → 'Armour Square'

MULTI-NEIGHBORHOOD ARTICLES:
- Articles can be assigned to multiple neighborhoods (up to 3)
- This captures stories that span multiple communities
- Each neighborhood assignment is stored in separate columns:
  neighborhood1, neighborhood2, neighborhood3

DATA QUALITY NOTES:
- Total mapped neighborhoods: {n_neighborhoods}
- Articles successfully mapped: {pct_mapped}%
- Articles requiring manual review: {articles_no_match}
"

methodology_text <- str_glue(methodology_text,
                             n_neighborhoods = nrow(mapping_summary),
                             pct_mapped = round((sum(mapping_summary$article_count) / nrow(article_data)) * 100, 1),
                             articles_no_match = sum(mapping_summary$community == "chicago")
)

# Save methodology
writeLines(methodology_text, here("output/quality_checks/00_methodology.txt"))

cat("\n✅ Methodology saved to output/quality_checks/00_methodology.txt\n")

# =============================================================================
# SUMMARY REPORT ----
# =============================================================================

cat("\n\n📋 DATA QUALITY SUMMARY REPORT\n")
cat("=", rep("=", 50), "\n", sep = "")

summary_report <- list(
  total_articles = nrow(article_data),
  date_range = paste(min(article_data$article_date), "to", max(article_data$article_date)),
  total_communities = nrow(mapping_summary),
  total_population = sum(census_summary$total_population, na.rm = TRUE),
  topics_tracked = length(unique(article_data$topic_match)),
  avg_articles_per_community = round(mean(mapping_summary$article_count)),
  communities_with_low_coverage = nrow(low_coverage)
)

cat("\n")
cat("Total Articles:", format(summary_report$total_articles, big.mark = ","), "\n")
cat("Date Range:", summary_report$date_range, "\n")
cat("Communities Covered:", summary_report$total_communities, "\n")
cat("Total Population:", format(summary_report$total_population, big.mark = ","), "\n")
cat("Topics Tracked:", summary_report$topics_tracked, "\n")
cat("Avg Articles/Community:", summary_report$avg_articles_per_community, "\n")
cat("Low Coverage Communities (<10 articles):", summary_report$communities_with_low_coverage, "\n")

cat("\n\n✅ ALL QUALITY CHECKS COMPLETE!\n")
cat("📁 Output saved to: output/quality_checks/\n\n")