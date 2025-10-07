# CLEANING THE DATA ----
library(tidyverse)
library(here)

# read in data ----
api <- readRDS(here("data/api_scrape.rds"))
census <- read_csv(here("data/ACS_5_Year_Data_by_Community_Area_20251007.csv"))
conflicts_prefer(dplyr::filter)
api_clean <- api |> 
  select(id, date, slug, status, link, author, 
         categories, tags, credibility_indicators, 
         title.rendered, content.rendered, excerpt.rendered,
         slp_primary_category.name, parsely.meta.articleSection,
         parsely.meta.keywords, 
         "yoast_head_json.twitter_misc.Est. reading time") |> 
  rename("Community Area" = parsely.meta.articleSection)

dim(api_clean)
# 28K rows
