
# 
# colnames(api)
# # 
# api1 <- api %>%
#   filter(id == 928341)
# api1$content.rendered 
# 
#   

View (api)

api_to_csv <- api$slug %>% 
  slice_head (n = 100) 

# 
# extract_locations_llm (api1$content.rendered, model = "llama3:8b")
# 
# api1$content.rendered

# clean gaz csv
library (tidyverse)
gaz <- read_csv("data/chi_boundaries.csv") %>%
  select (COMMUNITY, AREA_NUM_1)
View (gaz)

# --- packages ---
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidyr)
library(jsonlite)
library(furrr)
library(digest)
library(tibble)
library(xml2)
library(rvest)
`%||%` <- function(x, y) if (is.null(x)) y else x

# 0) Load data
article2 <- readRDS("data/api_scrape.rds") %>% 
  slice_head(n = 20) %>%
  select (content.rendered)

View (article2)
gaz <- read_csv("data/chi_boundaries.csv") %>%
  select (COMMUNITY, AREA_NUM_1)

find_slug_from_html <- function(html_frag) {
  doc <- read_html(paste0("<div>", html_frag %||% "", "</div>"))
  
  # all top-level paragraphs
  ps  <- html_elements(doc, xpath = "//p[not(ancestor::figure) and not(ancestor::aside)]")
  if (length(ps) == 0) return(NA_character_)
  
  promo_rx <- regex(
    paste(
      "sign up", "newsletter", "subscribe", "donate", "support our work",
      "advertisement", "sponsor", "daily recaps", "this is part of our series",
      sep = "|"
    ),
    ignore_case = TRUE
  )
  
  slug_rx <- "^\\s*([A-Z][A-Z\\s&\\-']{2,}?)\\s+[—–-]\\s+"
  
  # Walk paragraphs: skip promos; return first slug match
  for (p in ps) {
    t <- html_text2(p)
    if (str_detect(t, promo_rx)) next
    m <- str_match(t, slug_rx)
    if (!is.na(m[1,2])) return(str_squish(m[1,2]))
  }
  
  # Fallback: try whole text if no <p> matched
  all_txt <- html_text2(doc)
  m <- str_match(all_txt, slug_rx)
  ifelse(is.na(m[1,2]), NA_character_, str_squish(m[1,2]))
}
# 2) Optional: get the article text with promos removed (useful for your LLM step)
clean_body_no_promo <- function(html_frag) {
  doc <- read_html(paste0("<div>", html_frag %||% "", "</div>"))
  ps  <- html_elements(doc, xpath = "//p[not(ancestor::figure) and not(ancestor::aside)]")
  txt <- html_text2(ps)
  
  promo_rx <- regex(
    paste(
      "sign up", "newsletter", "subscribe", "donate", "support our work",
      "advertisement", "sponsor", "daily recaps", "this is part of our series",
      sep = "|"
    ),
    ignore_case = TRUE
  )
  
  # drop leading promos only
  i <- 1L; n <- length(txt)
  while (i <= n && str_detect(txt[i], promo_rx)) i <- i + 1L
  
  kept <- if (i <= n) txt[i:n] else character()
  out  <- str_squish(paste(kept, collapse = " "))
  
  # remove the leading slug "CHICAGO — " etc. from body copy if present
  out <- str_replace(out, "^\\s*[A-Z][A-Z\\s&\\-']{2,}?\\s+[—–-]\\s+", "")
  
  out
}

# 3) Apply to your data
api_clean <- article2 %>%
  mutate(
    sub_community = map_chr(content.rendered, find_slug_from_html),
    article_text  = map_chr(content.rendered, clean_body_no_promo)
  )

# Peek
api_clean %>% select(sub_community, article_text) %>% head()
View (api_clean)

#__________________________________________________



# Make an article id and a compact, lowercased text snippet for matching
articles <- article2 %>%
  mutate(
    article_id = row_number(),
    # Convert HTML fragment -> plain text
    article_text = map_chr(content.rendered, ~{
      html_frag <- paste0("<div>", .x %||% "", "</div>")     # wrap fragment so read_html parses it
      doc <- read_html(html_frag, options = "HUGE")
      txt <- html_text2(doc)                                 # strips tags & decodes entities
      str_squish(txt)
    }),
    # Lowercased, truncated snippet for fast matching (no HTML here)
    text_snip = str_to_lower(substr(article_text, 1L, 1500L))
  ) %>%
  select(article_id, content.rendered, article_text, text_snip)

gaz <- gaz %>%
  mutate(COMMUNITY = toupper(trimws(COMMUNITY))) %>%
  distinct(COMMUNITY, AREA_NUM_1)

gaz_rx <- gaz %>%
  transmute(
    COMMUNITY,
    rx = paste0("\\b", str_replace_all(COMMUNITY, "\\s+", "\\\\s+"), "\\b") |> tolower()
  )

# --- 1) extract leading ALL-CAPS slug like "NEAR NORTH SIDE —" or "AUSTIN —"
extract_slug <- function(txt) {
  m <- str_match(txt, "^\\s*([A-Z][A-Z\\s&\\-']{2,}?)\\s+[-—–]\\s+")
  slug <- m[,2]
  ifelse(is.na(slug), NA_character_, str_squish(slug))
}

slug_tbl <- articles %>%
  transmute(article_id, slug_raw = extract_slug(article_text)) %>%
  mutate(slug_norm = ifelse(is.na(slug_raw), NA_character_,
                            str_replace_all(toupper(trimws(slug_raw)), "\\s+", " "))) %>%
  left_join(gaz |> transmute(COMMUNITY, in_gaz = TRUE),
            by = c("slug_norm" = "COMMUNITY")) %>%
  mutate(community_from_slug = ifelse(!is.na(in_gaz), slug_norm, NA_character_)) %>%
  select(article_id, community_from_slug)

# --- 2) gazetteer regex fallback (scan the body) for those without slug match
articles2 <- articles %>%
  left_join(slug_tbl, by = "article_id") %>%
  mutate(has_slug = !is.na(community_from_slug))

detect_any <- function(txt) {
  hits <- gaz_rx$COMMUNITY[str_detect(txt, gaz_rx$rx)]
  unique(hits)
}

gaz_hits <- articles2 %>%
  filter(!has_slug) %>%
  transmute(article_id,
            communities_gaz = map(text_snip, detect_any))

# --- 3) combine results (prefer slug > gazetteer)
combined <- articles2 %>%
  left_join(gaz_hits, by = "article_id") %>%
  mutate(
    communities_gaz = communities_gaz %||% list(character()),
    communities = case_when(
      has_slug ~ list(community_from_slug),
      lengths(communities_gaz) > 0 ~ communities_gaz,
      TRUE ~ list(character())
    ),
    source = case_when(
      has_slug ~ "slug",
      lengths(communities_gaz) > 0 ~ "gaz",
      TRUE ~ "needs_llm"
    )
  ) %>%
  select(article_id, article_text, communities, source)

combined <- combined %>%
  mutate(
    communities = purrr::map(communities, ~{
      v <- .x
      v <- v[!is.na(v)]
      v <- stringr::str_trim(v)
      v <- v[v != "" & toupper(v) != "NA"]
      unique(v)
    })
  )

# ---------------------------------------------------------
# Creating custom tags for ones without AI
# --- pick a tiny, "location-y" slice to keep prompts fast ---
pick_locationy <- function(txt, max_chars = 800) {
  if (is.na(txt) || !nzchar(txt)) return("")
  sents <- unlist(strsplit(txt, "(?<=[.!?])\\s+", perl = TRUE))
  cues <- c("\\bin\\b","near","neighborhood","block",
            "on the (south|west|north) side",
            "street","avenue","boulevard","park","square","station",
            "\\bward\\b","alder")
  keep <- sapply(sents, function(s) any(str_detect(tolower(s), paste(cues, collapse="|"))))
  out <- paste(head(sents[keep], 3), collapse = " ")
  substr(str_squish(out), 1, max_chars)
}

# --- LLM extractor that ONLY returns names from your gazetteer ---
extract_community_llm <- function(text, gaz, model = "llama3:8b") {
  stopifnot("COMMUNITY" %in% names(gaz))
  allowed <- unique(toupper(trimws(gaz$COMMUNITY)))
  allowed <- allowed[nchar(allowed) > 0]
  
  sys <- paste(
    "You are a strict data extractor.",
    "Identify Chicago COMMUNITY AREAS mentioned in this article.",
    "Return ONLY exact strings from ALLOWED_COMMUNITIES. Do NOT invent new names.",
    'Output must be valid JSON: { "communities": [string], "confidence": number }',
    "If none match, return an empty list for communities.",
    sep = "\n"
  )
  
  allowed_block <- paste0("ALLOWED_COMMUNITIES:\n- ", paste(allowed, collapse = "\n- "))
  
  prompt <- paste(
    sys, allowed_block,
    "\n--- ARTICLE START ---\n", text, "\n--- ARTICLE END ---\n",
    "Only output the JSON. No explanations.",
    sep = "\n"
  )
  
  raw <- ollama_generate(prompt, model = model)   # your existing client
  
  json_str <- stringr::str_extract(raw, "\\{[\\s\\S]*\\}\\s*$")
  if (is.na(json_str)) stop("Model did not return JSON.")
  
  parsed <- jsonlite::fromJSON(json_str, simplifyVector = TRUE)
  
  comms <- parsed$communities %||% character()
  comms <- unique(toupper(trimws(comms)))
  comms <- comms[comms %in% allowed]  # guardrail
  
  list(
    communities = comms,
    confidence  = as.numeric(parsed$confidence %||% NA_real_)
  )
}

# --- run LLM only for rows that still need it ---
needs_llm <- combined %>% filter(source == "needs_llm")

if (nrow(needs_llm) > 0) {
  needs_llm <- needs_llm %>%
    mutate(min_text = purrr::map_chr(article_text, pick_locationy))
  
  # Run Ollama on the minimal text, safely
  llm_res <- purrr::map(needs_llm$min_text, function(txt) {
    tryCatch(
      extract_community_llm(text = txt, gaz = gaz, model = "llama3:8b"),
      error = function(e) NULL
    )
  })
  
  # Collect results (handle NULLs cleanly)
  llm_tags <- tibble::tibble(
    article_id      = needs_llm$article_id,
    communities_llm = purrr::map(llm_res, function(x) if (is.null(x)) character() else x$communities),
    confidence      = purrr::map_dbl(llm_res, function(x) if (is.null(x)) NA_real_ else as.numeric(x$confidence))
  )
  
  # Merge back into your combined table
  combined <- combined %>%
    dplyr::left_join(llm_tags, by = "article_id") %>%
    dplyr::mutate(
      communities = ifelse(source == "needs_llm" & lengths(communities_llm) > 0,
                           communities_llm, communities),
      source = ifelse(source == "needs_llm" & lengths(communities_llm) > 0,
                      "llm", source)
    ) %>%
    dplyr::select(-communities_llm, -confidence)
}

View (needs_llm)