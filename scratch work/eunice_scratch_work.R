library(rvest)
library(httr2)
library(jsonlite)
library(stringr)
library(tibble)
library(rvest)
library(tibble)

one_url <- "https://blockclubchicago.org/2025/09/25/as-domestic-violence-surges-advocates-demand-more-city-funding-to-fight-crisis/"

pg <- read_html(one_url)

headline <- pg |>
  html_elements("h1.entry-title, .entry-header h1, h1") |>
  html_text2() |>
  (\(x) if (length(x)) x[1] else NA)()

article_text <- pg |>
  html_elements("article .entry-content p, .entry-content p, .post-content p, .content p") |>
  html_text2() |>
  paste(collapse = " ")

art <- tibble(
  url = one_url,
  headline = headline,
  article_text = if (nzchar(article_text)) article_text else NA_character_
)

print(art$headline)
cat(substr(art$article_text, 1, 200), "...\n")

# Function 

`%||%` <- function(x, y) if (is.null(x)) y else x

if (!exists("ollama_generate")) {
  ollama_generate <- function(prompt, model = "llama3:8b", timeout = 180) {
    request("http://127.0.0.1:11434/api/generate") |>
      req_body_json(list(model = model, prompt = prompt, stream = FALSE)) |>
      req_timeout(timeout) |>
      req_perform() |>
      resp_body_json() |>
      (\(x) x$response)()
  }
}

extract_locations_llm <- function(text, model = "llama3:8b") {
  sys <- paste(
    "You are a data extraction model.",
    "Extract WHERE this Chicago news article takes place.",
    "Focus on Chicago neighborhoods, Chicago landmarks, and nearby suburbs.",
    "Return ONLY valid JSON. No code fences, no prose.",
    "Schema:",
    "{",
    '  "neighborhoods": [string],',
    '  "cities": [string],',
    '  "landmarks": [string],',
    '  "confidence": number',
    "}",
    sep = "\n"
  )
  
  prompt <- paste(sys,
                  "\n--- ARTICLE START ---\n",
                  text,
                  "\n--- ARTICLE END ---\n")
  
  raw <- ollama_generate(prompt, model = model)
  
  json_str <- stringr::str_extract(raw, "\\{[\\s\\S]*\\}\\s*$")
  if (is.na(json_str)) stop("Model did not return JSON.")
  
  parsed <- jsonlite::fromJSON(json_str, simplifyVector = TRUE)
  parsed$neighborhoods <- unique(trimws(tolower(parsed$neighborhoods %||% character())))
  parsed$cities        <- unique(trimws(tolower(parsed$cities %||% character())))
  parsed$landmarks     <- unique(trimws(tolower(parsed$landmarks %||% character())))
  parsed$confidence    <- as.numeric(parsed$confidence %||% NA_real_)
  parsed
}

locs <- extract_locations_llm(art$article_text)
str(locs)              # structure
tibble::as_tibble(locs)
