# shorthand notes for proj (ask jillian if confusing)
# plz reference ORG STRCUTURE from three bars in the top left of this panel

# scraper ----
# run daily at 3 AM
# save scraped data to central database (SQLite?)
# dashboard that reads this pre-scraped dataset and displays it

# incremental updates ----
# store last_scrape_date somewhere
# on next update, fetch only posts published after that date.
# append them to existing dataset

# we're going to need a scraper script referenced in shiny dash

# for dash that is fast LOL ----
# modular structure so each module only loads content for that page
# rough example:
# ui <- navbarPage(
#   "My Dashboard",
#   tabPanel("Overview", overview_ui("overview")),
#   tabPanel("Neighborhoods", neighborhoods_ui("neighborhoods")),
#   tabPanel("Credibility Indicators", credibility_ui("credibility"))
# )
# 
# server <- function(input, output, session) {
#   callModule(overview_server, "overview")
#   callModule(neighborhoods_server, "neighborhoods")
#   callModule(credibility_server, "credibility")
# }

# preload data once on app startup instead of fetching from REST API each user session:
# app.R
# data <- readRDS("blockclub_data.rds")

# only render what's visible for user
# research promises / future / shiny.fluent

# ORG STRUCTURE ----
# my_dashboard/
#   ├─ app.R
#   ├─ modules/
#     │  ├─ overview.R
#     │  ├─ neighborhoods.R
#     │  ├─ credibility.R
#   ├─ data/
#   │  └─ blockclub_data.rds
#   ├─ www/        # for images, JS, CSS