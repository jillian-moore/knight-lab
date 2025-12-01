# MAKE DASHBOARD URL ACCESSIBLE ----

# install necessary packages ----
install.packages("curl")
install.packages("openssl")
install.packages('rsconnect')

# deploy app ----
rsconnect::deployApp(
  appFiles = c(
    "app.R",
    "modules/",
    "data/",
    "www/"
  )
)