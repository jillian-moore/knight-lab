# MAKE DASHBOARD URL ACCESSIBLE

# install necessary packages ----
install.packages("curl")
install.packages("openssl")
install.packages('rsconnect')

# deploy app ----
rsconnect::deployApp(
  appFiles = c(
    "modules/",
    "data/",     
    "www/"    
  ),
  appName = "knight_lab"
)