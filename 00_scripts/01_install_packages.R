# DS4B 102-R: SHINY APPS - LEVEL 1 ----
# R PACKAGES

r_pkgs <- c(
  # Core
  "tidyverse",
  "tidyquant",

  # Database
  "odbc",
  "RSQLite",

  # LLM
  "shinychat",
  "ellmer",

  # Visualization
  "plotly",

  # Shiny-verse
  # "flexdashboard",
  "bslib", # in plaats van flex dashboard
  "shiny",
  "shinyWidgets",
  "shinyjs",

  # Modeling & Machine Learning
  "parsnip",
  "rsample",
  "xgboost"
)

install.packages(r_pkgs)
