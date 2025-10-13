# 0.1 packages ----
core_tidyverse <- c(
  "ggplot2",
  "dplyr",
  "tidyr",
  "purrr",
  "tibble",
  "stringr",
  "forcats"
)
other_pkgs <- c(
  "parsnip",
  "hardhat",
  "tidyquant",
  "odbc",
  "RSQLite",
  "shinychat",
  "ellmer",
  "plotly",
  "bslib",
  "shiny",
  "shinyWidgets",
  "shinyjs"
)
all_pkgs <- c(core_tidyverse, other_pkgs)
lapply(all_pkgs, library, character.only = TRUE)

# 0.2 functions ----
source("00_scripts/02_process_data.R")
source("00_scripts/03_make_predictions.R")

# 0.3 models
model_xgboost <- readRDS("00_models/model_xgboost.rds")

# 1 DATA ----
# 1.1 get data ----
con <- odbc::dbConnect(RSQLite::SQLite(), "00_data/bikes_database.db")
# con <- dbConnect(RSQLite::SQLite(), "00_data/bikes_database.db")
# dbListTables(con)
bikes_tbl <- dplyr::tbl(con, "bikes") %>% dplyr::collect()
odbc::dbDisconnect(con)

# 1.2 prep data ----
category_hierachy_tbl <- bikes_tbl %>%
  separate_bike_description(keep_description_column = TRUE, append = FALSE) %>%
  dplyr::distinct(category_1, category_2)

# 2. Model ----
train_tbl <-
  bikes_tbl %>%
  separate_bike_description() |>
  separate_bike_model() |>
  dplyr::select(-c(bike.id, model, description, model_tier)) %>%
  dplyr::select(price, everything())

set.seed(1234)
model_xgboost <- parsnip::boost_tree(
  mode = "regression",
  mtry = 30,
  learn_rate = 0.25,
  tree_depth = 7
) %>%
  parsnip::set_engine(engine = "xgboost") %>%
  parsnip::fit(price ~ ., data = train_tbl)


# UI ----
ui <- bslib::page_navbar(
  title = "Bike Pricing",
  theme = bs_theme(brand = TRUE),

  # UI 1. sidebar ----
  sidebar = sidebar(
    # Bike Model Input ----
    textInput(
      inputId = "text_bike_model",
      label = h4("Bike Model"),
      value = "Jekyll Al 1 Black Inc."
    ),
    # Bike Family (Category 2) ----
    pickerInput(
      inputId = "picker_category_2",
      label = h4("Bike Family"),
      choices = category_hierachy_tbl$category_2,
      selected = "Over Mountain"
    ),
    # Frame Material ----
    pickerInput(
      inputId = "picker_frame_material",
      label = h4("Frame Material"),
      choices = c("Aluminum", "Carbon"),
      selected = "Aluminum"
    ),
    # Action buttons ----
    actionButton(
      inputId = "apply",
      label = "Apply",
      icon = icon(name = "play", lib = "font-awesome")
    ),
    actionButton("reset", label = "Reset", icon = icon("sync"))
    # textInput("api_key", "open_ai API key"),
    # chat_ui(
    #   id = "chat",
    #   messages = "**Hello!** How can I help you today?"
    # )
  ), # end sidebar
  # UI 2. MAIN ----
  nav_panel(
    title = "Main",
    plotlyOutput("plotly_1", width = "100%")
  ),
  nav_panel(
    title = "Details",
    tableOutput("table_1")
  )
)

# SERVER ----
server <- function(input, output, session) {
  useShinyjs()

  # SERV 1. sidebar ----
  # * extract bike type ----
  bike_type <- reactive({
    category_hierachy_tbl %>%
      filter(category_2 == input$picker_category_2) %>%
      pull(category_1)
  })

  # * extract new bike ----
  new_bike_tbl <- eventReactive(
    eventExpr = input$apply,
    valueExpr = {
      generate_new_bike(
        bike_model = input$text_bike_model,
        category_1 = bike_type(),
        category_2 = input$picker_category_2,
        frame_material = input$picker_frame_material,
        .ml_model = model_xgboost
      )
    },
    ignoreNULL = FALSE
  )

  # * Reset inputs ----
  observeEvent(eventExpr = input$reset, handlerExpr = {
    updateTextInput(
      session = session,
      inputId = "text_bike_model",
      value = "Jekyll Al 1 Black Inc."
    )

    updatePickerInput(
      session = session,
      inputId = "picker_category_2",
      selected = "Over Mountain"
    )

    updatePickerInput(
      session = session,
      inputId = "picker_frame_material",
      selected = "Aluminum"
    )

    delay(ms = 300, expr = {
      click(id = "apply")
    })
  })
  # chat <-
  #   ellmer::chat_openai(
  #     system_prompt = "Respond to the user as succinctly as possible."
  #   )

  # observeEvent(input$chat_user_input, {
  #   stream <- chat$stream_async(input$chat_user_input)
  #   chat_append("chat", stream)
  # })

  # SERV 2. MAIN ----
  # SERV 2.1 Plotly plot ----
  output$plotly_1 <- renderPlotly({
    bind_bike_prediction(bikes_tbl, new_bike_tbl()) %>%
      plot_bike_prediction()
  })

  # SERV 2.2 Table ----
  output$table_1 <- renderTable({
    new_bike_tbl() %>% format_table()
  })
}

# RUN APP ----
shinyApp(ui, server)
