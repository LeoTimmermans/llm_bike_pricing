generate_new_bike <-
  function(bike_model, category_1, category_2, frame_material, .ml_model) {
    box::use(
      dplyr[bind_cols, bind_rows, mutate, rename, select],
      tibble[tibble],
      tidyr[gather, spread],
    )

    new_bike_tbl <- tibble(
      model = bike_model,
      category_1 = category_1,
      category_2 = category_2,
      frame_material = frame_material
    ) %>%
      separate_bike_model()

    predict(.ml_model, new_data = new_bike_tbl) %>%
      bind_cols(new_bike_tbl) %>%
      rename(price = .pred)
  }

format_table <-
  function(new_bike_tbl) {
    box::use(
      dplyr[mutate],
      tidyr[gather, spread]
    )

    new_bike_tbl %>%
      mutate(price = scales::dollar(price, accuracy = 1)) %>%
      gather(
        key = "New Model Attribute",
        value = "value",
        -model,
        factor_key = TRUE
      ) %>%
      spread(key = model, value = value)
  }


bind_bike_prediction <-
  function(bikes_tbl, new_bike_tbl) {
    box::use(
      dplyr[bind_rows, mutate, select]
    )

    bikes_tbl %>%
      separate_bike_description() %>%
      mutate(estimate = "Actual") %>%
      bind_rows(
        new_bike_tbl %>% mutate(estimate = "Prediction")
      ) %>%
      select(estimate, model, category_1, category_2, frame_material, price)
  }

#' Plot bike price predictions with brand styling
#'
#' Creates a violin + jitter plot of bike price predictions, grouped by
#' bike family and faceted by frame material. The function supports both
#' static ggplot output and interactive Plotly output. Colors, typography,
#' and backgrounds can be styled consistently with a brand configuration
#' (see [load_brand()], [brand_theme()], and [brand_scales()]).
#'
#' @param data A data frame containing at least the following columns:
#'   \describe{
#'     \item{price}{Numeric. Unit price of the bike.}
#'     \item{model}{Character. Model name.}
#'     \item{category_1}{Character. High-level bike type.}
#'     \item{category_2}{Character. Bike family (used for x-axis).}
#'     \item{frame_material}{Character. Frame material (used for facets).}
#'     \item{estimate}{Logical or categorical. Used for color mapping.}
#'   }
#' @param interactive Logical. If `TRUE` (default), returns an interactive
#'   Plotly object with tooltips. If `FALSE`, returns a static ggplot.
#' @param brand Optional list of brand settings, typically the output of
#'   [load_brand()]. If `NULL`, default Cannondale colors and fonts are used.
#'
#' @return A `ggplot` object if `interactive = FALSE`, otherwise a
#'   `plotly` object.
#'
#' @details
#' The plot shows the distribution of bike prices (on a log scale) across
#' families (`category_2`), with violin plots for distribution shape and
#' jittered points for individual bikes. Tooltips include unit price,
#' model, type, family, and frame material.
#'
#' @examples
#' \dontrun{
#' brand <- load_brand("_brand.yml")
#' plot_bike_prediction(bike_data, interactive = FALSE, brand = brand)
#' }
#'
#' @seealso [load_brand()], [brand_theme()], [brand_scales()]
#'
#' @export
plot_bike_prediction <- function(data, interactive = TRUE, brand = NULL) {
  box::use(
    dplyr[mutate],
    forcats[fct_reorder],
    ggplot2[
      aes,
      coord_flip,
      facet_wrap,
      geom_jitter,
      geom_violin,
      ggplot,
      labs,
      scale_y_log10,
      theme
    ],
    plotly[ggplotly],
    stringr[str_glue]
  )

  if (is.null(brand)) {
    brand <- load_brand("_brand.yml")
  }

  # Ensure estimate is logical or factor with known levels
  data <- data %>%
    mutate(estimate = as.factor(estimate))
  data |> glimpse()

  g <- data %>%
    mutate(category_2 = fct_reorder(category_2, price)) %>%
    mutate(
      label_text = str_glue(
        "Unit Price: {scales::dollar(price, accuracy = 1)}
         Model: {model}
         Bike Type: {category_1}
         Bike Family: {category_2}
         Frame Material: {frame_material}"
      )
    ) %>%
    ggplot(aes(category_2, price, color = estimate)) +
    geom_violin() +
    geom_jitter(aes(text = label_text), width = 0.1, alpha = 0.5) +
    facet_wrap(~frame_material) +
    coord_flip() +
    scale_y_log10(labels = scales::dollar_format(accuracy = 1)) +
    scale_color_manual(
      values = c(
        "Actual" = brand$color$secondary,
        "Prediction" = brand$color$primary
      )
    ) +
    brand_theme(brand) +
    labs(title = "", x = "", y = "Log Scale")

  if (interactive) {
    return(ggplotly(g, tooltip = "text"))
  } else {
    return(g)
  }
}
