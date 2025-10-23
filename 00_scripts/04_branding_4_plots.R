#' Load brand configuration from YAML
#'
#' Reads a `_brand.yml` file and returns a structured list containing
#' brand metadata, logo paths, color palette, and typography settings.
#' This function provides a single source of truth for styling across
#' Shiny apps and ggplot visualizations.
#'
#' @param path Character string. Path to the `_brand.yml` file.
#'   Defaults to `"_brand.yml"`.
#'
#' @return A named list with elements such as `meta`, `logo`, `color`,
#'   and `typography`.
#'
#' @examples
#' \dontrun{
#' brand <- load_brand("_brand.yml")
#' brand$color$primary
#' }
#'
#' @export
# safer version
load_brand <- function(path = "_brand.yaml") {
  box::use(yaml[read_yaml])

  if (!file.exists(path)) {
    warning(glue::glue("Brand file not found at {path}. Using defaults."))
    return(list(
      color = list(
        primary = "#00B140",
        secondary = "#004B87",
        background = "#FFFFFF",
        text = "#333333",
        white = "#FFFFFF"
      ),
      typography = list(base = "Open Sans")
    ))
  }

  read_yaml(path)
}


#' Generate a ggplot2 theme from brand settings
#'
#' Creates a `ggplot2::theme()` object using typography and color
#' definitions from a brand configuration list (as returned by
#' [load_brand()]). This ensures consistent fonts, text colors,
#' and background styling across all plots.
#'
#' @param brand A list containing brand settings, typically the
#'   output of [load_brand()].
#'
#' @return A `ggplot2::theme` object that can be added to plots.
#'
#' @examples
#' \dontrun{
#' brand <- load_brand("_brand.yml")
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   brand_theme(brand)
#' }
#'
#' @export
brand_theme <- function(brand) {
  box::use(ggplot2[theme, element_text, element_rect])

  colors <- brand$color
  font <- brand$typography$base

  theme(
    text = element_text(family = font, color = colors$text),
    plot.background = element_rect(fill = colors$background, color = NA),
    panel.background = element_rect(fill = colors$background, color = NA),
    strip.background = element_rect(fill = colors$secondary, color = NA),
    strip.text = element_text(color = colors$white)
  )
}

#' Generate brand-based color scales
#'
#' Provides `ggplot2` color and fill scales based on brand settings.
#' This function maps logical or categorical values to the brand's
#' primary and secondary colors, ensuring consistent use of the
#' corporate palette.
#'
#' @param brand A list containing brand settings, typically the
#'   output of [load_brand()].
#'
#' @return A list of `ggplot2` scale objects (`scale_color_manual`
#'   and `scale_fill_manual`) that can be added to plots.
#'
#' @examples
#' \dontrun{
#' brand <- load_brand("_brand.yml")
#' ggplot(mtcars, aes(mpg, wt, color = factor(cyl))) +
#'   geom_point() +
#'   brand_scales(brand)
#' }
#'
#' @export
brand_scales <- function(brand) {
  box::use(ggplot2[scale_color_manual, scale_fill_manual])

  colors <- brand$color

  list(
    scale_color_manual(
      values = c(
        "TRUE" = colors$primary,
        "FALSE" = colors$secondary
      )
    ),
    scale_fill_manual(
      values = c(
        "TRUE" = colors$primary,
        "FALSE" = colors$secondary
      )
    )
  )
}
