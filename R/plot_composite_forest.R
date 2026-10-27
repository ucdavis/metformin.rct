#' @export

plot_composite_forest <- function(
    est_data,
    arrow_labs = c("Placebo", "Metformin"),
    min_mult = 1.15,
    max_mult = min_mult,
    padding = FALSE,
    grouped = TRUE) {
  # subset data
  plot_data <- est_data |>
    flextable::as_grouped_data(
      groups = "Instrument"
    ) |>
    dplyr::mutate(
      Outcome = dplyr::if_else(
        !is.na(Instrument),
        Instrument,
        paste0("    ", Outcome)
      )
    ) |>
    dplyr::select(-Instrument) |>
    dplyr::mutate(
      " " = paste(rep(" ", 40), collapse = " "),
      Analysis = ifelse(is.na(Analysis), " ", Analysis)
    ) |>
    dplyr::select(
      Outcome, Analysis, Estimate, low, hi, ` `
    )
  # expand x limits
  xlim <- c(
    min(plot_data$low, na.rm = TRUE) * min_mult,
    max(plot_data$hi, na.rm = TRUE) * max_mult
  )

  if (grouped) {
    # create grouped forest plot
    forestploter::forest(plot_data[, c(1:2, 6)],
                         est = plot_data$Estimate,
                         lower = plot_data$low,
                         upper = plot_data$hi,
                         #sizes = df$se,
                         ci_column = 3,
                         ref_line = 0,
                         arrow_lab = arrow_labs,
                         xlim = xlim,
                         add_padding = padding,
                         theme = forestploter::forest_theme(ci_Theight = 0.33)
    )
  } else {
    # create grouped forest plot
    forestploter::forest(plot_data[, c(1, 6)],
                         est = plot_data$Estimate,
                         lower = plot_data$low,
                         upper = plot_data$hi,
                         #sizes = df$se,
                         ci_column = 2,
                         ref_line = 0,
                         arrow_lab = arrow_labs,
                         xlim = xlim,
                         add_padding = padding,
                         theme = forestploter::forest_theme(ci_Theight = 0.33)
    )
  }

}
