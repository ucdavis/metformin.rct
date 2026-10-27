#' @export

plot_ancova_forest <- function(obj, labels,
                               min_mult,
                               max_mult,
                               arrow_labs = c("Placebo", "Metformin"),
                               padding = FALSE,
                               var_name = NULL,
                               grouped = FALSE,
                               group_name = NULL) {
  if (is.null(var_name)) var_name <- "Variable"
  if (is.null(group_name)) group_name <- "Group"

  tmp <- dplyr::bind_rows(
    lapply(
      1:length(obj),
      function(x) {
        extract_est(obj[[x]]$fit) |>
          dplyr::mutate(
            !!group_name := names(obj)[x],
            !!var_name := labels[x],
            .before = 1
          ) |>
          dplyr::mutate(
            " " = paste(rep(" ", 40), collapse = " ")
          )
      }
    )
  )

  xlim <- c(
    min(tmp$low) * min_mult,
    max(tmp$hi) * max_mult
  )

  if (!grouped) {
    forestploter::forest(tmp[, c(2, 6)],
                         est = tmp$Estimate,
                         lower = tmp$low,
                         upper = tmp$hi,
                         #sizes = df$se,
                         ci_column = 2,
                         ref_line = 0,
                         arrow_lab = arrow_labs,
                         xlim = xlim,
                         add_padding = padding,
                         theme = forestploter::forest_theme(ci_Theight = 0.33)
    )
  } else {
    forestploter::forest(tmp[, c(1:2, 6)],
                         est = tmp$Estimate,
                         lower = tmp$low,
                         upper = tmp$hi,
                         #sizes = df$se,
                         ci_column = 3,
                         ref_line = 0,
                         arrow_lab = arrow_labs,
                         xlim = xlim,
                         add_padding = padding,
                         theme = forestploter::forest_theme(ci_Theight = 0.33)
    )
  }

}
