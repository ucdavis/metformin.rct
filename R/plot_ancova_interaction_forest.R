#' Plot ANCOVA Interaction Forest Plot
#'
#' Creates a forest plot displaying Treatment by stratification variable interaction
#' coefficient estimates with 95% confidence intervals across multiple instruments.
#'
#' @param obj List of fit objects from fit_ancova_interaction() or fit_cgi_ancova_interaction()
#' @param labels Named vector of outcome labels
#' @param min_mult Multiplier for minimum x-axis limit. Default is 1.1
#' @param max_mult Multiplier for maximum x-axis limit. Default is 1.1
#' @param arrow_labs Vector of two labels for forest plot arrows. These are typically
#'   the stratification levels (e.g., c("Full", "Mosaic") or c("Age 6-11", "Age 12+"))
#' @param padding Whether to add padding. Default is FALSE
#' @param var_name Name for variable column. Default is "Variable"
#' @param grouped Whether to create grouped forest plot. Default is FALSE
#' @param group_name Name for group column when grouped=TRUE. Default is "Group"
#'
#' @return A forest plot object
#'
#' @export

plot_ancova_interaction_forest <- function(obj, labels,
                                          min_mult = 1.1,
                                          max_mult = 1.1,
                                          arrow_labs = c("Favors Age 6-11", "Favors Age 12+"),
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
        extract_interaction_est(
          obj[[x]]$fit,
          obj[[x]]$strata_var,
          obj[[x]]$strata_levels
        ) |>
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
                         ci_column = 3,
                         ref_line = 0,
                         arrow_lab = arrow_labs,
                         xlim = xlim,
                         add_padding = padding,
                         theme = forestploter::forest_theme(ci_Theight = 0.33)
    )
  }

}
