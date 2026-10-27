#' @export

fit_cgi_ancova <- function(
    dataset,
    outcome,
    outcome_label,
    demo_vars = c("sex", "demo_age_center", "leit3_nv_iq_center")) {
  # subset data
  data <- dataset |>
    dplyr::select(
      fxs_sts_id, redcap_event_name, Treatment, Site,
      dplyr::any_of(c(outcome, demo_vars))
    ) |>
    dplyr::mutate(
      redcap_event_name = dplyr::case_when(
        redcap_event_name == "Baseline Visit (Visit 1)" ~ 1,
        redcap_event_name == "Week 8 Visit (Visit 2)" ~ 2,
        redcap_event_name == "Week 16 (Visit 3/Early Term)" ~ 3
      )
    )

  # convert data to wide format
  data_wide <- data |>
    tidyr::pivot_wider(
      id_cols = c(fxs_sts_id, Treatment, Site, dplyr::any_of(demo_vars)),
      names_from = redcap_event_name,
      values_from = outcome,
      names_prefix = glue::glue("{outcome}_")
    )

  outcome_1 <- glue::glue("{outcome}_1")
  outcome_3 <- glue::glue("{outcome}_3")

  # subset to cases with baseline and week 16 values
  data_wide <- data_wide |>
    dplyr::filter(
      !is.na(.data[[outcome_1]]) &
        !is.na(.data[[outcome_3]])
    )

  # formula #
  # Helper to build covariate string
  demo_str <- if (!is.null(demo_vars)) {
    paste(demo_vars, collapse = " + ")
  } else {
    NULL
  }

  # baseline model
  if (!is.null(demo_vars)) {
    baseline_formula <- glue::glue(
      "{outcome}_1 ~ {demo_str} + Site + Treatment"
    ) |> as.formula()
  } else {
    baseline_formula <- glue::glue(
      "{outcome}_1 ~ Site + Treatment"
    ) |> as.formula()
  }

  # full ancova model
  if (!is.null(demo_vars)) {
    ancova_formula <- glue::glue(
      "{outcome}_3 ~ {outcome}_1 + {demo_str} + Site + Treatment"
    ) |> as.formula()
  } else {
    ancova_formula <- glue::glue(
      "{outcome}_3 ~ {outcome}_1 + Site + Treatment"
    ) |> as.formula()
  }

  # fit baseline
  fit_baseline = lm(
    formula = baseline_formula,
    data = data_wide
  )

  # fit full ancova
  fit_full <- lm(
    formula = ancova_formula,
    data = data_wide
  )

  tx_table <- table(data_wide$Treatment)
  n_placebo <- tx_table["Placebo"]
  n_met <- tx_table["Metformin"]

  fit_tbl <- construct_cgi_ancova_table(
    outcome = outcome_label,
    baseline_model = fit_baseline,
    full_model = fit_full,
    n_p = n_placebo,
    n_m = n_met
  )

  ans <- list(
    "fit" = fit_full,
    "tbl" = fit_tbl,
    "n.met" = n_met,
    "n.placebo" = n_placebo
  )

  return(ans)
}
