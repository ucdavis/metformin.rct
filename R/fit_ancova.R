#' @export

fit_ancova <- function(
    dataset,
    outcome,
    outcome_label,
    demo_vars = c("sex", "demo_age_center", "leit3_nv_iq_center"),
    covariates = NULL,
    site = TRUE) {
  # subset data
  data <- dataset |>
    dplyr::select(
      fxs_sts_id, redcap_event_name, Treatment, Site,
      dplyr::any_of(c(outcome, demo_vars, covariates))
    ) |>
    dplyr::mutate(
      redcap_event_name = dplyr::case_when(
        redcap_event_name == "Baseline Visit (Visit 1)" ~ 1,
        redcap_event_name == "Week 8 Visit (Visit 2)" ~ 2,
        redcap_event_name == "Week 16 (Visit 3/Early Term)" ~ 3
      )
    )
  # convert data to wide format
  # Build id_cols based on what's available
  # base_id_cols <- c("fxs_sts_id", "Treatment", "Site")
  # if (!is.null(demo_vars)) {
  #   id_cols_expr <- c(base_id_cols, dplyr::any_of(demo_vars))
  # } else {
  #   id_cols_expr <- base_id_cols
  # }

  if (!is.null(covariates)) {
    data_wide <- data |>
      tidyr::pivot_wider(
        id_cols = c(fxs_sts_id, Treatment, Site, dplyr::any_of(demo_vars)),
        names_from = redcap_event_name,
        values_from = dplyr::any_of(c(outcome, covariates))
      )
  } else {
    data_wide <- data |>
      tidyr::pivot_wider(
        id_cols = c(fxs_sts_id, Treatment, Site, dplyr::any_of(demo_vars)),
        names_from = redcap_event_name,
        values_from = outcome,
        names_prefix = glue::glue("{outcome}_")
      )
  }

  outcome_1 <- glue::glue("{outcome}_1")
  outcome_3 <- glue::glue("{outcome}_3")

  # subset to cases with baseline and week 16 values and calculate delta
  data_wide <- data_wide |>
    dplyr::filter(
      !is.na(.data[[outcome_1]]) &
        !is.na(.data[[outcome_3]])
    ) |>
    dplyr::mutate(
      delta = .data[[outcome_3]] - .data[[outcome_1]]
    )

  # formula #
  # Helper to build covariate string
  demo_str <- if (!is.null(demo_vars)) {
    paste(demo_vars, collapse = " + ")
  } else {
    NULL
  }

  # baseline
  if (!is.null(covariates) && !is.null(demo_vars)) {
    baseline_formula <- glue::glue(
      "{outcome}_1 ~ {demo_str} + {covariates}_1 + Site + Treatment"
    ) |> as.formula()
  } else if (!is.null(covariates)) {
    baseline_formula <- glue::glue(
      "{outcome}_1 ~ {covariates}_1 + Site + Treatment"
    ) |> as.formula()
  } else if (!is.null(demo_vars)) {
    baseline_formula <- glue::glue(
      "{outcome}_1 ~ {demo_str} + Site + Treatment"
    ) |> as.formula()
  } else {
    baseline_formula <- glue::glue(
      "{outcome}_1 ~ Site + Treatment"
    ) |> as.formula()
  }

  # week 16
  if (!is.null(covariates) && !is.null(demo_vars)) {
    w16_formula <- glue::glue(
      "{outcome}_3 ~ {demo_str} + {covariates}_3 + Site + Treatment"
    ) |> as.formula()
  } else if (!is.null(covariates)) {
    w16_formula <- glue::glue(
      "{outcome}_3 ~ {covariates}_3 + Site + Treatment"
    ) |> as.formula()
  } else if (!is.null(demo_vars)) {
    w16_formula <- glue::glue(
      "{outcome}_3 ~ {demo_str} + Site + Treatment"
    ) |> as.formula()
  } else {
    w16_formula <- glue::glue(
      "{outcome}_3 ~ Site + Treatment"
    ) |> as.formula()
  }

  # delta
  if (!is.null(covariates) && !is.null(demo_vars)) {
    delta_formula <- glue::glue(
      "delta ~ {outcome}_1 + {demo_str} + {covariates}_3 + Site + Treatment"
    ) |> as.formula()
  } else if (!is.null(covariates)) {
    delta_formula <- glue::glue(
      "delta ~ {outcome}_1 + {covariates}_3 + Site + Treatment"
    ) |> as.formula()
  } else if (!is.null(demo_vars)) {
    delta_formula <- glue::glue(
      "delta ~ {outcome}_1 + {demo_str} + Site + Treatment"
    ) |> as.formula()
  } else {
    delta_formula <- glue::glue(
      "delta ~ {outcome}_1 + Site + Treatment"
    ) |> as.formula()
  }

  # ancova
  if (!is.null(covariates) && !is.null(demo_vars)) {
    ancova_formula <- glue::glue(
      "{outcome}_3 ~ {outcome}_1 + {demo_str} + {covariates}_3 + Site + Treatment"
    ) |> as.formula()
  } else if (!is.null(covariates)) {
    ancova_formula <- glue::glue(
      "{outcome}_3 ~ {outcome}_1 + {covariates}_3 + Site + Treatment"
    ) |> as.formula()
  } else if (!is.null(demo_vars)) {
    ancova_formula <- glue::glue(
      "{outcome}_3 ~ {outcome}_1 + {demo_str} + Site + Treatment"
    ) |> as.formula()
  } else {
    ancova_formula <- glue::glue(
      "{outcome}_3 ~ {outcome}_1 + Site + Treatment"
    ) |> as.formula()
  }

  # if Site == FALSE, remove Site from all formulas
  if (!site) {
    baseline_formula <- update(baseline_formula, . ~ . - Site)
    w16_formula <- update(w16_formula, . ~ . - Site)
    delta_formula <- update(delta_formula, . ~ . - Site)
    ancova_formula <- update(ancova_formula, . ~ . - Site)
  }

  # fit baseline
  fit_baseline = lm(
    formula = baseline_formula,
    data = data_wide
  )
  # fit week 16
  fit_wk16 <- lm(
    w16_formula,
    data = data_wide
  )
  # fit ancova
  fit <- lm(
    formula = ancova_formula,
    data = data_wide
  )
  # fit delta
  fit_delta <- lm(
    delta_formula,
    data = data_wide
  )


  tx_table <- table(data_wide$Treatment)
  n_placebo <- tx_table["Placebo"]
  n_met <- tx_table["Metformin"]

  fit_tbl <- construct_ancova_table(
    outcome = outcome_label,
    baseline_model = fit_baseline,
    week16_model = fit_wk16,
    delta_model = fit_delta,
    full_model = fit,
    n_p = n_placebo,
    n_m = n_met
  )

  ans <- list(
    "fit" = list(
      baseline = fit_baseline,
      week16 = fit_wk16,
      delta = fit_delta,
      fit = fit
    ),
    "tbl" = fit_tbl,
    "n.met" = n_met,
    "n.placebo" = n_placebo
  )

  return(ans)
}
