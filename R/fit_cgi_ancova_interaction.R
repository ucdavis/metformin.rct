#' Fit CGI ANCOVA Models with Treatment by Stratification Variable Interaction
#'
#' Fits simplified ANCOVA models for CGI outcomes with Treatment by stratification
#' variable interaction. Only fits baseline and full models (no week 16 or delta
#' models like the standard ANCOVA interaction function).
#'
#' @param dataset Dataset containing the variables
#' @param outcome Name of the outcome variable
#' @param outcome_label Label for the outcome (used in output tables)
#' @param demo_vars Vector of demographic covariate names. Default is
#'   c("sex", "demo_age_center", "leit3_nv_iq_center")
#' @param strata_var Name of the stratification variable (must have exactly
#'   two levels). Default is "age_group" for age-stratified analyses, but can
#'   be any two-level categorical variable (e.g., "Molecular.Category")
#' @param strata_label Label for the stratification variable
#'   (used in output tables). Default is "Age Group".
#'
#' @return A list containing:
#'   \item{fit}{Full ANCOVA model with interaction}
#'   \item{tbl}{Summary table with stratified results}
#'   \item{n.met.level1}{N for Metformin in first stratification level}
#'   \item{n.placebo.level1}{N for Placebo in first stratification level}
#'   \item{n.met.level2}{N for Metformin in second stratification level}
#'   \item{n.placebo.level2}{N for Placebo in second stratification level}
#'   \item{strata_var}{Name of the stratification variable}
#'   \item{strata_levels}{Vector of two stratification level names}
#'
#' @export

fit_cgi_ancova_interaction <- function(
    dataset,
    outcome,
    outcome_label,
    demo_vars = c("sex", "demo_age_center", "leit3_nv_iq_center"),
    strata_var = "age_group",
    strata_label = "Age Group") {
  # subset data
  data <- dataset |>
    dplyr::select(
      fxs_sts_id, redcap_event_name, Treatment, Site,
      dplyr::any_of(c(outcome, demo_vars, strata_var))
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
      id_cols = c(fxs_sts_id, Treatment, Site, dplyr::all_of(strata_var), dplyr::any_of(demo_vars)),
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
        !is.na(.data[[outcome_3]]) &
        !is.na(.data[[strata_var]])
    )

  # formula #
  # Helper to build covariate string
  demo_str <- if (!is.null(demo_vars)) {
    paste(demo_vars, collapse = " + ")
  } else {
    NULL
  }

  # baseline model with interaction
  if (!is.null(demo_vars)) {
    baseline_formula <- glue::glue(
      "{outcome}_1 ~ {demo_str} + Site + Treatment * {strata_var}"
    ) |> as.formula()
  } else {
    baseline_formula <- glue::glue(
      "{outcome}_1 ~ Site + Treatment * {strata_var}"
    ) |> as.formula()
  }

  # full ancova model with interaction
  if (!is.null(demo_vars)) {
    ancova_formula <- glue::glue(
      "{outcome}_3 ~ {outcome}_1 + {demo_str} + Site + Treatment * {strata_var}"
    ) |> as.formula()
  } else {
    ancova_formula <- glue::glue(
      "{outcome}_3 ~ {outcome}_1 + Site + Treatment * {strata_var}"
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

  # Get n by treatment and stratification variable
  tx_strata_table <- table(data_wide$Treatment, data_wide[[strata_var]])

  # Get the unique levels of the stratification variable (sorted)
  strata_levels <- sort(unique(data_wide[[strata_var]]))

  # Extract n for each combination
  n_placebo_level1 <- tx_strata_table["Placebo", strata_levels[1]]
  n_met_level1 <- tx_strata_table["Metformin", strata_levels[1]]
  n_placebo_level2 <- tx_strata_table["Placebo", strata_levels[2]]
  n_met_level2 <- tx_strata_table["Metformin", strata_levels[2]]

  fit_tbl <- construct_cgi_ancova_interaction_table(
    outcome = outcome_label,
    baseline_model = fit_baseline,
    full_model = fit_full,
    n_p_level1 = n_placebo_level1,
    n_m_level1 = n_met_level1,
    n_p_level2 = n_placebo_level2,
    n_m_level2 = n_met_level2,
    strata_var = strata_var,
    strata_levels = strata_levels,
    strata_label = strata_label
  )

  ans <- list(
    "fit" = fit_full,
    "tbl" = fit_tbl,
    "n.met.level1" = n_met_level1,
    "n.placebo.level1" = n_placebo_level1,
    "n.met.level2" = n_met_level2,
    "n.placebo.level2" = n_placebo_level2,
    "strata_var" = strata_var,
    "strata_levels" = strata_levels
  )

  return(ans)
}
