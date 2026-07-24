#' Construct CGI ANCOVA Interaction Summary Table
#'
#' Constructs a simplified summary table from CGI ANCOVA models with Treatment by
#' stratification variable interaction. Only includes baseline and full model
#' results (no week 16 or delta models).
#'
#' @param outcome Outcome label for the table
#' @param baseline_model Fitted baseline model (lm object)
#' @param full_model Fitted full ANCOVA model (lm object)
#' @param n_p_level1 Sample size for Placebo in first stratification level
#' @param n_m_level1 Sample size for Metformin in first stratification level
#' @param n_p_level2 Sample size for Placebo in second stratification level
#' @param n_m_level2 Sample size for Metformin in second stratification level
#' @param strata_var Name of the stratification variable. Default is "age_group"
#' @param strata_levels Vector of two stratification level names. Default is
#'   c("Age 6-11", "Age 12+")
#' @param strata_label Label for the stratification variable
#'   (used in output tables). Default is "Age Group".
#'
#' @return A data frame with stratified CGI ANCOVA results including LS means,
#'   confidence intervals, and interaction term estimates
#'
#' @export

construct_cgi_ancova_interaction_table <- function(
    outcome,
    baseline_model,
    full_model,
    n_p_level1,
    n_m_level1,
    n_p_level2,
    n_m_level2,
    strata_var = "age_group",
    strata_levels = c("Age 6-11", "Age 12+"),
    strata_label = "Age Group") {

  # get LS means stratified by Treatment and stratification variable #
  # Create the emmeans formula dynamically
  emmeans_formula <- as.formula(paste("~ Treatment |", strata_var))

  # baseline
  baseline_emmeans <- emmeans::emmeans(baseline_model, emmeans_formula)
  baseline_means <- baseline_emmeans |>
    tibble::as_tibble() |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::where(is.numeric),
        ~ .x |> gtsummary::style_number(digits = 2)
      ),
      ci = glue::glue("({lower.CL}, {upper.CL})")
    ) |>
    dplyr::rename(mean = emmean) |>
    dplyr::select(dplyr::all_of(strata_var), Treatment, mean, ci)

  # baseline change - contrast within each stratification level
  baseline_change_means <- baseline_emmeans |>
    pairs(reverse = TRUE) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      moe = SE * qnorm(0.975),
      ci = glue::glue(
        "({(estimate - moe) |> gtsummary::style_number(digits = 2)}, ",
        "{(estimate + moe) |> gtsummary::style_number(digits = 2)})"),
      dplyr::across(
        .cols = dplyr::where(is.numeric),
        ~ .x |> gtsummary::style_number(digits = 2)
      )
    ) |>
    dplyr::rename(mean = estimate) |>
    dplyr::select(dplyr::all_of(strata_var), contrast, mean, ci)

  # full model
  full_emmeans <- emmeans::emmeans(full_model, emmeans_formula)
  full_means <- full_emmeans |>
    tibble::as_tibble() |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::where(is.numeric),
        ~ .x |> gtsummary::style_number(digits = 2)
      ),
      ci = glue::glue("({lower.CL}, {upper.CL})")
    ) |>
    dplyr::rename(mean = emmean) |>
    dplyr::select(dplyr::all_of(strata_var), Treatment, mean, ci)

  # full model change - contrast within each stratification level
  full_change <- full_emmeans |>
    pairs(reverse = TRUE) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      moe = SE * qnorm(0.975),
      ci = glue::glue(
        "({(estimate - moe) |> gtsummary::style_number(digits = 2)}, ",
        "{(estimate + moe) |> gtsummary::style_number(digits = 2)})"),
      dplyr::across(
        .cols = dplyr::where(is.numeric),
        ~ .x |> gtsummary::style_number(digits = 2)
      )
    ) |>
    dplyr::rename(mean = estimate) |>
    dplyr::select(dplyr::all_of(strata_var), contrast, mean, ci)

  # Extract interaction term from full model
  # Construct the interaction term name dynamically
  interaction_term_name <- paste0("TreatmentMetformin:", strata_var, strata_levels[2])

  interaction_est <- summary(full_model)$coefficients[
    interaction_term_name, "Estimate"
  ] |> gtsummary::style_number(digits = 2)
  interaction_ci <-
    paste0(
      "(",
      confint(full_model)[interaction_term_name, ] |>
        gtsummary::style_number(digits = 2) |>
        paste(
          collapse = ", "
        ),
      ")"
    )

  interaction_pval <- summary(full_model)$coefficients[
    interaction_term_name, "Pr(>|t|)"
  ]
  interaction_pval_fmt <- gtsummary::style_pvalue(interaction_pval, digits = 3)

  # construct table for first stratification level #
  tbl_level1 <- data.frame(
    # outcome label
    Outcome = outcome,
    Strat_Group = strata_levels[1],
    # placebo
    n_p = n_p_level1,
    baseline_p = baseline_means |>
      dplyr::filter(Treatment == "Placebo", .data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(mean),
    baseline_p_ci = baseline_means |>
      dplyr::filter(Treatment == "Placebo", .data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(ci),
    full_p = full_means |>
      dplyr::filter(Treatment == "Placebo", .data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(mean),
    full_p_ci = full_means |>
      dplyr::filter(Treatment == "Placebo", .data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(ci),
    # metformin
    n_m = n_m_level1,
    baseline_m = baseline_means |>
      dplyr::filter(Treatment == "Metformin", .data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(mean),
    baseline_m_ci = baseline_means |>
      dplyr::filter(Treatment == "Metformin", .data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(ci),
    full_m = full_means |>
      dplyr::filter(Treatment == "Metformin", .data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(mean),
    full_m_ci = full_means |>
      dplyr::filter(Treatment == "Metformin", .data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(ci),
    # between-group differences
    baseline_contrast_mean = baseline_change_means |>
      dplyr::filter(.data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(mean),
    baseline_contrast_ci = baseline_change_means |>
      dplyr::filter(.data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(ci),
    full_contrast_mean = full_change |>
      dplyr::filter(.data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(mean),
    full_contrast_ci = full_change |>
      dplyr::filter(.data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(ci),
    # interaction term
    interaction_est = interaction_est,
    interaction_ci = interaction_ci,
    interaction_p = interaction_pval_fmt
  )

  # construct table for second stratification level #
  tbl_level2 <- data.frame(
    # outcome label
    Outcome = outcome,
    Strat_Group = strata_levels[2],
    # placebo
    n_p = n_p_level2,
    baseline_p = baseline_means |>
      dplyr::filter(Treatment == "Placebo", .data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(mean),
    baseline_p_ci = baseline_means |>
      dplyr::filter(Treatment == "Placebo", .data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(ci),
    full_p = full_means |>
      dplyr::filter(Treatment == "Placebo", .data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(mean),
    full_p_ci = full_means |>
      dplyr::filter(Treatment == "Placebo", .data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(ci),
    # metformin
    n_m = n_m_level2,
    baseline_m = baseline_means |>
      dplyr::filter(Treatment == "Metformin", .data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(mean),
    baseline_m_ci = baseline_means |>
      dplyr::filter(Treatment == "Metformin", .data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(ci),
    full_m = full_means |>
      dplyr::filter(Treatment == "Metformin", .data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(mean),
    full_m_ci = full_means |>
      dplyr::filter(Treatment == "Metformin", .data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(ci),
    # between-group differences
    baseline_contrast_mean = baseline_change_means |>
      dplyr::filter(.data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(mean),
    baseline_contrast_ci = baseline_change_means |>
      dplyr::filter(.data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(ci),
    full_contrast_mean = full_change |>
      dplyr::filter(.data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(mean),
    full_contrast_ci = full_change |>
      dplyr::filter(.data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(ci),
    # interaction term
    interaction_est = interaction_est,
    interaction_ci = interaction_ci,
    interaction_p = interaction_pval_fmt
  )

  # combine tables and rename Strat_Group to strata_label
  ans <- dplyr::bind_rows(tbl_level1, tbl_level2) |>
    dplyr::rename(strata_label = Strat_Group)

  return(ans)
}
