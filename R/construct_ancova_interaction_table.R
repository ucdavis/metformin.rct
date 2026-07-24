#' Construct ANCOVA Interaction Summary Table
#'
#' Constructs a summary table from ANCOVA models with Treatment by stratification
#' variable interaction, showing stratified results for each level.
#'
#' @param outcome Outcome label for the table
#' @param baseline_model Fitted baseline model (lm object)
#' @param week16_model Fitted week 16 model (lm object)
#' @param delta_model Fitted delta (change) model (lm object)
#' @param full_model Fitted full ANCOVA model (lm object)
#' @param n_p_level1 Sample size for Placebo in first stratification level
#' @param n_m_level1 Sample size for Metformin in first stratification level
#' @param n_p_level2 Sample size for Placebo in second stratification level
#' @param n_m_level2 Sample size for Metformin in second stratification level
#' @param strata_var Name of the stratification variable. Default is "age_group"
#' @param strata_levels Vector of two stratification level names. Default is
#'   c("Age 6-11", "Age 12+")
#'
#' @return A data frame with stratified ANCOVA results including LS means,
#'   confidence intervals, and interaction term estimates
#'
#' @export

construct_ancova_interaction_table <- function(
    outcome,
    baseline_model,
    week16_model,
    delta_model,
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

  # week 16
  week16_emmeans <- emmeans::emmeans(week16_model, emmeans_formula)
  week16_means <- week16_emmeans |>
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

  # delta
  delta_emmeans <- emmeans::emmeans(delta_model, emmeans_formula)
  delta_means <- delta_emmeans |>
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

  # delta change - contrast within each stratification level
  delta_change <- delta_emmeans |>
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

  # change between groups from ANCOVA - contrast within each stratification level
  change_means <- full_model |>
    emmeans::emmeans(emmeans_formula) |>
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

  # Extract interaction term p-value from full model
  # Construct the interaction term name dynamically
  # The interaction term format is: TreatmentMetformin:strata_varLevel2
  # where Level2 is the second (higher) level of the stratification variable
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
    week16_p = week16_means |>
      dplyr::filter(Treatment == "Placebo", .data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(mean),
    week16_p_ci = week16_means |>
      dplyr::filter(Treatment == "Placebo", .data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(ci),
    p_change = delta_means |>
      dplyr::filter(Treatment == "Placebo", .data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(mean),
    p_change_ci = delta_means |>
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
    week16_m = week16_means |>
      dplyr::filter(Treatment == "Metformin", .data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(mean),
    week16_m_ci = week16_means |>
      dplyr::filter(Treatment == "Metformin", .data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(ci),
    m_change = delta_means |>
      dplyr::filter(Treatment == "Metformin", .data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(mean),
    m_change_ci = delta_means |>
      dplyr::filter(Treatment == "Metformin", .data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(ci),
    # between-group changes
    baseline_contrast_mean = baseline_change_means |>
      dplyr::filter(.data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(mean),
    baseline_contrast_ci = baseline_change_means |>
      dplyr::filter(.data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(ci),
    contrast_mean = change_means |>
      dplyr::filter(.data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(mean),
    contrast_ci = change_means |>
      dplyr::filter(.data[[strata_var]] == strata_levels[1]) |>
      dplyr::pull(ci)
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
    week16_p = week16_means |>
      dplyr::filter(Treatment == "Placebo", .data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(mean),
    week16_p_ci = week16_means |>
      dplyr::filter(Treatment == "Placebo", .data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(ci),
    p_change = delta_means |>
      dplyr::filter(Treatment == "Placebo", .data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(mean),
    p_change_ci = delta_means |>
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
    week16_m = week16_means |>
      dplyr::filter(Treatment == "Metformin", .data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(mean),
    week16_m_ci = week16_means |>
      dplyr::filter(Treatment == "Metformin", .data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(ci),
    m_change = delta_means |>
      dplyr::filter(Treatment == "Metformin", .data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(mean),
    m_change_ci = delta_means |>
      dplyr::filter(Treatment == "Metformin", .data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(ci),
    # between-group changes
    baseline_contrast_mean = baseline_change_means |>
      dplyr::filter(.data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(mean),
    baseline_contrast_ci = baseline_change_means |>
      dplyr::filter(.data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(ci),
    contrast_mean = change_means |>
      dplyr::filter(.data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(mean),
    contrast_ci = change_means |>
      dplyr::filter(.data[[strata_var]] == strata_levels[2]) |>
      dplyr::pull(ci)
  )

  # Combine both stratification levels and rename Strat_Group to strata_label
  ans <- dplyr::bind_rows(tbl_level1, tbl_level2) |>
    dplyr::rename(strata_label = Strat_Group) |>
    dplyr::mutate(
      interaction_est = interaction_est,
      interaction_ci = interaction_ci,
      interaction_p = interaction_pval_fmt
      )

  return(ans)
}
