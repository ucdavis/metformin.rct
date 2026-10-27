#' @export

construct_ancova_table <- function(
    outcome,
    baseline_model,
    week16_model,
    delta_model,
    full_model,
    n_p,
    n_m) {
  # get LS means #
  # baseline
  baseline_emmeans <- emmeans::emmeans(baseline_model, ~ Treatment)
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
    dplyr::select(Treatment, mean, ci)

  baseline_change_means <- baseline_emmeans |> pairs(reverse = TRUE) |>
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
    dplyr::select(contrast, mean, ci)
  # week 16
  week16_emmeans <- emmeans::emmeans(week16_model, ~ Treatment)
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
    dplyr::select(Treatment, mean, ci)
  # delta
  delta_emmeans <- emmeans::emmeans(delta_model, ~ Treatment)
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
    dplyr::select(Treatment, mean, ci)

  delta_change <- delta_emmeans |> pairs(reverse = TRUE) |>
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
    dplyr::select(contrast, mean, ci)

  # change between groups
  change_means <- full_model |>
    emmeans::emmeans(~ Treatment) |>
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
    dplyr::select(contrast, mean, ci, p.value)

  # construct table #
  ans <- data.frame(
    # outcome label
    Outcome = outcome,
    # placebo
    n_p = n_p,
    baseline_p = baseline_means |>
      dplyr::filter(Treatment == "Placebo") |>
      dplyr::pull(mean),
    baseline_p_ci = baseline_means |>
      dplyr::filter(Treatment == "Placebo") |>
      dplyr::pull(ci),
    week16_p = week16_means |>
      dplyr::filter(Treatment == "Placebo") |>
      dplyr::pull(mean),
    week16_p_ci = week16_means |>
      dplyr::filter(Treatment == "Placebo") |>
      dplyr::pull(ci),
    # want within-group change
    p_change = delta_means |>
      dplyr::filter(Treatment == "Placebo") |>
      dplyr::pull(mean),
    p_change_ci = delta_means |>
      dplyr::filter(Treatment == "Placebo") |>
      dplyr::pull(ci),
    # metformin
    n_m = n_m,
    baseline_m = baseline_means |>
      dplyr::filter(Treatment == "Metformin") |>
      dplyr::pull(mean),
    baseline_m_ci = baseline_means |>
      dplyr::filter(Treatment == "Metformin") |>
      dplyr::pull(ci),
    week16_m = week16_means |>
      dplyr::filter(Treatment == "Metformin") |>
      dplyr::pull(mean),
    week16_m_ci = week16_means |>
      dplyr::filter(Treatment == "Metformin") |>
      dplyr::pull(ci),
    # within-group change
    m_change = delta_means |>
      dplyr::filter(Treatment == "Metformin") |>
      dplyr::pull(mean),
    m_change_ci = delta_means |>
      dplyr::filter(Treatment == "Metformin") |>
      dplyr::pull(ci),
    # between-group changes
    baseline_contrast_mean = baseline_change_means$mean,
    baseline_contrast_ci = baseline_change_means$ci,
    contrast_mean = change_means$mean,
    contrast_ci = change_means$ci,
    contrast_p = change_means$p.value
  )

  return(ans)
}
