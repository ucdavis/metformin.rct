#' @export

construct_cgi_ancova_table <- function(
    outcome,
    baseline_model,
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

  # full model
  full_emmeans <- emmeans::emmeans(full_model, ~ Treatment)
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
    dplyr::select(Treatment, mean, ci)

  # full model difference
  full_change <- full_emmeans |> pairs(reverse = TRUE) |>
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
    full_p = full_means |>
      dplyr::filter(Treatment == "Placebo") |>
      dplyr::pull(mean),
    full_p_ci = full_means |>
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
    full_m = full_means |>
      dplyr::filter(Treatment == "Metformin") |>
      dplyr::pull(mean),
    full_m_ci = full_means |>
      dplyr::filter(Treatment == "Metformin") |>
      dplyr::pull(ci),
    # between-group differences
    baseline_contrast_mean = baseline_change_means$mean,
    baseline_contrast_ci = baseline_change_means$ci,
    full_contrast_mean = full_change$mean,
    full_contrast_ci = full_change$ci,
    full_contrast_p = full_change$p.value
  )

  return(ans)
}
