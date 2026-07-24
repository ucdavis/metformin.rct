#' @title Generate summary statistics for demographics and characteristics
#'  limited to subjects included in ANCOVA analysis
#' @description Creates demographic summary tables stratified by treatment group
#'  and/or age group, limited to subjects with complete ANCOVA data for specified outcomes.
#'  This ensures the demographic table reflects the same population analyzed in ANCOVA.
#'  The function mimics the exact filtering logic of fit_ancova() to identify subjects
#'  with complete data for outcome (baseline and Week 16), covariates, Treatment, and Site.
#' @param dataset A data frame containing the analytic dataset
#' @param outcome_vars Vector of outcome variable names used in ANCOVA (e.g., ELS outcomes)
#' @param ancova_demo_vars Vector of demographic/covariate variables used in ANCOVA model
#'   (default: c("sex", "demo_age_center", "leit3_nv_iq_center"))
#' @param demo_vars Vector of demographic variable names to summarize in the table
#' @param demo_labels Named list of labels for demographic variables
#' @param age_stratify Logical, whether to stratify by age group (6-11 vs 12+)
#' @param demo_age_var Character, name of the age variable (default: "demo_age")
#' @keywords internal
#' @export
#'
summarize_demographics_ancova <- function(
    dataset,
    outcome_vars,
    ancova_demo_vars = c("sex", "demo_age_center", "leit3_nv_iq_center"),
    demo_vars = c("demo_age", "sex", "Race", "Ethnicity", "weight_kg",
                  "leit3_nv_iq", "cgis_soi_fct", "Molecular.Category"),
    demo_labels = list(
      "demo_age" = "Age (years)",
      "sex" = "Sex",
      "Race" = "Race",
      "Ethnicity" = "Ethnicity",
      "weight_kg" = "Weight (kg)",
      "leit3_nv_iq" = "IQ",
      "cgis_soi_fct" = "CGIS Severity of Illness",
      "Molecular.Category" = "Molecular Category"
    ),
    age_stratify = FALSE,
    demo_age_var = "demo_age"
) {
  # Filter to baseline visit
  baseline_data <- dataset |>
    dplyr::filter(redcap_event_name == "Baseline Visit (Visit 1)")

  # Identify subjects with complete ANCOVA data by mimicking fit_ancova() logic
  # For each outcome, pivot to wide format and check for complete cases
  complete_subjects <- list()

  for (outcome in outcome_vars) {
    # Mimic fit_ancova() data preparation
    data_wide <- dataset |>
      dplyr::select(
        fxs_sts_id, redcap_event_name, Treatment, Site,
        dplyr::any_of(c(outcome, ancova_demo_vars))
      ) |>
      dplyr::filter(
        redcap_event_name %in% c("Baseline Visit (Visit 1)",
                                 "Week 16 (Visit 3/Early Term)")
      ) |>
      dplyr::mutate(
        redcap_event_name = dplyr::case_when(
          redcap_event_name == "Baseline Visit (Visit 1)" ~ 1,
          redcap_event_name == "Week 16 (Visit 3/Early Term)" ~ 3
        )
      ) |>
      tidyr::pivot_wider(
        id_cols = c(fxs_sts_id, Treatment, Site, dplyr::any_of(ancova_demo_vars)),
        names_from = redcap_event_name,
        values_from = outcome,
        names_prefix = glue::glue("{outcome}_")
      )

    # Get subjects with complete data (no NAs in outcome_1, outcome_3, covariates, Treatment, Site)
    outcome_1 <- paste0(outcome, "_1")
    outcome_3 <- paste0(outcome, "_3")

    complete_for_outcome <- data_wide |>
      dplyr::filter(
        !is.na(.data[[outcome_1]]) & !is.na(.data[[outcome_3]]) &
        !is.na(Treatment) & !is.na(Site)
      ) |>
      # Also need complete covariate data
      tidyr::drop_na(dplyr::any_of(ancova_demo_vars)) |>
      dplyr::pull(fxs_sts_id)

    complete_subjects[[outcome]] <- complete_for_outcome
  }

  # Combine subjects with complete data for any ELS outcome
  all_complete_subjects <- unique(unlist(complete_subjects))

  # Filter baseline data to only include subjects with complete ANCOVA data
  baseline_ancova <- baseline_data |>
    dplyr::filter(fxs_sts_id %in% all_complete_subjects)

  if (!age_stratify) {
    # Generate overall table by treatment group
    tbl <- baseline_ancova |>
      gtsummary::tbl_summary(
        by = Treatment,
        label = demo_labels,
        type = c(demo_age, weight_kg, leit3_nv_iq) ~ "continuous",
        statistic = c(demo_age, weight_kg, leit3_nv_iq) ~ c("{mean} ({sd})"),
        include = dplyr::all_of(demo_vars),
        digits = list(c(demo_age, weight_kg, leit3_nv_iq) ~ c(1, 1, 0, 0, 0)),
        missing_text = "Missing"
      ) |>
      gtsummary::add_overall() |>
      gtsummary::add_p(
        pvalue_fun = gtsummary::label_style_pvalue(digits = 3)
      ) |>
      gtsummary::separate_p_footnotes() |>
      gtsummary::modify_table_body(
        ~ .x |>
          dplyr::filter(
            !(row_type == "level" &
                dplyr::if_all(
                  .cols = gtsummary::all_stat_cols(),
                  ~ .x == "0 (0%)"
                )
            )
          )
      )

    ans <- tbl
  } else {
    # Add age group variable
    baseline_ancova <- baseline_ancova |>
      dplyr::mutate(
        age_group = dplyr::case_when(
          .data[[demo_age_var]] < 12 ~ "Age 6-11",
          .data[[demo_age_var]] >= 12 ~ "Age 12+"
        ) |>
          factor(levels = c("Age 6-11", "Age 12+"))
      )

    # Generate tables stratified by age group
    # Table 1: All subjects
    tbl_all <- baseline_ancova |>
      gtsummary::tbl_summary(
        by = Treatment,
        label = demo_labels,
        type = c(demo_age, weight_kg, leit3_nv_iq) ~ "continuous",
        statistic = c(demo_age, weight_kg, leit3_nv_iq) ~ c("{mean} ({sd})"),
        include = dplyr::all_of(demo_vars),
        digits = list(c(demo_age, weight_kg, leit3_nv_iq) ~ c(1, 1, 0, 0, 0)),
        missing_text = "Missing"
      ) |>
      gtsummary::add_overall(last = TRUE) |>
      gtsummary::add_p(
        pvalue_fun = gtsummary::label_style_pvalue(digits = 3)
      ) |>
      gtsummary::separate_p_footnotes() |>
      gtsummary::modify_caption("All Subjects (ANCOVA complete cases)") |>
      gtsummary::modify_table_body(
        ~ .x |>
          dplyr::filter(
            !(row_type == "level" &
                dplyr::if_all(
                  .cols = gtsummary::all_stat_cols(),
                  ~ .x == "0 (0%)"
                )
            )
          )
      )

    # Table 2: Age 6-11
    tbl_6_11 <- baseline_ancova |>
      dplyr::filter(age_group == "Age 6-11") |>
      gtsummary::tbl_summary(
        by = Treatment,
        label = demo_labels,
        type = c(demo_age, weight_kg, leit3_nv_iq) ~ "continuous",
        statistic = c(demo_age, weight_kg, leit3_nv_iq) ~ c("{mean} ({sd})"),
        include = dplyr::all_of(demo_vars),
        digits = list(c(demo_age, weight_kg, leit3_nv_iq) ~ c(1, 1, 0, 0, 0)),
        missing_text = "Missing"
      ) |>
      gtsummary::add_overall(last = TRUE) |>
      gtsummary::add_p(
        pvalue_fun = gtsummary::label_style_pvalue(digits = 3)
      ) |>
      gtsummary::separate_p_footnotes() |>
      gtsummary::modify_caption("Age 6-11 (ANCOVA complete cases)") |>
      gtsummary::modify_table_body(
        ~ .x |>
          dplyr::filter(
            !(row_type == "level" &
                dplyr::if_all(
                  .cols = gtsummary::all_stat_cols(),
                  ~ .x == "0 (0%)"
                )
            )
          )
      )

    # Table 3: Age 12+
    tbl_12_up <- baseline_ancova |>
      dplyr::filter(age_group == "Age 12+") |>
      gtsummary::tbl_summary(
        by = Treatment,
        label = demo_labels,
        type = c(demo_age, weight_kg, leit3_nv_iq) ~ "continuous",
        statistic = c(demo_age, weight_kg, leit3_nv_iq) ~ c("{mean} ({sd})"),
        include = dplyr::all_of(demo_vars),
        digits = list(c(demo_age, weight_kg, leit3_nv_iq) ~ c(1, 1, 0, 0, 0)),
        missing_text = "Missing"
      ) |>
      gtsummary::add_overall(last = TRUE) |>
      gtsummary::add_p(
        pvalue_fun = gtsummary::label_style_pvalue(digits = 3)
      ) |>
      gtsummary::separate_p_footnotes() |>
      gtsummary::modify_caption("Age 12+ (ANCOVA complete cases)") |>
      gtsummary::modify_table_body(
        ~ .x |>
          dplyr::filter(
            !(row_type == "level" &
                dplyr::if_all(
                  .cols = gtsummary::all_stat_cols(),
                  ~ .x == "0 (0%)"
                )
              )
          )
      )

    ans <- list(
      "all" = tbl_all,
      "age_6_11" = tbl_6_11,
      "age_12_up" = tbl_12_up,
      "merged" = gtsummary::tbl_merge(
        tbls = list(tbl_all, tbl_6_11, tbl_12_up),
        tab_spanner = c("**All**", "**Age 6-11**", "**Age 12+**")
      )
    )
  }

  return(ans)
}
