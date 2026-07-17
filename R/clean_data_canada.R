#' @title Clean Metformin data from Canadian Sites
#' @export

clean_data_canada <- function(
    dataset,
    unnecessary_vars = NULL) {
  dataset |>
    # remove any Test entries
    dplyr::filter(!grepl("test", .data$fxs_sts_id, ignore.case = TRUE)) |>
    # remove unnecessary variables
    dplyr::select(-dplyr::all_of(unnecessary_vars)) |>
    # clean weight, height, and BMI variables
    clean_bmi() |>
    # create race and ethnicity variables
    create_race_canada() |>
    create_ethnicity_canada() |>
    # clean variables from Leiter
    clean_leiter() |>
    # clean variables from peds ql
    clean_pql_canada() |>
    # # clean variables from ADAMS
    clean_adams_canada() |>
    # # clean variables from SNAP-IV
    clean_snap_canada() |>
    # # Clean variables from ABC
    clean_abc_canada() |>
    # clean CSHQ variables
    clean_cshq_canada() |>
    # # clean variables from ELS
    clean_els_canada() |>
    # clean ados variables
    clean_ados_canada() |>
    # create french els var
    create_els_french() |>
    # clean numeric variables - set missing codes to NA
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::where(is.numeric),
        ~ clean_numeric(.x)
      )
    ) |>
    dplyr::group_by(.data$fxs_sts_id) |>
    # populate sex across all visits from baseline value
    tidyr::fill(
      .data$sex,
      .direction = "downup"
    ) |>
    dplyr::ungroup() |>
    # convert dates to class Date
    clean_dates_canada() |>
    # arrange data by ID and visit
    dplyr::arrange(.data$fxs_sts_id, .data$redcap_event_name)
}
