#' @title Clean Metformin data from UC Davis
#' @export

clean_data_ucd <- function(
    dataset,
    unnecessary_vars = c("fmrp_fxsid")) {

  dataset |>
    # remove Test entries
    dplyr::filter(.data$fxs_sts_id != "Test") |>
    # remove unneccessary variables
    dplyr::select(-dplyr::all_of(unnecessary_vars)) |>
    # clean weight, height, and BMI variables
    clean_bmi() |>
    # create race and ethnicity variables
    create_race() |>
    create_ethnicity() |>
    # clean variables from Leiter
    clean_leiter() |>
    # clean variables from peds ql
    clean_pql() |>
    # # clean variables from ADAMS
    clean_adams() |>
    # clean ados variables
    clean_ados() |>
    # clean molecular variables
    clean_molecular() |>
    # create french els
    create_els_french() |>
    # clean numeric variables - set missing codes to NA
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::where(is.numeric),
        ~ clean_numeric(.x)
      )
    ) |>
    # populate childstudyid across all visits from baseline value
    dplyr::group_by(.data$fxs_sts_id) |>
    tidyr::fill(
      .data$childstudyid,
      .direction = "downup"
    ) |>
    # populate sex across all visits from baseline value
    tidyr::fill(
      .data$sex,
      .direction = "downup"
    ) |>
    dplyr::ungroup() |>
    # convert dates to class Date
    clean_dates() |>
    # arrange data by ID and visit
    dplyr::arrange(.data$childstudyid, .data$redcap_event_name)
}
