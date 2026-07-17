## clean data UC Davis helpers ##

clean_bmi <- function(
    dataset,
    bmi_var = "bmi",
    other_vars = c("weight_kg", "height_cm")) {
  dataset |>
    dplyr::mutate(
      # clean weight and height
      dplyr::across(
        .cols = dplyr::all_of(other_vars),
        ~ clean_numeric(.x)
      ),
      # clean bmi
      # if height and weight = 999 then calculated bmi = 999/(9.99^2)
      bmi = dplyr::if_else(
        condition = (.data$bmi == (999 / (9.99 ^ 2)) | .data$bmi < 10),
        true = NA_real_,
        false = .data$bmi
      )
    )
}


# functions to create race and ehtnicity variables
create_race <- function(
    dataset,
    race_vars = c(
      "race___1", "race___2", "race___3", "race___4",
      "race___5", "race___6", "race___7"
    )) {
  dataset |>
    dplyr::group_by(.data$fxs_sts_id) |>
    tidyr::fill(
      dplyr::all_of(race_vars)
    ) |>
    dplyr::ungroup() |>
    # convert race variables back to 0/1
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(race_vars),
        ~ dplyr::if_else(
          condition = .x == "Checked",
          true = 1,
          false = 0
        )
      )
    ) |>
    # create Race variable
    dplyr::mutate(
      Race = dplyr::case_when(
        (rowSums(dplyr::across(dplyr::all_of(race_vars))) > 1 |
           race___4 == 1 | race___5 == 1 | race___6 == 1 | race___7 == 1) ~
          "Other",
        race___1 == 1 ~ "White",
        race___2 == 1 ~ "Black",
        race___3 == 1 ~ "Asian"
      ) |>
        factor(
          levels = c(
            "White", "Black", "Asian", "Other"
          )
        )
    )
}

create_ethnicity <- function(
    dataset,
    ethn_vars = c(
      "ethn___1", "ethn___2", "ethn___3", "ethn___4",
      "ethn___5", "ethn___6", "ethn___7", "ethn_spec"
    )) {

  # vector of checked/unchecked ehtnicity variables to convert to numeric
  ethn_num <- ethn_vars[ethn_vars != "ethn_spec"]

  dataset |>
    # convert ethnicity variables back to 0/1
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(ethn_num),
        ~ dplyr::if_else(
          condition = .x == "Checked",
          true = 1,
          false = 0
        )
      ),
      ethn_spec = dplyr::na_if(.data$ethn_spec, "")
    ) |>
    dplyr::group_by(.data$fxs_sts_id) |>
    tidyr::fill(
      dplyr::all_of(ethn_vars),
      .direction = "down"
    ) |>
    dplyr::ungroup() |>
    # update to 3 categories: Hisp/Not Hisp/Other/Unknown|Not Reported
    dplyr::mutate(
      sum_ethn = rowSums(
        dplyr::across(.cols = c(.data$ethn___1:.data$ethn___7))
      ),
      Ethnicity = dplyr::case_when(
        .data$ethn___1 == 1 ~ "Hispanic/Latino",
        (.data$ethn___7 == 1 | .data$sum_ethn == 0) ~ "Unknown/Not Reported",
        .default = "Non-Hispanic/Latino"
      ) |>
        factor(
          levels = c(
            "Hispanic/Latino", "Non-Hispanic/Latino", "Unknown/Not Reported"
          )
        )
    )
}

# clean leiter variables
clean_leiter <- function(
    dataset,
    vars = c(
      "leit3_fg_rs", "leit3_fc_raw1_8", "leit3_fc_raw9_15", "leit3_fc_rstotal",
      "leit3_ca_rs1_7", "leit3_ca_rs8_17", "leit3_so_rs", "leit3_vp_rs",
      "leit3_fg_sc_rs", "leit3_fc_sc_rs", "leit3_ca_sc_rs", "leit3_so_sc_rs",
      "leit3_vp_sc_rs", "leit3_fg_sc_ss", "leit3_fc_sc_ss", "leit3_ca_sc_ss",
      "leit3_so_sc_ss", "leit3_vp_sc_ss", "leit3_sum_sc_ss", "leit3_nv_iq"
    )) {
  dataset |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::any_of(vars),
        ~ as.numeric(.x)
      )
    )
}

# clean PedsQL variables
clean_pql <- function(
    dataset,
    vars = c(
      "pql_pf_tot", "pql_ef_tot", "pql_socf_tot", "pql_schf_tot",
      "pql_pf_tot_cor", "pql_ef_tot_cor", "pql_socf_tot_cor", "pql_schf_tot_cor"
    )) {
  dataset |>
    # set "" strings to NA_character_
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::where(is.character),
        ~ ifelse(.x == "", NA_character_, .x)
      )
    ) |>
    # if Corrected variable is not missing, update Total var value
    dplyr::mutate(
      pql_pf_tot = ifelse(
        test = !is.na(.data$pql_pf_tot_cor),
        yes = .data$pql_pf_tot_cor,
        no = .data$pql_pf_tot
      ),
      pql_ef_tot = ifelse(
        test = !is.na(.data$pql_ef_tot_cor),
        yes = .data$pql_ef_tot_cor,
        no = .data$pql_ef_tot
      ),
      pql_socf_tot = ifelse(
        test = !is.na(.data$pql_socf_tot_cor),
        yes = .data$pql_socf_tot_cor,
        no = .data$pql_socf_tot
      ),
      pql_schf_tot = ifelse(
        test = !is.na(.data$pql_schf_tot_cor),
        yes = .data$pql_schf_tot_cor,
        no = .data$pql_schf_tot
      )
    ) |>
    # convert all pedsql variables to numeric
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(vars),
        ~ as.numeric(.x) |> suppressWarnings()
      )
    ) |>
    # remove corrected variables
    dplyr::select(
      -c("pql_pf_tot_cor", "pql_ef_tot_cor", "pql_socf_tot_cor",
         "pql_schf_tot_cor")
    )
}

# clean adams
clean_adams <- function(
    dataset,
    adams_vars = c(
      "adams_manic", "adams_depress", "adams_avoid",
      "adams_anxious", "adams_obsess"
    )) {
  dataset |>
    # make sure ADAMS variables are numeric
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(adams_vars),
        ~ as.numeric(.x) |> suppressWarnings()
      )
    )
}

# function to process ados variables
clean_ados <- function(
    dataset,
    ados_vars = c(
      "ados_age", "a2_cs_as", "a2_cs_acs",
      "a2_m1_satotal", "a2_m1_rrbtotal", "a2_m1_total",
      "a2_m2_satotal", "a2_m2_rrbtotal", "a2_m2_total",
      "a2_m3_satotal", "a2_m3_rrbtotal", "a2_m3_total",
      "a2_m4_sbri_total", "a2_m4_comtotal", "a2_m4_rsi_total", "a2_m4_csi_total"
    )) {
  dataset |>
    # make sure all ados variables are numeric
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(ados_vars),
        ~ as.numeric(.x) |> suppressWarnings()
      )
    ) |>
    # create common a2_social and a2_restricted variable across modules
    dplyr::mutate(
      a2_social = dplyr::case_when(
        !is.na(.data$a2_m1_satotal) ~ .data$a2_m1_satotal,
        !is.na(.data$a2_m2_satotal) ~ .data$a2_m2_satotal,
        !is.na(.data$a2_m3_satotal) ~ .data$a2_m3_satotal,
        !is.na(.data$a2_m4_csi_total) ~ .data$a2_m4_csi_total
      ),
      a2_restricted = dplyr::case_when(
        !is.na(.data$a2_m1_rrbtotal) ~ .data$a2_m1_rrbtotal,
        !is.na(.data$a2_m2_rrbtotal) ~ .data$a2_m2_rrbtotal,
        !is.na(.data$a2_m3_rrbtotal) ~ .data$a2_m3_rrbtotal,
        !is.na(.data$a2_m4_sbri_total) ~ .data$a2_m4_sbri_total,
      )
    )
}

# clean molecular variables
clean_molecular <- function(
    dataset,
    molec_vars = c(
      "tassone_cat", "tassone_cgg", "tassone_meth", "tassone_ar",
      "tassone_fmr1", "tassone_fmr1_se", "tassone_cyfip1", "tassone_cyfip1_se",
      "tassone_cyp450_1", "tassone_cyp450_2", "tassone_cyp450_3",
      "tassone_mmp9", "tassone_s6k", "tassone_eif4e", "fmrp_fxsid", "fmrp_rhmc",
      "fmrp_std_rhmc", "fmrp_se_rhmc", "fmrp_cv"
    )) {
  dataset |>
    dplyr::mutate(
      # tassone_cat has mixed cases, set lower
      tassone_cat = tolower(.data$tassone_cat) |>
        factor(
          levels = c(
            "full mutation",
            "meth mosaic",
            "size mosaic"
          )
        ),
      # convert fmrp_cv to numeric
      fmrp_cv = as.numeric(.data$fmrp_cv) |> suppressWarnings()
    )
}

## create french ELS indicator ##
create_els_french <- function(
    dataset,
    fr_id = c("FXSMET-18-3-001",
              "FXSMET-18-3-003",
              "FXSMET-18-3-004",
              "FXSMET-18-3-006",
              "FXSMET-18-3-007",
              "FXSMET-18-3-008",
              "FXSMET-18-3-009",
              "FXSMET-18-3-011",
              "FXSMET-18-3-018",
              "FXSMET-18-3-019",
              "FXSMET-18-3-020",
              "FXSMET-18-3-024",
              "FXSMET-18-3-028",
              "FXSMET-18-3-029",
              "FXSMET-18-3-030")) {
  dataset |>
    dplyr::mutate(
      # create Fr version indicator
      els_french = dplyr::if_else(
        condition = .data$fxs_sts_id %in% fr_id,
        true = 1,
        false = 0
      )
    )
}

# function to convert 'date' variables to class date
clean_dates <- function(
    dataset,
    date_vars = c(
      "nar_doe", "con_doe"
    )) {
  dataset |>
    dplyr::mutate(
      dplyr::across(
        .cols = c(dplyr::any_of(date_vars), tidyr::contains("date")),
        ~ as.Date(.x)
      )
    )
}

# function to replace missing codes with NA for numeric variables
clean_numeric <- function(
    x,
    missing_codes = c(
      "999",
      "888",
      "777",
      extra_codes
    ),
    extra_codes = NULL) {
  dplyr::if_else(
    condition = x %in% missing_codes,
    true = NA_real_,
    false = x |> as.numeric() |> suppressWarnings()
  )
}
