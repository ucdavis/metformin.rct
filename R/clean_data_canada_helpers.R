## Clean data Canada helpers ##
#### clean weight/height/bmi variables ####
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

# functions to create race and ethnicity variables
create_race_canada <- function(
    dataset,
    race_vars = c(
      "race___1", "race___2", "race___3", "race___4",
      "race___5", "race___6", "race___7", "race___8", "race___9"
    )) {
  races_sum <- race_vars[1:7] # nolintr: variable is used but lintr unsure

  dataset |>
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
        (rowSums(dplyr::across(dplyr::all_of(races_sum))) > 1 |
           race___4 == 1 | race___5 == 1 | race___6 == 1 | race___7 == 1 |
           race___8 == 1) ~
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

create_ethnicity_canada <- function(
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
      # set empty string to NA
      ethn_spec = dplyr::na_if(.data$ethn_spec, "")
    ) |>
    # fill in ethnicity variables for Visits 2 and 3
    dplyr::group_by(.data$fxs_sts_id) |>
    tidyr::fill(
      c(.data$ethn___1:.data$ethn_spec),
      .direction = "down"
    ) |>
    dplyr::ungroup() |>
    # create Ethnicity variable
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
clean_pql_canada <- function(dataset) {
  # extract names for each pql domain #
  # all pql variables, excluding totals and date
  pql_vars <- dataset |>
    dplyr::select(dplyr::contains("pql")) |>
    # exclude total
    dplyr::select(-dplyr::contains("tot")) |>
    # exclude date
    dplyr::select(-dplyr::contains("date")) |>
    names()

  pql_totals <- dataset |>
    dplyr::select(dplyr::contains("pql")) |>
    # keep only totals
    dplyr::select(dplyr::contains("tot")) |>
    names()

  # physical functioning
  pql_physical <- pql_vars[stringr::str_detect(pql_vars, "pql_pf")]

  # emotional functioning
  pql_emotional <- pql_vars[stringr::str_detect(pql_vars, "pql_ef")]

  # social functioning
  pql_social <- pql_vars[stringr::str_detect(pql_vars, "pql_socf")]

  # school functioning
  pql_school <- pql_vars[stringr::str_detect(pql_vars, "pql_schf")]

  dataset |>
    # remove old totals
    dplyr::select(-dplyr::all_of(pql_totals)) |>
    # clean numeric to handle 777/888/999
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(pql_vars),
        ~ clean_numeric(.x)
      )
    ) |>
    # convert 0-4 scale to 100-0 scale to match UC Davis
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(pql_vars),
        ~ dplyr::case_when(
          .x == 0 ~ 100,
          .x == 1 ~ 75,
          .x == 2 ~ 50,
          .x == 3 ~ 25,
          .x == 4 ~ 0
        )
      )
    ) |>
    # calculate domain means
    dplyr::mutate(
      pql_pf_tot = rowMeans(
        dplyr::across(dplyr::all_of(pql_physical)), na.rm = TRUE
      ),
      pql_ef_tot = rowMeans(
        dplyr::across(dplyr::all_of(pql_emotional)), na.rm = TRUE
      ),
      pql_socf_tot = rowMeans(
        dplyr::across(dplyr::all_of(pql_social)), na.rm = TRUE
      ),
      pql_schf_tot = rowMeans(
        dplyr::across(dplyr::all_of(pql_school)), na.rm = TRUE
      )
    )

}

# clean adams
clean_adams_canada <- function(
    dataset,
    adams_vars = c(
      "man_hyp_tot", "depres_tot", "soc_avoid_tot",
      "gen_anx_tot", "ocb_tot"
    )) {
  dataset |>
    # make sure ADAMS variables are numeric
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(adams_vars),
        ~ as.numeric(.x) |> suppressWarnings()
      )
    ) |>
    # rename to match Davis names
    dplyr::rename(adams_manic = "man_hyp_tot") |>
    dplyr::rename(adams_depress = "depres_tot") |>
    dplyr::rename(adams_avoid = "soc_avoid_tot") |>
    dplyr::rename(adams_anxious = "gen_anx_tot") |>
    dplyr::rename(adams_obsess = "ocb_tot") |>
    # Canada and UCD REDCaps have different scales (1-4) vs (0-3)
    # subtract comp scores by number of items to rescale and match UCD
    dplyr::mutate(
      # sum of 5 items
      adams_manic = .data$adams_manic - 5,
      # sum of 7 items
      adams_depress = .data$adams_depress - 7,
      adams_avoid = .data$adams_avoid - 7,
      adams_anxious = .data$adams_anxious - 7,
      # sum of 3 items
      adams_obsess = .data$adams_obsess - 3
    )
}

## clean snap-iv and calculate adhd total and odd total ##
clean_snap_canada <- function(
    dataset,
    snap_vars = c(
      "qq1", "qq2", "qq3", "qq4", "qq5", "qq6", "qq7", "qq8", "qq9", "qq11",
      "qq12", "qq13", "qq14", "qq15", "qq16", "qq17", "qq18", "qq19", "qq21",
      "qq22", "qq24", "qq23", "qq25", "qq26", "qq27", "qq28"
    ),
    adhd_vars = c(
      "qq1", "qq2", "qq3", "qq4", "qq5", "qq6", "qq7", "qq8", "qq9", "qq11",
      "qq12", "qq13", "qq14", "qq15", "qq16", "qq17", "qq18", "qq19"
    ),
    odd_vars = c(
      "qq21",
      "qq22", "qq24", "qq23", "qq25", "qq26", "qq27", "qq28"
    )) {

  dataset |>
    # clean numeric
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::any_of(snap_vars),
        ~ clean_numeric(.x)
      )
    ) |>
    # rescale scores to match UCD: change from 1-4 to 0-3
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::any_of(snap_vars),
        ~ .x - 1
      )
    ) |>
    # calculate composite scores
    dplyr::mutate(
      snap_adhd_combtot = rowSums(
        dplyr::across(.cols = dplyr::any_of(adhd_vars)),
        na.rm = TRUE
      ),
      snap_odd_tot = rowSums(
        dplyr::across(.cols = dplyr::any_of(odd_vars)),
        na.rm = TRUE
      )
    ) |>
    # if all vars NA, set composite score to NA
    dplyr::mutate(
      snap_adhd_combtot = dplyr::if_else(
        condition = dplyr::if_all(
          .cols = dplyr::all_of(adhd_vars),
          ~ is.na(.x)
        ),
        true = NA_real_,
        false = .data$snap_adhd_combtot
      ),
      snap_odd_tot = dplyr::if_else(
        condition = dplyr::if_all(
          .cols = dplyr::all_of(odd_vars),
          ~ is.na(.x)
        ),
        true = NA_real_,
        false = .data$snap_odd_tot
      )
    ) |>
    # max possible score of snap_adhd_combtot = 54, odd_vars = 24, NA if >
    dplyr::mutate(
      snap_adhd_combtot = dplyr::if_else(
        condition = .data$snap_adhd_combtot > 54,
        true = NA_real_,
        false = .data$snap_adhd_combtot
      ),
      snap_odd_tot = dplyr::if_else(
        condition = .data$snap_odd_tot > 24,
        true = NA_real_,
        false = .data$snap_odd_tot
      )
    )
}

## clean abc data ##
clean_abc_canada <- function(
    dataset,
    abc_vars = c(
      "abc_comp_rev", "ss_i_irrit_rev", "ss_ii_leth_rev",
      "ss_iii_stereo_rev", "ss_iv_hyper_rev", "ss_v_inapp_rev",
      "ss_vi_socavoid_rev"
    )) {
  dataset |>
    # clean numeric
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::any_of(abc_vars),
        ~ clean_numeric(.x)
      )
    ) |>
    # Canada and UCD REDCaps have different scales (1-4) vs (0-3)
    # subtract comp scores by number of items to rescale and match UCD
    dplyr::mutate(
      # comp score is sum of 55 items
      abc_comp_rev = .data$abc_comp_rev - 55,
      # subscale 1 is sum of 18 items
      ss_i_irrit_rev = .data$ss_i_irrit_rev - 18,
      # subscale 2 is sum of 13 items
      ss_ii_leth_rev = .data$ss_ii_leth_rev - 13,
      # subscale 3 is sum of 6 items
      ss_iii_stereo_rev = .data$ss_iii_stereo_rev - 6,
      # subscale 4 is sum of 10 items
      ss_iv_hyper_rev = .data$ss_iv_hyper_rev - 10,
      # subscale 5 and 6 are sum of 4 items
      ss_v_inapp_rev = .data$ss_v_inapp_rev - 4,
      ss_vi_socavoid_rev = .data$ss_vi_socavoid_rev - 4
    )
}

## clean cshq and calculate cshq total score ##
clean_cshq_canada <- function(
    dataset,
    cshq_vars = c(
      "cshq_1", "cshq_2", "cshq_3", "cshq_4", "cshq_5", "cshq_6", "cshq_7",
      "cshq_8", "cshq_9", "cshq_10", "cshq_11", "cshq_12", "cshq_13", "cshq_14",
      "cshq_15", "cshq_16", "cshq_17", "cshq_18", "cshq_19", "cshq_20",
      "cshq_21", "cshq_22", "cshq_23", "cshq_24", "cshq_25", "cshq_26",
      "cshq_27", "cshq_28", "cshq_29", "cshq_30", "cshq_31", "cshq_32",
      "cshq_33"
    )) {
  dataset |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::any_of(cshq_vars),
        ~ clean_numeric(.x)
      )
    ) |>
    dplyr::mutate(
      cshq_ts = rowSums(
        dplyr::across(.cols = dplyr::any_of(cshq_vars)),
        na.rm = TRUE
      )
    ) |>
    # max possible score of cshq_ts is 3 * 33, set to NA if >
    dplyr::mutate(
      cshq_ts = dplyr::if_else(
        condition = .data$cshq_ts > 99,
        true = NA_real_,
        false = .data$cshq_ts
      )
    ) |>
    # if all items NA then sum should be NA
    dplyr::mutate(
      cshq_ts = dplyr::case_when(
        dplyr::if_all(
          .cols = dplyr::all_of(cshq_vars),
          ~ is.na(.x)
        ) ~ NA,
        .default = .data$cshq_ts
      )
    )
}

clean_els_canada <- function(dataset) {
  # extract els variable names
  nar_vars <- dataset |>
    dplyr::select(tidyselect::starts_with("nar_")) |>
    colnames()

  con_vars <- dataset |>
    dplyr::select(tidyselect::starts_with("con_")) |>
    colnames()

  nar_vars_num <- nar_vars[nar_vars != "nar_doe"]
  con_vars_num <- con_vars[con_vars != "con_doe"]

  dataset <- dataset |>
    dplyr::mutate(
      # convert nar and con scores to numeric
      dplyr::across(
        .cols = dplyr::all_of(c(nar_vars_num, con_vars_num)),
        ~ as.numeric(.x)
      ),
      # replace 0 with NA for ndw_50, ndw, mlum, utwmz
      dplyr::across(
        .cols = c(dplyr::contains("ndw"), dplyr::contains("mlum"),
                  dplyr::contains("utpermin")),
        ~ dplyr::na_if(.x, 0)
      ),
      # values coded as -1, replace
      dplyr::across(
        .cols = c(dplyr::all_of(nar_vars_num), dplyr::all_of(con_vars_num)),
        ~ dplyr::if_else(
          condition = .x == -1,
          true = NA_real_,
          false = .x
        )
      )
    )
}


# variable to process ados variables
clean_ados_canada <- function(
    dataset,
    ados_vars = c(
      "a2", "z2", "z3", "z4", "z6",
      "a2_v2", "z2_v2", "z3_v2", "z4_v2", "z6_v2",
      "a2_v2_v2", "z2_v2_v2", "z3_v2_v2", "z4_v2_v2", "z6_v2_v2",
      "a2_v2_v2_v2", "z2_v2_v2_v2", "z3_v2_v2_v2", "z4_v2_v2_v2", "z6_v2_v2_v2"
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
        !is.na(z2) ~ .data$z2,
        !is.na(z2_v2) ~ .data$z2_v2,
        !is.na(z2_v2_v2) ~ .data$z2_v2_v2,
        !is.na(z2_v2_v2_v2) ~ .data$z2_v2_v2_v2
      ),
      a2_restricted = dplyr::case_when(
        !is.na(z3) ~ .data$z3,
        !is.na(z3_v2) ~ .data$z3_v2,
        !is.na(z3_v2_v2) ~ .data$z3_v2_v2,
        !is.na(z3_v2_v2_v2) ~ .data$z3_v2_v2_v2,
      )
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
clean_dates_canada <- function(
    dataset,
    date_vars = c(
      "vas_dov", "cgi_date_2", "cgis_date_2", "abc_date2", "pql_date2",
      "leit3_date", "vabs3_date", "cshq_date2", "a2", "a2_v2", "a2_v2_v2",
      "a2_v2_v2_v2", "nar_doe", "con_doe"
    )) {
  dataset |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(date_vars),
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
