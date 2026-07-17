#### Process Metformin Data: Combine UCD and Canada datasets ####
devtools::load_all()
# colnames
ucd_columns <- colnames(ucd_clean)
canada_columns <- colnames(canada_clean)

# columns to keep:
#   id, studyid, site, age, weight, height, bmi, leit3_nv_iq, cgi,
#   pql_pf_tot, pql_ef_tot, pql_socf_tot, pql_schf_tot,
#   vas_beh, vas_lang, vas_eat,
#   snap_adhd, snap_odd, abc_comp_rev, ss_i, ss_ii, ss_iii, ss_iv,
#   ss_v, ss_vi, adams_manic, adams_depress, adams_avoid, adams_anxious,
#   adams_obsess, a2_social, a2_restricted, vabs, cshq1:32, cshq_ts,
#   ELS French, ELS Nar, ELS Con, Treatment, Dates, Molecular Category
ucd_vars <- c(
  # Demographics
  "fxs_sts_id", "childstudyid", "screen_fail", "redcap_event_name",
  "demo_age", "demo_age_month", "sex", "Race", "Ethnicity", "weight_kg",
  "height_cm", "bmi", "leit3_nv_iq",
  # CGI
  "cgis_soi", "cgi_i",
  # PQL
  "pql_pf_tot", "pql_ef_tot", "pql_socf_tot", "pql_schf_tot",
  # SNAP
  "snap_adhd_combtot", "snap_odd_tot",
  # ABC
  "abc_comp_rev", "ss_i_irrit_rev", "ss_ii_leth_rev", "ss_iii_stereo_rev",
  "ss_iv_hyper_rev", "ss_v_inapp_rev", "ss_vi_socavoid_rev",
  # ADAMS
  "adams_manic", "adams_depress", "adams_avoid", "adams_anxious",
  "adams_obsess",
  # ADOS
  "a2_social", "a2_restricted",
  # Vineland
  "vabs3_rc_rs", "vabs3_rc_ss", "vabs3_ec_rs", "vabs3_ec_ss",
  "vabs3_wc_rs", "vabs3_wc_ss", "vabs3_pd_rs", "vabs3_pd_ss",
  "vabs3_dd_rs", "vabs3_dd_ss", "vabs3_cd_rs", "vabs3_cd_ss",
  "vabs3_is_rs", "vabs3_is_ss", "vabs3_ps_rs", "vabs3_ps_ss",
  "vabs3_cs_rs", "vabs3_cs_ss",
  "vabs3_gm_rs", "vabs3_gm_ss", "vabs3_fm_rs", "vabs3_fm_ss",
  "vabs3_ac_ss", "vabs3_c_ss", "vabs3_dl_ss", "vabs3_s_ss", "vabs3_m_ss",
  # Visual analog scale (VAS)
  "vas_tb1_beh", "vas_tb2_lang", "vas_tb3_eat",
  # CSHQ
  "cshq_1", "cshq_2", "cshq_3", "cshq_4", "cshq_5", "cshq_6", "cshq_7",
  "cshq_8", "cshq_9", "cshq_10", "cshq_11", "cshq_12", "cshq_13", "cshq_14",
  "cshq_15", "cshq_16", "cshq_17", "cshq_18", "cshq_19", "cshq_20", "cshq_21",
  "cshq_22", "cshq_23", "cshq_24", "cshq_25", "cshq_26", "cshq_27", "cshq_28",
  "cshq_29", "cshq_30", "cshq_31", "cshq_32", "cshq_33", "cshq_ts",
  # ELS Language Indicator
  "els_french",
  # ELS NAR
  "nar_ndw50_a", "nar_ndw_a", "nar_utpermin", "nar_mlum_a",
  "nar_perc_intellig_t", "nar_perc_utwmz_t", "nar_perc_unintelut_t",
  "nar_perc_partintelut_t",
  "nar_totnumut_a", "nar_totnumut_t", "nar_totverbalut_t",
  "nar_nonverbalut_t", "nar_perc_totverbalut_t", "nar_perc_nonverbalut_t",
  # ELS CON
  "con_ndw50_a", "con_ndw_a", "con_utpermin", "con_mlum_a",
  "con_perc_intellig_t", "con_perc_utwmz_t", "con_perc_unintelut_t",
  "con_perc_partintelut_t",
  "con_totnumut_a", "con_totnumut_t", "con_totverbalut_t", "con_nonverbalut_t",
  "con_perc_totverbalut_t", "con_perc_nonverbalut_t",
  # Treatment
  "Treatment",
  # Dates
  "cgi_date_2", "pql_date2", "snap_date_2", "abc_date2", "adams_date2",
  "vabs3_date", "vas_dov", "cshq_date2", "nar_doe", "con_doe",
  # Molecular Category
  "tassone_cat", "Molecular.Category"
)
ucd_columns_keep <- ucd_columns[ucd_columns %in% ucd_vars]
ucd_columns_keep <- ucd_columns_keep[match(ucd_vars, ucd_columns_keep)]

canada_var <- c(
  # Demographics
  "fxs_sts_id", "rand_code", "rand_inclyn", "redcap_event_name",
  "redcap_data_access_group", "lang", "demo_age", "demo_age_month", "sex",
  "Race", "Ethnicity", "weight_kg", "height_cm", "bmi", "leit3_nv_iq",
  # CGI
  "cgis_soi", "cgi_i",
  # PQL
  "pql_pf_tot", "pql_ef_tot", "pql_socf_tot", "pql_schf_tot",
  # SNAP
  "snap_adhd_combtot", "snap_odd_tot",
  # ABC
  "abc_comp_rev", "ss_i_irrit_rev", "ss_ii_leth_rev", "ss_iii_stereo_rev",
  "ss_iv_hyper_rev", "ss_v_inapp_rev", "ss_vi_socavoid_rev",
  # ADAMS
  "adams_manic", "adams_depress", "adams_avoid",
  "adams_anxious", "adams_obsess",
  # ADOS
  "a2_social", "a2_restricted",
  # Vineland
  "vabs3_rc_rs", "vabs3_rc_ss", "vabs3_ec_rs", "vabs3_ec_ss",
  "vabs3_wc_rs", "vabs3_wc_ss", "vabs3_pd_rs", "vabs3_pd_ss",
  "vabs3_dd_rs", "vabs3_dd_ss", "vabs3_cd_rs", "vabs3_cd_ss",
  "vabs3_is_rs", "vabs3_is_ss", "vabs3_ps_rs", "vabs3_ps_ss",
  "vabs3_cs_rs", "vabs3_cs_ss",
  "vabs3_gm_rs", "vabs3_gm_ss", "vabs3_fm_rs", "vabs3_fm_ss",
  "vabs3_ac_ss", "vabs3_c_ss", "vabs3_dl_ss", "vabs3_s_ss", "vabs3_m_ss",
  # Visual analog scale (VAS)
  "vas_tb1_beh", "vas_tb2_lang", "vas_tb3_eat",
  # CSHQ
  "cshq_1", "cshq_2", "cshq_3", "cshq_4", "cshq_5", "cshq_6", "cshq_7",
  "cshq_8", "cshq_9", "cshq_10", "cshq_11", "cshq_12", "cshq_13", "cshq_14",
  "cshq_15", "cshq_16", "cshq_17", "cshq_18", "cshq_19", "cshq_20", "cshq_21",
  "cshq_22", "cshq_23", "cshq_24", "cshq_25", "cshq_26", "cshq_27", "cshq_28",
  "cshq_29", "cshq_30", "cshq_31", "cshq_32", "cshq_33", "cshq_ts",
  # ELS Language Indicator
  "els_french",
  # ELS NAR
  "nar_ndw50_a", "nar_ndw_a", "nar_utpermin", "nar_mlum_a",
  "nar_perc_intellig_t", "nar_perc_utwmz_t", "nar_perc_unintelut_t",
  "nar_perc_partintelut_t",
  "nar_totnumut_a", "nar_totnumut_t", "nar_totverbalut_t", "nar_nonverbalut_t",
  "nar_perc_totverbalut_t", "nar_perc_nonverbalut_t",
  # ELS CON
  "con_ndw50_a", "con_ndw_a", "con_utpermin", "con_mlum_a",
  "con_perc_intellig_t", "con_perc_utwmz_t", "con_perc_unintelut_t",
  "con_perc_partintelut_t",
  "con_totnumut_a", "con_totnumut_t", "con_totverbalut_t", "con_nonverbalut_t",
  "con_perc_totverbalut_t", "con_perc_nonverbalut_t",
  # Treatment
  "Treatment",
  # Dates
  "cgi_date_2", "pql_date2", "q3", "abc_date2", "p6", "vabs3_date", "vas_dov",
  "cshq_date2", "nar_doe", "con_doe",
  # Molecular Category
  "tassone_cat", "Molecular.Category"
)

canada_columns_keep <- canada_columns[canada_columns %in% canada_var]
canada_columns_keep <- canada_columns_keep[
  match(canada_var, canada_columns_keep)
]

# compare names betwen ucd and canada (excluding redcap_access_group and lang)
name_comp <- data.frame(
  "UCD" = ucd_columns_keep,
  "Canada" = canada_columns_keep[-c(5, 6)]
) |>
  dplyr::mutate(Check = UCD == Canada)

# bind dataset
all_data <- rbind(
  ucd_clean |>
    dplyr::select(dplyr::all_of(ucd_columns_keep)) |>
    dplyr::mutate(
      Source = "UCD",
      Site = "UCD",
      lang = NA_character_,
      # flip order of screen fail to match canada"s rand_inclyn
      screen_fail = dplyr::case_when(
        screen_fail == "No" ~ "Yes",
        screen_fail == "Yes" ~ "No"
      ),
      snap_age = NA,
      adams_age = NA
    ) |>
    dplyr::rename(rand_inclyn = screen_fail,
                  rand_code = childstudyid) |>
    dplyr::relocate(.data$lang, .after = redcap_event_name),
  canada_clean |>
    dplyr::select(dplyr::all_of(canada_columns_keep)) |>
    dplyr::mutate(
      Source = "Canada",
      Site = dplyr::case_when(
        .data$redcap_data_access_group == "edmonton" ~ "Edmonton",
        .data$redcap_data_access_group == "stj" ~ "STJ"
      ),
      snap_date_2 = NA,
      adams_date2 = NA
    ) |>
    dplyr::select(-redcap_data_access_group) |>
    dplyr::rename(snap_age = q3,
                  adams_age = p6)
) |>
  dplyr::relocate(.data$Source, .after = .data$fxs_sts_id) |>
  dplyr::relocate(.data$Site, .after = .data$Source) |>
  dplyr::mutate(
    Treatment = dplyr::if_else(
      condition = .data$Treatment == "Control",
      true = "Placebo",
      false = .data$Treatment
    )
  )

### export labels ###
# combine labels
all_labels <- c(
  ucd_labels,
  canada_labels[!(names(canada_labels) %in% names(ucd_labels))]
)
# missing labels?
dplyr::setdiff(colnames(all_data), names(all_labels))

all_labels$Source <- "Source"
all_labels$Site <- "Site"
all_labels$els_french <- "ELS French"
all_labels$Treatment <- "Treatment"
all_labels$snap_age <- "Age, SNAP IV"
all_labels$adams_age <- "Age, ADAMS"

dplyr::setdiff(colnames(all_data), names(all_labels))
export_labels <- all_labels[match(colnames(all_data), names(all_labels))]


# save
met_combined <- all_data
met_labels <- export_labels

usethis::use_data(
  met_combined,
  overwrite = TRUE
)

usethis::use_data(
  met_labels,
  overwrite = TRUE
)
