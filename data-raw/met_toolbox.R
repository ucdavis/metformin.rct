## Toolbox Data ##
devtools::load_all()

# combine ucd and canada data
met_tlbx <- dplyr::bind_rows(
  ucd_tlbx_admin |>
    dplyr::select(dplyr::any_of(names(canada_tlbx_admin))) |>
    dplyr::mutate(
      Site = "UCD",
      .after = fxs_sts_id
    ),
  canada_tlbx_admin |>
    dplyr::mutate(
      Site = dplyr::case_when(
        grepl("FXSMET-18-2", fxs_sts_id) ~ "Edmonton",
        grepl("FXSMET-18-3", fxs_sts_id) ~ "STJ"
      ),
      .after = fxs_sts_id
    )
)

met_tlbx_clean <- met_tlbx |>
  dplyr::left_join(
    x = _,
    y = met_combined |>
      dplyr::select(fxs_sts_id, redcap_event_name, Treatment, demo_age,
                    leit3_nv_iq, sex, els_french),
    by = c('fxs_sts_id', 'redcap_event_name')
  ) |>
  dplyr::group_by(fxs_sts_id) |>
  tidyr::fill(
    demo_age, leit3_nv_iq,
    .direction = "downup"
  ) |>
  dplyr::ungroup() |>
  clean_toolbox() |>
  dplyr::mutate(
    t2 = dplyr::if_else(redcap_event_name == "Week 8 Visit (Visit 2)", 1, 0),
    t3 = dplyr::if_else(redcap_event_name == "Week 16 (Visit 3/Early Term)", 1, 0),
    t23 = dplyr::if_else(redcap_event_name == "Baseline Visit (Visit 1)", 0 , 1),
    time = dplyr::case_when(
      redcap_event_name == "Baseline Visit (Visit 1)" ~ 0,
      redcap_event_name == "Week 8 Visit (Visit 2)" ~ 8,
      redcap_event_name == "Week 16 (Visit 3/Early Term)" ~ 16
    ),
    Metformin = as.numeric(Treatment == "Metformin"),
    Placebo = as.numeric(Treatment == "Placebo"),
    dplyr::across(
      .cols = c(demo_age, leit3_nv_iq),
      ~ .x - mean(.x, na.rm = TRUE),
      .names = "{col}_center"
    ),
    Treatment = Treatment |> factor(levels = c("Placebo", "Metformin"))
  ) |>
  dplyr::filter(
    fxs_sts_id %in% met_analytic_ids
  )

usethis::use_data(met_tlbx_clean, overwrite = TRUE)
