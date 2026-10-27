devtools::load_all()

#### import data ####
redcap_raw <- read.csv(
  file = paste0(
    "./inst/extdata/Metformin-AllDataForDavis_DATA_2025-01-29_1503.csv"
  )
)
# new admin data
new_admin <- openxlsx::read.xlsx(
  "./inst/extdata/Toolbox/Toolbox_admin_UCD_MTL Site Completed.xlsx"
)

edm_tlbx_raw <- read.csv("./inst/extdata/Toolbox/Alberta_MET_November6_DH.csv")
stj_tlbx_raw <- openxlsx::read.xlsx(
  "./inst/extdata/Toolbox/stj_export_mdp.xlsx"
)

#### prepare data ####
redcap <- redcap_raw |>
  dplyr::filter(.data$redcap_event_name != "week_8_visit_visit_arm_1") |>
  dplyr::mutate(
    redcap_event_name = dplyr::case_when(
      .data$redcap_event_name == "baseline_visit_vis_arm_1" ~
        "Baseline Visit (Visit 1)",
      .data$redcap_event_name == "week_16_visit_3ear_arm_1" ~
        "Week 16 (Visit 3/Early Term)"
    ) |>
      factor(
        levels = c(
          "Baseline Visit (Visit 1)",
          "Week 16 (Visit 3/Early Term)"
        )
      )
  ) |>
  dplyr::select("fxs_sts_id", "redcap_event_name") |>
  dplyr::left_join(
    x = _,
    y = new_admin |> dplyr::select(-"Treatment"),
    by = c("fxs_sts_id", "redcap_event_name")
  )

edm_tlbx <- edm_tlbx_raw |>
  dplyr::mutate(
    tmp_event = stringr::str_sub(.data$record_id, -3, -1) |> trimws(),
    redcap_event_name = dplyr::case_when(
      stringr::str_detect(.data$tmp_event, "EY") ~ "Extension, Drop",
      stringr::str_detect(.data$tmp_event, "1") ~ "Baseline Visit (Visit 1)",
      stringr::str_detect(.data$tmp_event, "3") ~ "Week 16 (Visit 3/Early Term)"
    ),

    # strip event name from ID
    record_id = gsub(
      " V\\d$", "", .data$record_id
    ) |> gsub(" EY\\d$", "", x = _),
    dplyr::across(
      .cols = dplyr::where(is.character),
      ~ dplyr::na_if(.x, "")
    ),
    # Determing which picture sequence to use
    tcb_picseq_final = dplyr::case_when(
      (!is.na(.data$tcb_picseq_uncorscr) & is.na(.data$tcb_picseq_uncorscr_2)) ~
        .data$tcb_picseq_uncorscr,
      (is.na(.data$tcb_picseq_uncorscr) & !is.na(.data$tcb_picseq_uncorscr_2)) ~
        .data$tcb_picseq_uncorscr_2,
      (!is.na(.data$tcb_picseq_uncorscr) & !is.na(.data$tcb_picseq_uncorscr_2) &
         (.data$tcb_picseq_raw_2 == 0)) ~
        .data$tcb_picseq_uncorscr,
      (!is.na(.data$tcb_picseq_uncorscr) & !is.na(.data$tcb_picseq_uncorscr_2) &
         (.data$tcb_picseq_raw == 10 | .data$tcb_picseq_raw_2 > 0)) ~
        .data$tcb_picseq_uncorscr_2
    )
  ) |>
  dplyr::relocate(.data$tmp_event, .after = record_id) |>
  dplyr::filter(!is.na(.data$record_id))

stj_tlbx <- stj_tlbx_raw |>
  dplyr::mutate(
    AssessmentName = dplyr::case_when(
      .data$AssessmentName == "V1" ~ "Baseline Visit (Visit 1)",
      .data$AssessmentName == "V3" ~ "Week 16 (Visit 3/Early Term)",
      .default = .data$AssessmentName
    ),
    dplyr::across(
      .cols = dplyr::where(is.character),
      ~ dplyr::na_if(.x, "")
    ),
    # Determing which picture sequence to use
    tcb_picseq_final = dplyr::case_when(
      (!is.na(.data$tcb_picseq_uncorscr) & is.na(.data$tcb_picseq_uncorscr_2)) ~
        .data$tcb_picseq_uncorscr,
      (is.na(.data$tcb_picseq_uncorscr) & !is.na(.data$tcb_picseq_uncorscr_2)) ~
        .data$tcb_picseq_uncorscr_2,
      (!is.na(.data$tcb_picseq_uncorscr) & !is.na(.data$tcb_picseq_uncorscr_2) &
         (.data$tcb_picseq_raw_2 == 0)) ~
        .data$tcb_picseq_uncorscr,
      (!is.na(.data$tcb_picseq_uncorscr) & !is.na(.data$tcb_picseq_uncorscr_2) &
         (.data$tcb_picseq_raw == 10 | .data$tcb_picseq_raw_2 > 0)) ~
        .data$tcb_picseq_uncorscr_2
    )
  ) |>
  dplyr::rename(
    tcb_vocb_uncorscr = "tcb_vocab_uncorscr"
  )

## combine edm and stj toolbox data ##
names(edm_tlbx)
names(stj_tlbx)

canada_tlbx <- dplyr::bind_rows(
  edm_tlbx |>
    dplyr::select(
      "record_id", "redcap_event_name", dplyr::starts_with("tcb_")
    ),
  stj_tlbx |>
    dplyr::select(
      "record_id", "AssessmentName", dplyr::starts_with("tcb_")
    ) |>
    dplyr::rename(redcap_event_name = "AssessmentName")
) |>
  dplyr::filter(!is.na(.data$record_id)) |>
  dplyr::select(
    "record_id", "redcap_event_name", dplyr::ends_with("_uncorscr"),
    "tcb_picseq_uncorscr_2", "tcb_picseq_final"
  )

## merge toolbox data with redcap admin data ##
canada_tlbx_admin <- dplyr::left_join(
  redcap |>
    dplyr::mutate(
      fxs_sts_id = .data$fxs_sts_id |> toupper()
    ),
  canada_tlbx,
  by = c("fxs_sts_id" = "record_id", "redcap_event_name")
)

usethis::use_data(canada_tlbx_admin, overwrite = TRUE)
