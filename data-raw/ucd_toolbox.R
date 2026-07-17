devtools::load_all()

#### import data ####
redcap_raw <- read.csv(
  file = paste0(
    "./data-ext/Toolbox/CTSC4328ADoubleBlind-",
    "METUCDToolboxQC_DATA_2024-10-31_1621.csv"
  )
)

tlbx_raw <- read.csv("./data-ext/Toolbox/UCD_MET_November6.csv")

new_admin <- openxlsx::read.xlsx(
  "./data-ext/Toolbox/Toolbox_admin_UCD_MTL Site Completed.xlsx"
)

# incorporate new admin data
redcap_raw2 <- redcap_raw |>
  dplyr::filter(redcap_event_name != "week_8_visit_visit_arm_1") |>
  dplyr::mutate(
    redcap_event_name = dplyr::case_when(
      redcap_event_name == "baseline_visit_vis_arm_1" ~ "Baseline Visit (Visit 1)",
      redcap_event_name == "week_16_visit_3ear_arm_1" ~ "Week 16 (Visit 3/Early Term)"
    ) |>
      factor(
        levels = c(
          "Baseline Visit (Visit 1)",
          "Week 16 (Visit 3/Early Term)"
        )
      )
  ) |>
  dplyr::select(-dplyr::ends_with("_valid")) |>
  dplyr::left_join(
    x = _,
    y = new_admin |>
      dplyr::select(-Treatment) |>
      dplyr::mutate(
        redcap_event_name = factor(
          redcap_event_name,
          levels = c(
            "Baseline Visit (Visit 1)", "Week 16 (Visit 3/Early Term)"
          )
        )
      ),
    by = c("fxs_sts_id", "redcap_event_name")
  )


#### prepare data ####
redcap <- redcap_raw2 |>
  dplyr::mutate(
    childstudyid = dplyr::na_if(childstudyid, "")
  ) |>
  dplyr::group_by(fxs_sts_id) |>
  tidyr::fill(
    childstudyid,
    .direction = "down"
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    # Determing which picture sequence to use
    tcb_picseq_final = dplyr::case_when(
      (!is.na(tcb_picseq_uncorscr) & is.na(tcb_picseq_uncorscr_2)) ~ tcb_picseq_uncorscr,
      (is.na(tcb_picseq_uncorscr) & !is.na(tcb_picseq_uncorscr_2)) ~ tcb_picseq_uncorscr_2,
      (!is.na(tcb_picseq_uncorscr) & !is.na(tcb_picseq_uncorscr_2) &
         (tcb_picseq_raw_2 == 0)) ~ tcb_picseq_uncorscr,
      (!is.na(tcb_picseq_uncorscr) & !is.na(tcb_picseq_uncorscr_2) &
         (tcb_picseq_raw == 10 | tcb_picseq_raw_2 > 0)) ~ tcb_picseq_uncorscr_2
    ),
    # Crystal Composite: Oral reading + Picture Vocabulary
    crystl_valid = dplyr::if_else(
      condition = dplyr::if_all(
        .cols = c(tcb_read_valid, tcb_vocb_valid),
        ~ .x == 1
      ),
      true = 1,
      false = 0
    ),
    # Fluid Composite: Flanker + DCCS + Pattern Comparison + Picture Seq + LSWM
    fluid_valid = dplyr::if_else(
      condition = dplyr::if_all(
        .cols = c(tcb_flnkr_valid, tcb_dccs_valid, tcb_ptrncomp_valid,
                  tcb_picseq_valid, tcb_lstsrt_valid),
        ~ .x == 1
      ),
      true = 1,
      false = 0
    ),
    # Cognition Composite: Reading + Picture Vocab + Flanker + DCCS +
    #   Pattern Comparison + Picture Seq + LSWM
    cognition_valid = dplyr::if_else(
      condition = dplyr::if_all(
        .cols = c(tcb_read_valid, tcb_vocb_valid, tcb_flnkr_valid,
                  tcb_dccs_valid, tcb_ptrncomp_valid, tcb_picseq_valid,
                  tcb_lstsrt_valid),
        ~ .x == 1
      ),
      true = 1,
      false = 0
    )
  )

ucd_tlbx_admin <- redcap

usethis::use_data(ucd_tlbx_admin, overwrite = TRUE)
