#### clean UCD data for METFORMIN trial ####
devtools::load_all()
## double check the redcap_data.R is updated to most recent CSV file ##
source("./data-raw/ucd_redcap_data.R")

# fill in childstudyid for all visits
data <- data |>
  dplyr::mutate(
    childstudyid = dplyr::case_when(
      childstudyid == "" ~ NA_character_,
      .default = childstudyid
    )
  ) |>
  dplyr::group_by(fxs_sts_id) |>
  tidyr::fill(
    childstudyid,
    .direction = "downup"
  ) |>
  dplyr::ungroup()

## import group assignments ##
blinded_groups <- readxl::read_xlsx(
  path = "./inst/extdata/DSMB_ IDS Blinded Data to date.xlsx",
  range = readxl::cell_cols("A:C")
) |>
  dplyr::select(
    `Sub. Number`, `Treatment Code`
  )

# merge data pull with group assignments
data_all <- merge(
  data,
  blinded_groups,
  by.x = "childstudyid",
  by.y = "Sub. Number",
  sort = FALSE,
  all.x = TRUE
) |>
  dplyr::arrange(
    fxs_sts_id, redcap_event_name
  ) |>
  dplyr::mutate(
    Treatment = dplyr::case_when(
      `Treatment Code` == "A" ~ "Metformin",
      `Treatment Code` == "B" ~ "Control"
    )
  )

# clean data #
data_cleaned <- metformin::clean_data_ucd(data_all)

# update 'tassone_cat' (methylation group)
new_methylation_cat <- openxlsx::read.xlsx(
  xlsxFile = paste0(
    "./inst/extdata/Metformin Table-sent to Kyoungmi_20221007-Fixed+Data_FT2_mdp.xlsx"
  ),
  startRow = 2
)

new_meth_cat <- new_methylation_cat |>
  dplyr::select(`STS.#/.FXD.ID.#`, Molecular.Category.rechecked) |>
  dplyr::mutate(
    Molecular.Category.rechecked = Molecular.Category.rechecked |> tolower(),
    fxs_sts_id = stringr::str_sub(`STS.#/.FXD.ID.#`, start = 1, end = 10),
    Molecular.Category = dplyr::case_when(
      stringr::str_detect(Molecular.Category.rechecked, "mosaic") ~ "Mosaic",
      stringr::str_detect(Molecular.Category.rechecked, "full") ~ "Full"
    )
  ) |>
  dplyr::select(fxs_sts_id, Molecular.Category.rechecked, Molecular.Category)


data_cleaned <- data_cleaned |>
  dplyr::left_join(
    x = _,
    y = new_meth_cat |> dplyr::select(fxs_sts_id, Molecular.Category),
    by = "fxs_sts_id"
  )

# add labels for new ADOS variables
data_labels$a2_social <- "Social + Communication"
data_labels$a2_restricted <- "Restricted and Repetitive Behaviors"
# variables removed during cleaning process
dropped_variables <- c(
  "pql_pf_tot_cor", "pql_ef_tot_cor", "pql_socf_tot_cor", "pql_schf_tot_cor",
  "fmrp_fxsid"
)

# label data_cleaned using labels from data, minus dropped variables
labelled::var_label(data_cleaned) <- data_labels[
  !(names(data_labels) %in% dropped_variables)
]

ucd_clean <- data_cleaned
ucd_labels <- data_labels

# save cleaned data and data labels #
usethis::use_data(
  ucd_clean,
  overwrite = TRUE
)

usethis::use_data(
  ucd_labels,
  overwrite = TRUE
)
