#### clean Canada data for Metformin trial ####
devtools::load_all()
## double check the redcap_data.R is updated to most recent CSV file ##
source("./data-raw/canada_redcap_data.R")

# clean data #
data_cleaned <- clean_data_canada(data) |>
  dplyr::mutate(
    fxs_sts_id = toupper(.data$fxs_sts_id)
  )

# which variables do not have labels?
colnames(data_cleaned)[!(colnames(data_cleaned) %in% names(data_labels))]

# add treatment group assignment
edm_groups <- read.csv(
  file = "./inst/extdata/metformin_edmonton_unblinded.csv"
)

mont_groups <- openxlsx::read.xlsx(
  "./inst/extdata/20240819 Blinded code (group A - group B).xlsx"
)

mont_unblinded <- openxlsx::read.xlsx(
  "./inst/extdata/rando list_ Metformin Fragile-FXSMET-2018.xlsx"
)

data_cleaned <- data_cleaned |>
  dplyr::left_join(
    x = _, y = edm_groups,
    by = c(
      "fxs_sts_id" = "FXS.STS.ID..",
      "sex" = "Gender",
      "rand_code" = "Randomization"
    )
  ) |>
  dplyr::left_join(
    x = _,
    y = mont_unblinded,
    by = c("rand_code" = "ID", "sex" = "Strata")
  ) |>
  tidyr::unite(col = "Treatment", Group.x, Group.y, na.rm = TRUE) |>
  dplyr::mutate(
    Treatment = dplyr::na_if(.data$Treatment, "")
  ) |>
  dplyr::group_by(.data$fxs_sts_id) |>
  tidyr::fill(
    .data$Treatment, .direction = "down"
  ) |>
  dplyr::ungroup()


check_mont <- dplyr::left_join(
  data_cleaned |> dplyr::filter(.data$redcap_data_access_group == "stj"),
  mont_groups,
  by = c("fxs_sts_id" = "Participant.number")
) |>
  dplyr::select(
    "fxs_sts_id", "redcap_data_access_group", "rand_inclyn",
    `Assignment.(blinded)`, "Treatment"
  ) |>
  dplyr::slice_head(by = .data$fxs_sts_id)

## add molecular category data ##
molecular_cat <- openxlsx::read.xlsx(
  xlsxFile = paste0(
    "./inst/extdata/MetFormin - Fragil X -Montreal+ Edmonton Feb 2025_mdp.xlsx"
  )
)

molecular_cat <- molecular_cat |>
  dplyr::rename(tassone_cat = "Molecular.Category") |>
  dplyr::rename(fxs_sts_id = "Code.Participant") |>
  dplyr::mutate(
    tassone_cat = .data$tassone_cat |> tolower(),
    Molecular.Category = dplyr::case_when(
      stringr::str_detect(.data$tassone_cat, "mosaic") ~ "Mosaic",
      stringr::str_detect(.data$tassone_cat, "full") ~ "Full"
    )
  ) |>
  dplyr::select(
    "fxs_sts_id", "tassone_cat", "Molecular.Category"
  )

data_cleaned <- dplyr::left_join(
  x = data_cleaned,
  y = molecular_cat,
  by = "fxs_sts_id"
)

## add labels for variables missing a label ##
# ADAMS
data_labels$adams_manic <- "Manic/Hyperactive Behavior Total"
data_labels$adams_depress <- "Depressed Mood Total"
data_labels$adams_avoid <- "Social Avoidance Total"
data_labels$adams_anxious <- "General Anxiety Total"
data_labels$adams_obsess <- "Obsessive/Compulsive Behavior Total"
# Race/Ethnicity # nolintr: comment label, not code
data_labels$Race <- "Race"
data_labels$Ethnicity <- "Ethnicity"
# SNAP
data_labels$snap_adhd_combtot <- "ADHD Combined Total"
data_labels$snap_odd_tot <- "ODD Total"
# ADOS
data_labels$a2_social <- "Social + Communication"
data_labels$a2_restricted <- "Restricted and Repetitive Behaviors"
# CHSQ
data_labels$cshq_ts <- "CSHQ Total Score"
# molecular categories
data_labels$tassone_cat <- "Molecular Category (Raw)"
data_labels$Molecular.Category <- "Molecular Category"

## dropped variable ##
# list of unnecessary variables to remove
dropped_variables <- c(
  # CSHQ items
  # "cshq_1", "cshq_2", "cshq_3", "cshq_4", "cshq_5", "cshq_6", "cshq_7",
  # "cshq_8", "cshq_9" , "cshq_10", "cshq_11", "cshq_12", "cshq_13", "cshq_14",
  # "cshq_15", "cshq_16", "cshq_17", "cshq_18", "cshq_19", "cshq_20",
  # "cshq_21", "cshq_22", "cshq_23", "cshq_24", "cshq_25", "cshq_26",
  # "cshq_27", "cshq_28", "cshq_29", "cshq_30", "cshq_31", "cshq_32", "cshq_33",
  # Peds QL items
  "pql_pf_1", "pql_pf_2", "pql_pf_3", "pql_pf_4", "pql_pf_5", "pql_pf_6",
  "pql_pf_7", "pql_pf_8",
  "pql_ef_1", "pql_ef_2", "pql_ef_3", "pql_ef_4", "pql_ef_5",
  "pql_socf_1", "pql_socf_2", "pql_socf_3", "pql_socf_4", "pql_socf_5",
  "pql_schf_1", "pql_schf_2", "pql_schf_3", "pql_schf_4", "pql_schf_5",
  # SNAP items
  "qq1", "qq2", "qq3", "qq4", "qq5", "qq6", "qq7", "qq8", "qq9", "qq11",
  "qq12", "qq13", "qq14", "qq15", "qq16", "qq17", "qq18", "qq19", "qq21",
  "qq22", "qq24", "qq23", "qq25", "qq26", "qq27", "qq28"
)

data_cleaned <- data_cleaned |>
  dplyr::select(-tidyr::any_of(dropped_variables)) |>
  dplyr::relocate(
    .data$cshq_1:.data$cshq_33,
    .after = .data$Molecular.Category
  )

# label data_cleaned using labels from data, minus dropped variables
labelled::var_label(data_cleaned) <- data_labels[
  names(data_labels) %in% colnames(data_cleaned)
]

canada_clean <- data_cleaned
canada_labels <- data_labels

# save cleaned data and data labels #
usethis::use_data(
  canada_clean,
  overwrite = TRUE
)

usethis::use_data(
  canada_labels,
  overwrite = TRUE
)
