calculate_pql_overall <- function(dataset) {
  pf_n <- 8
  ef_n <- 5
  socf_n <- 5
  schf_n <- 5

  dataset |>
    dplyr::mutate(
      pql_overall = weighted.mean(
        x = c(pql_schf_tot, pql_socf_tot, pql_pf_tot, pql_ef_tot),
        w = c(schf_n, socf_n, pf_n, ef_n),
        na.rm = TRUE
      ),
      .by = c("fxs_sts_id", "redcap_event_name")
    ) |>
    dplyr::mutate(
      pql_overall = labelled::set_label_attribute(
        pql_overall,
        "PQL Overall Score"
      )
    )
}
