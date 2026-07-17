#' @title Clean Toolbox data
#' @param dataset Toolbox data.frame
#' @export

clean_toolbox <- function(dataset) {
  dataset |>
    dplyr::mutate(
      # flanker
      tcb_flnkr_analytic = dplyr::if_else(
        .data$tcb_flnkr_valid == 1, .data$tcb_flnkr_uncorscr, NA
      ),
      # dccs
      tcb_dccs_analytic = dplyr::if_else(
        .data$tcb_dccs_valid == 1, .data$tcb_dccs_uncorscr, NA
      ),
      # vocab
      tcb_vocb_analytic = dplyr::if_else(
        .data$tcb_vocb_valid == 1, .data$tcb_vocb_uncorscr, NA
      ),
      # ptrncomp
      tcb_ptrncomp_analytic = dplyr::if_else(
        .data$tcb_ptrncomp_valid == 1, .data$tcb_ptrncomp_uncorscr, NA
      ),
      # lstsrt
      tcb_lstsrt_analytic = dplyr::if_else(
        .data$tcb_lstsrt_valid == 1, .data$tcb_lstsrt_uncorscr, NA
      ),
      # picseq
      tcb_picseq_analytic = dplyr::if_else(
        .data$tcb_picseq_valid == 1, .data$tcb_picseq_final, NA
      ),
      # read
      tcb_read_analytic = dplyr::if_else(
        .data$tcb_read_valid == 1, .data$tcb_read_uncorscr, NA
      ),
      # speedmatch
      tcb_spdmtch_analytic = dplyr::if_else(
        .data$tcb_spdmtch_valid == 1, .data$tcb_spdmtch_uncorscr, NA
      ),
      ## composite scores ##
      tcb_crystl_analytic = dplyr::if_else(
        condition = dplyr::if_all(
          .cols = c("tcb_read_valid", "tcb_vocb_valid"),
          ~ .x == 1
        ),
        true = .data$tcb_crystl_uncorscr,
        false = NA
      ),
      tcb_earlycomp_analytic = dplyr::if_else(
        condition = dplyr::if_all(
          .cols = c("tcb_read_valid", "tcb_vocb_valid", "tcb_flnkr_valid",
                    "tcb_dccs_valid", "tcb_ptrncomp_valid", "tcb_picseq_valid",
                    "tcb_lstsrt_valid"),
          ~ .x == 1
        ),
        true = .data$tcb_earlycomp_uncorscr,
        false = NA
      ),
      tcb_cogcomp_analytic = dplyr::if_else(
        condition = dplyr::if_all(
          .cols = c("tcb_read_valid", "tcb_vocb_valid", "tcb_flnkr_valid",
                    "tcb_dccs_valid", "tcb_ptrncomp_valid", "tcb_picseq_valid",
                    "tcb_lstsrt_valid"),
          ~ .x == 1
        ),
        true = .data$tcb_cogcomp_uncorscr,
        false = NA
      ),
      tcb_fluid_analytic = dplyr::if_else(
        condition = dplyr::if_all(
          .cols = c("tcb_flnkr_valid", "tcb_dccs_valid", "tcb_ptrncomp_valid",
                    "tcb_picseq_valid", "tcb_lstsrt_valid"),
          ~ .x == 1
        ),
        true = .data$tcb_fluid_uncorscr,
        false = NA
      )
    )
}
