#' Extract Interaction Term Estimate from ANCOVA Model
#'
#' Extracts the Treatment by stratification variable interaction coefficient
#' estimate and confidence interval from a fitted ANCOVA model.
#'
#' @param model Fitted lm model object with Treatment*stratification interaction
#' @param strata_var Name of the stratification variable
#' @param strata_levels Vector of two stratification level names
#'
#' @return Data frame with Estimate, low (2.5%), and hi (97.5%) confidence limits
#'
#' @export

extract_interaction_est <- function(model, strata_var, strata_levels) {
  # Extract the interaction term coefficient
  # The interaction term is TreatmentMetformin:{strata_var}{level2}
  coef_name <- paste0("TreatmentMetformin:", strata_var, strata_levels[2])

  est <- summary(model)$coefficients[coef_name, "Estimate"]
  ci <- confint(model, coef_name)

  ans <- data.frame(
    Estimate = est,
    low = ci[1, "2.5 %"],
    hi = ci[1, "97.5 %"]
  )

  return(ans)
}
