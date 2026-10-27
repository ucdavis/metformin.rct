extract_est <- function(model) {
  est <- summary(model)$coefficients["TreatmentMetformin", "Estimate"]
  ci <- confint(model, "TreatmentMetformin")

  ans <- data.frame(
    Estimate = est,
    low = ci[1, "2.5 %"],
    hi = ci[1, "97.5 %"]
  )

  return(ans)
}
