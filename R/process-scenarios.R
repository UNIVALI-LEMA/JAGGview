process_scenarios <- function(fit_list, vars) {
  temp00 <- lapply(fit_list, function(fit) {
    cbind.data.frame(
      Year = fit$yr,
      Scenario = fit$scenario,
      if(vars == "I") {
        fit$settings$I
      } else {
        fit$settings$SE2
      }
    )
  })
  temp01 <- lapply(fit_list, function(fit) {
    c("Year", "Scenario", unique(fit$diags$name))
  })
  tmp01 <- mapply(rename_columns, temp00, temp01, SIMPLIFY = FALSE)
  result <- bind_rows(tmp01)
  return(result)
}