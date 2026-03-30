process_cpues <- function(fit_list, vars) {
  temp00 <- lapply(fit_list, function(fit) {
    cbind.data.frame(
      Scenario = fit$scenario,
      if(vars == "ppd") {
          array_to_dataframe(fit$cpue.ppd)
      } else {
          array_to_dataframe(fit$cpue.hat)
      }
    )
  })
  result <- bind_rows(temp00)
  return(result)
}