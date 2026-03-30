process_runs <- function(fit_list, vars = "runs") {
  temp00 <- lapply(
    fit_list,
    function(fit) {
      cbind.data.frame(
        Year = fit$yr,
        Scenario = fit$scenario,
        Ref = 0,
        if(vars == "runs") {
          t(fit$residuals)
        } else {
          t(fit$residuals)
        }
      )
    }
  )
  result <- bind_rows(temp00)
  return(result)
}