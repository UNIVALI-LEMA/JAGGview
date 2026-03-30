process_posteriors <- function(fit_list) {
  temp00 <- lapply(
    fit_list,
    function(fit) {
      cbind.data.frame(
        Scenario = fit$scenario,
        fit$pars_posterior
      )
    }
  )
  result <- bind_rows(temp00)
  return(result)
}