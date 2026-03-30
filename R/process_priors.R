process_priors <- function(fit_list) {
  temp00 <- lapply(
    fit_list,
    function(fit) {
      cbind.data.frame(
        Scenario = fit$scenario,
        K.pr = fit$settings$K.pr,
        r.pr = fit$settings$r.pr,
        psi.pr = fit$settings$psi.pr,
        psi.dist = fit$settings$psi.dist,
        proc.pr = fit$settings$igamma
      )
    }
  )
  result <- bind_rows(temp00)
  return(result)
}