process_mase <- function(fit_list) {
  temp00 <- lapply(
    fit_list,
    function(fit) {
      jbmase(fit) %>% 
        mutate(
          x = 2015, 
          y = 1.9,
          Scenario = fit[[1]]$scenario
        )
    }
  )
  result <- bind_rows(temp00)
  return(result)
}