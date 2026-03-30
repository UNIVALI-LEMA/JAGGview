process_retro <- function(hc_list) {
  temp00 <- lapply(
    hc_list,
    function(hc) {
      temp01 <- lapply(
        names(hc),
        function(nm) {
          peel <- ifelse(grepl("^-", nm), nm, "Ref")
          cbind.data.frame(
            id = peel,
            Scenario = hc[[nm]]$scenario,
            array_to_dataframe(hc[[nm]]$timeseries)
          )
        }
      )
    }
  )
  result <- bind_rows(temp00)
  return(result)
}