process_pfunc <- function(hc_list) {
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
            hc[[nm]]$pfunc
          )
        }
      )
    }
  )
  result <- bind_rows(temp00)

  return(result)
}