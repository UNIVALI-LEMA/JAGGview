process_hindcasts <- function(fit_list) {
  temp00 <- lapply(
    fit_list,
    function(fit) {
      temp01 <- lapply(
        names(fit),
        function(nm) {
          data <- fit[[nm]]
          peel <- ifelse(grepl("^-", nm), nm, "Ref")
          data.frame(
            Peel = peel,
            data$diags
          )
        }
      )
      dplyr::bind_rows(temp01)
    }
  )
  dplyr::bind_rows(temp00)
}