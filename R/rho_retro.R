rho_retro <- function(hc_list) {
  temp00 <- lapply(
    hc_list,
    function(hc) {
      cbind.data.frame(
        Scenario = names(hc[1]),
        extract_rhos(jbplot_retro(hc, as.png = FALSE))
      )
    }
  )
  result <- bind_rows(temp00)
}