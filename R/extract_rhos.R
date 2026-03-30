extract_rhos <- function(rho) {
  vec01 <- as.numeric(rho[nrow(rho),])
  vec02 <- c("B", "F", "BBmsy", "FFmsy", "procB", "MSY")
  vec03 <- c(
    "Biomass", "Fishing Mortality", "B/Bmsy", "F/Fmsy",
    "Process Error on log(Biomass)", "Surplus Production"
  )
  result <- data.frame("Index" = vec02, "Index2" = vec03, "rho" = vec01)
  return(result)
}