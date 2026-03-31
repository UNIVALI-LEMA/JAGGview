retrospective_analysis_data <- function(hc_raw_data) {
  #####@> Extracting values...
  tmp17 <- process_retro(hc_raw_data) %>%
    filter(Index %in% c("B", "F", "BBmsy", "FFmsy", "procB")) %>%
    mutate(
      Index2 = ifelse(
        Index == "B", 
        "Biomass",
        ifelse(
          Index == "F", 
          "Fishing Mortality",
          ifelse(
            Index == "BBmsy", 
            "B/Bmsy",
            ifelse(
              Index == "FFmsy", 
              "F/Fmsy",
              "Process Error on log(Biomass)"
            )
          )
        )
      )
    ) %>%
    mutate(
      id = factor(
        id,
        c(
          "Ref", "-2023", "-2022", "-2021", "-2020", 
          "-2019", "-2018", "-2017", "-2016"
        )
      )
    ) %>%
    mutate(
      teste = ifelse(
        id == "Ref", TRUE,
        ifelse(
          id == "-2023" & Year == 2023, FALSE,
          ifelse(
            id == "-2022" & Year >= 2022, FALSE,
            ifelse(
              id == "-2021" & Year >= 2021, FALSE,
              ifelse(
                id == "-2020" & Year >= 2020, FALSE,
                ifelse(
                  id == "-2019" & Year >= 2019, FALSE,
                  ifelse(
                    id == "-2018" & Year >= 2018, FALSE,
                    ifelse(
                      id == "-2017" & Year >= 2017, FALSE,
                      ifelse(
                        id == "-2016" & Year >= 2016, FALSE, 
                        TRUE
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )

  tmp18 <- process_pfunc(hc_raw_data) %>%
    mutate(
      Index = "MSY",
      Index2 = "Surplus Production"
    ) %>%
      mutate(
        id = factor(
          id, 
          levels = c(
            "Ref", "-2023", "-2022", "-2021", "-2020", 
            "-2019", "-2018", "-2017", "-2016"
          )
        )
    )

  # #####@> Extracting rhos...
  temp02 <- rho_retro(hc_raw_data) %>% mutate(x = 2010)

  list(
    data = tmp17,
    surplus_data = tmp18,
    rho_data = temp02
  )
}