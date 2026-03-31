hindcast_data <- function(hc_raw_data) {
  ######@> Plot hindcasting...
  hc <- process_hindcasts(hc_raw_data)

  #####@> Extracting data...
  # tmp14 <- do.call(rbind, lapply(hc, function(x) x$diags)) %>%
  tmp14 <- hc %>%
    mutate(
      retro = case_when(
        retro.peels == 0 ~ "Ref",
        retro.peels == 1 ~ "-2023",
        retro.peels == 2 ~ "-2022",
        retro.peels == 3 ~ "-2021",
        retro.peels == 4 ~ "-2020",
        retro.peels == 5 ~ "-2019",
        retro.peels == 6 ~ "-2018",
        retro.peels == 7 ~ "-2017",
        retro.peels == 8 ~ "-2016",
      )
    ) %>%
    mutate(
      retro = factor(
        retro, 
        levels = c(
          "Ref", "-2023", "-2022", "-2021", "-2020", 
          "-2019", "-2018", "-2017", "-2016"
        )
      )
    ) %>%
    # filter(name == "Joint_LL_R2_DLN") %>%
    rename(
      Scenario = level,
      Index = name
    )
  
  tmp15 <- tmp14 %>%
    filter(hindcast == TRUE) %>%
    filter(year > 2015) %>%
    group_by(retro.peels) %>%
    filter(year == min(year)) %>%
    ungroup %>%
    data.frame
  
  tmp16 <- filter_by_condition(tmp14, "retro.peels", "hindcast", "year")

  #####@> MASE analysis...
  mase <- process_mase(hc_raw_data)

  list(
    data = tmp14,
    hindcast_data_1 = tmp15,
    hindcast_data_2 = tmp16,
    mase_data = mase
  )
}