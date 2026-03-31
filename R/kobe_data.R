kobe_data <- function(list_models) {
  #####@> Extracting data...
  tmp11 <- list_models %>%
    summarise(
      Fratio = median(harvest),
      Bratio = median(stock),
      .by = c(year, Scenario)
    ) %>%
    arrange(Scenario, year)
  col01 <- data.frame(
    xmin = c(0, 0), xmax = c(1, 1), ymin = c(0, 0), ymax = c(1, 1), 
    col = "yellow"
  )
  col02 <- data.frame(
    xmin = c(1, 1), xmax = c(6, 6), ymin = c(1, 1), ymax = c(6, 6), 
    col = "orange"
  )
  col03 <- data.frame(
    xmin = c(0, 0), xmax = c(1, 1), ymin = c(1, 1), ymax = c(6, 6), 
    col = "red"
  )
  col04 <- data.frame(
    xmin = c(1, 1), xmax = c(6, 6), ymin = c(0, 0), ymax = c(1, 1), 
    col = "#00FF00"
  )
  tmp11b <- filter(tmp11, year %in% c(1950, 1986, 2023))
  tmp11c <- filter(list_models, year == 2023)

  k.out <- data.frame(x = NULL, y = NULL, Scenario = NULL, q = NULL)
  for(i in unique(list_models$Scenario)) {
    x <- filter(list_models, Scenario == i)
    x <- filter(x, year == 2023)
    kernelF <- gplots::ci2d(
      x$stock, 
      x$harvest, 
      nbins = 151, 
      factor = 1.5, 
      ci.levels = c(0.5, 0.8, 0.95),
      show = "none",
      col = 1
    )
    q50 <- kernelF$contours$"0.5"
    q50$Scenario <- i; q50$q <- "50%"
    q80 <- kernelF$contours$"0.8"
    q80$Scenario <- i; q80$q <- "80%"
    q95 <- kernelF$contours$"0.95"
    q95$Scenario <- i; q95$q <- "95%"
    tmp <- rbind(q50, q80, q95)
    k.out <- rbind(
      k.out, 
      data.frame(
        x = tmp$x,
        y = tmp$y,
        Scenario = tmp$Scenario,
        q = tmp$q
      )
    )
  }
  list(
    col01 = col01[1, , drop = FALSE],
    col02 = col02[1, , drop = FALSE],
    col03 = col03[1, , drop = FALSE],
    col04 = col04[1, , drop = FALSE],
    k.out = k.out,
    tmp11 = tmp11,
    tmp11b = tmp11b
  )
}