runs_tests_data <- function(list_models) {
  tmp05 <- process_runs(list_models)

  #####@> Runstest...
  out.test <- data.frame(
    expand.grid(
      Index = names(tmp05)[4:ncol(tmp05)], 
      Scenario = unique(tmp05$Scenario),
      ymin = as.integer(1950), 
      ymax = as.integer(2023), 
      lcl = NA, 
      ucl = NA, 
      pvalue = NA
    )
  )
  
  for(i in 4:ncol(tmp05)) {
    for(j in unique(tmp05$Scenario)) {
      name <- names(tmp05)[i]
      index <- tmp05[tmp05$Scenario == j, i]
      index <- index[complete.cases(index)]
      test <- jbruns_sig3(index, type = "resid")
      out.test$lcl[out.test$Index == name &
                  out.test$Scenario == j] <- test$sig3lim[1]
      out.test$ucl[out.test$Index == name &
                  out.test$Scenario == j] <- test$sig3lim[2]
      out.test$pvalue[out.test$Index == name &
                      out.test$Scenario == j] <- test$p.runs
    }
  }
  out.test$class <- ifelse(out.test$pvalue < 0.05, "red", "green")
  out.test <- out.test[complete.cases(out.test),]

  ####@> Pivoting table...
  tmp05 <- pivot_longer(
    tmp05, names_to = "Index", values_to = "Res",4:ncol(tmp05)
  ) %>%
    filter(complete.cases(.)) %>%
    left_join(out.test, by = c("Scenario", "Index")) %>%
    select(Year:Res, lcl, ucl) %>%
    mutate(
      class = ifelse(Res < lcl | Res > ucl, "red", "white"),
      Index = factor(
        Index, 
        levels = c(
          "Joint_LL_R2_Early", "Joint_LL_R2_DLN", "Joint_LL_R2_allCPCs"
        )
      )
    ) %>%
    droplevels()

  ###@> Including p-value in plot...
  out.test <- out.test %>%
      mutate(x = mean(1950:2023), y = 0.65)
  
  loess_fit <- loess(Res ~ Year, data = tmp05)
  tmp05$fit <- predict(loess_fit)
  pred <- predict(loess_fit, se = TRUE)

  tmp05$fit   <- pred$fit
  tmp05$upper <- pred$fit + 1.96 * pred$se.fit
  tmp05$lower <- pred$fit - 1.96 * pred$se.fit

  dados_RMSE <- cpue_conflicts_data(list_models)

  list(
    cpue_residuals = tmp05,
    SE3 = out.test,
    dados_RMSE = dados_RMSE
  )
}