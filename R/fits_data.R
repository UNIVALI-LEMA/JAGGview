fits_data <- function(list_models) {
  ###@> Index...
  tmp01 <- process_scenarios(list_models, vars = "I")
  
  ###@> SE...
  tmp02 <- process_scenarios(list_models, vars = "SE2")

  ##@> Replacing NA...
  tmp02 <- replace_na_with_na(tmp02, tmp01)

  ####@> Merging tmp01 and tmp02...
  tmp01 <- pivot_longer(
    data = tmp01, names_to = "Index", values_to = "Mean", 3:ncol(tmp01)
  )
  tmp02 <- pivot_longer(
    data = tmp02, names_to = "Index", values_to = "SE", 3:ncol(tmp02)
  )
  tmp00 <- full_join(tmp01, tmp02)

  ####@> Estimating Upper and Lower errors...
  tmp00$error <- with(tmp00, (1.96 * sqrt(SE)))
  tmp00$Ui <- with(tmp00, Mean + error)
  tmp00$Li <- with(tmp00, Mean - error)
  tmp00 <- tmp00[complete.cases(tmp00),]

  ####@> Organizing the structure...
  tmp00$Index <- factor(
    x = tmp00$Index, levels = c("Joint_LL_R2_Early","Joint_LL_R2_DLN", "Joint_LL_R2_allCPCs")
  )

  tmp00 <- tmp00 %>%
    select(-SE) %>%
    filter(Index != is.na(Index))

  ####@> Fit (CI 80%)...
  tmp03 <- process_cpues(list_models, vars = "ppd")
  tmp03$Index[is.na(tmp03$Index)] <- "Joint_LL_R02_DLN"

  tmp03$Index <- factor(
    x = tmp03$Index, levels = c("Joint_LL_R2_Early", "Joint_LL_R2_DLN", "Joint_LL_R2_allCPCs")
  )

  tmp03 <- tmp03 %>% 
    select(-c(se, obserror)) %>%
    filter(Index != is.na(Index))

  ####@> Fit (CI 95%)...
  tmp04 <- process_cpues(list_models, vars = "hat")
  tmp04$Index[is.na(tmp04$Index)] <- "Joint_LL_R02_DLN"

  tmp04$Index <- factor(
    x = tmp04$Index, levels = c("Joint_LL_R2_Early", "Joint_LL_R2_DLN", "Joint_LL_R2_allCPCs")
  )

  tmp04 <- tmp04 %>% 
    select(-c(se, obserror, mu)) %>%
    filter(Index != is.na(Index))

  list(
    Li_Ui = tmp00,
    CI_80 = tmp03,
    CI_95 = tmp04
  )
}