priors_posteriors_data <- function(list_models) {
  #####@> Priors...
  tmp12 <- process_priors(list_models)

  out02 <- data.frame(
    Scenario = NULL, 
    K01 = NULL, 
    K02 = NULL, 
    r01 = NULL,
    r02 = NULL, 
    psi01 = NULL, 
    psi02 = NULL, 
    sigma01 = NULL,
    sigma02x = NULL, 
    sigma02y = NULL
  )
  for(i in unique(tmp12$Scenario)) {
      init <- filter(tmp12, Scenario == i)
      scen <- i
      K01 <- sort(rlnorm(10000, log(init$K.pr[1]), init$K.pr[2]))
      K02 <- dlnorm(K01, log(init$K.pr[1]), init$K.pr[2])
      r01 <- sort(rlnorm(10000, log(init$r.pr[1]), init$r.pr[2]))
      r02 <- dlnorm(r01, log(init$r.pr[1]), init$r.pr[2])
      psi01 <- sort(rlnorm(10000, log(init$psi.pr[1]), init$psi.pr[2]))
      psi02 <- dlnorm(psi01, log(init$psi.pr[1]), init$psi.pr[2])
      sigma01 <- seq(0.0001, 1, l = 10000)
      sigma02 <- dgamma(sigma01, init$proc.pr[1], init$proc.pr[2], log = TRUE)
      out02 <- rbind(
        out02, 
        data.frame(
          Scenario = scen,
          K01 = K01,
          K02 = K02,
          r01 = r01,
          r02 = r02,
          psi01 = psi01,
          psi02 = psi02,
          sigma01 = sigma01,
          sigma02 = sigma02
        )
      )
  }

  #####@> Posteriors...
  tmp13 <- process_posteriors(list_models)

  out03 <- data.frame(
    Scenario = NULL, K01 = NULL, K02 = NULL, r01 = NULL,r02 = NULL, 
    psi01 = NULL, psi02 = NULL, sigma01 = NULL, sigma02x = NULL, 
    sigma02y = NULL
  )
  for(i in unique(tmp13$Scenario)) {
    init <- filter(tmp13, Scenario == i)
    scen <- i
    K01 <- stats::density(init$K, adjust = 2)$x
    K02 <- stats::density(init$K, adjust = 2)$y
    r01 <- stats::density(init$r, adjust = 2)$x
    r02 <- stats::density(init$r, adjust = 2)$y
    psi01 <- stats::density(init$psi, adjust = 2)$x
    psi02 <- stats::density(init$psi, adjust = 2)$y
    sigma01 <- stats::density(init$sigma2, adjust = 2)$x
    sigma02 <- stats::density(init$sigma2, adjust = 2)$y
    out03 <- rbind(
      out03, data.frame(
        Scenario = scen, 
        K01 = K01, 
        K02 = K02, 
        r01 = r01, 
        r02 = r02, 
        psi01 = psi01, 
        psi02 = psi02, 
        sigma01 = sigma01, 
        sigma02 = sigma02
      )
    )
  }

  #####@> PPVR and PPVM...
  temp00 <- out02 %>%
    summarise(
      mu.K = mean(K01),
      sd.K = sd(K01),
      mu.r = mean(r01),
      sd.r = sd(r01),
      mu.psi = mean(psi01),
      sd.psi = sd(psi01),
      .by = Scenario
    )
  temp01 <- tmp13 %>%
    summarise(
      mu.K = mean(K),
      sd.K = sd(K),
      mu.r = mean(r),
      sd.r = sd(r),
      mu.psi = mean(psi),
      sd.psi = sd(psi),
      .by = Scenario
    )
  
  PPVR <- data.frame(
    Scenario = temp00$Scenario, 
    x = 0.85, 
    y = 0.88,
    K = round((temp01$sd.K/temp01$mu.K)^2/(temp00$sd.K/temp00$mu.K)^2, 3),
    r = round((temp01$sd.r/temp01$mu.r)^2/(temp00$sd.r/temp00$mu.r)^2, 3),
    psi = round((temp01$sd.psi/temp01$mu.psi)^2/(temp00$sd.psi/temp00$mu.psi)^2,
              3))

  PPMR <- data.frame(
    Scenario = temp00$Scenario,
    x = 0.85, 
    y = 0.93,
    K = round(temp01$mu.K/temp00$mu.K, 3),
    r = round(temp01$mu.r/temp00$mu.r, 3),
    psi = round(temp01$mu.psi/temp00$mu.psi, 3))
  
  multiplicadores <- data.frame(
    variavel = c("K", "r", "psi"),
    limite = c(8000000, 0.3, 1.6)
  )

  list(
    prior = out02,
    posterior = out03,
    PPVR = PPVR,
    PPMR = PPMR,
    mult = multiplicadores
  )
}