priors_posteriors_ggplot <- function(df_lists, var, palette) {
  var1 <- paste0(var, "01")
  var2 <- paste0(var, "02")
  prior <- df_lists$prior %>%
    select(c(Scenario, all_of(c(var1, var2)))) %>%
    rename(
      value_1 = all_of(var1),
      value_2 = all_of(var2)
    )
  
  posterior <- df_lists$posterior %>%
    select(c(Scenario, all_of(c(var1, var2)))) %>%
    rename(
      value_1 = all_of(var1),
      value_2 = all_of(var2)
    )
  
  mult <- df_lists$mult %>%
    filter(variavel == var) %>%
    pull(limite)

  ggplot() +
    geom_area(data = prior, aes(x = value_1, y = value_2),
              fill = my_pal[1], alpha = 0.5, colour = "black") +
    geom_area(data = posterior, aes(x = value_1, y = value_2),
              fill = my_pal[2], alpha = 0.5, colour = "black") +
    geom_text(data = df_lists$PPMR,
              aes(x = x * mult, y = y * max(posterior$value_2),
                  label = paste0("PPMR = ", K))) +
    geom_text(data = df_lists$PPVR,
              aes(x = x * mult, y = y * max(posterior$value_2),
                  label = paste0("PPVR = ", K))) +
    facet_wrap(~Scenario, ncol = 3) +
    coord_cartesian(xlim = c(0, mult)) +
    labs(x = var, y = "Density") +
    scale_y_continuous(expand = c(0, 0)) +
    my_theme() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
}