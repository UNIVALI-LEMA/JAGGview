my_theme <- function(base_size = 18, base_family = "Lato") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.ticks = element_line(colour = rgb01),
      axis.line = element_line(colour = rgb01, linewidth = 0.3),
      axis.text = element_text(colour = rgb02, size = 14),
      axis.title = element_text(size = 18),
      legend.background = element_blank(),
      legend.key = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_line(
        linetype = "solid",
        linewidth = 0.2,
        colour = "gray90"
      ),
      strip.text = element_text(
        colour = "white",
        margin = ggplot2::margin(
          0.3,
          0.3,
          0.3,
          0.3,
          "cm"
        ),
        face = "bold",
        size = 14
      ),
      strip.background = element_rect(
        fill = "#232425",
        colour = rgb02
      ),
      plot.background = element_blank(),
      plot.margin = margin(
        t = 0.2, r = 0.8, b = 0.4, l = 0.4, unit = "cm"
      ),
      complete = TRUE
    )
}