kobe_ggplot <- function(df) {
  ggplot() +
    geom_rect(data = df$col01, 
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "yellow") +
    geom_rect(data = df$col02, 
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "orange") +
    geom_rect(data = df$col03, 
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "red") +
    geom_rect(data = df$col04, 
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "green") +
    geom_hline(yintercept = 1, linetype = "longdash") +
    geom_vline(xintercept = 1, linetype = "longdash") +
    geom_polygon(
      data = df$k.out,
      aes(x = x, y = y, fill = factor(q, levels = c("95%", "80%", "50%"))),
      colour = "gray30") +
    geom_path(data = df$tmp11, aes(x = Bratio, y = Fratio)) +
    geom_point(data = df$tmp11b,
               aes(x = Bratio, y = Fratio, shape = factor(year)),
               size = 4, fill = "white") +
    facet_wrap(~ Scenario, scales = "free_x", ncol = 3) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 6, 1)) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, 6, 1)) +
    scale_shape_manual(values = c(21, 22, 23)) +
    scale_fill_manual(values = c("cornsilk4", "grey", "cornsilk2")) +
    labs(x = expression(B/B[MSY]), y = expression(F/F[MSY]), fill = "",
         colour = "", shape = "") +
    coord_cartesian(xlim = c(0, 4), ylim = c(0, 3)) +
    my_theme() +
    theme(legend.position = "top")
}