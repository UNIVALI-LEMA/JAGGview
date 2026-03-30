empty_ggplot <- function(title) {
  ggplot() +
    theme_void() +
    geom_text(aes(0 ,0 ,label = title))
}