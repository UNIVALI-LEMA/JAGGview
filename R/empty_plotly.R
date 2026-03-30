empty_plotly <- function(title){
  plotly_empty(type = "scatter", mode = "markers") %>%
    config(
      displayModeBar = FALSE
    ) %>%
    layout(
      title = list(
        text = title,
        y = 0.5
      ),
      dragmode = FALSE
    )
}