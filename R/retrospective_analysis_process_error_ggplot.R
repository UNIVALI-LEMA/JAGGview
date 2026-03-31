retrospective_analysis_process_error_ggplot <- function(
  df_lists, title_y = "Process error on log(Biomass)"
) {
  retrospective_analysis_temporal_series_ggplot(df_lists, "procB", title_y)
}