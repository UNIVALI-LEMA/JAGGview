.summary_table_server <- function(
  input, output, session, list_fit_models, list_hc_models, hind_data, pp_data, 
  ra_data
) {

  output$summary_table_estimates <- render_gt({
    .dynamic_table(get_estimates(list_fit_models))
  })

  output$summary_table_pars <- render_gt({
    .dynamic_table(get_pars(list_fit_models))
  })

  output$summary_table_stats <- render_gt({
    .dynamic_table(get_stats(list_fit_models))
  })

  output$summary_table_hc_estimates <- render_gt({
    .dynamic_table(get_hc_estimates(list_hc_models))
  })

  output$summary_table_hc_pars <- render_gt({
    .dynamic_table(get_hc_pars(list_hc_models))
  })

  output$summary_table_hc_stats <- render_gt({
    .dynamic_table(get_hc_stats(list_hc_models))
  })

  output$summary_table_mase <- render_gt({
    .dynamic_table(get_mase(hind_data))
  })

  output$summary_table_ppmr <- render_gt({
    .dynamic_table(get_ppmr(pp_data))
  })

  output$summary_table_ppvr <- render_gt({
    .dynamic_table(get_ppvr(pp_data))
  })

  output$summary_table_refpts <- render_gt({
    .dynamic_table(get_refpts(list_fit_models))
  })

  output$summary_table_rho <- render_gt({
    .dynamic_table(get_rho(ra_data))
  })

}

