#' @keywords internal
.build_server <- function(
  fits_data, hind_data, kobe_data, pp_data, ra_data, res_data, traj_data, 
  list_fit_models, list_hc_models, animation, use_si_suffix, text_size_tb, 
  position_tb
) {
  function(input, output, session) {
    session$onFlushed(function() {
      disable("confirm_button")
    }, once = TRUE)

    .cpue_res_server(
      input, output, session, res_data, use_si_suffix, text_size_tb, position_tb
    )
    .fits_server(
      input, output, session, fits_data, use_si_suffix
    )
    .hindcast_server(
      input, output, session, hind_data, use_si_suffix, text_size_tb, position_tb
    )
    .kobe_server(
      input, output, session, kobe_data, animation, use_si_suffix
    )
    .priors_posteriors_K_server(
      input, output, session, pp_data, use_si_suffix, text_size_tb, position_tb
    )
    .priors_posteriors_r_server(
      input, output, session, pp_data, use_si_suffix, text_size_tb, position_tb
    )
    .priors_posteriors_psi_server(
      input, output, session, pp_data, use_si_suffix, text_size_tb, position_tb
    )
    .retrospective_analysis_B_server(
      input, output, session, ra_data, use_si_suffix, text_size_tb, position_tb
    )
    .retrospective_analysis_F_server(
      input, output, session, ra_data, use_si_suffix, text_size_tb, position_tb
    )
    .retrospective_analysis_BBmsy_server(
      input, output, session, ra_data, use_si_suffix, text_size_tb, position_tb
    )
    .retrospective_analysis_FFmsy_server(
      input, output, session, ra_data, use_si_suffix, text_size_tb, position_tb
    )
    .retrospective_analysis_procB_server(
      input, output, session, ra_data, use_si_suffix, text_size_tb, position_tb
    )
    .retrospective_analysis_MSY_server(
      input, output, session, ra_data, use_si_suffix, text_size_tb, position_tb
    )
    .runs_tests_server(
      input, output, session, res_data, use_si_suffix, text_size_tb, position_tb
    )
    .traj_BB0_server(
      input, output, session, traj_data, animation, use_si_suffix
    )
    .traj_BBmsy_server(
      input, output, session, traj_data, animation, use_si_suffix
    )
    .traj_FFmsy_server(
      input, output, session, traj_data, animation, use_si_suffix
    )
    .traj_Bdev_server(
      input, output, session, traj_data, animation, use_si_suffix
    )
    .traj_B_server(
      input, output, session, traj_data, animation, use_si_suffix
    )
    .traj_H_server(
      input, output, session, traj_data, animation, use_si_suffix
    )
    .traj_Catch_server(
      input, output, session, traj_data, animation, use_si_suffix
    )
    .summary_table_server(
      input, output, session, list_fit_models, list_hc_models, hind_data, 
      pp_data, ra_data
    )
  }
}