#' @keywords internal
.build_server <- function(fits_df, hind_df, pp_df, res_df, kobe_df, traj_df, ra_df) {
  function(input, output, session) {

    .cpue_res_server(input, output, session, res_df)

    .fits_server(input, output, session, fits_df)
    
    .hindcast_server(input, output, session, hind_df)
    
    .kobe_server(input, output, session, kobe_df)
    
    .priors_posteriors_K_server(input, output, session, pp_df)

    .priors_posteriors_r_server(input, output, session, pp_df)
    
    .priors_posteriors_psi_server(input, output, session, pp_df)

    .retrospective_analysis_B_server(input, output, session, ra_df)

    .retrospective_analysis_F_server(input, output, session, ra_df)

    .retrospective_analysis_BBmsy_server(input, output, session, ra_df)

    .retrospective_analysis_FFmsy_server(input, output, session, ra_df)
    
    .retrospective_analysis_procB_server(input, output, session, ra_df)

    .retrospective_analysis_MSY_server(input, output, session, ra_df)

    .runs_tests_server(input, output, session, res_df)

    .traj_BB0_server(input, output, session, traj_df)

    .traj_BBmsy_server(input, output, session, traj_df)
    
    .traj_FFmsy_server(input, output, session, traj_df)
    
    .traj_Bdev_server(input, output, session, traj_df)

    .traj_B_server(input, output, session, traj_df)
    
    .traj_H_server(input, output, session, traj_df)

    .traj_Catch_server(input, output, session, traj_df)
    }
}