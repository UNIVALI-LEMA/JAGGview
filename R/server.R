#' @keywords internal
.build_server <- function(
  fits_df, hind_df, pp_df, res_df, kobe_df, traj_df, ra_df, animation
) {
  function(input, output, session) {

    # observeEvent(
    #   c(
    #     input$navmenu, input$priors_posteriors_tabs, 
    #     input$retrospective_analysis_tabs, input$trajectories_tabs
    #   ), {
    #   if (input$navmenu == "tab_fits") {
    #     # tic("tab_fits")
    #   }
    #   else if (input$navmenu == "tab_runs_tests") {
    #     # tic("tab_runs_tests")
    #   }
    #   else if (input$navmenu == "tab_cpue_residuals") {
    #     # tic("tab_cpue_residuals")
    #   }
    #   else if (input$navmenu == "tab_priors_posteriors") {
    #     if (input$priors_posteriors_tabs == "tab_pp_K") {
    #       # tic("tab_pp_K")
    #     }
    #     else if (input$priors_posteriors_tabs == "tab_pp_r") {
    #       # tic("tab_pp_r")
    #     }
    #     else if (input$priors_posteriors_tabs == "tab_pp_psi") {
    #       # tic("tab_pp_psi")
    #     }
    #   }
    #   else if (input$navmenu == "tab_retrospective_analysis") {
    #     if (input$retrospective_analysis_tabs == "tab_ra_B") {
    #       # tic("tab_ra_B")
    #     }
    #     else if (input$retrospective_analysis_tabs == "tab_ra_F") {
    #       # tic("tab_ra_F")
    #     }
    #     else if (input$retrospective_analysis_tabs == "tab_ra_BBmsy") {
    #       # tic("tab_ra_BBmsy")
    #     }
    #     else if (input$retrospective_analysis_tabs == "tab_ra_FFmsy") {
    #       # tic("tab_ra_FFmsy")
    #     }
    #     else if (input$retrospective_analysis_tabs == "tab_ra_procB") {
    #       # tic("tab_ra_procB")
    #     }
    #     else if (input$retrospective_analysis_tabs == "tab_ra_MSY") {
    #       # tic("tab_ra_MSY")
    #     }
    #   }
    #   else if (input$navmenu == "tab_hindcast") {
    #     # tic("tab_hindcast")
    #   }
    #   else if (input$navmenu == "tab_trajectories") {
    #     if (input$trajectories_tabs == "tab_traj_BB0") {
    #       # tic("tab_traj_BB0")
    #     }
    #     else if (input$trajectories_tabs == "tab_traj_BBmsy") {
    #       # tic("tab_traj_BBmsy")
    #     }
    #     else if (input$trajectories_tabs == "tab_traj_FFmsy") {
    #       # tic("tab_traj_FFmsy")
    #     }
    #     else if (input$trajectories_tabs == "tab_traj_Bdev") {
    #       # tic("tab_traj_Bdev")
    #     }
    #     else if (input$trajectories_tabs == "tab_traj_B") {
    #       # tic("tab_traj_B")
    #     }
    #     else if (input$trajectories_tabs == "tab_traj_H") {
    #       # tic("tab_traj_H")
    #     }
    #     else if (input$trajectories_tabs == "tab_traj_Catch") {
    #       # tic("tab_traj_Catch")
    #     }
    #   }
    #   else if (input$navmenu == "tab_kobe") {
    #     # tic("tab_kobe")
    #   }
    # })

    .cpue_res_server(input, output, session, res_df)
    .fits_server(input, output, session, fits_df)
    .hindcast_server(input, output, session, hind_df)
    .kobe_server(input, output, session, kobe_df, animation)
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
    .traj_BB0_server(input, output, session, traj_df, animation)
    .traj_BBmsy_server(input, output, session, traj_df, animation)
    .traj_FFmsy_server(input, output, session, traj_df, animation)
    .traj_Bdev_server(input, output, session, traj_df, animation)
    .traj_B_server(input, output, session, traj_df, animation)
    .traj_H_server(input, output, session, traj_df, animation)
    .traj_Catch_server(input, output, session, traj_df, animation)
    }
}