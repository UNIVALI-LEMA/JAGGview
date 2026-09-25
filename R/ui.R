plot_height <- "calc(100vh - 57px - 30px)"
plot_height_tab <- "calc(100vh - 57px - 30px - 42px)"

.build_ui <- function(
  fits_data, hind_data, kobe_data, pp_data, ra_data, res_data, traj_data, 
  use_si_suffix
) {
  addResourcePath("www", system.file("www", package = "JAGGview"))

  dashboardPage(
    help = NULL,
    dark = NULL,
    header = dashboardHeader(
      title = dashboardBrand(
        title = "JAGGview",
        image = "www/logo.png",
        color = "primary"
      ),
      controlbarIcon = icon("sliders"),
      navbarMenu(
        id = "navmenu",
        navbarTab(
          tabName = "tab_fits",
          text = "Fits"
        ),
        navbarTab(
          tabName = "tab_runs_tests",
          text = "Runs Tests"
        ),
        navbarTab(
          tabName = "tab_cpue_residuals",
          text = "CPUE Residuals"
        ),
        navbarTab(
          tabName = "tab_priors_posteriors",
          text = "Priors x Posteriors"
        ),
        navbarTab(
          tabName = "tab_retrospective_analysis",
          text = "Retrospective Analysis"
        ),
        navbarTab(
          tabName = "tab_hindcast",
          text = "Hindcast"
        ),
        navbarTab(
          tabName = "tab_trajectories",
          text = "Trajectories"
        ),
        navbarTab(
          tabName = "tab_kobe",
          text = "Kobe"
        ),
        navbarTab(
          tabName = "tab_summary_table",
          text = "Summary Table"
        )
      )
    ),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css"),
      ),
      useShinyjs(),
      tabItems(
        tabItem(
          tabName = "tab_fits",
          div(
            style = paste("height:", plot_height),
            plotlyOutput("fits", width = "100%", height = "100%")
          )
        ),
        tabItem(
          tabName = "tab_runs_tests",
          div(
            style = paste("height:", plot_height),
            plotlyOutput("runs_tests", width = "100%", height = "100%")
          )
        ),
        tabItem(
          tabName = "tab_cpue_residuals",
          div(
            style = paste("height:", plot_height),
            plotlyOutput("cpue_residuals", width = "100%", height = "100%")
          )
        ),
        tabItem(
          tabName = "tab_priors_posteriors",
          tabsetPanel(
            id = "priors_posteriors_tabs",
            tabPanel(
              title = "Carrying Capacity",
              value = "tab_pp_K",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput(
                  "priors_posteriors_K", width = "100%", height = "100%"
                )
              )
            ),
            tabPanel(
              title = "Intrinsic growth rate",
              value = "tab_pp_r",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput(
                  "priors_posteriors_r", width = "100%", height = "100%"
                )
              )
            ),
            tabPanel(
              title = "Initial biomass depletion ratio",
              value = "tab_pp_psi",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput(
                  "priors_posteriors_psi", width = "100%", height = "100%"
                )
              )
            )
          )
        ),
        tabItem(
          tabName = "tab_retrospective_analysis",
          tabsetPanel(
            id = "retrospective_analysis_tabs",
            tabPanel(
              title = "Biomass",
              value = "tab_ra_B",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput(
                  "retrospective_analysis_B", width = "100%", height = "100%"
                )
              )
            ),
            tabPanel(
              title = "Fishing Mortality",
              value = "tab_ra_F",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput(
                  "retrospective_analysis_F", width = "100%", height = "100%"
                )
              )
            ),
            tabPanel(
              title = tags$span(
                "B/B", tags$sub("MSY", style = "margin-left: -3px;")
              ),
              value = "tab_ra_BBmsy",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput(
                  "retrospective_analysis_BBmsy", 
                  width = "100%", height = "100%"
                )
              )
            ),
            tabPanel(
              title = tags$span(
                "F/F", tags$sub("MSY", style = "margin-left: -3px;")
              ),
              value = "tab_ra_FFmsy",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput(
                  "retrospective_analysis_FFmsy", 
                  width = "100%", height = "100%"
                )
              )
            ),
            tabPanel(
              title = "Process Error",
              value = "tab_ra_procB",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput(
                  "retrospective_analysis_procB", 
                  width = "100%", height = "100%"
                )
              )
            ),
            tabPanel(
              title = "Surplus Production",
              value = "tab_ra_MSY",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput(
                  "retrospective_analysis_MSY", width = "100%", height = "100%"
                )
              )
            )
          )
        ),
        tabItem(
          tabName = "tab_hindcast",
          div(
            style = paste("height:", plot_height),
            plotlyOutput("hindcast", width = "100%", height = "100%")
          )
        ),
        tabItem(
          tabName = "tab_trajectories",
          tabsetPanel(
            id = "trajectories_tabs",
            tabPanel(
              title = tags$span(
                "B/B", tags$sub("0", style = "margin-left: -3px;")
              ),
              value = "tab_traj_BB0",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput(
                  "trajectories_BB0", width = "100%", height = "100%"
                )
              )
            ),
            tabPanel(
              title = tags$span(
                "B/B", tags$sub("MSY", style = "margin-left: -3px;")
              ),
              value = "tab_traj_BBmsy",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput(
                  "trajectories_BBmsy", width = "100%", height = "100%"
                )
              )
            ),
            tabPanel(
              title = tags$span(
                "F/F", tags$sub("MSY", style = "margin-left: -3px;")
              ),
              value = "tab_traj_FFmsy",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput(
                  "trajectories_FFmsy", width = "100%", height = "100%"
                )
              )
            ),
            tabPanel(
              title = "Process error",
              value = "tab_traj_Bdev",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput(
                  "trajectories_Bdev", width = "100%", height = "100%"
                )
              )
            ),
            tabPanel(
              title = "Biomass",
              value = "tab_traj_B",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput("trajectories_B", width = "100%", height = "100%")
              )
            ),
            tabPanel(
              title = "Harvest rate",
              value = "tab_traj_H",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput("trajectories_H", width = "100%", height = "100%")
              )
            ),
            tabPanel(
              title = "Catch",
              value = "tab_traj_Catch",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput(
                  "trajectories_Catch", width = "100%", height = "100%"
                )
              )
            )
          )
        ),
        tabItem(
          tabName = "tab_kobe",
          div(
            style = paste("height:", plot_height),
            plotlyOutput("kobe", width = "100%", height = "100%")
          )
        ),
        tabItem(
          tabName = "tab_summary_table",
          tabsetPanel(
            id = "summary_tables_tabs",
            tabPanel(
              title = "Estimates",
              value = "tab_sumtbl_estimates",
              div(
                style = paste("height:", plot_height_tab),
                gt_output("summary_table_estimates")
              )
            ),
            tabPanel(
              title = "Parameters",
              value = "tab_sumtbl_pars",
              div(
                style = paste("height:", plot_height_tab),
                gt_output("summary_table_pars")
              )
            ),
            tabPanel(
              title = "Stats",
              value = "tab_sumtbl_stats",
              div(
                style = paste("height:", plot_height_tab),
                gt_output("summary_table_stats")
              )
            ),
            tabPanel(
              title = "Hindcast Estimates",
              value = "tab_sumtbl_hc_estimates",
              div(
                style = paste("height:", plot_height_tab),
                gt_output("summary_table_hc_estimates")
              )
            ),
            tabPanel(
              title = "Hindcast Parameters",
              value = "tab_sumtbl_hc_pars",
              div(
                style = paste("height:", plot_height_tab),
                gt_output("summary_table_hc_pars")
              )
            ),
            tabPanel(
              title = "Hindcast Stats",
              value = "tab_sumtbl_hc_stats",
              div(
                style = paste("height:", plot_height_tab),
                gt_output("summary_table_hc_stats")
              )
            ),
            tabPanel(
              title = "MASE",
              value = "tab_sumtbl_mase",
              div(
                style = paste("height:", plot_height_tab),
                gt_output("summary_table_mase")
              )
            ),
            tabPanel(
              title = "PPMR",
              value = "tab_sumtbl_ppmr",
              div(
                style = paste("height:", plot_height_tab),
                gt_output("summary_table_ppmr")
              )
            ),
            tabPanel(
              title = "PPVR",
              value = "tab_sumtbl_ppvr",
              div(
                style = paste("height:", plot_height_tab),
                gt_output("summary_table_ppvr")
              )
            ),
            tabPanel(
              title = "Reference Points",
              value = "tab_sumtbl_refpts",
              div(
                style = paste("height:", plot_height_tab),
                gt_output("summary_table_refpts")
              )
            ),
            tabPanel(
              title = "Retrospective Bias Metrics",
              value = "tab_sumtbl_rho",
              div(
                style = paste("height:", plot_height_tab),
                gt_output("summary_table_rho")
              )
            )
          )
        )
      )
    ),
    controlbar = dashboardControlbar(
      id = "controlbar",
      controlbarMenu(
        id = "controlbarMenu",
        type = "hidden",
        controlbarItem(
          title = "Filter",
          icon = icon("filter"),
          conditionalPanel(
            condition = "input.navmenu == 'tab_fits'",
            selectInput(
              inputId = "fits_scenarios",
              label = "Scenarios: ",
              choices = unique(fits_data$Scenario),
              selected = unique(fits_data$Scenario),
              multiple = TRUE
            ),
            selectInput(
              inputId = "fits_indices",
              label = "Indices: ",
              choices = unique(fits_data$Index),
              selected = unique(fits_data$Index),
              multiple = TRUE
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "fits_title_x",
                label = div(
                  class = "title-container",
                  "Title X:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Year",
                value = "Year"
              )
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "fits_title_y",
                label = div(
                  class = "title-container",
                  "Title Y:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Abundance index",
                value = "Abundance index"
              )
            ),
            colourInput(
              inputId = "fits_color",
              label = "Select color: ",
              value = "#1B4F8A"
            ),
            div(
              div(
                strong("X limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px; ",
                numericInput(
                  inputId = "fits_x_min",
                  label = NULL,
                  value = min(fits_data$Year, na.rm = TRUE),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "fits_x_max",
                  label = NULL,
                  value = max(fits_data$Year, na.rm = TRUE),
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "fits_y_min",
                  label = NULL,
                  value = .round_to_nearest(
                    min(fits_data$lci_95, na.rm = TRUE), FALSE
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "fits_y_max",
                  label = NULL,
                  value = .round_to_nearest(
                    max(fits_data$uci_95, na.rm = TRUE), TRUE
                  ),
                  width = "100%"
                )
              )
            ),
            div(
              class = "input-wrapper",
              checkboxInput(
                inputId = "fits_si_suffix", 
                label = "Use SI suffixes", 
                value = use_si_suffix
              ),
              div(
                class = "info-container",
                div(
                  class = "title-card",
                  icon("circle-info"),
                  div(
                    class = "title-popup hover-popup",
                    "If marked, then hover information will use formatted",
                    " numbers with International System (SI) of prefixes",
                    div(class = "info-card-popup-triangle")
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_runs_tests'",
            selectInput(
              inputId = "runs_tests_scenarios",
              label = "Scenarios: ",
              choices = unique(res_data$cpue_residuals$Scenario),
              selected = unique(res_data$cpue_residuals$Scenario),
              multiple = TRUE
            ),
            selectInput(
              inputId = "runs_tests_indices",
              label = "Indices: ",
              choices = unique(res_data$cpue_residuals$Index),
              selected = unique(res_data$cpue_residuals$Index),
              multiple = TRUE
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "runs_tests_title_x",
                label = div(
                  class = "title-container",
                  "Title X:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Year",
                value = "Year"
              )
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "runs_tests_title_y",
                label = div(
                  class = "title-container",
                  "Title Y:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Residuals",
                value = "Residuals"
              )
            ),
            numericInput(
              inputId = "runs_tests_text_size",
              label = "Select text size: ",
              value = 16,
              width = "100%"
            ),
            div(
              div(
                strong("X limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "runs_tests_x_min",
                  label = NULL,
                  value = min(res_data$SE3$ymin),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "runs_tests_x_max",
                  label = NULL,
                  value = max(res_data$SE3$ymax),
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "runs_tests_y_min",
                  label = NULL,
                  value = .round_to_nearest(
                    min(res_data$SE3$lcl, na.rm = TRUE), FALSE, 2.5
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "runs_tests_y_max",
                  label = NULL,
                  value = .round_to_nearest(
                    max(res_data$SE3$ucl, na.rm = TRUE), TRUE, 2.5
                  ),
                  width = "100%"
                )
              )
            ),
            selectInput(
              inputId = "runs_tests_position",
              label = "Position:",
              choices = c("top-left", "top-center", "top-right", "bottom-left", 
              "bottom-center", "bottom-right"),
              selected = "top-left",
              multiple = FALSE,
              width = "100%"
            ),
            div(
              class = "input-wrapper",
              checkboxInput(
                inputId = "runs_tests_si_suffix", 
                label = "Use SI suffixes", 
                value = use_si_suffix
              ),
              div(
                class = "info-container",
                div(
                  class = "title-card",
                  icon("circle-info"),
                  div(
                    class = "title-popup hover-popup",
                    "If marked, then hover information will use formatted",
                    " numbers with International System (SI) of prefixes",
                    div(class = "info-card-popup-triangle")
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_cpue_residuals'",
            selectInput(
              inputId = "cpue_res_scenarios",
              label = "Scenarios: ",
              choices = unique(res_data$cpue_residuals$Scenario),
              selected = unique(res_data$cpue_residuals$Scenario),
              multiple = TRUE
            ),
            selectInput(
              inputId = "cpue_res_indices",
              label = "Indices: ",
              choices = unique(res_data$cpue_residuals$Index),
              selected = unique(res_data$cpue_residuals$Index),
              multiple = TRUE
            ),
            uiOutput("cpue_res_color_inputs"),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "cpue_res_title_x",
                label = div(
                  class = "title-container",
                  "Title X:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Year",
                value = "Year"
              )
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "cpue_res_title_y",
                label = div(
                  class = "title-container",
                  "Title Y:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Residuals",
                value = "Residuals"
              )
            ),
            numericInput(
              inputId = "cpue_res_text_size",
              label = "Select text size: ",
              value = 16,
              width = "100%"
            ),
            div(
              div(
                strong("X limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "cpue_res_x_min",
                  label = NULL,
                  value = min(res_data$cpue_residuals$Year, na.rm = TRUE),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "cpue_res_x_max",
                  label = NULL,
                  value = max(res_data$cpue_residuals$Year, na.rm = TRUE),
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "cpue_res_y_min",
                  label = NULL,
                  value = .round_to_nearest(
                    min(res_data$cpue_residuals$Res, na.rm = TRUE), FALSE
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "cpue_res_y_max",
                  label = NULL,
                  value = .round_to_nearest(
                    max(res_data$cpue_residuals$Res, na.rm = TRUE), TRUE
                  ),
                  width = "100%"
                )
              )
            ),
            selectInput(
              inputId = "cpue_res_position",
              label = "Position:",
              choices = c("top-left", "top-center", "top-right", "bottom-left", 
              "bottom-center", "bottom-right"),
              selected = "top-left",
              multiple = FALSE,
              width = "100%"
            ),
            div(
              class = "input-wrapper",
              checkboxInput(
                inputId = "cpue_res_si_suffix", 
                label = "Use SI suffixes", 
                value = use_si_suffix
              ),
              div(
                class = "info-container",
                div(
                  class = "title-card",
                  icon("circle-info"),
                  div(
                    class = "title-popup hover-popup",
                    "If marked, then hover information will use formatted",
                    " numbers with International System (SI) of prefixes",
                    div(class = "info-card-popup-triangle")
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_priors_posteriors' && 
              input.priors_posteriors_tabs == 'tab_pp_K'",
            selectInput(
              inputId = "pp_K_scenarios",
              label = "Scenarios: ",
              choices = unique(
                c(pp_data$prior$Scenario, pp_data$posterior$Scenario)
              ),
              selected = unique(
                c(pp_data$prior$Scenario, pp_data$posterior$Scenario)
              ),
              multiple = TRUE
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "pp_K_title_x",
                label = div(
                  class = "title-container",
                  "Title X:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Carrying capacity (K)",
                value = "Carrying capacity (K)"
              )
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "pp_K_title_y",
                label = div(
                  class = "title-container",
                  "Title Y:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Density",
                value = "Density"
              )
            ),
            colourInput(
              inputId = "pp_K_prior_color",
              label = "Select color: ",
              value = "#1B4F8A"
            ),
            colourInput(
              inputId = "pp_K_posterior_color",
              label = "Select color: ",
              value = "#2A9D5C"
            ),
            numericInput(
              inputId = "pp_K_text_size",
              label = "Select text size: ",
              value = 16,
              width = "100%"
            ),
            div(
              div(
                strong("X limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "pp_K_x_min",
                  label = NULL,
                  value = floor(
                    min(
                      pp_data$prior$K01, pp_data$posterior$K01, na.rm = TRUE
                    ) - 1
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "pp_K_x_max",
                  label = NULL,
                  value = ceiling(
                    quantile(
                      c(pp_data$prior$K01, pp_data$posterior$K01), 
                      0.95, na.rm = TRUE
                    )
                  ),
                  width = "100%"
                )
              )
            ),
            selectInput(
              inputId = "pp_K_position",
              label = "Position:",
              choices = c("top-left", "top-center", "top-right", "bottom-left", 
              "bottom-center", "bottom-right"),
              selected = "top-left",
              multiple = FALSE,
              width = "100%"
            ),
            div(
              class = "input-wrapper",
              checkboxInput(
                inputId = "pp_K_si_suffix", 
                label = "Use SI suffixes", 
                value = use_si_suffix
              ),
              div(
                class = "info-container",
                div(
                  class = "title-card",
                  icon("circle-info"),
                  div(
                    class = "title-popup hover-popup",
                    "If marked, then hover information will use formatted",
                    " numbers with International System (SI) of prefixes",
                    div(class = "info-card-popup-triangle")
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_priors_posteriors' && 
              input.priors_posteriors_tabs == 'tab_pp_r'",
            selectInput(
              inputId = "pp_r_scenarios",
              label = "Scenarios: ",
              choices = unique(
                c(pp_data$prior$Scenario, pp_data$posterior$Scenario)
              ),
              selected = unique(
                c(pp_data$prior$Scenario, pp_data$posterior$Scenario)
              ),
              multiple = TRUE
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "pp_r_title_x",
                label = div(
                  class = "title-container",
                  "Title X:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Intrinsic growth rate (r)",
                value = "Intrinsic growth rate (r)"
              )
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "pp_r_title_y",
                label = div(
                  class = "title-container",
                  "Title Y:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Density",
                value = "Density"
              )
            ),
            colourInput(
              inputId = "pp_r_prior_color",
              label = "Select color: ",
              value = "#1B4F8A"
            ),
            colourInput(
              inputId = "pp_r_posterior_color",
              label = "Select color: ",
              value = "#2A9D5C"
            ),
            numericInput(
              inputId = "pp_r_text_size",
              label = "Select text size: ",
              value = 16,
              width = "100%"
            ),
            div(
              div(
                strong("X limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "pp_r_x_min",
                  label = NULL,
                  value = round(
                    min(pp_data$prior$r01, pp_data$posterior$r01, na.rm = TRUE),
                    3
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "pp_r_x_max",
                  label = NULL,
                  value = round(
                    max(pp_data$prior$r01, pp_data$posterior$r01, na.rm = TRUE),
                    3
                  ),
                  width = "100%"
                )
              )
            ),
            selectInput(
              inputId = "pp_r_position",
              label = "Position:",
              choices = c("top-left", "top-center", "top-right", "bottom-left", 
              "bottom-center", "bottom-right"),
              selected = "top-left",
              multiple = FALSE,
              width = "100%"
            ),
            div(
              class = "input-wrapper",
              checkboxInput(
                inputId = "pp_r_si_suffix", 
                label = "Use SI suffixes", 
                value = use_si_suffix
              ),
              div(
                class = "info-container",
                div(
                  class = "title-card",
                  icon("circle-info"),
                  div(
                    class = "title-popup hover-popup",
                    "If marked, then hover information will use formatted",
                    " numbers with International System (SI) of prefixes",
                    div(class = "info-card-popup-triangle")
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_priors_posteriors' && 
              input.priors_posteriors_tabs == 'tab_pp_psi'",
            selectInput(
              inputId = "pp_psi_scenarios",
              label = "Scenarios: ",
              choices = unique(
                c(pp_data$prior$Scenario, pp_data$posterior$Scenario)
              ),
              selected = unique(
                c(pp_data$prior$Scenario, pp_data$posterior$Scenario)
              ),
              multiple = TRUE
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "pp_psi_title_x",
                label = div(
                  class = "title-container",
                  "Title X:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Initial biomass depletion ratio (psi)",
                value = "Initial biomass depletion ratio (psi)"
              )
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "pp_psi_title_y",
                label = div(
                  class = "title-container",
                  "Title Y:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Density",
                value = "Density"
              )
            ),
            colourInput(
              inputId = "pp_psi_prior_color",
              label = "Select color: ",
              value = "#1B4F8A"
            ),
            colourInput(
              inputId = "pp_psi_posterior_color",
              label = "Select color: ",
              value = "#2A9D5C"
            ),
            numericInput(
              inputId = "pp_psi_text_size",
              label = "Select text size: ",
              value = 16,
              width = "100%"
            ),
            div(
              div(
                strong("X limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "pp_psi_x_min",
                  label = NULL,
                  value = round(
                    min(
                      pp_data$prior$psi01, pp_data$posterior$psi01, na.rm = TRUE
                    ), 3
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "pp_psi_x_max",
                  label = NULL,
                  value = round(
                    max(
                      pp_data$prior$psi01, pp_data$posterior$psi01, na.rm = TRUE
                    ), 3
                  ),
                  width = "100%"
                )
              )
            ),
            selectInput(
              inputId = "pp_psi_position",
              label = "Position:",
              choices = c("top-left", "top-center", "top-right", "bottom-left", 
              "bottom-center", "bottom-right"),
              selected = "top-left",
              multiple = FALSE,
              width = "100%"
            ),
            div(
              class = "input-wrapper",
              checkboxInput(
                inputId = "pp_psi_si_suffix", 
                label = "Use SI suffixes", 
                value = use_si_suffix
              ),
              div(
                class = "info-container",
                div(
                  class = "title-card",
                  icon("circle-info"),
                  div(
                    class = "title-popup hover-popup",
                    "If marked, then hover information will use formatted",
                    " numbers with International System (SI) of prefixes",
                    div(class = "info-card-popup-triangle")
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_retrospective_analysis' && 
              input.retrospective_analysis_tabs == 'tab_ra_B'",
            selectInput(
              inputId = "ra_B_scenarios",
              label = "Scenarios: ",
              choices = unique(ra_data$data$Scenario),
              selected = unique(ra_data$data$Scenario),
              multiple = TRUE
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "ra_B_title_x",
                label = div(
                  class = "title-container",
                  "Title X:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Year",
                value = "Year"
              )
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "ra_B_title_y",
                label = div(
                  class = "title-container",
                  "Title Y:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Biomass (t)",
                value = "Biomass (t)"
              )
            ),
            numericInput(
              inputId = "ra_B_text_size",
              label = "Select text size: ",
              value = 16,
              width = "100%"
            ),
            div(
              div(
                strong("X limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "ra_B_x_min",
                  label = NULL,
                  value = min((ra_data$data %>% filter(Index == "B"))$Year),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_B_x_max",
                  label = NULL,
                  value = max((ra_data$data %>% filter(Index == "B"))$Year),
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "ra_B_y_min",
                  label = NULL,
                  value = .round_to_nearest(
                    min(
                      (ra_data$data %>% filter(Index == "B"))$lci, 
                      na.rm = TRUE
                    ), FALSE, 1.1
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_B_y_max",
                  label = NULL,
                  value = .round_to_nearest(
                    max(
                      (ra_data$data %>% filter(Index == "B"))$uci, 
                      na.rm = TRUE
                    ), TRUE, 1.1
                  ),
                  width = "100%"
                )
              )
            ),
            selectInput(
              inputId = "ra_B_position",
              label = "Position:",
              choices = c("top-left", "top-center", "top-right", "bottom-left", 
              "bottom-center", "bottom-right"),
              selected = "top-left",
              multiple = FALSE,
              width = "100%"
            ),
            div(
              class = "input-wrapper",
              checkboxInput(
                inputId = "ra_B_si_suffix", 
                label = "Use SI suffixes", 
                value = use_si_suffix
              ),
              div(
                class = "info-container",
                div(
                  class = "title-card",
                  icon("circle-info"),
                  div(
                    class = "title-popup hover-popup",
                    "If marked, then hover information will use formatted",
                    " numbers with International System (SI) of prefixes",
                    div(class = "info-card-popup-triangle")
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_retrospective_analysis' && 
              input.retrospective_analysis_tabs == 'tab_ra_F'",
            selectInput(
              inputId = "ra_F_scenarios",
              label = "Scenarios: ",
              choices = unique(ra_data$data$Scenario),
              selected = unique(ra_data$data$Scenario),
              multiple = TRUE
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "ra_F_title_x",
                label = div(
                  class = "title-container",
                  "Title X:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Year",
                value = "Year"
              )
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "ra_F_title_y",
                label = div(
                  class = "title-container",
                  "Title Y:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Fishing Mortality (F)",
                value = "Fishing Mortality (F)"
              )
            ),
            numericInput(
              inputId = "ra_F_text_size",
              label = "Select text size: ",
              value = 16,
              width = "100%"
            ),
            div(
              div(
                strong("X limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "ra_F_x_min",
                  label = NULL,
                  value = min((ra_data$data %>% filter(Index == "F"))$Year),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_F_x_max",
                  label = NULL,
                  value = max((ra_data$data %>% filter(Index == "F"))$Year),
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "ra_F_y_min",
                  label = NULL,
                  value = .round_to_nearest(
                    min(
                      (ra_data$data %>% filter(Index == "F"))$lci, na.rm = TRUE
                    ), FALSE, 1.1
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_F_y_max",
                  label = NULL,
                  value = .round_to_nearest(
                    max(
                      (ra_data$data %>% filter(Index == "F"))$uci, na.rm = TRUE
                    ), TRUE, 1.1
                  ),
                  width = "100%"
                )
              )
            ),
            selectInput(
              inputId = "ra_F_position",
              label = "Position:",
              choices = c("top-left", "top-center", "top-right", "bottom-left", 
              "bottom-center", "bottom-right"),
              selected = "top-left",
              multiple = FALSE,
              width = "100%"
            ),
            div(
              class = "input-wrapper",
              checkboxInput(
                inputId = "ra_F_si_suffix", 
                label = "Use SI suffixes", 
                value = use_si_suffix
              ),
              div(
                class = "info-container",
                div(
                  class = "title-card",
                  icon("circle-info"),
                  div(
                    class = "title-popup hover-popup",
                    "If marked, then hover information will use formatted",
                    " numbers with International System (SI) of prefixes",
                    div(class = "info-card-popup-triangle")
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_retrospective_analysis' && 
              input.retrospective_analysis_tabs == 'tab_ra_BBmsy'",
            selectInput(
              inputId = "ra_BBmsy_scenarios",
              label = "Scenarios: ",
              choices = unique(ra_data$data$Scenario),
              selected = unique(ra_data$data$Scenario),
              multiple = TRUE
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "ra_BBmsy_title_x",
                label = div(
                  class = "title-container",
                  "Title X:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Year",
                value = "Year"
              )
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "ra_BBmsy_title_y",
                label = div(
                  class = "title-container",
                  "Title Y:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "B/Bmsy",
                value = "B/Bmsy"
              )
            ),
            numericInput(
              inputId = "ra_BBmsy_text_size",
              label = "Select text size: ",
              value = 16,
              width = "100%"
            ),
            div(
              div(
                strong("X limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "ra_BBmsy_x_min",
                  label = NULL,
                  value = min((ra_data$data %>% filter(Index == "BBmsy"))$Year),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_BBmsy_x_max",
                  label = NULL,
                  value = max((ra_data$data %>% filter(Index == "BBmsy"))$Year),
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "ra_BBmsy_y_min",
                  label = NULL,
                  value = .round_to_nearest(
                    min(
                      (ra_data$data %>% filter(Index == "BBmsy"))$lci,
                       na.rm = TRUE
                      ), FALSE, 1.1
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_BBmsy_y_max",
                  label = NULL,
                  value = .round_to_nearest(
                    max(
                      (ra_data$data %>% filter(Index == "BBmsy"))$uci, 
                      na.rm = TRUE
                    ), TRUE, 1.1
                  ),
                  width = "100%"
                )
              )
            ),
            selectInput(
              inputId = "ra_BBmsy_position",
              label = "Position:",
              choices = c("top-left", "top-center", "top-right", "bottom-left", 
              "bottom-center", "bottom-right"),
              selected = "top-left",
              multiple = FALSE,
              width = "100%"
            ),
            div(
              class = "input-wrapper",
              checkboxInput(
                inputId = "ra_BBmsy_si_suffix", 
                label = "Use SI suffixes", 
                value = use_si_suffix
              ),
              div(
                class = "info-container",
                div(
                  class = "title-card",
                  icon("circle-info"),
                  div(
                    class = "title-popup hover-popup",
                    "If marked, then hover information will use formatted",
                    " numbers with International System (SI) of prefixes",
                    div(class = "info-card-popup-triangle")
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_retrospective_analysis' && 
              input.retrospective_analysis_tabs == 'tab_ra_FFmsy'",
            selectInput(
              inputId = "ra_FFmsy_scenarios",
              label = "Scenarios: ",
              choices = unique(ra_data$data$Scenario),
              selected = unique(ra_data$data$Scenario),
              multiple = TRUE
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "ra_FFmsy_title_x",
                label = div(
                  class = "title-container",
                  "Title X:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Year",
                value = "Year"
              )
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "ra_FFmsy_title_y",
                label = div(
                  class = "title-container",
                  "Title Y:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "F/Fmsy",
                value = "F/Fmsy"
              )
            ),
            numericInput(
              inputId = "ra_FFmsy_text_size",
              label = "Select text size: ",
              value = 16,
              width = "100%"
            ),
            div(
              div(
                strong("X limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "ra_FFmsy_x_min",
                  label = NULL,
                  value = min((ra_data$data %>% filter(Index == "FFmsy"))$Year),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_FFmsy_x_max",
                  label = NULL,
                  value = max((ra_data$data %>% filter(Index == "FFmsy"))$Year),
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "ra_FFmsy_y_min",
                  label = NULL,
                  value = .round_to_nearest(
                    min(
                      (ra_data$data %>% filter(Index == "FFmsy"))$lci, 
                      na.rm = TRUE
                    ), FALSE, 1.1
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_FFmsy_y_max",
                  label = NULL,
                  value = .round_to_nearest(
                    max(
                      (ra_data$data %>% filter(Index == "FFmsy"))$uci, 
                      na.rm = TRUE
                    ), TRUE, 1.1
                  ),
                  width = "100%"
                )
              )
            ),
            selectInput(
              inputId = "ra_FFmsy_position",
              label = "Position:",
              choices = c("top-left", "top-center", "top-right", "bottom-left", 
              "bottom-center", "bottom-right"),
              selected = "top-left",
              multiple = FALSE,
              width = "100%"
            ),
            div(
              class = "input-wrapper",
              checkboxInput(
                inputId = "ra_FFmsy_si_suffix", 
                label = "Use SI suffixes", 
                value = use_si_suffix
              ),
              div(
                class = "info-container",
                div(
                  class = "title-card",
                  icon("circle-info"),
                  div(
                    class = "title-popup hover-popup",
                    "If marked, then hover information will use formatted",
                    " numbers with International System (SI) of prefixes",
                    div(class = "info-card-popup-triangle")
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_retrospective_analysis' && 
              input.retrospective_analysis_tabs == 'tab_ra_procB'",
            selectInput(
              inputId = "ra_procB_scenarios",
              label = "Scenarios: ",
              choices = unique(ra_data$data$Scenario),
              selected = unique(ra_data$data$Scenario),
              multiple = TRUE
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "ra_procB_title_x",
                label = div(
                  class = "title-container",
                  "Title X:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Year",
                value = "Year"
              )
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "ra_procB_title_y",
                label = div(
                  class = "title-container",
                  "Title Y:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Process error on log(Biomass)",
                value = "Process error on log(Biomass)"
              )
            ),
            numericInput(
              inputId = "ra_procB_text_size",
              label = "Select text size: ",
              value = 16,
              width = "100%"
            ),
            div(
              div(
                strong("X limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "ra_procB_x_min",
                  label = NULL,
                  value = min((ra_data$data %>% filter(Index == "procB"))$Year),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_procB_x_max",
                  label = NULL,
                  value = max((ra_data$data %>% filter(Index == "procB"))$Year),
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "ra_procB_y_min",
                  label = NULL,
                  value = .round_to_nearest(
                    min(
                      (ra_data$data %>% filter(Index == "procB"))$lci, 
                      na.rm = TRUE
                    ), FALSE, 1.1
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_procB_y_max",
                  label = NULL,
                  value = .round_to_nearest(
                    max(
                      (ra_data$data %>% filter(Index == "procB"))$uci, 
                      na.rm = TRUE
                    ), TRUE, 1.1
                  ),
                  width = "100%"
                )
              )
            ),
            selectInput(
              inputId = "ra_procB_position",
              label = "Position:",
              choices = c("top-left", "top-center", "top-right", "bottom-left", 
              "bottom-center", "bottom-right"),
              selected = "top-left",
              multiple = FALSE,
              width = "100%"
            ),
            div(
              class = "input-wrapper",
              checkboxInput(
                inputId = "ra_procB_si_suffix", 
                label = "Use SI suffixes", 
                value = use_si_suffix
              ),
              div(
                class = "info-container",
                div(
                  class = "title-card",
                  icon("circle-info"),
                  div(
                    class = "title-popup hover-popup",
                    "If marked, then hover information will use formatted",
                    " numbers with International System (SI) of prefixes",
                    div(class = "info-card-popup-triangle")
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_retrospective_analysis' && 
              input.retrospective_analysis_tabs == 'tab_ra_MSY'",
            selectInput(
              inputId = "ra_MSY_scenarios",
              label = "Scenarios: ",
              choices = unique(ra_data$data$Scenario),
              selected = unique(ra_data$data$Scenario),
              multiple = TRUE
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "ra_MSY_title_x",
                label = div(
                  class = "title-container",
                  "Title X:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Biomass (t)",
                value = "Biomass (t)"
              )
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "ra_MSY_title_y",
                label = div(
                  class = "title-container",
                  "Title Y:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Surplus Production (t)",
                value = "Surplus Production (t)"
              )
            ),
            numericInput(
              inputId = "ra_MSY_text_size",
              label = "Select text size: ",
              value = 16,
              width = "100%"
            ),
            div(
              div(
                strong("X limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "ra_MSY_x_min",
                  label = NULL,
                  value = min(
                    (ra_data$surplus_data %>% filter(Index == "MSY"))$SB_i
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_MSY_x_max",
                  label = NULL,
                  value = max(
                    (ra_data$surplus_data %>% filter(Index == "MSY"))$SB_i
                  ),
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "ra_MSY_y_min",
                  label = NULL,
                  value = .round_to_nearest(
                    min(
                      (ra_data$surplus_data %>% filter(Index == "MSY"))$SP, 
                      na.rm = TRUE
                    ), FALSE, 1.1
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_MSY_y_max",
                  label = NULL,
                  value = .round_to_nearest(
                    max(
                      (ra_data$surplus_data %>% filter(Index == "MSY"))$SP, 
                      na.rm = TRUE
                    ), TRUE, 1.1
                  ),
                  width = "100%"
                )
              )
            ),
            selectInput(
              inputId = "ra_MSY_position",
              label = "Position:",
              choices = c("top-left", "top-center", "top-right", "bottom-left", 
              "bottom-center", "bottom-right"),
              selected = "top-left",
              multiple = FALSE,
              width = "100%"
            ),
            div(
              class = "input-wrapper",
              checkboxInput(
                inputId = "ra_MSY_si_suffix", 
                label = "Use SI suffixes", 
                value = use_si_suffix
              ),
              div(
                class = "info-container",
                div(
                  class = "title-card",
                  icon("circle-info"),
                  div(
                    class = "title-popup hover-popup",
                    "If marked, then hover information will use formatted",
                    " numbers with International System (SI) of prefixes",
                    div(class = "info-card-popup-triangle")
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_hindcast'",
            selectInput(
              inputId = "hc_scenarios",
              label = "Scenarios: ",
              choices = unique(hind_data$data$Scenario),
              selected = unique(hind_data$data$Scenario),
              multiple = TRUE
            ),
            selectInput(
              inputId = "hc_indices",
              label = "Indices: ",
              choices = unique(hind_data$data$Index),
              selected = unique(hind_data$data$Index),
              multiple = TRUE
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "hc_title_x",
                label = div(
                  class = "title-container",
                  "Title X:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Year",
                value = "Year"
              )
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "hc_title_y",
                label = div(
                  class = "title-container",
                  "Title Y:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Index",
                value = "Index"
              )
            ),
            numericInput(
              inputId = "hc_text_size",
              label = "Select text size: ",
              value = 16,
              width = "100%"
            ),
            div(
              div(
                strong("X limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "hc_x_min",
                  label = NULL,
                  value = min(hind_data$data$year),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "hc_x_max",
                  label = NULL,
                  value = max(hind_data$data$year),
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "hc_y_min",
                  label = NULL,
                  value = .round_to_nearest(
                    min(hind_data$data$hat.lci, na.rm = TRUE), FALSE
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "hc_y_max",
                  label = NULL,
                  value = .round_to_nearest(
                    max(hind_data$data$hat.uci, na.rm = TRUE), TRUE
                  ),
                  width = "100%"
                )
              )
            ),
            selectInput(
              inputId = "hc_position",
              label = "Position:",
              choices = c("top-left", "top-center", "top-right", "bottom-left", 
              "bottom-center", "bottom-right"),
              selected = "top-left",
              multiple = FALSE,
              width = "100%"
            ),
            div(
              class = "input-wrapper",
              checkboxInput(
                inputId = "hc_si_suffix", 
                label = "Use SI suffixes", 
                value = use_si_suffix
              ),
              div(
                class = "info-container",
                div(
                  class = "title-card",
                  icon("circle-info"),
                  div(
                    class = "title-popup hover-popup",
                    "If marked, then hover information will use formatted",
                    " numbers with International System (SI) of prefixes",
                    div(class = "info-card-popup-triangle")
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_trajectories' && 
              input.trajectories_tabs == 'tab_traj_BB0'",
            selectInput(
              inputId = "traj_BB0_scenarios",
              label = "Scenarios: ",
              choices = unique(traj_data$Scenario),
              selected = unique(traj_data$Scenario),
              multiple = TRUE
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "traj_BB0_title_x",
                label = div(
                  class = "title-container",
                  "Title X:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Year",
                value = "Year"
              )
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "traj_BB0_title_y",
                label = div(
                  class = "title-container",
                  "Title Y:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "B/B0",
                value = "B/B0"
              )
            ),
            colourInput(
              inputId = "traj_BB0_color",
              label = "Select color: ",
              value = "#1B4F8A"
            ),
            div(
              div(
                strong("X limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "traj_BB0_x_min",
                  label = NULL,
                  value = min(
                    (traj_data %>% filter(indicator == "BB0"))$year, 
                    na.rm = TRUE
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "traj_BB0_x_max",
                  label = NULL,
                  value = max(
                    (traj_data %>% filter(indicator == "BB0"))$year, 
                    na.rm = TRUE
                  ),
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "traj_BB0_y_min",
                  label = NULL,
                  value = .round_to_nearest(
                    min(
                      (traj_data %>% filter(indicator == "BB0"))$lcl, 
                      na.rm = TRUE
                    ), FALSE, 1.1
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "traj_BB0_y_max",
                  label = NULL,
                  value = .round_to_nearest(
                    max(
                      (traj_data %>% filter(indicator == "BB0"))$ucl, 
                      na.rm = TRUE
                    ), TRUE, 1.1
                  ),
                  width = "100%"
                )
              )
            ),
            div(
              class = "input-wrapper",
              checkboxInput(
                inputId = "traj_BB0_si_suffix", 
                label = "Use SI suffixes", 
                value = use_si_suffix
              ),
              div(
                class = "info-container",
                div(
                  class = "title-card",
                  icon("circle-info"),
                  div(
                    class = "title-popup hover-popup",
                    "If marked, then hover information will use formatted",
                    " numbers with International System (SI) of prefixes",
                    div(class = "info-card-popup-triangle")
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_trajectories' && 
              input.trajectories_tabs == 'tab_traj_BBmsy'",
            selectInput(
              inputId = "traj_BBmsy_scenarios",
              label = "Scenarios: ",
              choices = unique(traj_data$Scenario),
              selected = unique(traj_data$Scenario),
              multiple = TRUE
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "traj_BBmsy_title_x",
                label = div(
                  class = "title-container",
                  "Title X:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Year",
                value = "Year"
              )
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "traj_BBmsy_title_y",
                label = div(
                  class = "title-container",
                  "Title Y:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "B/Bmsy",
                value = "B/Bmsy"
              )
            ),
            colourInput(
              inputId = "traj_BBmsy_color",
              label = "Select color: ",
              value = "#1B4F8A"
            ),
            div(
              div(
                strong("X limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "traj_BBmsy_x_min",
                  label = NULL,
                  value = min(
                    (traj_data %>% filter(indicator == "BBmsy"))$year, 
                    na.rm = TRUE
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "traj_BBmsy_x_max",
                  label = NULL,
                  value = max(
                    (traj_data %>% filter(indicator == "BBmsy"))$year, 
                    na.rm = TRUE
                  ),
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "traj_BBmsy_y_min",
                  label = NULL,
                  value = .round_to_nearest(
                    min(
                      (traj_data %>% filter(indicator == "BBmsy"))$lcl, 
                      na.rm = TRUE
                    ), 
                    FALSE, 1.1
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "traj_BBmsy_y_max",
                  label = NULL,
                  value = .round_to_nearest(
                    max(
                      (traj_data %>% filter(indicator == "BBmsy"))$ucl, 
                      na.rm = TRUE
                    ), 
                    TRUE, 1.1
                  ),
                  width = "100%"
                )
              )
            ),
            div(
              class = "input-wrapper",
              checkboxInput(
                inputId = "traj_BBmsy_si_suffix", 
                label = "Use SI suffixes", 
                value = use_si_suffix
              ),
              div(
                class = "info-container",
                div(
                  class = "title-card",
                  icon("circle-info"),
                  div(
                    class = "title-popup hover-popup",
                    "If marked, then hover information will use formatted",
                    " numbers with International System (SI) of prefixes",
                    div(class = "info-card-popup-triangle")
                  )
                )
              )
            ),
            numericInput(
              inputId = "traj_BBmsy_blim",
              label = "Blim:",
              value = 0.4,
              step = 0.1,
              width = "100%"
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_trajectories' && 
              input.trajectories_tabs == 'tab_traj_FFmsy'",
            selectInput(
              inputId = "traj_FFmsy_scenarios",
              label = "Scenarios: ",
              choices = unique(traj_data$Scenario),
              selected = unique(traj_data$Scenario),
              multiple = TRUE
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "traj_FFmsy_title_x",
                label = div(
                  class = "title-container",
                  "Title X:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Year",
                value = "Year"
              )
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "traj_FFmsy_title_y",
                label = div(
                  class = "title-container",
                  "Title Y:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "F/Fmsy",
                value = "F/Fmsy"
              )
            ),
            colourInput(
              inputId = "traj_FFmsy_color",
              label = "Select color: ",
              value = "#1B4F8A"
            ),
            div(
              div(
                strong("X limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "traj_FFmsy_x_min",
                  label = NULL,
                  value = min(
                    (traj_data %>% filter(indicator == "FFmsy"))$year, 
                    na.rm = TRUE
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "traj_FFmsy_x_max",
                  label = NULL,
                  value = max(
                    (traj_data %>% filter(indicator == "FFmsy"))$year, 
                    na.rm = TRUE
                  ),
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "traj_FFmsy_y_min",
                  label = NULL,
                  value = .round_to_nearest(
                    min(
                      (traj_data %>% filter(indicator == "FFmsy"))$lcl, 
                      na.rm = TRUE), FALSE, 1.1
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "traj_FFmsy_y_max",
                  label = NULL,
                  value = .round_to_nearest(
                    max(
                      (traj_data %>% filter(indicator == "FFmsy"))$ucl, 
                      na.rm = TRUE), TRUE, 1.1
                  ),
                  width = "100%"
                )
              )
            ),
            div(
              class = "input-wrapper",
              checkboxInput(
                inputId = "traj_FFmsy_si_suffix", 
                label = "Use SI suffixes", 
                value = use_si_suffix
              ),
              div(
                class = "info-container",
                div(
                  class = "title-card",
                  icon("circle-info"),
                  div(
                    class = "title-popup hover-popup",
                    "If marked, then hover information will use formatted",
                    " numbers with International System (SI) of prefixes",
                    div(class = "info-card-popup-triangle")
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_trajectories' && 
              input.trajectories_tabs == 'tab_traj_Bdev'",
            selectInput(
              inputId = "traj_Bdev_scenarios",
              label = "Scenarios: ",
              choices = unique(traj_data$Scenario),
              selected = unique(traj_data$Scenario),
              multiple = TRUE
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "traj_Bdev_title_x",
                label = div(
                  class = "title-container",
                  "Title X:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Year",
                value = "Year"
              )
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "traj_Bdev_title_y",
                label = div(
                  class = "title-container",
                  "Title Y:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Process Error on log(Biomass)",
                value = "Process Error on log(Biomass)"
              )
            ),
            colourInput(
              inputId = "traj_Bdev_color",
              label = "Select color: ",
              value = "#1B4F8A"
            ),
            div(
              div(
                strong("X limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "traj_Bdev_x_min",
                  label = NULL,
                  value = min(
                    (traj_data %>% filter(indicator == "Bdev"))$year, 
                    na.rm = TRUE
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "traj_Bdev_x_max",
                  label = NULL,
                  value = max(
                    (traj_data %>% filter(indicator == "Bdev"))$year, 
                    na.rm = TRUE
                  ),
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "traj_Bdev_y_min",
                  label = NULL,
                  value = .round_to_nearest(
                    min(
                      (traj_data %>% filter(indicator == "Bdev"))$lcl, 
                      na.rm = TRUE
                    ), FALSE, 1.1
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "traj_Bdev_y_max",
                  label = NULL,
                  value = .round_to_nearest(
                    max(
                      (traj_data %>% filter(indicator == "Bdev"))$ucl, 
                      na.rm = TRUE
                    ), TRUE, 1.1
                  ),
                  width = "100%"
                )
              )
            ),
            div(
              class = "input-wrapper",
              checkboxInput(
                inputId = "traj_Bdev_si_suffix", 
                label = "Use SI suffixes", 
                value = use_si_suffix
              ),
              div(
                class = "info-container",
                div(
                  class = "title-card",
                  icon("circle-info"),
                  div(
                    class = "title-popup hover-popup",
                    "If marked, then hover information will use formatted",
                    " numbers with International System (SI) of prefixes",
                    div(class = "info-card-popup-triangle")
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_trajectories' && 
              input.trajectories_tabs == 'tab_traj_B'",
            selectInput(
              inputId = "traj_B_scenarios",
              label = "Scenarios: ",
              choices = unique(traj_data$Scenario),
              selected = unique(traj_data$Scenario),
              multiple = TRUE
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "traj_B_title_x",
                label = div(
                  class = "title-container",
                  "Title X:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Year",
                value = "Year"
              )
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "traj_B_title_y",
                label = div(
                  class = "title-container",
                  "Title Y:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Biomass (t)",
                value = "Biomass (t)"
              )
            ),
            colourInput(
              inputId = "traj_B_color",
              label = "Select color: ",
              value = "#1B4F8A"
            ),
            div(
              div(
                strong("X limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "traj_B_x_min",
                  label = NULL,
                  value = min(
                    (traj_data %>% filter(indicator == "B"))$year, na.rm = TRUE
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "traj_B_x_max",
                  label = NULL,
                  value = max(
                    (traj_data %>% filter(indicator == "B"))$year, na.rm = TRUE
                  ),
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "traj_B_y_min",
                  label = NULL,
                  value = .round_to_nearest(
                    min(
                      (traj_data %>% filter(indicator == "B"))$lcl, na.rm = TRUE
                    ), FALSE, 1.1
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "traj_B_y_max",
                  label = NULL,
                  value = .round_to_nearest(
                    max(
                      (traj_data %>% filter(indicator == "B"))$ucl, na.rm = TRUE
                    ), TRUE, 1.1
                  ),
                  width = "100%"
                )
              )
            ),
            div(
              class = "input-wrapper",
              checkboxInput(
                inputId = "traj_B_si_suffix", 
                label = "Use SI suffixes", 
                value = use_si_suffix
              ),
              div(
                class = "info-container",
                div(
                  class = "title-card",
                  icon("circle-info"),
                  div(
                    class = "title-popup hover-popup",
                    "If marked, then hover information will use formatted",
                    " numbers with International System (SI) of prefixes",
                    div(class = "info-card-popup-triangle")
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_trajectories' && 
              input.trajectories_tabs == 'tab_traj_H'",
            selectInput(
              inputId = "traj_H_scenarios",
              label = "Scenarios: ",
              choices = unique(traj_data$Scenario),
              selected = unique(traj_data$Scenario),
              multiple = TRUE
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "traj_H_title_x",
                label = div(
                  class = "title-container",
                  "Title X:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Year",
                value = "Year"
              )
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "traj_H_title_y",
                label = div(
                  class = "title-container",
                  "Title Y:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Harvest rate",
                value = "Harvest rate"
              )
            ),
            colourInput(
              inputId = "traj_H_color",
              label = "Select color: ",
              value = "#1B4F8A"
            ),
            div(
              div(
                strong("X limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "traj_H_x_min",
                  label = NULL,
                  value = min(
                    (traj_data %>% filter(indicator == "H"))$year, na.rm = TRUE
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "traj_H_x_max",
                  label = NULL,
                  value = max(
                    (traj_data %>% filter(indicator == "H"))$year, na.rm = TRUE
                  ),
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "traj_H_y_min",
                  label = NULL,
                  value = .round_to_nearest(
                    min(
                      (traj_data %>% filter(indicator == "H"))$lcl, na.rm = TRUE
                    ), 
                    FALSE, 1.1
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "traj_H_y_max",
                  label = NULL,
                  value = .round_to_nearest(
                    max(
                      (traj_data %>% filter(indicator == "H"))$ucl, na.rm = TRUE
                    ), 
                    TRUE, 1.1
                  ),
                  width = "100%"
                )
              )
            ),
            div(
              class = "input-wrapper",
              checkboxInput(
                inputId = "traj_H_si_suffix", 
                label = "Use SI suffixes", 
                value = use_si_suffix
              ),
              div(
                class = "info-container",
                div(
                  class = "title-card",
                  icon("circle-info"),
                  div(
                    class = "title-popup hover-popup",
                    "If marked, then hover information will use formatted",
                    " numbers with International System (SI) of prefixes",
                    div(class = "info-card-popup-triangle")
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_trajectories' && 
              input.trajectories_tabs == 'tab_traj_Catch'",
            selectInput(
              inputId = "traj_Catch_scenarios",
              label = "Scenarios: ",
              choices = unique(traj_data$Scenario),
              selected = unique(traj_data$Scenario),
              multiple = TRUE
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "traj_Catch_title_x",
                label = div(
                  class = "title-container",
                  "Title X:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Year",
                value = "Year"
              )
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "traj_Catch_title_y",
                label = div(
                  class = "title-container",
                  "Title Y:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "Catch",
                value = "Catch"
              )
            ),
            colourInput(
              inputId = "traj_Catch_color",
              label = "Select color: ",
              value = "#1B4F8A"
            ),
            div(
              div(
                strong("X limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "traj_Catch_x_min",
                  label = NULL,
                  value = min(
                    (traj_data %>% filter(indicator == "Catch"))$year, 
                    na.rm = TRUE
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "traj_Catch_x_max",
                  label = NULL,
                  value = max(
                    (traj_data %>% filter(indicator == "Catch"))$year, 
                    na.rm = TRUE
                  ),
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "traj_Catch_y_min",
                  label = NULL,
                  value = .round_to_nearest(
                    min(
                      (traj_data %>% filter(indicator == "Catch"))$lcl, 
                      na.rm = TRUE
                    ), FALSE, 1.1
                  ),
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "traj_Catch_y_max",
                  label = NULL,
                  value = .round_to_nearest(
                    max(
                      (traj_data %>% filter(indicator == "Catch"))$ucl, 
                      na.rm = TRUE
                    ), TRUE, 1.1
                  ),
                  width = "100%"
                )
              )
            ),
            div(
              class = "input-wrapper",
              checkboxInput(
                inputId = "traj_Catch_si_suffix", 
                label = "Use SI suffixes", 
                value = use_si_suffix
              ),
              div(
                class = "info-container",
                div(
                  class = "title-card",
                  icon("circle-info"),
                  div(
                    class = "title-popup hover-popup",
                    "If marked, then hover information will use formatted",
                    " numbers with International System (SI) of prefixes",
                    div(class = "info-card-popup-triangle")
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_kobe'",
            selectInput(
              inputId = "kobe_scenarios",
              label = "Scenarios: ",
              choices = unique(kobe_data$ci_data$Scenario),
              selected = unique(kobe_data$ci_data$Scenario),
              multiple = TRUE
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "kobe_title_x",
                label = div(
                  class = "title-container",
                  "Title X:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "B/Bmsy",
                value = "B/Bmsy"
              )
            ),
            div(
              class = "input-wrapper",
              textInput(
                inputId = "kobe_title_y",
                label = div(
                  class = "title-container",
                  "Title Y:",
                  div(
                    class = "info-container",
                    div(
                      class = "title-card",
                      icon("circle-info"),
                      div(
                        class = "title-popup hover-popup",
                        "Accepts plain text or expressions like B/Bmsy, which will",
                        " be automatically formatted as B/B<sub>MSY</sub>",
                        div(class = "info-card-popup-triangle")
                      )
                    )
                  )
                ),
                placeholder = "F/Fmsy",
                value = "F/Fmsy"
              )
            ),
            div(
              div(
                strong("X limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "kobe_x_min",
                  label = NULL,
                  value = 0,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "kobe_x_max",
                  label = NULL,
                  value = kobe_data$col02$xmax,
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; 
                align-items: center; 
                justify-content: space-between; 
                padding: 0px 15px;",
                numericInput(
                  inputId = "kobe_y_min",
                  label = NULL,
                  value = 0,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "kobe_y_max",
                  label = NULL,
                  value = kobe_data$col02$ymax,
                  width = "100%"
                )
              )
            ),
            div(
              class = "input-wrapper",
              checkboxInput(
                inputId = "kobe_si_suffix", 
                label = "Use SI suffixes", 
                value = use_si_suffix
              ),
              div(
                class = "info-container",
                div(
                  class = "title-card",
                  icon("circle-info"),
                  div(
                    class = "title-popup hover-popup",
                    "If marked, then hover information will use formatted",
                    " numbers with International System (SI) of prefixes",
                    div(class = "info-card-popup-triangle")
                  )
                )
              )
            )
          ),
          div(
            style = "display: flex; 
            margin-top: 25px; 
            margin-bottom: 25px; 
            justify-content: center; 
            align-items: center; 
            width: 100%; 
            height: 100%;",
            actionButton(
              inputId = "confirm_button",
              label = "Confirm Changes",
              width = "90%",
              style = "white-space: normal; 
              paddding-left: 0px; 
              padding-right: 0px;"
            )
          )
        )
      )
    )
  )
}