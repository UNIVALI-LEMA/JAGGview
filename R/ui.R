plot_height <- "calc(100vh - 57px - 30px)"
plot_height_tab <- "calc(100vh - 57px - 30px - 42px)"

.build_ui <- function(
  fits_df, pp_df, res_df, kobe_df, traj_df, hind_df, ra_df
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
        )
      )
    ),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
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
                plotlyOutput("priors_posteriors_K", width = "100%", height = "100%")
              )
            ),
            tabPanel(
              title = "Intrinsic growth rate",
              value = "tab_pp_r",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput("priors_posteriors_r", width = "100%", height = "100%")
              )
            ),
            tabPanel(
              title = "Initial biomass depletion ratio",
              value = "tab_pp_psi",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput("priors_posteriors_psi", width = "100%", height = "100%")
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
                plotlyOutput("retrospective_analysis_B", width = "100%", height = "100%")
              )
            ),
            tabPanel(
              title = "Fishing Mortality",
              value = "tab_ra_F",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput("retrospective_analysis_F", width = "100%", height = "100%")
              )
            ),
            tabPanel(
              title = "B/Bmsy",
              value = "tab_ra_BBmsy",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput("retrospective_analysis_BBmsy", width = "100%", height = "100%")
              )
            ),
            tabPanel(
              title = "F/Fmsy",
              value = "tab_ra_FFmsy",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput("retrospective_analysis_FFmsy", width = "100%", height = "100%")
              )
            ),
            tabPanel(
              title = "Process Error",
              value = "tab_ra_procB",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput("retrospective_analysis_procB", width = "100%", height = "100%")
              )
            ),
            tabPanel(
              title = "Surplus Production",
              value = "tab_ra_MSY",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput("retrospective_analysis_MSY", width = "100%", height = "100%")
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
            tabPanel(
              title = "BB0",
              value = "tab_traj_BB0",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput("trajectories_BB0", width = "100%", height = "100%")
              )
            ),
            tabPanel(
              title = "BBmsy",
              value = "tab_traj_BBmsy",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput("trajectories_BBmsy", width = "100%", height = "100%")
              )
            ),
            tabPanel(
              title = "FFmsy",
              value = "tab_traj_FFmsy",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput("trajectories_FFmsy", width = "100%", height = "100%")
              )
            ),
            tabPanel(
              title = "Bdev",
              value = "tab_traj_Bdev",
              div(
                style = paste("height:", plot_height_tab),
                plotlyOutput("trajectories_Bdev", width = "100%", height = "100%")
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
                plotlyOutput("trajectories_Catch", width = "100%", height = "100%")
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
        )
      )
    ),
    controlbar = dashboardControlbar(
      controlbarMenu(
        id = "controlbarMenu",
        controlbarItem(
          title = "Filter",
          icon = icon("filter"),
          conditionalPanel(
            condition = "input.navmenu == 'tab_fits'",
            selectInput(
              inputId = "fits_scenarios",
              label = "Scenarios: ",
              choices = unique(fits_df$Scenario),
              selected = unique(fits_df$Scenario),
              multiple = TRUE
            ),
            selectInput(
              inputId = "fits_indices",
              label = "Indices: ",
              choices = unique(fits_df$Index),
              selected = unique(fits_df$Index),
              multiple = TRUE
            ),
            textInput(
              inputId = "fits_title_x",
              label = "Title X:",
              placeholder = "Year"
            ),
            textInput(
              inputId = "fits_title_y",
              label = "Title Y:",
              placeholder = "Abundance index"
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
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "fits_x_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "fits_x_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "fits_y_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "fits_y_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_runs_tests'",
            selectInput(
              inputId = "runs_tests_scenarios",
              label = "Scenarios: ",
              choices = unique(res_df$cpue_residuals$Scenario),
              selected = unique(res_df$cpue_residuals$Scenario),
              multiple = TRUE
            ),
            selectInput(
              inputId = "runs_tests_indices",
              label = "Indices: ",
              choices = unique(res_df$cpue_residuals$Index),
              selected = unique(res_df$cpue_residuals$Index),
              multiple = TRUE
            ),
            textInput(
              inputId = "runs_tests_title_x",
              label = "Title X:",
              placeholder = "Year"
            ),
            textInput(
              inputId = "runs_tests_title_y",
              label = "Title Y:",
              placeholder = "Residuals"
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
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "runs_tests_x_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "runs_tests_x_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "runs_tests_y_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "runs_tests_y_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_cpue_residuals'",
            selectInput(
              inputId = "cpue_res_scenarios",
              label = "Scenarios: ",
              choices = unique(res_df$cpue_residuals$Scenario),
              selected = unique(res_df$cpue_residuals$Scenario),
              multiple = TRUE
            ),
            selectInput(
              inputId = "cpue_res_indices",
              label = "Indices: ",
              choices = unique(res_df$cpue_residuals$Index),
              selected = unique(res_df$cpue_residuals$Index),
              multiple = TRUE
            ),
            textInput(
              inputId = "cpue_res_title_x",
              label = "Title X:",
              placeholder = "Year"
            ),
            textInput(
              inputId = "cpue_res_title_y",
              label = "Title Y:",
              placeholder = "Residuals"
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
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "cpue_res_x_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "cpue_res_x_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "cpue_res_y_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "cpue_res_y_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_priors_posteriors' && input.priors_posteriors_tabs == 'tab_pp_K'",
            selectInput(
              inputId = "pp_K_scenarios",
              label = "Scenarios: ",
              choices = unique(c(pp_df$prior$Scenario, pp_df$posterior$Scenario)),
              selected = unique(c(pp_df$prior$Scenario, pp_df$posterior$Scenario)),
              multiple = TRUE
            ),
            textInput(
              inputId = "pp_K_title_x",
              label = "Title X:",
              placeholder = "Carrying capacity (K)"
            ),
            textInput(
              inputId = "pp_K_title_y",
              label = "Title Y:",
              placeholder = "Density"
            ),
            uiOutput("pp_K_color_inputs"),
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
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "pp_K_x_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "pp_K_x_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "pp_K_y_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "pp_K_y_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_priors_posteriors' && input.priors_posteriors_tabs == 'tab_pp_r'",
            selectInput(
              inputId = "pp_r_scenarios",
              label = "Scenarios: ",
              choices = unique(c(pp_df$prior$Scenario, pp_df$posterior$Scenario)),
              selected = unique(c(pp_df$prior$Scenario, pp_df$posterior$Scenario)),
              multiple = TRUE
            ),
            textInput(
              inputId = "pp_r_title_x",
              label = "Title X:",
              placeholder = "Intrinsic growth rate (r)"
            ),
            textInput(
              inputId = "pp_r_title_y",
              label = "Title Y:",
              placeholder = "Density"
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
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "pp_r_x_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "pp_r_x_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "pp_r_y_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "pp_r_y_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_priors_posteriors' && input.priors_posteriors_tabs == 'tab_pp_psi'",
            selectInput(
              inputId = "pp_psi_scenarios",
              label = "Scenarios: ",
              choices = unique(c(pp_df$prior$Scenario, pp_df$posterior$Scenario)),
              selected = unique(c(pp_df$prior$Scenario, pp_df$posterior$Scenario)),
              multiple = TRUE
            ),
            textInput(
              inputId = "pp_psi_title_x",
              label = "Title X:",
              placeholder = "Initial biomass depletion ratio (psi)"
            ),
            textInput(
              inputId = "pp_psi_title_y",
              label = "Title Y:",
              placeholder = "Density"
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
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "pp_psi_x_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "pp_psi_x_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "pp_psi_y_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "pp_psi_y_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_retrospective_analysis' && input.retrospective_analysis_tabs == 'tab_ra_B'",
            selectInput(
              inputId = "ra_B_scenarios",
              label = "Scenarios: ",
              choices = unique(ra_df$data$Scenario),
              selected = unique(ra_df$data$Scenario),
              multiple = TRUE
            ),
            textInput(
              inputId = "ra_B_title_x",
              label = "Title X:",
              placeholder = "Year"
            ),
            textInput(
              inputId = "ra_B_title_y",
              label = "Title Y:",
              placeholder = "Biomass (t)"
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
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "ra_B_x_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_B_x_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "ra_B_y_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_B_y_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_retrospective_analysis' && input.retrospective_analysis_tabs == 'tab_ra_F'",
            selectInput(
              inputId = "ra_F_scenarios",
              label = "Scenarios: ",
              choices = unique(ra_df$data$Scenario),
              selected = unique(ra_df$data$Scenario),
              multiple = TRUE
            ),
            textInput(
              inputId = "ra_F_title_x",
              label = "Title X:",
              placeholder = "Year"
            ),
            textInput(
              inputId = "ra_F_title_y",
              label = "Title Y:",
              placeholder = "Fishing Mortality (F)"
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
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "ra_F_x_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_F_x_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "ra_F_y_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_F_y_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_retrospective_analysis' && input.retrospective_analysis_tabs == 'tab_ra_BBmsy'",
            selectInput(
              inputId = "ra_BBmsy_scenarios",
              label = "Scenarios: ",
              choices = unique(ra_df$data$Scenario),
              selected = unique(ra_df$data$Scenario),
              multiple = TRUE
            ),
            textInput(
              inputId = "ra_BBmsy_title_x",
              label = "Title X:",
              placeholder = "Year"
            ),
            textInput(
              inputId = "ra_BBmsy_title_y",
              label = "Title Y:",
              placeholder = "B/Bmsy"
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
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "ra_BBmsy_x_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_BBmsy_x_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "ra_BBmsy_y_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_BBmsy_y_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_retrospective_analysis' && input.retrospective_analysis_tabs == 'tab_ra_FFmsy'",
            selectInput(
              inputId = "ra_FFmsy_scenarios",
              label = "Scenarios: ",
              choices = unique(ra_df$data$Scenario),
              selected = unique(ra_df$data$Scenario),
              multiple = TRUE
            ),
            textInput(
              inputId = "ra_FFmsy_title_x",
              label = "Title X:",
              placeholder = "Year"
            ),
            textInput(
              inputId = "ra_FFmsy_title_y",
              label = "Title Y:",
              placeholder = "F/Fmsy"
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
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "ra_FFmsy_x_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_FFmsy_x_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "ra_FFmsy_y_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_FFmsy_y_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_retrospective_analysis' && input.retrospective_analysis_tabs == 'tab_ra_procB'",
            selectInput(
              inputId = "ra_procB_scenarios",
              label = "Scenarios: ",
              choices = unique(ra_df$data$Scenario),
              selected = unique(ra_df$data$Scenario),
              multiple = TRUE
            ),
            textInput(
              inputId = "ra_procB_title_x",
              label = "Title X:",
              placeholder = "Year"
            ),
            textInput(
              inputId = "ra_procB_title_y",
              label = "Title Y:",
              placeholder = "Process error on log(Biomass)"
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
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "ra_procB_x_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_procB_x_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "ra_procB_y_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_procB_y_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.navmenu == 'tab_retrospective_analysis' && input.retrospective_analysis_tabs == 'tab_ra_MSY'",
            selectInput(
              inputId = "ra_MSY_scenarios",
              label = "Scenarios: ",
              choices = unique(ra_df$data$Scenario),
              selected = unique(ra_df$data$Scenario),
              multiple = TRUE
            ),
            textInput(
              inputId = "ra_MSY_title_x",
              label = "Title X:",
              placeholder = "Biomass (t)"
            ),
            textInput(
              inputId = "ra_MSY_title_y",
              label = "Title Y:",
              placeholder = "Surplus Production (t)"
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
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "ra_MSY_x_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_MSY_x_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            ),
            div(
              div(
                strong("Y limits:")
              ),
              div(
                style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
                numericInput(
                  inputId = "ra_MSY_y_min",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                ), 
                tags$span(
                  "-",
                  style = "font-size: 20px; color: white; padding: 0 5px;"
                ),
                numericInput(
                  inputId = "ra_MSY_y_max",
                  label = NULL,
                  value = NULL,
                  width = "100%"
                )
              )
            )
          ),
          div(
            style = "display: flex; margin-top: 25px; margin-bottom: 25px; justify-content: center; align-items: center; width: 100%; height: 100%;",
            actionButton(
              inputId = "confirm_button",
              label = "Confirm Changes",
              width = "90%",
              style = "white-space: normal; paddding-left: 0px; padding-right: 0px;"
            )
          )
        )
      )
    )
  )
}