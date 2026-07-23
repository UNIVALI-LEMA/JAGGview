plot_height <- "calc(100vh - 57px - 30px)"
plot_height_tab <- "calc(100vh - 57px - 30px - 42px)"

ui <- dashboardPage(
  help = NULL,
  dark = NULL,
  header = dashboardHeader(
    title = dashboardBrand(
      title = "JAGGview",
      color = "primary"
    ),
    # controlbarIcon = icon("sliders"),
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
  # ),
  # controlbar = dashboardControlbar(
  #   disable = TRUE#,
  #   # controlbarMenu(
  #   #   id = "controlbarMenu",
  #   #   controlbarItem(
  #   #     title = "Filter",
  #   #     icon = icon("filter"),
  #   #     conditionalPanel(
  #   #       condition = "input.navmenu == 'tab_fits'",
  #   #       selectInput(
  #   #         inputId = "fits_scenarios",
  #   #         label = "Scenarios: ",
  #   #         choices = c("S01", "S02"),
  #   #         selected = "S01",
  #   #         multiple = TRUE
  #   #       ),
  #   #       selectInput(
  #   #         inputId = "fits_indices",
  #   #         label = "Indices: ",
  #   #         choices = c("test_index_1", "test_index_2"),
  #   #         selected = c("test_index_1", "test_index_2"),
  #   #         multiple = TRUE
  #   #       ),
  #   #       textInput(
  #   #         inputId = "fits_title_x",
  #   #         label = "Title X:",
  #   #         placeholder = "Year"
  #   #       ),
  #   #       textInput(
  #   #         inputId = "fits_title_y",
  #   #         label = "Title Y:",
  #   #         placeholder = "Abundance index"
  #   #       ),
  #   #       colourInput(
  #   #         inputId = "fits_color",
  #   #         label = "Select color: ",
  #   #         value = "#1B4F8A"
  #   #       ),
  #   #       div(
  #   #         div(
  #   #           strong("X limits:")
  #   #         ),
  #   #         div(
  #   #           style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
  #   #           numericInput(
  #   #             inputId = "fits_x_min",
  #   #             label = NULL,
  #   #             value = NULL,
  #   #             width = "100%"
  #   #           ), 
  #   #           tags$span(
  #   #             "-",
  #   #             style = "font-size: 20px; color: white; padding: 0 5px;"
  #   #           ),
  #   #           numericInput(
  #   #             inputId = "fits_x_max",
  #   #             label = NULL,
  #   #             value = NULL,
  #   #             width = "100%"
  #   #           )
  #   #         )
  #   #       ),
  #   #       div(
  #   #         div(
  #   #           strong("Y limits:")
  #   #         ),
  #   #         div(
  #   #           style = "display: flex; align-items: center; justify-content: space-between; padding: 0px 15px; ",
  #   #           numericInput(
  #   #             inputId = "fits_x_min",
  #   #             label = NULL,
  #   #             value = NULL,
  #   #             width = "100%"
  #   #           ), 
  #   #           tags$span(
  #   #             "-",
  #   #             style = "font-size: 20px; color: white; padding: 0 5px;"
  #   #           ),
  #   #           numericInput(
  #   #             inputId = "fits_x_max",
  #   #             label = NULL,
  #   #             value = NULL,
  #   #             width = "100%"
  #   #           )
  #   #         )
  #   #       )
  #   #     )
  #   #   )
  #   # )
  )
)