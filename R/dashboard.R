#' Launch the report as a dashboard
#' 
#' Launches an interactive \pkg{shiny}/\pkg{bs4Dash} dashboard summarizing 
#' model fits, hindcast, priors x posteriors, retrospective analysis, runs 
#' tests, kobe plots, and trajectories. There are two ways to supply the data 
#' for the dashboard: (i) the main one, passing each pre-computed data 
#' structure directly through \code{fits_data}, \code{hind_data}, 
#' \code{kobe_data}, \code{pp_data}, \code{ra_data}, \code{res_data} and 
#' \code{traj_data}, as returned by the package's respective \code{*_data()} 
#' functions (see \strong{Examples}); or (ii) passing \code{filename} with the 
#' path(s) to one or more saved model results file(s) 
#' (\code{.RData}/\code{.rds}), from which all of those data structures are 
#' computed internally.#' 
#' 
#' @param fits_data A named list of data frames as returned by
#'   \code{fits_data()}. It must contain the elements \code{Li_Ui},
#'   \code{CI_80}, and \code{CI_95}.
#' @param hind_data A named list as returned by \code{hindcast_data()}. It must
#'   contain the elements \code{data}, \code{data_points}, 
#'   \code{data_lines} and \code{mase_data}.
#' @param kobe_data A named list as returned by \code{kobe_data()}. It must 
#'   contain the elements \code{col01}, \code{col02}, \code{col03}, 
#'   \code{col04}, \code{ci_data}, \code{data_lines} and \code{highlight_years}.
#' @param pp_data A named list as returned by \code{priors_posteriors_data()}. 
#'   It must contain the elements \code{prior}, \code{posterior}, \code{PPVR} 
#'   and \code{PPMR}.
#' @param ra_data A named list as returned by
#'   \code{retrospective_analysis_data()}. It must contain the elements 
#'   \code{data}, \code{surplus_data} and \code{rho_data}.
#' @param res_data A named list as returned by \code{runs_tests_data()}. It 
#'   must contain \code{cpue_residuals}, \code{SE3}, \code{RMSE_data}.
#' @param traj_data A data frame as returned by \code{trajectories_data()}.
#' @param filename A character string with the name of the \code{.RData} file 
#'   containing the model objects to be loaded (e.g. results from \pkg{JABBA}). 
#' @param dir A character string with the directory where \code{filename} is 
#'   located. Defaults to the currente working directory (\code{getwd()}).
#' @param animation A boolean value that if TRUE, shows animations in some 
#'   plots. Defaults to TRUE.
#' @param verbose A boolean value that if TRUE, shows progress through messages 
#'   in console. Defaults to FALSE.
#' 
#' @return 
#' Invisibly launch the interactive dashboard, a \pkg{shiny} object produced by 
#' \code{shinyApp()}, in the default viewer/browser.
#' 
#' @details
#' If \code{filename} is supplied, the function loads all objects contained in 
#' each \code{.Rdata}/\code{.rds} file and, through the internal helper 
#' \code{.classify_object()}, which classifies it as a model fit, a hindcast 
#' object, or an ignored object.
#' 
#' From the classified fit objects, the function pre-computes model fits 
#' (\code{fits_data()}), priors x posteriors (\code{priors_posteriors_data()}) 
#' and runs tests (\code{runs_tests_data()}), as well as kobe plots and 
#' trajectories, both obtained together via the internal helper 
#' \code{.ensemble_data()}. From #' the classified hindcast objects, it 
#' pre-computes hindcasts (\code{hindcast_data()}) and retrospective analysis 
#' (\code{retrospective_analysis_data()}).
#' 
#' The internal helpers \code{.build_server()} and \code{.build_ui()} then use 
#' the pre-computed (or user-supplied) data objects above to assemble, 
#' respectively, the \code{server} and \code{ui} objects, which are passed to
#' \code{shinyApp()} to build the Shiny application.
#' 
#' @examples
#' \dontrun{
#' fit.S01 <- fit_jabba()
#' fit.S02 <- fit_jabba()
#' list_fit_models <- list(fit.S01, fit.S02)
#' hc_S01 <- hindcast_jabba()
#' hc_S02 <- hindcast_jabba()
#' list_hc_models <- list(hc_S01, hc_S02)
#' 
#' fits_data <- fits_data(list_fit_models)
#' hind_data <- hindcast_data(list_hc_models)
#' kobe_data <- kobe_data(list_fit_models)
#' pp_data <- priors_posteriors_data(list_fit_models)
#' ra_data <- retrospective_analysis_data(list_hc_models)
#' res_data <- runs_tests_data(list_fit_models)
#' traj_data <- trajectories_data(list_fit_models)
#' 
#' create_report(
#'   fits_data = fits_data, hind_data =  hind_data, kobe_data =  kobe_data,
#'   pp_data = pp_data, ra_data = ra_data, res_data = res_data, 
#'   traj_data = traj_data, animation = FALSE
#' )
#' 
#' create_report(filename = "model_results.RData")
#' 
#' create_report(filename = "model_results.RData", dir = "dev", verbose = TRUE)
#' }
#' 
#' @export
#' @importFrom dplyr %>% case_when filter full_join mutate rename select
#' @importFrom purrr map reduce
#' @importFrom bs4Dash actionButton controlbarItem controlbarMenu dashboardBody 
#' dashboardBrand dashboardControlbar dashboardHeader dashboardPage 
#' dashboardSidebar navbarMenu navbarTab tabItem tabItems tabsetPanel 
#' updateControlbar
#' @importFrom shiny addResourcePath conditionalPanel icon numericInput 
#' observeEvent reactive reactiveVal reactiveValues reactiveValuesToList 
#' renderUI req selectInput shinyApp tabPanel textInput uiOutput
#' @importFrom plotly add_lines add_markers add_ribbons add_segments add_text 
#' add_trace animation_button animation_slider ggplotly layout plot_ly 
#' plotlyOutput renderPlotly subplot toWebGL
#' @importFrom colourpicker colourInput
#' @importFrom htmltools div strong tagList tags
#' @importFrom htmlwidgets onRender
#' @importFrom rlang flatten
#' @importFrom scales alpha
#' @importFrom shinyjs disable enable useShinyjs
#' @importFrom stringr str_split_i
#' @importFrom grDevices colorRampPalette
#' @importFrom JABBA ss3col
#' @importFrom tools file_ext file_path_sans_ext
create_report <- function(
  fits_data = data.frame(), hind_data = list(), kobe_data = list(), 
  pp_data = list(), ra_data = list(), res_data = list(), 
  traj_data = data.frame(), filename = NULL,  dir = getwd(), animation = TRUE, 
  verbose = FALSE
) {

  if (!is.null(filename)) {
    n_files <- length(filename)

    if (n_files == 0 ) {
      stop("Parameter 'filename' expected to have at least one element.")
    }
    if (length(dir) != 1) {
      stop("Parameter 'dir' must have length 1.")
    }

    paths <- file.path(dir, filename)

    missing <- !file.exists(paths)
    if (any(missing)) {
      stop("File (s) not found: ", paste0(paths[missing], collapse = ", "))
    }
    
    if (verbose) cat(sprintf("File(s) found (%d)", n_files))
    
    fits_list <- list()
    hc_list <- list()
    ignored_names <- character(0)

    for (path in paths) {
      if (verbose) cat("\nLoading file: ", path)
      
      ext <- tolower(file_ext(path))

      if (ext == "rdata") {
        env <- new.env(parent = emptyenv())
        objects <- load(path, envir = env)
        name_width <- max(nchar(objects))

        for (name in objects) {
          obj <- get(name, envir = env)
          rm(list = name, envir = env)

          res <- .classify_object(
            obj, name, name_width, fits_list, hc_list, ignored_names, verbose
          )
        }
        rm(env, name_width, objects)
        gc()
      } else if (ext == "rds") {
        obj <- readRDS(path)
        name <- file_path_sans_ext(basename(path))
        res <- .classify_object(
          obj, name, nchar(name), fits_list, hc_list, ignored_names, verbose
        )
        rm(obj, name)
      }
      fits_list <- res$fits_list
      hc_list <- res$hc_list
      ignored_names <- res$ignore_names    
    }
    rm(paths, n_files, missing, res)
    if (verbose) cat("\n")
    
    fits_NULL <- identical(fits_list, list())
    hc_NULL <- identical(hc_list, list())

    if (all(c(fits_NULL, hc_NULL))) {
      stop(
        paste0(
          "No valid data found. The input must contain either fit or hindcast", 
          " data from a JABBA model."
        )
      )
    }

    if (!fits_NULL) {
      fits_data <- fits_data(fits_list)
      fits_data <- reduce(
        list(
          fits_data$Li_Ui, 
          fits_data$CI_80 %>% rename(mu_80 = mu, lci_80 = lci, uci_80 = uci), 
          fits_data$CI_95 %>% rename(lci_95 = lci, uci_95 = uci)
        ),
        full_join,
        by = c("Year", "Scenario", "Index")
      )
      fits_data <- fits_data %>%
        mutate(Year = as.integer(Year))
      if (verbose) message("Fits data was sucessfully obtained")
      pp_data <- priors_posteriors_data(fits_list)
      if (verbose) message("Priors x Posterior data was sucessfully obtained")
      res_data <- runs_tests_data(fits_list)
      if (verbose) message("Residuals data was sucessfully obtained")
      ensemble_data <- .ensemble_data(fits_list)
      kobe_data <- ensemble_data$kobe_datas
      if (verbose) message("Kobe data was sucessfully obtained")
      traj_data <- ensemble_data$trajectories_data
      if (verbose) message("Trajectories data was sucessfully obtained")
      rm(ensemble_data)
    }
    rm(fits_list, fits_NULL)
    gc()
    if (!hc_NULL) {
      hind_data <- hindcast_data(hc_list)
      if (verbose) message("Hindcast data was sucessfully obtained")
      ra_data <- retrospective_analysis_data(hc_list)
      if (verbose) message("Retrospective Analysis data was sucessfully obtained")
    }
    rm(hc_list, hc_NULL)
    gc()
  }
  else {
    if (!identical(fits_data, data.frame())){
      fits <- reduce(
        list(
          fits_data$Li_Ui, 
          fits_data$CI_80 %>% rename(mu_80 = mu, lci_80 = lci, uci_80 = uci), 
          fits_data$CI_95 %>% rename(lci_95 = lci, uci_95 = uci)
        ),
        full_join,
        by = c("Year", "Scenario", "Index")
      )
      fits_data <- fits %>%
        mutate(Year = as.integer(Year))
      rm(fits)
    }
  }
  
  server <- .build_server(
    fits_data = fits_data,
    pp_data = pp_data,
    res_data = res_data,
    kobe_data = kobe_data,
    traj_data = traj_data,
    hind_data = hind_data,
    ra_data = ra_data,
    animation = animation
  )
  
  ui <- .build_ui(
    fits_data = fits_data,
    pp_data = pp_data,
    res_data = res_data,
    kobe_data = kobe_data,
    traj_data = traj_data,
    hind_data = hind_data,
    ra_data = ra_data
  )
  if (verbose) message("Initializing Interactive Data Visualization")
  shinyApp(ui, server)
}
