#' Launch the report as a dashboard
#' 
#' Loads a saved model results file (\code{.RData}) and launches an interactive 
#' \pkg{shiny}/\pkg{bs4Dash} dashboard summarizing the model fits, hindcast, 
#' priors x posteriors, retrospective analysis, runs tests, kobe plots, and 
#' trajectories contained in the file.
#' 
#' @param filename A character string with the name of the \code{.RData} file 
#'   containing the model objects to be loaded (e.g. results from \pkg{JABBA}). 
#' @param dir A character string with the directory where \code{filename} is 
#'   located. Defaults to the currente working directory (\code{getwd()}).
#' @param verbose A boolean value that if TRUE, shows progress through messages 
#'   in console. Defaults to FALSE.
#' 
#' @return 
#' Invisibly launch the interactive dashboard in the default viewer/browser.
#' 
#' @details
#' The function loads all objects in \code{filename} and classifies each one as 
#' a model fit, a hindcast object, or an ignored object, using the internal 
#' helpers \code{.is_fit_jabba()} and \code{.is_hindcast_jabba()}.
#' 
#' From the classified fit and hindcast objects, the function pre-computes all 
#' data structures required by the dashboard panels, including model fits
#' (\code{fits_data()}), hindcasts (\code{hindcast_data()}), kobe plots 
#' (\code{kobe_data()}), priors x posteriors (\code{priors_posteriors_data()}),
#' retrospective analysis (\code{retrospective_analysis_data()}), runs tests
#' (\code{runs_tests_data()}), and trajectories (\code{trajectories_data()}).
#' 
#' The \code{server.R} and \code{ui.R} files, bundled with the package under 
#' \code{R/}, are then sourced into the function's own enviroment (so that the 
#' pre-computed data objects above are directly available to them), and the 
#' resulting \code{ui} and \code{server} objects used to launch the Shiny 
#' application via \code{shinyApp()}.
#' 
#' @examples
#' \dontrun{
#' create_report("model_results.RData")
#' }
#' 
#' @export
#' @importFrom dplyr rename
#' @importFrom purrr reduce map
#' @importFrom bs4Dash dashboardPage dashboardHeader dashboardBrand navbarMenu 
#' navbarTab dashboardSidebar dashboardBody tabItems tabItem tabsetPanel 
#' dashboardControlbar controlbarMenu controlbarItem
#' @importFrom shiny icon tabPanel conditionalPanel selectInput textInput 
#' numericInput shinyApp
#' @importFrom plotly plotlyOutput renderPlotly add_segments add_markers 
#' add_lines add_ribbons add_text layout subplot ggplotly plot_ly add_trace
#' @importFrom colourpicker colourInput
#' @importFrom htmltools div strong tags
#' @importFrom rlang flatten
#' @importFrom dplyr filter
#' @importFrom scales alpha
create_report <- function(filename, dir = getwd(), verbose = FALSE) {
  path <- file.path(dir, filename)

  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }
  if (verbose) cat("File found")

  env <- new.env(parent = emptyenv())
  if (verbose) cat("\nLoading file")
  objects <- load(path, envir = env)

  if (verbose) cat("\nGeting data from file\n")
  data <- mget(objects, envir = env)

  fits_list <- list()
  hc_list <- list()
  ignored_list <- list()

  name_width <- max(nchar(objects))

  for (name in objects) {
    obj <- get(name, envir = env)

    if (.is_fit_jabba(obj)) {
      fits_list <- append(fits_list, list(obj))
      status <- "Identified as fit jabba"
    }
    else if (.is_hindcast_jabba(obj)) {
      hc_list <- append(hc_list, list(obj))
      status <- "Identified as hindcast jabba"
    }
    else {
      ignored_list <- append(ignored_list, list(obj))
      status <- "Ignored file"
    }

    if (verbose) {
      message(sprintf(
        "Checking: %-*s | %s", name_width, name, status
      ))
    }
  }

  fits_df <- fits_data(fits_list)

  li_ui  <- fits_df$Li_Ui
  ci_80  <- fits_df$CI_80 %>% rename(mu_80 = mu, lci_80 = lci, uci_80 = uci)
  ci_95  <- fits_df$CI_95 %>% rename(lci_95 = lci, uci_95 = uci)

  fits_df <- reduce(
    list(li_ui, ci_80, ci_95),
    full_join,
    by = c("Year", "Scenario", "Index")
  )

  fits_df <- fits_df %>%
    mutate(Year = as.integer(Year))
  if (verbose) message("Fits data was sucessfully obtained")
  hind_df <- hindcast_data(hc_list)
  if (verbose) message("Hindcast data was sucessfully obtained")
  pp_df <- priors_posteriors_data(fits_list)
  if (verbose) message("Priors x Posterior data was sucessfully obtained")
  ra_df <- retrospective_analysis_data(hc_list)
  if (verbose) message("Retrospective Analysis data was sucessfully obtained")
  res_df <- runs_tests_data(fits_list)
  if (verbose) message("Residuals data was sucessfully obtained")
  ensemble_df <- .ensemble_data(fits_list)
  kobe_df <- ensemble_df$kobe_dfs
  if (verbose) message("Kobe data was sucessfully obtained")
  traj_df <- ensemble_df$trajectories_df
  if (verbose) message("Trajectories data was sucessfully obtained")
  
  env <- environment()

  sys.source(file.path("R", "server.R"), envir = env)
  sys.source(file.path("R", "ui.R"), envir = env)

  if (verbose) message("Initializing Interactive Data Visualization")
  shinyApp(ui, server)
}
