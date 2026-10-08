#' Launch the report as a dashboard
#' 
#' Launches an interactive \pkg{shiny}/\pkg{bs4Dash} dashboard summarizing 
#' model fits, hindcast, priors x posteriors, retrospective analysis, runs 
#' tests, Kobe plots, and trajectories. The data can be supplied in three ways, 
#' with the following order of priority: (i) \code{filename}, with the path(s) 
#' to one or more saved model results files (\code{.RData} or \code{.rds}), 
#' from which everything is computed internally; (ii) the pre-computed data 
#' structures \code{fits_data}, \code{hind_data}, \code{kobe_data}, 
#' \code{pp_data}, \code{ra_data}, \code{res_data} and \code{traj_data}, as 
#' returned by the respective \code{*_data()} functions of the package; or 
#' (iii) the model objects \code{list_fit_models} and \code{list_hc_models}, 
#' from which the data structures that were not supplied are computed, only
#' if \code{use_models_for_data = TRUE} (see \strong{Details} and
#' \strong{Examples}).
#'
#' @details
#' The data source is chosen in the following order:
#'
#' \enumerate{
#'   \item \strong{\code{filename}}: if supplied, it has priority over all
#'   the other data arguments. All the objects in each file are loaded and
#'   classified by the internal helper \code{.classify_object()} as a model
#'   fit, a hindcast, or an ignored object. Files with other extensions are
#'   not supported. All the data structures are computed from the fits and
#'   hindcasts found in the files, replacing the ones supplied through
#'   \code{fits_data}, \code{hind_data}, etc. If \code{list_fit_models} or
#'   \code{list_hc_models} are empty, the fits and hindcasts found in the
#'   files are used in their place.
#'   \item \strong{No \code{filename}}: each data structure is handled
#'   separately.
#'   \itemize{
#'     \item If it was supplied by the user (for example, \code{kobe_data}),
#'     it is used as it is.
#'     \item If it was left empty, \code{use_models_for_data = TRUE} and the
#'     models are available, it is computed from \code{list_fit_models}
#'     (for \code{fits_data}, \code{pp_data}, \code{res_data},
#'     \code{kobe_data} and \code{traj_data}) or from \code{list_hc_models}
#'     (for \code{hind_data} and \code{ra_data}). \code{kobe_data} and
#'     \code{traj_data} are computed together by the internal helper
#'     \code{.ensemble_data()}.
#'     \item If it was left empty and \code{use_models_for_data = FALSE}, or
#'     there are no models to compute it from, it stays empty, a warning is
#'     issued, and the corresponding section of the dashboard has no data.
#'   }
#' }
#' 
#' With the default \code{use_models_for_data = FALSE}, only the data
#' structures supplied explicitly are used. The model objects
#' (\code{list_fit_models} and \code{list_hc_models}) are still needed to
#' obtain the values shown in the summary tables of the dashboard, and if
#' they are not supplied, those tables have no data. With
#' \code{use_models_for_data = TRUE}, the data structures that were not
#' supplied are computed from the models, so the user can supply only the
#' model objects, or mix them with some pre-computed structures.
#'
#' If \code{verbose = TRUE}, a message reports the origin of each data
#' structure (input files, models or user).
#'
#' The internal helpers \code{.build_server()} and \code{.build_ui()} use the
#' data (computed or supplied) to build the \code{server} and \code{ui}
#' objects, which are passed to \code{shiny::shinyApp()}.
#'
#' The \code{warn_level} argument sets the R option \code{warn} while the
#' data are being prepared, and the previous value is restored when the
#' function exits. It does not change how warnings are handled after the
#' dashboard is launched.
#'
#' The dashboard is launched when the returned object is printed, for example
#' when the function is called at the console. In scripts, use
#' \code{shiny::runApp()} on the returned object.
#' 
#' With \code{animation = TRUE}, \pkg{plotly} may issue many repeated
#' warnings about the \code{frameOrder} attribute. They do not affect the
#' result.
#' 
#' @param fits_data A named list as returned by \code{\link{fits_data}()},
#'   with the elements \code{Li_Ui}, \code{CI_80} and \code{CI_95}. If empty,
#'   it is computed from \code{list_fit_models} only if 
#'   \code{use_models_for_data = TRUE}. Ignored if \code{filename} is supplied. 
#'   Defaults to \code{list()}.
#' @param hind_data A named list as returned by
#'   \code{\link{hindcast_data}()}, with the elements \code{data},
#'   \code{data_points}, \code{data_lines}, \code{mase_data} and
#'   \code{min_year_retro}. If empty, it is computed from 
#'   \code{list_hc_models} only if \code{use_models_for_data = TRUE}. Ignored 
#'   if \code{filename} is supplied. Defaults to \code{list()}.
#' @param kobe_data A named list as returned by \code{\link{kobe_data}()},
#'   with the elements \code{col01}, \code{col02}, \code{col03},
#'   \code{col04}, \code{ci_data}, \code{data_lines} and 
#'   \code{highlight_years}. If empty, it is computed from
#'   \code{list_fit_models} only if \code{use_models_for_data = TRUE}. Ignored 
#'   if \code{filename} is supplied. Defaults to \code{list()}.
#' @param pp_data A named list as returned by
#'   \code{\link{priors_posteriors_data}()}, with the elements \code{prior},
#'   \code{posterior}, \code{PPVR} and \code{PPMR}. If empty, it is computed
#'   from \code{list_fit_models} only if \code{use_models_for_data = TRUE}. 
#'   Ignored if \code{filename} is supplied. Defaults to \code{list()}.
#' @param ra_data A named list as returned by
#'   \code{\link{retrospective_analysis_data}()}, with the elements 
#'   \code{data}, \code{surplus_data} and \code{rho_data}. If empty, it is
#'   computed from \code{list_hc_models}. Ignored if \code{filename} is 
#'   supplied. Defaults to \code{list()}.
#' @param res_data A named list as returned by \code{\link{runs_tests_data}()}, 
#'   with the elements \code{cpue_residuals}, \code{SE3} and \code{RMSE_data}. 
#'   If empty, it is computed from \code{list_fit_models} only if
#'   \code{use_models_for_data = TRUE}. Ignored if \code{filename} is supplied. 
#'   Defaults to \code{list()}.
#' @param traj_data A data frame as returned by
#'   \code{\link{trajectories_data}()}. If empty, it is computed from 
#'   \code{list_fit_models} only if \code{use_models_for_data = TRUE}. Ignored 
#'   if \code{filename} is supplied. Defaults to \code{data.frame()}.
#' @param list_fit_models A list of fits returned by \code{JABBA::fit_jabba()}, 
#'   or a single fit. Used to obtain the values shown in the summary tables
#'   and, if \code{use_models_for_data = TRUE}, to compute the data
#'   structures derived from the fits that were not supplied. If 
#'   \code{filename} is used and this argument is empty, the fits found in
#'   the files are used. Defaults to \code{list()}.
#' @param list_hc_models A list of hindcasts returned by 
#'   \code{JABBA::hindcast_jabba()}, or a single hindcast. Used to obtain the 
#'   values shown in the summary tables and, if 
#'   \code{use_models_for_data = TRUE}, to compute the data structures derived 
#'   from the hindcasts that were not supplied.  If \code{filename} is used and 
#'   this argument is empty, the hindcasts found in the files are
#'   used. Defaults to \code{list()}.
#' @param use_models_for_data A boolean value that if \code{TRUE}, computes 
#'   from \code{list_fit_models} and \code{list_hc_models} the data structures
#'   that were not supplied. If \code{FALSE}, only the data structures
#'   supplied explicitly are used, and the model objects are used just to
#'   obtain the values shown in the summary tables. The sections whose data
#'   are not supplied stay empty and a warning is issued. Ignored if 
#'   \code{filename} is supplied. Defaults to \code{FALSE}.
#' @param dir A character string with the directory where \code{filename} is
#'   located. Defaults to the current working directory (\code{getwd()}).
#' @param filename Optional. A character vector with the name(s) of the
#'   \code{.RData} or \code{.rds} file(s) containing the JABBA model objects
#'   to be loaded. If supplied, it has priority over the other data
#'   arguments. If \code{NULL}, the data are taken from the pre-computed data
#'   structures and/or the model objects. Defaults to \code{NULL}.
#' @param animation A boolean value that if \code{TRUE}, shows animations in
#'   some plots. Defaults to \code{TRUE}. Building the animated plots may
#'   issue repeated \pkg{plotly} warnings (for example, "'scatter' objects
#'   don't have these attributes: 'frameOrder'"). They are harmless and the
#'   animations work normally. Use \code{animation = FALSE} to avoid them,
#'   or \code{warn_level = -1} to hide them.
#' @param verbose A boolean value that if \code{TRUE}, shows progress through
#'   messages in the console. Defaults to \code{FALSE}.
#' @param use_si_suffix A boolean value that if \code{TRUE}, will indicate 
#'   whether SI suffixes will be used, or if \code{FALSE} then shows the 
#'   absolute number, Defaults to \code{FALSE}.
#' @param warn_level An integer value that sets how warnings are handled while
#'   the data are being prepared. Must be one of \code{-1} (warnings are
#'   ignored), \code{0} (warnings are shown after the function finishes, the R
#'   default), \code{1} (warnings are shown immediately) or \code{2}
#'   (warnings are turned into errors). Defaults to \code{1}.
#' 
#' @return A \code{shiny.appobj} object, as returned by
#'   \code{shiny::shinyApp()}. Printing it launches the dashboard in the
#'   default viewer or browser.
#' 
#' @examples
#' \dontrun{
#' # Build the JABBA input, fit the model and run the hindcast per scenario
#' jb.S01 <- JABBA::build_jabba(...)
#' fit.S01 <- JABBA::fit_jabba(jb.S01, ...)
#' hc_S01 <- JABBA::hindcast_jabba(jb.S01, fit.S01, ...)
#'
#' jb.S02 <- JABBA::build_jabba(...)
#' fit.S02 <- JABBA::fit_jabba(jb.S02, ...)
#' hc_S02 <- JABBA::hindcast_jabba(jb.S02, fit.S02, ...)
#'
#' list_fit_models <- list(fit.S01, fit.S02)
#' list_hc_models <- list(hc_S01, hc_S02)
#'
#' # (i) Saved model results (highest priority)
#' create_report(filename = "model_results.RData")
#'
#' # File in another directory, with progress messages
#' create_report(
#'   filename = "model_results.RData", dir = "dev", verbose = TRUE
#' )
#'
#' # (ii) Pre-computed data (the models are still used for the summary tables)
#' fits <- fits_data(list_fit_models)
#' hind <- hindcast_data(list_hc_models)
#' kobe <- kobe_data(list_fit_models)
#' pp <- priors_posteriors_data(list_fit_models)
#' ra <- retrospective_analysis_data(list_hc_models)
#' res <- runs_tests_data(list_fit_models)
#' traj <- trajectories_data(list_fit_models)
#'
#' create_report(
#'   fits_data = fits, hind_data = hind, kobe_data = kobe,
#'   pp_data = pp, ra_data = ra, res_data = res, traj_data = traj,
#'   list_fit_models = list_fit_models, list_hc_models = list_hc_models,
#'   animation = FALSE, use_si_suffix = TRUE
#' )
#'
#' # (iii) Only the models (all the data are computed from them)
#' create_report(
#'   list_fit_models = list_fit_models, list_hc_models = list_hc_models, 
#'   use_models_for_data = TRUE
#' )
#'
#' # Mixed: Kobe and trajectories data supplied, the others computed from the 
#' # models
#' create_report(
#'   kobe_data = kobe, traj_data = traj, list_fit_models = list_fit_models,
#'   list_hc_models = list_hc_models, use_models_for_data = TRUE, verbose = TRUE
#' )
#'
#' # Show warnings only at the end
#' create_report(
#'   list_fit_models = list_fit_models, use_models_for_data = TRUE, 
#'   warn_level = 0
#' )
#' }
#' 
#' @export
#' @importFrom dplyr %>% case_when filter full_join mutate rename select
#' @importFrom purrr map reduce
#' @importFrom bs4Dash actionButton controlbarItem controlbarMenu dashboardBody 
#' dashboardBrand dashboardControlbar dashboardHeader dashboardPage 
#' dashboardSidebar navbarMenu navbarTab tabItem tabItems tabsetPanel 
#' updateControlbar
#' @importFrom shiny addResourcePath checkboxInput conditionalPanel icon 
#' numericInput observeEvent reactive reactiveVal reactiveValues 
#' reactiveValuesToList renderUI req selectInput shinyApp showNotification 
#' tabPanel textInput uiOutput updateSelectInput
#' @importFrom plotly add_lines add_markers add_ribbons add_segments add_text 
#' add_trace animation_button animation_slider ggplotly layout plot_ly 
#' plotlyOutput renderPlotly subplot
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
#' @importFrom gt render_gt gt_output
create_report <- function(
  fits_data = list(), hind_data = list(), kobe_data = list(), 
  pp_data = list(), ra_data = list(), res_data = list(), 
  traj_data = data.frame(), list_fit_models = list(), list_hc_models = list(), 
  use_models_for_data = FALSE, dir = getwd(), filename = NULL, 
  animation = TRUE, verbose = FALSE, use_si_suffix = FALSE, warn_level = 1
) {

  if (!warn_level %in% c(-1, 0, 1, 2) || length(warn_level) != 1) {
    stop("Parameter 'warn_level' must be one of -1, 0, 1 or 2.")
  }
  old_options <- options(warn = warn_level)
  on.exit(options(old_options), add = TRUE)

  # Filename provided path
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
          fits_list <- res$fits_list
          hc_list <- res$hc_list
          ignored_names <- res$ignored_names
        }
        rm(env, name_width, objects)
        gc()
      } else if (ext == "rds") {
        obj <- readRDS(path)
        name <- file_path_sans_ext(basename(path))
        res <- .classify_object(
          obj, name, nchar(name), fits_list, hc_list, ignored_names, verbose
        )
        fits_list <- res$fits_list
        hc_list <- res$hc_list
        ignored_names <- res$ignored_names   
        rm(obj, name)
      } 
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
          fits_data$CI_80 %>% 
            rename(mu_80 = mu, lci_80 = lci, uci_80 = uci), 
          fits_data$CI_95 %>% 
            rename(lci_95 = lci, uci_95 = uci)
        ),
        full_join,
        by = c("Year", "Scenario", "Index")
      )
      fits_data <- fits_data %>% mutate(Year = as.integer(Year))
      .msg_source(verbose, "Fits", "file")
      pp_data <- priors_posteriors_data(fits_list)
      .msg_source(verbose, "Priors x Posterior", "file")
      res_data <- runs_tests_data(fits_list)
      .msg_source(verbose, "Residuals", "file")
      ensemble_data <- .ensemble_data(fits_list)
      kobe_data <- ensemble_data$kobe_dfs
      .msg_source(verbose, "Kobe", "file")
      traj_data <- ensemble_data$trajectories_df
      .msg_source(verbose, "Trajectories", "file")
      rm(ensemble_data)
      if (identical(list_fit_models, list())) list_fit_models <- fits_list
    }
    rm(fits_list, fits_NULL)
    gc()
    if (!hc_NULL) {
      hind_data <- hindcast_data(hc_list)
      .msg_source(verbose, "Hindcast", "file")
      ra_data <- retrospective_analysis_data(hc_list)
      .msg_source(verbose, "Retrospective Analysis", "file")
      if (identical(list_hc_models, list())) {
        list_hc_models <- hc_list
      }
    }
    rm(hc_list, hc_NULL)
    gc()
  }
  # No filename path
  else {
    # Fits
    if (identical(fits_data, list())) {
      if (identical(list_fit_models, list()) || !use_models_for_data) {
        .msg_source(verbose, "Fits", "none")
      }
      else {
        fits_data <- fits_data(list_fit_models)
        .msg_source(verbose, "Fits", "models")
      }
    }
    else {
      .msg_source(verbose, "Fits", "user")
    }
    if (!identical(fits_data, list())) {
      fits <- reduce(
        list(
          fits_data$Li_Ui, 
          fits_data$CI_80 %>% 
            rename(mu_80 = mu, lci_80 = lci, uci_80 = uci), 
          fits_data$CI_95 %>% 
            rename(lci_95 = lci, uci_95 = uci)
        ),
        full_join,
        by = c("Year", "Scenario", "Index")
      )
      fits_data <- fits %>% mutate(Year = as.integer(Year))
      rm(fits)
    }

    # Priors x Posteriors
    if (identical(pp_data, list())) {
      if (identical(list_fit_models, list()) || !use_models_for_data) {
        .msg_source(verbose, "Priors x Posteriors", "none")
      }
      else {
        pp_data <- priors_posteriors_data(list_fit_models)
        .msg_source(verbose, "Priors x Posteriors", "models")
      }
    }
    else {
      .msg_source(verbose, "Priors x Posteriors", "user")
    }

    # Residuals
    if (identical(res_data, list())) {
      if (identical(list_fit_models, list()) || !use_models_for_data) {
        .msg_source(verbose, "Residuals", "none")
      }
      else {
        res_data <- runs_tests_data(list_fit_models)
        .msg_source(verbose, "Residuals", "models")
      }
    }
    else {
      .msg_source(verbose, "Residuals", "user")
    }
    
    # Kobe
    var_kobe <- FALSE
    if (identical(kobe_data, list())) {
      if (identical(list_fit_models, list()) || !use_models_for_data) {
        .msg_source(verbose, "Kobe", "none")
      }
      else {
        var_kobe <- TRUE
        .msg_source(verbose, "Kobe", "models")
      }
    }
    else {
      .msg_source(verbose, "Kobe", "user")
    }

    # Trajectories
    var_traj <- FALSE
    if (identical(traj_data, data.frame())) {
      if (identical(list_fit_models, list()) || !use_models_for_data) {
        .msg_source(verbose, "Trajectories", "none")
      }
      else {
        var_traj <- TRUE
        .msg_source(verbose, "Trajectories", "models")
      }
    }
    else {
      .msg_source(verbose, "Trajectories", "user")
    }

    if (var_kobe || var_traj) {
      ensemble_data <- .ensemble_data(list_fit_models)
      if (var_kobe) kobe_data <- ensemble_data$kobe_dfs
      if (var_traj) traj_data <- ensemble_data$trajectories_df
    }

    # Hindcast
    if (identical(hind_data, list())) {
      if (identical(list_hc_models, list()) || !use_models_for_data) {
        .msg_source(verbose, "Hindcast", "none")
      }
      else {
        hind_data <- hindcast_data(list_hc_models)
        .msg_source(verbose, "Hindcast", "models")
      }
    }
    else {
      .msg_source(verbose, "Hindcast", "user")
    }
    
    # Retrospective Analysis
    if (identical(ra_data, list())) {
      if (identical(list_hc_models, list()) || !use_models_for_data) {
        .msg_source(verbose, "Retrospective Analysis", "none")
      }
      else {
        ra_data <- retrospective_analysis_data(list_hc_models)
        .msg_source(verbose, "Retrospective Analysis", "models")
      }
    }
    else {
      .msg_source(verbose, "Retrospective Analysis", "user")
    }
  }
  
  server <- .build_server(
    fits_data = fits_data,
    hind_data = hind_data,
    kobe_data = kobe_data,
    pp_data = pp_data,
    ra_data = ra_data,
    res_data = res_data,
    traj_data = traj_data,
    list_fit_models = list_fit_models,
    list_hc_models = list_hc_models,
    animation = animation,
    use_si_suffix = use_si_suffix
  )
  
  ui <- .build_ui(
    fits_data = fits_data,
    hind_data = hind_data,
    kobe_data = kobe_data,
    pp_data = pp_data,
    ra_data = ra_data,
    res_data = res_data,
    traj_data = traj_data,
    use_si_suffix = use_si_suffix
  )
  if (verbose) message("Initializing Interactive Data Visualization")
  shinyApp(ui, server)
}
