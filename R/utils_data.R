#' Convert a 3D array into a long-format data frame
#'
#' Internal helper that transforms a three-dimensional array into a
#' data frame by stacking slices along the third dimension and adding
#' index and year columns.
#'
#' @param arr A three-dimensional array.
#'
#' @return A data frame with combined slices and additional columns
#'   identifying the index and year.
#'
#' @keywords internal
.array_to_dataframe <- function(arr) {
  if (length(dim(arr)) != 3) {
    stop("The input must be a three-dimensional array.")
  }
  df_list <- list()
  dims <- dim(arr)
  for (k in 1:dims[3]) {
    slice <- arr[,,k]
    df <- as.data.frame(slice)
    df$Index <- dimnames(arr)[[3]][k]
    df$Year <- as.numeric(row.names(df))
    df_list[[k]] <- df
  }
  result <- do.call(rbind, df_list)
  return(result)
}

#' Replace values based on NA pattern of another data frame
#'
#' Internal helper that replaces values in one data frame with NA
#' wherever the corresponding positions in another data frame are NA.
#'
#' @param df1 A data frame to be modified.
#' @param df2 A data frame providing the NA pattern.
#'
#' @return A data frame with values replaced by NA where applicable.
#'
#' @keywords internal
.replace_na_with_na <- function(df1, df2) {
  df1[] <- lapply(names(df1), function(col) {
    replace(df1[[col]], is.na(df2[[col]]), NA)
  })
  return(df1)
}

#' Validate list of model fits
#'
#' Internal helper that checks whether the input is a valid list of
#' JABBA model outputs and verifies the presence and class of
#' required components.
#'
#' @param list_models A list of model outputs.
#'
#' @return Invisibly returns NULL if all validations pass, otherwise throws an error.
#'
#' @keywords internal
.validate_fits_input_data <- function(list_models) {
  if(.is_fit_jabba(list_models)) {
    stop("Expected a list of valid JABBA model outputs.")
  }

  if(!all(vapply(list_models, .is_fit_jabba, logical(1)))) {
    stop("All elements must be a valid JABBA model output.")
  }

  # fits
  .validate_column(list_models, "settings", "list")
  .validate_column(list_models$settings, "I", c("matrix", "array"))
  .validate_column(list_models$settings, "SE2", c("matrix", "array"))
  .validate_column(list_models, "cpue.ppd", "array")
  .validate_column(list_models, "cpue.hat", "array")
  .validate_column(list_models, "yr", "numeric")
  .validate_column(list_models, "scenario", "character")
  # runs_tests_cpue_residuals
  .validate_column(list_models, "residuals", c("matrix", "array"))
  .validate_column(list_models, "stats", "data.frame")
  # prios_posteriors
  .validate_column(list_models$settings, "K.pr", "numeric")
  .validate_column(list_models$settings, "r.pr", "numeric")
  .validate_column(list_models$settings, "psi.pr", "numeric")
  .validate_column(list_models$settings, "psi.dist", "character")
  .validate_column(list_models$settings, "igamma", "numeric")
  .validate_column(list_models, "pars_posterior", "data.frame")
}

#' Check if object is a valid JABBA model fit
#'
#' Internal helper that verifies whether an object is a list containing
#' all required components of a JABBA model output.
#'
#' @param model An object representing a model output.
#'
#' @return A logical value indicating whether the object is a valid JABBA fit.
#'
#' @keywords internal
.is_fit_jabba <- function(model) {
  cols_fit <- c(
    "assessment", "scenario", "settings", "inputseries", 
    "pars", "estimates", "yr", "catch", "est.catch",
    "cpue.hat", "cpue.ppd", "PPC", "timeseries", "refpts", 
    "pfunc", "diags", "residuals", "std.residuals", 
    "stats", "pars_posterior", "refpts_posterior", "kobe", 
    "flqs", "bppd", "kbtrj", "posteriors"#, "model"
  )
  is.list(model) && all(cols_fit %in% names(model))
}

#' Validate column class
#'
#' Internal helper that checks whether a specific column in a model
#' object inherits from the expected class.
#'
#' @param model A model object.
#' @param column A character string indicating the column name.
#' @param class_expected A character vector of expected class names.
#'
#' @return A logical value indicating whether the column has the expected class.
#'
#' @keywords internal
.is_column_valid <- function(model, column, class_expected) {
  inherits(model[[column]], class_expected)
}

#' Validate column across model list
#'
#' Internal helper that verifies whether a specific column exists
#' in all models and matches the expected class. If any model fails
#' validation, an informative error is thrown.
#'
#' @param list_models A list of model outputs.
#' @param column A character string indicating the column name.
#' @param class_expected A character vector of expected class names.
#'
#' @return Invisibly returns NULL if validation passes, otherwise throws an error.
#'
#' @keywords internal
.validate_column <- function(list_models, column, class_expected) {
  check <- vapply(
    list_models,
    function(m) .is_column_valid(m, column, class_expected),
    logical(1)
  )
  if(!all(check)) {
    invalid_idx <- which(!check)

    received_class <- vapply(
      list_models[invalid_idx],
      function(m) paste(class(m[[column]]), collapse = ", "),
      character(1)
    )

    stop(
      paste0(
        "Invalid '", column, "' in model(s): ", 
        paste(invalid_idx, collapse = ", "),
        ". Expected class: ", paste(class_expected, collapse = ", "),
        ". Received class: ", paste(received_class, collapse = " | ")
      )
    )
  }
}

#' Check if object is a valid hindcast JABBA model result
#' 
#' Internal helper that verifies whether an object is a list containing
#' all required components of a hindcast JABBA model result.
#' 
#' @param obj An object representing a JABBA model output.
#' 
#' @return A logical value indicating wheter the object is a valid hindcast 
#' JABBA result
#' 
#' @keywords internal
.is_hindcast_jabba <- function(obj) {
  if(!is.list(obj)) {
    stop()
  }

  if (length(obj) == 0) return(FALSE)
  
  elem <- obj[[1]]

  .is_fit_jabba(elem)
}

#'Validate list of hindicast models
#' 
#' Internal helper that checks whether the input is a valid list of
#' hindcast JABBA model outputs and verifies the presence and class of
#' required components.
#' 
#' @param list_models A list of model outputs.
#' 
#' @return Invisibily returns NULL if all validation pass, otherwise throws an error.
#' @keywords internal
.validate_hcs_input_data <- function(list_models) {
  
  if(.is_hindcast_jabba(list_models)) {
    stop("Expected a list of valid JABBA model outputs.")
  }

  if(!all(vapply(list_models, .is_hindcast_jabba, logical(1)))) {
    stop("All elements must be a valid JABBA model output.")
  }

  all(
    vapply(
      list_models,
      function(hc) {
        .validate_column(hc, "scenario", "character")
        .validate_column(hc, "timeseries", "array")
        .validate_column(hc, "pfunc", "data.frame")
        .validate_column(hc, "diags", "data.frame")
        TRUE
      },
      logical(1)
    )
  )
}

#' Check if an object is a valid JABBA ensemble output
#'
#' Internal helper that verifies whether an object is a data frame
#' containing the required columns for a JABBA model ensemble output.
#'
#' @param obj An object to be checked.
#'
#' @return A logical value indicating whether the object matches the
#'   expected structure.
#'
#' @keywords internal
.is_jbplot_ensemble <- function(obj) {
  cols <- c(
    "year", "run", "type", "iter", "stock",
    "harvest", "B", "H", "Bdev", "Catch", 
    "BB0", "BBfrac", "Bref"
  )
  is.data.frame(obj) && all(cols %in% names(obj))
}

#' Validate a JABBA ensemble object
#'
#' Internal helper that checks whether a data frame is a valid JABBA
#' model output. It verifies both the presence of required columns and
#' that all relevant columns are numeric.
#'
#' @param object_df A data frame representing JABBA model output.
#'
#' @return Invisibly returns \code{NULL}. Throws an error if validation fails.
#'
#' @details
#' Columns other than \code{year}, \code{run}, \code{type}, and \code{iter}
#' are expected to be numeric. If not, an informative error is raised
#' listing the invalid columns and their classes.
#'
#' @keywords internal
.validate_jbplot_ensemble <- function(object_df) {
  if(!.is_jbplot_ensemble(object_df)) {
    stop("Element must be a valid JABBA model output.")
  }

  cols <- setdiff(names(object_df), c("year", "run", "type", "iter"))
  check <- vapply(
    cols,
    function(col) .is_column_valid(object_df, col, "numeric"),
    logical(1)
  )
  if(!all(check)) {
    invalid_cols <- cols[!check]

    received_class <- vapply(
      invalid_cols,
      function(col) paste(class(object_df[[col]]), collapse = ", "),
      character(1)
    )

    stop(
      paste0(
        "Invalid column(s): ",
        paste(invalid_cols, collapse = ", "),
        ". Expected class: numeric",
        ". Received class: ",
        paste(received_class, collapse = " | ")
      )
    )
  }
}

#' Validate index values against available data
#'
#' Internal helper that checks whether all provided indices exist
#' in the available data indices.
#'
#' @param data_indices A vector of valid indices present in the data.
#' @param factor_indices A vector of indices to be validated.
#'
#' @return Invisibly returns \code{NULL}. Throws an error if any index
#'   is not found in \code{data_indices}.
#'
#' @keywords internal
.validate_indices <- function(data_indices, factor_indices) {
  if (!all(factor_indices %in% data_indices)) {
    stop("All indices past in 'indices' must exist in the data.")
  }
}

#' Validate color palette
#'
#' Checks whether all elements in a character vector are valid R colors.
#'
#' @param pal A character vector of color names or hexadecimal color codes.
#'
#' @return Returns \code{TRUE} if all elements are valid colors.
#'   Otherwise, the function stops with an error.
#'
#' @details
#' The function attempts to convert the provided values using
#' \code{grDevices::col2rgb()}. If any element is not a valid color,
#' an error is raised.
#'
#' @keywords internal
#' @importFrom grDevices col2rgb
.is_palette_valid <- function(pal) {
  res <- try(col2rgb(pal), silent = TRUE)
  if (inherits(res, "try-error")) {
    stop("All elements in palette are expected to be colors.")
  }
  return(TRUE)
}