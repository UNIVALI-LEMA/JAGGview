#' Automatically determine a suitable position for text in plots
#'
#' Internal helper that computes an appropriate (x, y) position for placing
#' text in a plot, based on the distribution of the data.
#'
#' The function combines one or more datasets, evaluates regions with lower
#' values (based on a quantile threshold), and selects a position that avoids
#' dense or high-value areas.
#'
#' @param data_list A data frame or a list of data frames containing the data.
#' @param col_x A string indicating the name of the column to be used as the 
#'   x-axis.
#' @param col_y A string indicating the name of the column to be used as the 
#'   y-axis.
#' @param xlim A numeric vector of length 2, used to informn the x-axis limits 
#'   of the plot.
#' @param ylim A numeric vector of length 2, used to informn the y-axis limits 
#'   of the plot.
#' @param margin A numeric value (between 0 and 0.5) defining the proportion of 
#'   the x-range to exclude from both ends when searching for candidate 
#'   positions. Defaults to 0.1.
#' @param low_quantile A numeric value (between 0 and 1) used to define 
#'   low-value regions in the data. Positions are selected among values below 
#'   this quantile.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{x}{The selected x-coordinate for the text}
#'   \item{y}{The selected y-coordinate for the text}
#' }
#'
#' @details
#' The function first aligns all datasets over a common x-axis and sums the
#' corresponding y-values. It then excludes edge regions based on \code{margin}
#' and identifies candidate positions where the combined y-values fall below
#' a specified quantile (\code{low_quantile}). Among these, the rightmost
#' candidate is selected. If no candidates are found, the global minimum is 
#' used.
#'
#' @keywords internal
.auto_text_position <- function(
  data_list, col_x, col_y, xlim, ylim, margin = 0.1, low_quantile = 0.2, 
  text_width_fraction = 0.15
) {
  
  if (is.data.frame(data_list)) {
    data_list <- list(data_list)
  }
  if (!is.list(data_list)) {
    data_list <- list(data_list)
  }
  if (margin < 0 || margin > 0.5) {
    stop("Expected parameter 'margin' to be between 0 and 0.5.")
  }
  if (low_quantile < 0 || low_quantile > 1) {
    stop("Expected parameter 'low_quantile' to be between 0 and 1.")
  }
  .axis_limit(xlim)

  .axis_limit(ylim)

  all_x <- unique(unlist(lapply(data_list, function(d) d[[col_x]])))
  all_x <- sort(all_x)

  if (!is.null(xlim)) {
    all_x <- all_x[all_x >= xlim[1] & all_x <= xlim[2]]
  }

  if (length(all_x) == 0) {
    stop("No values left after applying xlim")
  }

  y_matrix <- sapply(data_list, function(d) {
    x <- d[[col_x]]
    y <- d[[col_y]]
    
    valid <- !is.na(x) & !is.na(y)
    x <- x[valid]
    y <- y[valid]

    if (!is.null(xlim)) {
      keep <- x >= xlim[1] & x <= xlim[2]
      x <- x[keep]
      y <- y[keep]
    }
    
    y_full <- rep(0, length(all_x))
    match_idx <- match(x, all_x)
    y_full[match_idx] <- y
    
    return(y_full)
  })

  if (is.vector(y_matrix)) {
    y_matrix <- matrix(y_matrix, ncol = 1)
  }

  y_combined <- rowSums(y_matrix)

  x <- all_x
  y <- y_combined

  x_range <- range(x)
  x_min <- x_range[1] + diff(x_range) * margin
  x_max_eff <- x_range[2] - diff(x_range) * margin
  
  inside <- x >= x_min & x <= x_max_eff
  
  x_in <- x[inside]
  y_in <- y[inside]

  if (length(x_in) == 0) {
    x_in <- x
    y_in <- y
  }

  threshold <- quantile(y_in, low_quantile)

  candidates <- which(y_in <= threshold)

  text_width <- diff(x_range) * text_width_fraction

  valid_candidates <- c()

  for(i in candidates) {

  x_right <- x_in[i]
  x_left <- x_right - text_width

  idx_window <- which(
      x >= x_left &
      x <= x_right
    )

    if (length(idx_window) == 0) {
      next
    }

    if (max(y[idx_window]) <= threshold) {
      valid_candidates <- c(valid_candidates, i)
    }
  }

  if (length(valid_candidates) > 0) {
    idx_x <- valid_candidates[
      which.max(x_in[valid_candidates])
    ]
  } else if (length(candidates) > 0) {
    idx_x <- candidates[
      which.max(x_in[candidates])
    ]
  } else {
    idx_x <- which.min(y_in)
  }

  x_pos <- x_in[idx_x]

  y_pos <- ylim[2] - (ylim[2] - ylim[1]) * 0.1

  return(list(x = x_pos, y = y_pos))
}

#' Validate axis limits
#' 
#' Internal helper to validate axis limit vectors. Checks whether the provided 
#' limits contain exactly two numeric values and whether the lower limit is 
#' less than the upper limit.
#' 
#' @param lim A numeric vector of length 2 containing the lower and upper axis 
#'   limits. Can be 'NULL'
#' 
#' @return Invisibly returns 'NULL'. An error is thrown if the validation fails.
#' 
#' @keywords internal
.axis_limit <- function(lim) {
  if (!is.null(lim)) {
    param_name <- deparse(substitute(lim))
    if (length(lim) != 2) {
      stop(paste0(
        "Expected parameter '", param_name, "' to be a vector of length of 2."
      ))
    }
    if (lim[1] > lim[2]) {
      stop(paste0(
        "Expected first value of parameter '", param_name,
        "' to be less than the second."
      ))
    }
    if (!is.numeric(lim)) {
      stop(paste0(
        "Expected parameter '", param_name, "' to have a numeric class."
      ))
    }
  }
}

#' Create a default formatted gt table
#' 
#' Internal helper function used to generate consistently formatted \pkg{gt} 
#' tables across package outputs.
#' 
#' @param data A data frame to be formatted as a \pkg{gt} table.
#' @param digits A integer indicating the number of decimals places to display.
#' 
#' @return
#' A formatted \code{gt} table object with bold column labels and alternating
#' row background colors.
#' 
#' @details
#' This function applies a default visual style to tables, including bold
#' column headers and zebra-striping for improved readability.
#' 
#' @keywords internal
#' @importFrom gt gt tab_style cell_text cells_body cell_fill html fmt_number
#' cells_column_labels cols_label
#' @importFrom dplyr where
.default_table <- function(data, digits) {

  label_map <- list(
    bmsy = html("B<sub>MSY</sub>"),
    fmsy = html("F<sub>MSY</sub>"),
    msy  = "MSY",
    bb0 = html("b/b<sub>0</sub>"),
    k = "K"
  )

  labels <- label_map[names(label_map) %in% names(data)]
  
  table <- gt(data = data) %>%
    fmt_number(
      columns = where(is.numeric),
      decimals = digits
    ) %>%
    cols_label(.list = labels) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = cell_fill(color = "#f7f7f7"),
      locations = cells_body(
        rows = seq_len(nrow(data)) %% 2 == 0
      )
    )
  return(table)
}

#' Format numeric values with custom separators
#'
#' Internal helper that formats numeric values with a specified number of 
#' decimal places and custom thousands and decimal separators.
#'
#' @param number A numeric value (or a numeric vector value).
#' @param decimals Number of decimal places.
#' @param big.mark Thousands separator.
#' @param decimal.mark Decimal separator.
#'
#' @return A character value (or a chracter vector value) with formatted 
#'   numbers.
#'
#' @keywords internal
.format_number <- function(
  number, decimals = 2, big.mark = ",", decimal.mark = "."
) {
  format(
    round(number, decimals),
    big.mark = big.mark,
    decimal.mark = decimal.mark,
    nsmall = decimals,
    scientific = FALSE
  )
}

#' International System of Prefixes
#' 
#' @param number A numeric value (or a numeric vector of values) to be 
#'   formated using SI prefixes.
#' @param decimals Optional. An integer indicating the number of decimals places to 
#'   display in the formatted string.  
#' 
#' @return A character value (or a character vector of values) appended with 
#'   their corresponding SI unit symbol.
#' 
#' @examples 
#' \dontrun{
#'   number <- 1000000
#'   .international_system_prefixes(number, 2)
#' }
#' 
#' @keywords internal
.international_system_prefixes <- function(number, decimals = NULL) {
  format_single <- function(val) {
    if (is.na(val) || val == 0) return("0")
    
    abs_val <- abs(val)
    
    if (abs_val >= 1e6) {
      divisor <- 1e6
      suffix  <- "M"
    } else if (abs_val >= 1e3) {
      divisor <- 1e3
      suffix  <- "k"
    } else {
      divisor <- 1
      suffix  <- ""
    }
    
    scaled <- val / divisor
    
    dec <- if (is.null(decimals)) {
      if (scaled == round(scaled)) 0 else 1
    } else {
      decimals
    }
    
    paste0(format(round(scaled, dec), nsmall = dec, big.mark = ","), suffix)
  }
  
  sapply(number, format_single)
}

#' Validate color palette
#'
#' Checks whether all elements in a character vector are valid R colors.
#'
#' @param pal A character vector of color names or hexadecimal color codes.
#'
#' @return Returns \code{TRUE} if all elements are valid colors. Otherwise, the 
#'   function stops with an error.
#'
#' @details
#' The function attempts to convert the provided values using 
#' \code{grDevices::col2rgb()}. If any element is not a valid color, an error 
#' is raised.
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

#' Generate a default color palette
#' 
#' Creates a default color-blind-friendly palette with \code{n} colors. For up 
#' to three colors a subset of highly contrasting colors is used. For larger 
#' values, colors are selected from a predefined palette and interpolated when 
#' necessary.
#' 
#' @param n Number of colors required.
#' 
#' @return A character vector of hexadecimal color codes.
#' 
#' @keywords internal
.make_index_palette <- function(n) {
  base <- c(
    "#1B4F8A",
    "#17A6A6",
    "#2A9D5C",
    "#A3CB9A",
    "#F0A500",
    "#E05C2A",
    "#C0392B",
    "#8E44AD",
    "#5D6D7E",
    "#85C1E9"
  )
  
  if (n <= length(base)) {
    if (n <= 3) {
      return(base[c(1, 3, 6)[1:n]])
    }
    else {
      return(base[1:n])
    }
  } else {
    return(colorRampPalette(base)(n))
  }
}

#' Create zoom plot for hindcast visualization
#' 
#' Generates a zoomed ggplot for a specific Scenario and Index combination,
#' intended to be embedded as an inset in \code{hindcast_ggplot()}. The plot
#' displays the ribbon, fitted lines, and observed/predicted points restricted
#' to the hindcast window defined by \code{min_year} and \code{max_year}.
#'
#' @param df A \code{JAGGdata} object containing the components \code{data},
#'   \code{hindcast_data_1}, and \code{hindcast_data_2}, as returned by
#'   \code{hindcast_data()}.
#' @param scen A character string specifying the scenario name to filter.
#' @param idx A character string specifying the index name to filter.
#' @param min_year A numeric value indicating the first year of the zoom window
#'   (x-axis left bound).
#' @param max_year A numeric value indicating the last year of the zoom window
#'   (x-axis right bound).
#'
#' @return A \code{ggplot} object representing the zoomed inset plot, or
#'   \code{NULL} if no data is available for the given Scenario and Index
#'   combination.
#'
#' @details
#' This function is called internally by \code{hindcast_ggplot()} via
#' \code{Map()} to produce one inset per Scenario x Index facet. Combinations
#' with no data in \code{hindcast_data_1}, \code{hindcast_data_2}, or the
#' ribbon data are silently skipped by returning \code{NULL}, which prevents
#' empty panels from receiving an inset.
#' 
#' @keywords internal
.make_zoom_plot <- function(df, scen, idx, min_year, max_year) {
  zoom_data <- df$data %>%
    filter(year >= min_year, Scenario == scen, Index == idx)

  zoom_data_ribbon <- zoom_data %>%
    filter(retro.peels == 0)

  hc_data_1 <- df$hindcast_data_1 %>%
    filter(Scenario == scen, Index == idx)

  hc_data_2 <- df$hindcast_data_2 %>%
    filter(Scenario == scen, Index == idx)

  if (nrow(zoom_data_ribbon) == 0 || nrow(hc_data_1) == 0
    || nrow(hc_data_2) == 0) {
    return(NULL)
  }

  ggplot() +
    geom_ribbon(data = zoom_data_ribbon,
      aes(x = year, ymin = hat.lci, ymax = hat.uci), fill = "gray80") +
    geom_line(data = filter(zoom_data, hindcast == FALSE),
      aes(x = year, y = hat, colour = retro), linewidth = 1) +
    geom_line(data = hc_data_2,
      aes(x = year, y = hat, group = retro.peels),
      linewidth = 1, colour = "white") +
    geom_point(data = hc_data_1, show.legend = FALSE,
      aes(x = year, y = obs, fill = retro), pch = 21, size = 4) +
    geom_point(data = hc_data_1, show.legend = FALSE,
      aes(x = year, y = hat, fill = retro), pch = 21, size = 2) +
    labs(x = "Year", y = "Index", colour = "") +
    scale_fill_manual(values = ss3col(8)) +
    scale_colour_manual(values = c("black", ss3col(8))) +
    coord_cartesian(xlim = c(min_year, max_year)) +
    .my_theme() +
    theme(legend.position = "none")
}

#' Custom ggplot2 theme
#'
#' Internal helper that defines a customized ggplot2 theme used across plots in 
#' the package.
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @param rgb01 Primary line color.
#' @param rgb02 Secondary text/element color.
#'
#' @return A ggplot2 theme object.
#'
#' @keywords internal
#' @importFrom ggplot2 theme theme_bw element_line element_text element_blank 
#' element_rect margin %+replace%
.my_theme <- function(
  base_size = 18, base_family = "Lato", rgb01 = "black", rgb02 = "black"
) {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.ticks = element_line(colour = rgb01),
      axis.line = element_line(colour = rgb01, linewidth = 0.3),
      axis.text = element_text(colour = rgb02, size = 14),
      axis.title = element_text(size = 18),
      legend.background = element_blank(),
      legend.key = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_line(
        linetype = "solid",
        linewidth = 0.2,
        colour = "gray90"
      ),
      strip.text = element_text(
        colour = "white",
        margin = ggplot2::margin(
          0.3,
          0.3,
          0.3,
          0.3,
          "cm"
        ),
        face = "bold",
        size = 14
      ),
      strip.background = element_rect(
        fill = "#232425",
        colour = rgb02
      ),
      plot.background = element_blank(),
      plot.margin = margin(
        t = 0.2, r = 0.8, b = 0.4, l = 0.4, unit = "cm"
      ),
      complete = TRUE
    )
}

#' Resolve plotting palette
#' 
#' Returns the palette to be used in a plot. If \code{palette} is \code{NULL}, 
#' a default palette is generated using \code{.make_index_palette()}. Otherwise, 
#' the supplied palette is validated and checked to ensure that it contains at 
#' least \code{num} colors.
#' 
#' @param palette Optional character vector of hexadecimal color codes.
#' @param num Minimum number of colors required.
#' 
#' @return A character vector of hexadecimal color codes.
#' 
#' @keywords internal
.resolve_palette <- function(palette, num) {
  if (is.null(palette)) {
    return(.make_index_palette(num))
  } else {
    .is_palette_valid(palette)
    n_pal <- length(palette)
    if (n_pal < num) {
      stop(paste0(
        "The palette contains ", n_pal," color(s), but ", num, 
        " are required."
      ))
    }
    else {
      return(palette)
    }
  }
}

#' Round values to a convenient axis limit
#'
#' Internal helper that rounds a numeric value to the nearest order of 
#' magnitude, either upward (for maximum values) or downward (for minimum 
#' values), useful for defining plot axis limits.
#'
#' @param value A numeric value.
#' @param max A logical value indicating the rounding direction:
#'   \itemize{
#'     \item \code{TRUE}: round up (used for upper axis limits).
#'     \item \code{FALSE}: round down (used for lower axis limits).
#'   }
#' @param multiplier A number factor applied to the absolute value to create a 
#'   margin for axis limits. 
#'
#' @return A rounded numeric value.
#' 
#' @details
#' When \code{max = FALSE}, positive values are adjusted to include zero when 
#' appropriate, ensuring cleaner lower bounds in plots.
#'
#' @keywords internal
.round_to_nearest <- function(value, max, multiplier = 1.2) {
  sign_val <- sign(value)
  value_abs <- abs(value)

  value_adj <- value_abs * multiplier

  magnitude <- 10^(floor(log10(value_adj)))
  
  result <- ifelse(
    max, 
    ceiling(sign_val * value_adj / magnitude) * magnitude,
    floor(sign_val * value_adj / magnitude) * magnitude
  )

  if (!max) {
    result <- ifelse(result > 0, 0, result)
  }
  
  return(result)
}

#' Execute a heavy function with memory limit
#'
#' Internal helper that executes a function while monitoring the available 
#' system memory in a background process. If the available memory falls bellow
#' a specified safety threshold, the execution is interrupted to reduce the 
#' risk of exhausting system memory. 
#' 
#' @param fn A function to be executed.
#' @param args A named list of arguments passed to \code{fn}. 
#' @param reserve_mb A numeric value specifying the minimum amount of free
#'   system memory, in megabytes, to reserve for the operating system. Defaults 
#'   to 2048.
#' @param poll_interval A numeric value giving the time interval, in seconds, 
#'   between memory availability checks. Defaults to 0.5.
#' 
#' @details
#' This function is, currently, supported only on Linux and macOS, where the 
#' monitored process can be interrupted by sending an external \code{SIGINT} 
#' signal. A lightweight background R process periodically checks the amount of 
#' available system using \pkg{memuse}. If the available memory drops below 
#' \code{reserve_mb}, the main process is interrupted and a 
#' \code{"memoryLimitExceed"} condition is raised.
#' 
#' @keywords internal
#' @importFrom callr r_bg
#' @importFrom ps ps_handle ps_is_running
#' @importFrom memuse Sys.meminfo
#' @importFrom tools pskill SIGINT
.safe_execute_unix <- function(
  fn, args = list(), reserve_mb = 1024 * 2, poll_interval = 0.5
) {

  main_pid <- Sys.getpid()

  watcher <- r_bg(
    func = function(pid ,reserve_mb, poll_interval) {
      repeat {
        handle <- tryCatch(ps::ps_handle(pid),error = function(e) NULL)
        if (
          is.null(handle) || !ps::ps_is_running(handle)
        ) {
          break
        }

        free_ram <- as.numeric(memuse::Sys.meminfo()$freeram) / 1024^2

        if (free_ram < reserve_mb) {
          tools::pskill(pid,signal = tools::SIGINT)
          break
        }

        Sys.sleep(poll_interval)
      }
    },
    args = list(
      pid = main_pid,
      reserve_mb = reserve_mb,
      poll_interval = poll_interval
    )
  )

  on.exit(
    if (watcher$is_alive()) {
      watcher$kill()
    },
    add = TRUE
  )

  result <- tryCatch({do.call(fn, args)}, interrupt = function(i) {
    structure(
      class = c("memoryLimitExceeded", "error", "condition" ),
      list(
        message ="Execution interrupted: memory limit has been exceeded",
        call = sys.call()
      )
    )
  })

  if (inherits(result, "memoryLimitExceeded")) {
    stop(result)
  }

  result
}

#' Execute a function in a background process with memory monitoring
#'
#' Internal helper that executes a function in a separate background process
#' while monitoring the amount of available system memory. If the available
#' memory falls below a specified safety threshold, the background process is
#' interrupted to reduce the risk of exhausting system memory.
#'
#' @param fn A function to be executed in the background process.
#' @param args A named list of arguments passed to \code{fn}.
#' @param reserve_mb A numeric value specifying the minimum amount of free
#'   system memory, in megabytes, to reserve for the operating system.
#'   Defaults to 2048.
#' @param poll_interval A numeric value giving the time interval, in seconds,
#'   between memory availability checks. Defaults to 0.5.
#' @param package A logical value indicating whether the background process
#'   should be initialized with the current package environment. Defaults to
#'   \code{TRUE}.
#'
#' @details
#' The function uses \code{callr::r_bg()} to execute \code{fn} in a separate
#' R process. While the process is running, the available system memory is
#' periodically checked using \code{memuse::Sys.meminfo()}.
#'
#' If the amount of free memory falls below \code{reserve_mb}, the background
#' process is terminated and a \code{"memoryLimitExceeded"} condition is
#' raised. If the background process terminates with a non-zero exit status,
#' its error output is collected and returned as an R error.
#'
#' The background process is also automatically terminated when the function
#' exits, if it is still running.
#'
#' @return
#' The result returned by \code{fn} when the background process completes
#' successfully.
#'
#' @importFrom callr r_bg
#' @importFrom memuse Sys.meminfo
#' @keywords internal
.safe_execute_windows <- function(
  fn, args = list(), reserve_mb = 1024 * 2, poll_interval = 0.5, package = TRUE
) {

  proc <- r_bg(
    func = fn,
    args = args,
    package = package,
    supervise = TRUE
  )

  on.exit(
    if (proc$is_alive()) proc$kill(),
    add = TRUE
  )

  memory_exceeded <- FALSE

  repeat {
    if (!proc$is_alive()) {
      break
    }

    free_ram <- as.numeric(Sys.meminfo()$freeram) / 1024^2

    if (free_ram < reserve_mb) {
      proc$kill()
      memory_exceeded <- TRUE
      break
    }

    Sys.sleep(poll_interval)
  }

  if (memory_exceeded) {
    stop(structure(
      class = c(
        "memoryLimitExceeded",
        "error",
        "condition"
      ),
      list(
        message = "Execution interrupted: memory limit has been exceeded",
        call = sys.call()
      )
    ))
  }

  exit_status <- proc$get_exit_status()

  if (!identical(exit_status, 0L)) {

    error <- proc$read_all_error()

    stop(
      "Error in background process: ",
      paste(error, collapse = "\n")
    )
  }

  proc$get_result()
}

#' Execute a function with memory monitoring
#'
#' Internal wrapper that executes a function while monitoring available system
#' memory. The appropriate execution method is selected according to the
#' operating system.
#'
#' @param fn A function to be executed.
#' @param args A named list of arguments passed to \code{fn}.
#' @param reserve_mb A numeric value specifying the minimum amount of free
#'   system memory, in megabytes, to reserve for the operating system.
#'   Defaults to 2048.
#' @param poll_interval A numeric value giving the time interval, in seconds,
#'   between memory availability checks. Defaults to 0.5.
#'
#' @details
#' The function identifies the operating system using \code{Sys.info()} and
#' dispatches execution to the corresponding platform-specific helper.
#'
#' Linux and macOS systems use the Unix implementation, while Windows systems
#' use the Windows implementation. An error is raised if the current operating
#' system is not supported.
#'
#' @return
#' The result returned by \code{fn} when the execution completes successfully.
#'
#' @keywords internal
.safe_execute <- function(
  fn, args = list(), reserve_mb = 1024 * 2, poll_interval = 0.5
) {

  os <- Sys.info()[["sysname"]]

  if (os %in% c("Linux", "Darwin")) {
    .safe_execute_unix(fn, args, reserve_mb, poll_interval)
  } else if (os == "Windows") {
    .safe_execute_windows(fn, args, reserve_mb, poll_interval)
  } else {
    stop("Unsupported operating system: ", os)
  }
}