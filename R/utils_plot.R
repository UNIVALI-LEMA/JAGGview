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

#' Create an empty placeholder plotly widget
#'
#' Internal helper function used to generate a blank \pkg{plotly} widget
#' displaying only a centered title, typically used when no data is
#' available to plot.
#'
#' @param title A character string with the title to display in place of
#' the plot.
#'
#' @return
#' A \code{plotly} object with no traces displayed and a centered title.
#'
#' @details
#' This function is used as a fallback visual whenever the underlying data
#' for a plot is missing or empty, avoiding rendering errors while still
#' informing the user through the displayed title. Dragging/zooming
#' interactions are disabled since there is no data to explore.
#'
#' @keywords internal
#' @importFrom plotly plotly_empty layout
.empty_plotly <- function(title){
  plotly_empty(type = "scatter", mode = "markers") %>%
    layout(
      title = list(
        text = title,
        y = 0.5
      ),
      dragmode = FALSE
    )
}

#' Expand range of the available data
#' 
#' Internal helper that simmetrically expands  a numeric range (3.g., the 
#' limits of a plot axis) by given fraction of it's span, adding a visual 
#' margin around the data.
#' 
#' @param lim A numeric vector of length 2 giving the lower and upper bounds of 
#'   the range to be expanded
#' @param mult A numeric value giving the fraction of the range's span to add 
#'   as margin on each side. Defaults to \code{0.05}.
#' 
#' @return A numeric vector of length 2 with expanded lower and upper bounds.
#' 
#' @details
#' The expansion is computed as the difference between the bounds of \code{lim}
#' multiplied by \code{mult} , and aplied symmetrically to both ends of the 
#' range.
#' 
#' @keywords internal
.expand_range <- function(lim, mult = 0.05) {
  d <- diff(lim)
  lim + c(-1, 1) * d * mult
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

#' Get a reactive value or fall back to a default
#' 
#' Internal helper that evaluates a reactive value and returns it, unless it is 
#' \code{NULL}, an empty string, or \code{NA}, in which case a default value is 
#' returned instead
#' 
#' @param reactive_val A reactive expression (e.g., a Shiny \code{reactive()} 
#'   or \code{input}) to be evaluated.
#' @param default A value to be returned when \code{reactive_val} evaluates to 
#'   \code{NULL}, \code{""}, or \code{NA}.
#' 
#' @return The evaluated value of \code{reactive_val}, or \code{default} if it 
#'   is missing.
#' 
#' @details
#' This function is typically used to provide fallback values for Shiny inputs 
#' that have not yet been set or have been cleared by the user.
#' 
#' @keywords internal
.get_value_or_default <- function(reactive_val, default) {
  val <- reactive_val()
  if (is.null(val) || val == "" || is.na(val)) {
    default
  }
  else {
    val
  }
}

#' International System of Prefixes
#' 
#' @param number A numeric value (or a numeric vector of values) to be 
#'   formated using SI prefixes.
#' @param decimals Optional. An integer indicating the number of decimals 
#'   places to display in the formatted string.  
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
    } else if (abs_val >= 1) {
      divisor <- 1
      suffix  <- ""
    } else if (abs_val >= 1e-3) {
      divisor <- 1e-3
      suffix <- "m"
    } else if (abs_val >= 1e-6) {
      divisor <- 1e-6
      suffix <- "\u00B5"
    } else {
      divisor <- 1e-9
      suffix <- "n"
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

#' Check if a value is empty
#' 
#' Internal helper that checks whether a value is \code{NULL} or has zero 
#' length, useful for validating inputs before further processing.
#' 
#' @param x A value to be checked.
#' 
#' @return A logical value: \code{TRUE} if \code{NULL} or has length zero, 
#'   \code{FALSE} otherwise.
#' 
#' @keywords internal
.is_empty <- function(x) {
  is.null(x) || length(x) == 0
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

#' Prepare data for an NPC-positioned table annotation
#' 
#' Internal helper thta adds NPC (normalized parent coordinates) columns and a 
#' nested table column to a data frame, so it can be used directly with 
#' \code{geom_table_npc()} to place a small table annotation at a fixed 
#' relative position within each plot panel.
#' 
#' @param data A data frame containing the column to be summarised in the table.
#' @param pos_x A character string giving the horizontal NPC position. One of 
#'   \code{"left"}, \code{"center"}, or \code{"right"}. Any other value 
#'   defaults to \code{0} (left).
#' @param pos_y A character string giving the vertical NPC position. One of 
#'   \code{"top"}, \code{"middle"}, or \code{"bottom"}. Any other value 
#'   defaults to \code{1} (top).
#' @param col The (unquoted) column in \code{data} whose values will populate 
#'   the table.
#' @param col_name A character string giving the column name to be used in the 
#'   resulting table (e.g., \code{"MASE"}, \code{"RMSE"}).
#' @param suffix A character string appended to each formatted value (e.g., 
#'   \code{"\%"}). Defaults to \code{""}.
#' @param decimals A numeric value giving the number of decimals places used to 
#'   round the values. Defaults to \code{2}.
#' 
#' @return The input \code{data} with three additional columns:
#'   \itemize{
#'     \item \code{x}: the NPC horizontal position (\code{0}, \code{0.5}, or 
#'       \code{1}).
#'     \item \code{y}: the NPC vertical position (\code{0}, \code{0.5}, or 
#'       \code{1}).
#'     \item \code{tb}: a list-column of one-row data frames, each holding a 
#'       single formatted value, ready to be used as the \code{label} aesthetic 
#'       in \code{geom_table_npc()}.
#'   }
#' 
#' @details
#' This function is used to annotate faceted plots with small summary tables 
#' (e.g., goodness-of-fit metrics) positioned consistently at a corner or edge 
#' of each panel, regardles of the underlying data range or axis expansion.
#' 
#' @keywords internal
#' @importFrom purrr pmap
.prepare_npc_table_data <- function(
  data, pos_x, pos_y, col, col_name, suffix = "", decimals = 2
) {

  cols_data <- data %>% select({{col}})

  if (ncol(cols_data) != length(col_name)) {
    stop(paste0("Expected parameter 'col_name' to have the same length",
    "as the number of columns selected in 'col'."))
  }
    data %>%
      mutate(
        x = case_when(
          pos_x == "left" ~ 0,
          pos_x == "right" ~ 1,
          pos_x == "center" ~ 0.5,
          TRUE ~ 0
        ),
        y = case_when(
          pos_y == "top" ~ 1,
          pos_y == "bottom" ~ 0,
          pos_y == "middle" ~ 0.5,
          TRUE ~ 1
        ),
        tb = pmap(
          cols_data, 
          function(...) {
            vals <- c(...)
            data.frame(
              Metric = col_name,
              Value = paste0(round(vals, decimals), suffix)
            )
          }
        )
      )
  }

#' Resolve plotting palette
#' 
#' Returns the palette to be used in a plot. If \code{palette} is \code{NULL}, 
#' a default palette is generated using \code{.make_index_palette()}. 
#' Otherwise, the supplied palette is validated and checked to ensure that it 
#' contains at least \code{num} colors.
#' 
#' @param palette Optional character vector of hexadecimal color codes.
#' @param num Minimum number of colors required.
#' 
#' @return A character vector of hexadecimal color codes.
#' 
#' @keywords internal
.resolve_palette <- function(palette, num) {
  if(.is_empty(palette) || anyNA(palette) || any(palette == "", na.rm = TRUE)) {
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
      return(palette[seq_len(num)])
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
        handle <- tryCatch(ps::ps_handle(pid), error = function(e) NULL)
        if (is.null(handle) || !ps::ps_is_running(handle)) break

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

  on.exit(if (watcher$is_alive()) watcher$kill(), add = TRUE)

  result <- tryCatch({do.call(fn, args)}, interrupt = function(i) {
    structure(
      class = c("memoryLimitExceeded", "error", "condition"),
      list(
        message ="Execution interrupted: memory limit has been exceeded",
        call = sys.call()
      )
    )
  })

  if (inherits(result, "memoryLimitExceeded")) stop(result)

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
  proc <- r_bg(func = fn, args = args, package = package, supervise = TRUE)

  on.exit(if (proc$is_alive()) proc$kill(), add = TRUE)

  memory_exceeded <- FALSE

  repeat {
    if (!proc$is_alive()) break

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
      class = c("memoryLimitExceeded", "error", "condition"),
      list(
        message = "Execution interrupted: memory limit has been exceeded",
        call = sys.call()
      )
    ))
  }

  exit_status <- proc$get_exit_status()

  if (!identical(exit_status, 0L)) {
    error <- proc$read_all_error()
    stop("Error in background process: ", paste(error, collapse = "\n")
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
#' Build a compact metric table using plotly shapes and annotations
#' 
#' Internal helper that builds pixel-sized \pkg{plotly} shapes (cell borders) 
#' and annotations (header and value text) for a small metric table, anchored 
#' to the top-left corner of the panel, with dimensions based on the given font 
#' size rather than the panel's dimensions. This allows the table to be used as 
#' a substitute for \code{add_text()} when a fixed-size, panel-size independent 
#' tabular annotation is desired, with one row per metric selected in 
#' \code{col}.
#' 
#' @param data A data frame containing the column(s) to be summarised in the 
#'   table.
#' @param text_size A numeric value giving the font size, in the pixels, used 
#'   for the table's text. Also used as the basis for computing row height, 
#'   column width, and left padding (see \code{heigth_mult}, \code{width_mult}, 
#'   and \code{left_pad_mult}).
#' @param col One or more (unquoted) columns in \code{data} whose values will 
#'   populate the table, selected with tidyselect syntax (e.g., \code{Value}, 
#'   or \code{c(ppmr_value, ppvr_value)} for multiple rows).
#' @param col_name A character vector giving the row label to be used in the 
#'   resulting table for each column selected in \code{col}, in the same order 
#'   (e.g., \code{c("PPMR", "PPVR")}). Must have the same length as the number 
#'   of columns selected in \code{col}.
#' @param suffix A character string appended to each formatted value (e.g., 
#'   \code{"\%"}). Defaults to \code{""}.
#' @param decimals A numeric value giving the number of decimal places used to 
#'   round the values. Defaults to \code{2}.
#' @param heigth_mult A numeric value giving the row height as a multiple of 
#'   \code{text_size}, in pixels. Defaults to \code{1.2}.
#' @param width_mult A numeric value giving each column's width as a multiple 
#'   of \code{text_size}, in pixels. Defaults to \code{4}.
#' @param left_pad_mult A numeric value giving the left padding applied to each 
#'   column's text, as a multiple of \code{text_size}, in pixels. Defaults to 
#'   \code{0.25}.
#' 
#' @return A named list with two elements:
#'   \itemize{
#'     \item \code{shapes}: a list of \pkg{plotly} shape specifications 
#'       (\code{type = "rect"}) drawing the table's cell borders, one 
#'       header row plus one row per metric.
#'     \item \code{annotations}: a list of \pkg{plotly} annotation 
#'       specifications with the header text (\code{"Metric"}, \code{"Value"}) 
#'       and the formatted values for each metric.
#'   }
#'   Both elements are ready to be appended to a subplot's \code{shapes} and 
#'   \code{annotations} lists, respectively, in a \code{layout()} call.
#'
#' @details
#' All shapes and annotations are positioned in pixel units 
#' (\code{xsizemode/ysizemode = "pixel"}), anchored to the top-left corner of 
#' the panel (\code{xref = "paper"}, \code{yref = "paper"}, \code{xanchor = 0}, 
#' \code{yanchor = 1}). This keeps the table's size fixed relative to 
#' \code{text_size}, independent of the panel's actual pixel dimensions, so 
#' the table does not stretch or shrink disproportionately when the plotting 
#' area is resized.
#'
#' @keywords internal
.build_metric_table <- function(
  data, text_size, pos_x, pos_y, col, col_name, suffix = "", decimals = 2, 
  heigth_mult = 1.2, width_mult = 4, left_pad_mult = 0.25, 
  colors = c("#CCCCCC", "#F2F2F2")
) {

  cols_data <- data %>% select({{col}})

  if (ncol(cols_data) != length(col_name)) {
    stop(paste0("Expected parameter 'col_name' to have the same length",
    "as the number of columns selected in 'col'."))
  }

  shapes <- list()
  annotations <- list() 

  n_rows <- ncol(cols_data)

  heigth_line <- -heigth_mult*text_size
  width_line <- width_mult*text_size
  left_padding <- left_pad_mult*text_size
  if (pos_x == "left") {
    mult_x <- 1
    xanchor <- 0
    x0_2 <- mult_x*width_line
    x1_2 <- mult_x*width_line*2
    x_shift_1 <- left_padding
    x_shift_2 <- width_line*2 - left_padding
    x_shift_fix_1 <- width_line/2
    x_shift_fix_2 <- width_line + width_line/2
  } else if (pos_x == "right") {
    mult_x <- -1
    xanchor <- 1
    x0_2 <- mult_x*width_line
    x1_2 <- mult_x*width_line*2
    x_shift_1 <- left_padding - width_line*2
    x_shift_2 <- -left_padding
    x_shift_fix_1 <- mult_x*(width_line + width_line/2)
    x_shift_fix_2 <- mult_x*width_line/2
  }
  else if (pos_x == "center") {
    mult_x <- 1
    xanchor <- 0.5
    x0_2 <- 0
    x1_2 <- -1*width_line
    x_shift_1 <- left_padding - width_line
    x_shift_2 <- width_line - left_padding
    x_shift_fix_1 <- -1*width_line/2
    x_shift_fix_2 <- width_line/2
  }

  if (pos_y == "top") {
    yanchor <- 1
    mult_y <- 1
    y_shift_1 <- 0
  } else if (pos_y == "bottom") {
    yanchor <- 0
    mult_y <- -1
    y_shift_1 <- -heigth_line*(n_rows+1)
  }

  for (r in 0:n_rows) {
    y0 <- mult_y*r*heigth_line
    y1 <- mult_y*(r+1)*heigth_line

    color <- colors[(r %% 2)+1]

    shapes <- append(
      shapes,
      list(
        list(
          type = "rect",
          xref = "paper",
          yref = "paper",
          xsizemode = "pixel",
          xanchor = xanchor,
          x0 = 0,
          x1 = mult_x*width_line,
          yanchor = yanchor,
          y0 = y0, 
          y1 = y1,
          ysizemode = "pixel",
          line = list(width = 1),
          fillcolor = color
        ),
        list(
          type = "rect",
          xref = "paper",
          yref = "paper",
          xsizemode = "pixel",
          xanchor = xanchor,
          x0 = x0_2,
          x1 = x1_2,
          yanchor = yanchor,
          y0 = y0, 
          y1 = y1,
          ysizemode = "pixel",
          line = list(width = 1),
          fillcolor = color
        )
      )
    )
  }

  annotations <- append(
    annotations,
    list(
      list(
        x = xanchor,
        y = yanchor,
        xanchor = "center",
        xshift = x_shift_fix_1,
        yanchor = "top",
        yshift = y_shift_1,
        xref = "paper",
        yref = "paper",
        text = "<b>Metric</b>",
        showarrow = FALSE,
        font = list(
          size = text_size,
          color = "black"
        )
      ),
      list(
        x = xanchor,
        y = yanchor,
        xshift = x_shift_fix_2,
        xanchor = "center",
        yanchor = "top",
        yshift = y_shift_1,
        xref = "paper",
        yref = "paper",
        text = "<b>Value</b>",
        showarrow = FALSE,
        font = list(
          size = text_size,
          color = "black"
        )
      )
    )
  )

  for (r in seq_len(n_rows)) {
    var <- if (pos_y == "bottom") {
      n_rows + 1 - r
    } else{
      r
    }
    row_shift <- mult_y*var*heigth_line
    val <- cols_data[[r]]
    annotations <- append(
      annotations,
      list(
        list(
          x = xanchor,
          y = yanchor,
          xanchor = "left",
          xshift = x_shift_1,
          yanchor = "top",
          yshift = row_shift,
          xref = "paper",
          yref = "paper",
          text = col_name[r],
          showarrow = FALSE,
          font = list(
            size = text_size,
            color = "black"
          )
        ),
        list(
          x = xanchor,
          y = yanchor,
          xshift = x_shift_2,
          xanchor = "right",
          yanchor = "top",
          yshift = row_shift,
          xref = "paper",
          yref = "paper",
          text = paste0(round(val, decimals), suffix),
          showarrow = FALSE,
          font = list(
            size = text_size,
            color = "black"
          )
        )
      )
    )
  }

  list(
    shapes = shapes,
    annotations = annotations
  )
}