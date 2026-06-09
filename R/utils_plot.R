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

  if(!max) {
    result <- ifelse(result > 0, 0, result)
  }
  
  return(result)
}

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
#' @param ylim A numeric vector of length 2, used to informn the y-axis limits 
#'   of the plot.
#' @param margin A numeric value (between 0 and 0.5) defining the proportion of 
#'   the x-range to exclude from both ends when searching for candidate 
#'   positions. Defaults to 0.1.
#' @param low_quantile A numeric value (between 0 and 1) used to define 
#'   low-value regions in the data. Positions are selected among values below 
#'   this quantile.
#' @param x_max Optional. A numeric value (positive) used to restrict the 
#'   x-axis range considered in the calculation.
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
  data_list, col_x, col_y, ylim, margin = 0.1, low_quantile = 0.2, 
  text_width_fraction = 0.15, x_max = NULL
) {
  
  if (is.data.frame(data_list)) {
    data_list <- list(data_list)
  }
  if (!is.list(data_list)) {
    data_list <- list(data_list)
  }
  if(margin < 0 || margin > 0.5) {
    stop("Expected parameter 'margin' to be between 0 and 0.5.")
  }
  if(low_quantile < 0 || low_quantile > 1) {
    stop("Expected parameter 'low_quantile' to be between 0 and 1.")
  }
  if(x_max <= 0 && !is.null(x_max)) {
    stop("Expected parameter 'x_max' to be a positive number.")
  }
  if(is.null(ylim) || length(ylim) != 2) {
    stop("Expected parameter 'ylim' to be numeric vector of length 2.")
  }

  all_x <- unique(unlist(lapply(data_list, function(d) d[[col_x]])))
  all_x <- sort(all_x)

  if (!is.null(x_max)) {
    all_x <- all_x[all_x <= x_max]
  }

  if (length(all_x) == 0) {
    stop("No values left after applying x_max.")
  }

  y_matrix <- sapply(data_list, function(d) {
    x <- d[[col_x]]
    y <- d[[col_y]]
    
    valid <- !is.na(x) & !is.na(y)
    x <- x[valid]
    y <- y[valid]

    if (!is.null(x_max)) {
      keep <- x <= x_max
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

  for(i in candidates){

  x_right <- x_in[i]
  x_left <- x_right - text_width

  idx_window <- which(
      x >= x_left &
      x <= x_right
    )

    if(length(idx_window) == 0){
      next
    }

    if(max(y[idx_window]) <= threshold){
      valid_candidates <- c(valid_candidates, i)
    }
  }

  if(length(valid_candidates) > 0){
    idx_x <- valid_candidates[
      which.max(x_in[valid_candidates])
    ]
  } else if(length(candidates) > 0){
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