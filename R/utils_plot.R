#' International System of Prefixes
#' 
#' @param number A numeric value (or a numeric vector of values) to be 
#' formated using SI prefixes.
#' @param decimals An integer indicating the number of decimals places
#' to display in the formatted string. Defaults to 2. 
#' 
#' @return A character value (or a character vector of values) 
#' appended with their corresponding SI unit symbol.
#' 
#' @examples 
#' number <- 1000000
#' international_system_prefixes(number, 2)
#' 
#' @export
international_system_prefixes <- function(number, decimals = 2) {
  value <- ifelse(
    abs(number) >= 1e6, number / 1e6,
    ifelse(
      abs(number) >= 1e3, number / 1e3,
      ifelse(
        abs(number) >= 1, number,
        ifelse(
          abs(number) >= 1e-3, number * 1e3,
          ifelse(
            abs(number) >= 1e-6, number * 1e6,
            number * 1e9
          )
        )
      )
    )
  )
  
  suffix <- ifelse(
    abs(number) >= 1e6, "M",
    ifelse(
      abs(number) >= 1e3, "k",
      ifelse(
        abs(number) >= 1, "",
        ifelse(
          abs(number) >= 1e-3, "m",
          ifelse(
            abs(number) >= 1e-6, "\u00B5",
            "n"
          )
        )
      )
    )
  )
  
  paste0(trimws(.format_number(value, decimals = decimals)), suffix)
}

#' Format numeric values with custom separators
#'
#' Internal helper that formats numeric values with a specified number
#' of decimal places and custom thousands and decimal separators.
#'
#' @param number A numeric value (or a numeric vector value).
#' @param decimals Number of decimal places.
#' @param big.mark Thousands separator.
#' @param decimal.mark Decimal separator.
#'
#' @return A character value (or a chracter vector value) with 
#' formatted numbers.
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
#' Internal helper that defines a customized ggplot2 theme used
#' across plots in the package.
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
#' Internal helper that rounds a numeric value to the nearest
#' order of magnitude, either upward (for maximum values) or downward
#' (for minimum values), useful for defining plot axis limits.
#'
#' @param value A numeric value.
#' @param max A logical value indicating the rounding direction:
#'   \itemize{
#'     \item \code{TRUE}: round up (used for upper axis limits).
#'     \item \code{FALSE}: round down (used for lower axis limits).
#'   }
#' @param multiplier A number factor applied to the absolute value
#'  to create a margin for axis limits. 
#'
#' @return A rounded numeric value.
#' 
#' @details
#' When \code{max = FALSE}, positive values are adjusted to include zero
#' when appropriate, ensuring cleaner lower bounds in plots.
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