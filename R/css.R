

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse a canonical internal aesthetic name into its parts.
#'
#' This function is need because internally \code{ggplot2} only accepts
#' aesthetic names which are pure text.
#'
#' But for ggsvg, I need to encode a number of different variables into an
#' an aesthtic name.
#'
#' So for 'css()' aesthetics, I encode the parameters by collapsing a
#' a string with "_" as separator.
#'
#' I.e. "css_selector_property_format"
#'
#' This function takes this canonical internal format and breaks it into
#' its constituent parts for use determining `scale_svg_*()` functions
#' etc
#'
#' @param x single string of format "css_selector_property_format"
#' @return list object with select, property, format, aes_name
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_aes_type <- function(x) {


  bits <- stringr::str_split(x, "_")[[1]]
  bits <- trimws(bits)

  n <- length(bits)

  selector <- NULL

  is_css <- bits[[1]] == 'css'

  # canonical form: css_selector_property_format
  if (n == 4) {
    # It has a format
    selector <- bits[[2]]
    property <- bits[[3]]
    format   <- bits[[4]]
  } else if (n > 4) {
    # The property has an 'underscaore' in it.  Shouldn't happen with CSS?
    selector <- bits[[2]]
    property <- paste(bits[3:(n-1)], collapse = "_")
    format   <- bits[[n]]
  } else if (n == 3) {
    # 'format' not speciffied
    selector <- bits[[2]]
    property <- bits[[3]]
    format   <- "[x]"
  }

  # Put together a list of things about this aesthetic
  if (is_css && n >= 3 && nchar(selector) > 0 && nchar(property) > 0 && nchar(format) > 0) {
    res <- list(
      type     = "css",
      selector = selector,
      property = property,
      format   = format,
      aes_name = x
    )
  } else if (n == 2) {
    res <- list(
      type     = 'bespoke',
      property = bits[[2]],
      aes_name = x
    )
  } else {
    res <- list(type = 'unknown')
  }

  res
}




if (FALSE) {

  x <- "css_selector_property_format"

  parse_aes_type("css_selector_property_format")
  parse_aes_type("rect_fill_three")
  parse_aes_type("rect_fill")
  parse_aes_type("rect")


  is_valid_css_aes('css_hello')
  is_valid_css_aes('css_hello_there')
  is_valid_css_aes('css_hello_there_format')
  is_valid_css_aes('css_hello_there_mike_format')
  parse_aes_type('css_hello_there_mike_format')

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a "CSS aesthetic" definition into a glue string which will become
#' part of an actual "cascading style sheet" to be fed to rsvg package
#'
#' @param x single character string representing a potentitally bespoke or
#'        CSS aesthetic
#'
#' @return a glue string template for this CSS aesthetic
#'
#' @examples
#' \dontrun{
#' css_aes <- "css_circle_stroke_[x]"
#' css_aes_to_glue_string(css_aes)
#' > "circle {stroke : {{`css_circle_stroke_[x]`}} !important; }"
#' }
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_aes_to_glue_string <- function(x) {

  stopifnot(is_valid_css_aes(x))
  bits <- parse_aes_type(x)

  glue_format <- gsub("[x]", paste0("{{`", x, "`}}"), bits$format, fixed = TRUE)

  sprintf("%s {%s : %s !important; }", bits$selector, bits$property, glue_format)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Is a string a valid CSS Aesthetic of the form "css_selector_property_format"
#' or an acceptable alternate version?
#'
#' @inheritParams css_aes_to_glue_string
#'
#' @return logical
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_valid_css_aes <- function(x) {
  bits <- parse_aes_type(x)
  bits$type == 'css'
}






if (FALSE) {

  library(grid)

  `css_.big_stroke-width_[x]px` <- 999
  x <- "css_.big_stroke-width_[x]px"
  x <- css(".big", "stroke-width", format = "[x]px")
  x

  is_valid_css_aes(x)
  gs <- css_aes_to_glue_string(x)
  gs
  glue(gs, .open = "{{", .close = "}}")

  parse_aes_type(x)
}














