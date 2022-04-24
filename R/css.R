

parse_aes_type <- function(x) {


  bits <- stringr::str_split(x, "_")[[1]]
  bits <- trimws(bits)

  n <- length(bits)

  selector <- NULL

  is_css <- bits[[1]] == 'css'

  if (n == 4) {
    selector <- bits[[2]]
    property <- bits[[3]]
    format   <- bits[[4]]
  } else if (n > 4) {
    selector <- bits[[2]]
    property <- paste(bits[3:(n-1)], collapse = "_")
    format   <- bits[[n]]
  } else if (n == 3) {
    selector <- bits[[2]]
    property <- bits[[3]]
    format   <- "[x]"
  }

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
#' Convert a "CSS aesthetic" definition into a glue string
#'
#' @param x single character string representing a potentitally bespoke or
#'        CSS aesthetic
#'
#' @return a glue string template for this CSS aesthetic
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_aes_to_glue_string <- function(x) {

  stopifnot(is_valid_css_aes(x))
  bits <- parse_aes_type(x)

  glue_format <- gsub("[x]", paste0("{{`", x, "`}}"), bits$format, fixed = TRUE)

  sprintf("%s {%s : %s !important; }", bits$selector, bits$property, glue_format)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Is a string a valid CSS Aesthetic of the form "css=[selector]:[property]"
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














