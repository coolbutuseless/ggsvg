



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Extract the property name from a CSS aesthetic definition
#'
#' @param x CSS aesthetic string of the form: "css=[selector]:[property]"
#'
#' @return the property for this CSS aesthetic
#'
#' @importFrom utils tail
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_aes_property <- function(x) {
  bits <- stringr::str_split(x, ":")[[1]]
  if (length(bits) == 1) {
    return(character(0))
  }

  res <- tail(bits, 1)
  res <- trimws(res)
  if (nchar(res) == 0) {
    return(character(0))
  }

  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Extract the CSS selector from a CSS aesthetic definition
#'
#' @inheritParams css_aes_property
#'
#' @return the CSS selector for this CSS aesthetic
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_aes_selector <- function(x) {
  pos <- tail(as.vector(stringr::str_locate_all(x, ':')[[1]]), 1)
  res <- trimws(stringr::str_sub(x, start = 5, end = pos - 1))

  if (length(res) == 0 || nchar(res) == 0) {
    return(character(0))
  }

  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a "CSS aesthetic" definition into a glue string
#'
#' @inheritParams css_aes_property
#'
#' @return a glue string template for this CSS aesthetic
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_aes_to_glue_string <- function(x) {

  prop     <- css_aes_property(x)
  selector <- css_aes_selector(x)

  sprintf("%s {%s : {{`%s`}} !important; }", selector, prop, x)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Is a string a valid CSS Aesthetic of the form "css=[selector]:[property]"
#'
#' @inheritParams css_aes_property
#'
#' @return logical
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_valid_css_aes <- function(x) {

  startsWith(x, "css=") &&
    length(css_aes_selector(x)) > 0 &&
    length(css_aes_property(x)) > 0

}






if (FALSE) {

  library(grid)

  svg <- '
  <svg viewBox="0 0 100 100 ">
    <rect width="100" height="100" fill="#88ccaa" />
    <circle cx="50" cy="50" r="40" fill="white" />
  </svg>
  '

  css <- '
  circle {fill: red !important;}
  rect {stroke: black; stroke-width: 10 !important;}
  '


  grid.newpage(); grid::grid.draw(svg_to_rasterGrob(svg, css = css))



}
