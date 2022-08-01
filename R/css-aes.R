


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Specify an aesthetic that maps to a CSS Selector and Property
#'
#' This function should only be used with an \code{aes()} call in \code{geom_point_svg()},
#' or as an argument to a \code{ggsvg} scale object e.g.
#' \code{scale_svg_fill_discrete(aesthetics = css(...))}
#'
#'
#' @param selector CSS selector as a single character string e.g. "circle .big"
#' @param ... single named argument of the form \code{css_property = value}.
#'        The \code{value} will remain unevaluated for passing into \code{ggplot2::aes()}.
#'        e.g. \code{stroke = cyl}, \code{"stroke-width" = mpg}
#' @param format Advanced. This is a string specifying the formatting for the CSS property value.
#'        This is almost (but not quite) the equivalent of a formatting string
#'        for the \code{glue} package - however, the delimiters for the string
#'        are \code{[]} rather than \code{{}}.
#'
#'        Default: NULL is equivalent to "[x]" and will insert just the bare value.
#'        For example, if the CSS property required an explicit
#'        "px" suffix on the value, the format would be "[x]px"
#'
#' @return a named amed list (with length = 1) where the name is the full name of this CSS
#'         aesthetic, and the value is the unevaluated value passed in to the ...
#'
#' @examples
#' \dontrun{
#' # circle .bit { stroke = XX; }
#' css("circle .big", stroke = as.factor(cyl))
#'
#' # circle .bit { stroke = XXpx; }
#' css("circle .big", stroke = as.factor(cyl), format = "[x]px")
#' }
#'
#' @import rlang
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css <- function(selector, ..., format = NULL) {

  # Can only specify a single CSS Selector/Property/Value
  # But I have to "..." as I have no idea on what the user wants to specify
  # beforehand.
  stopifnot(
    is.character(selector), length(selector) == 1, nchar(selector) > 0,
    ...length() == 1
  )

  # Capture the unevaluated expressions
  dt <- rlang::exprs(..., .named = FALSE)

  # handle edge case
  char_only <- FALSE
  if (!rlang::is_named(dt)) {
    char_only <- TRUE
    dt <- rlang::exprs(..., .named = TRUE)
  }

  # The property is the LHS of the    "property = value" arg
  property <- names(dt)

  # remove quotes if present on the property name.
  # User might have quoted name for "stroke-width" which I have allowed as
  # an alternative to the backticks e.g. `stroke-width`
  if (startsWith(property , '"') || startsWith(property, "'")) {
    property <- stringr::str_sub(property, 2, -2)
  }

  # Create a full name for the aesthetic
  # e.g.   "css_circle .big_stroke"
  aes_name <- paste0(c("css", trimws(selector), trimws(property), format), collapse = "_")

  if (char_only) {
    return(aes_name)
  }

  names(dt) <- aes_name
  class(dt) <- "css_aes"

  dt
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Intercept a call to aes() to reparse the css() arguments
#'
#' This is an internal, non-exported replacement for \code{ggplot2::aes}
#'
#' @section Tecnnical:
#'
#' The \code{css()} helper makes it easier for the user to specify CSS
#' selectors for aesthetics, but the downside is that \code{ggplot2::aes()}
#' does not know how to deal with them.  i.e. \code{css()} calls within the
#' \code{aes()} need to be caught and evaludated first (in order to turn them
#' in to their canonical internal form).
#'
#' Within a call to \code{geom_point_svg()}, instead of calling \code{ggplot2::aes()},
#' a bit of \code{rlang} trickery is used to call this function instead.
#'
#' This function
#' \enumerate{
#'   \item{captures the \code{css()} calls}
#'   \item{evaluates the \code{css()} call to turn it into a vanilla aesthetic for ggplot}
#'   \item{after tidying all the \code{css()} arguments, passes everything to
#'         \code{ggplot2::aes()}}
#' }
#'
#' @param ... arguments as to \code{ggplot2::aes()}, but can include unevaluated
#'        \code{css()} calls.
#'
#' @return restults of \code{ggplot2::aes()} after \code{css()} arguments are transformed
#'         to their canonical internal format.
#'
#' @noRd
#' @import rlang
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my_aes <- function(...) {

  dts <- rlang::exprs(...)

  for (i in seq_along(dts)) {
    dt <- dts[[i]]
    if (rlang::is_call(dt) && rlang::call_name(dt) == 'css') {
      # message("css call: ", dt)
      css_aes <- eval(dt)
      dts[i] <- css_aes
      names(dts)[[i]] <- names(css_aes)
    }
  }

  do.call(ggplot2::aes, dts)
}






