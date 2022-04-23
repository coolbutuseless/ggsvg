


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Transform a CSS aesthetic selector into its canonical form for ggsvg
#'
#' In general, the user will not want or need to inspect the output of this
#' funciton.  Instead it is always used within an 'aes()' call for
#' \code{geom_point_svg()}, or one of the scale functions in \code{ggsvg} of
#' the form \code{scale_svg_*()}.
#'
#' @param selector CSS selector as a single character string e.g. "circle .big"
#' @param ... single named argument of the form \code{css_property = value}.
#'        The \code{value} will remain unevaluated for passing into \code{ggplot2::aes()}
#' @param format a string specifying the formatting for the CSS property value.
#'        Default: NULL is equivalent to ".x" and will insert just the bare value.
#'        For example, if the CSS property required an explicit
#'        "px" suffix on the value, the format would be ".xpx"
#'
#' @return length-1 named list where the name is the full name of this CSS
#'         aesthetic, and the value is the unevaluated value passed in to the ...
#'
#' @examples
#' \dontrun{
#' # circle .bit { stroke = XX; }
#' css("circle .big", stroke = as.factor(cyl))
#'
#' # circle .bit { stroke = XXpx; }
#' css("circle .big", stroke = as.factor(cyl), format = ".xpx")
#' }
#'
#' @import rlang
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css <- function(selector, ..., format = NULL) {
  stopifnot(
    is.character(selector), length(selector) == 1, nchar(selector) > 0,
    ...length() == 1
  )

  dt <- rlang::exprs(..., .named = FALSE)

  char_only <- FALSE
  if (!rlang::is_named(dt)) {
    char_only <- TRUE
    dt <- rlang::exprs(..., .named = TRUE)
  }

  property <- names(dt)

  # remove quotes if present on the property name
  if (startsWith(property , '"') || startsWith(property, "'")) {
    property <- stringr::str_sub(property, 2, -2)
  }

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
#' @param ... arguments as to aes()
#'
#' @return restults of aes() after css() argumnets are transformed
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

  do.call(aes, dts)
}



if (FALSE) {
  dt <- css("circle", fill = mpg)
  dt



  dts <- my_aes(x =1 , y =2, css("circle", fill = mpg))
  dts



  geomx <- function(mapping) {
    aes_call <- rlang::enquo(mapping)

    if (rlang::is_call(aes_call) && rlang::call_name(aes_call) == 'aes') {
      # replace the call to "aes()" with a call to "my_aes()"
      ex <- rlang::quo_get_expr(aes_call)
      ex[[1]] <- as.name("my_aes")
      aes_call <- rlang::quo_set_expr(aes_call, ex)
    }

    mapping <- rlang::eval_tidy(aes_call)
    # mapping <- aes_call

    mapping
  }


  aes_call <- mapping <- geomx(aes(x = 1, y = 2, css("circle .big", style = mpg)))
  mapping
  aes_call



  dummy <- function(...) {}
}




