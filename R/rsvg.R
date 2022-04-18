
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert SVG to a \code{grid} \code{rasterGrob} object
#'
#' @param svg_text character string containing valid SVG
#' @param width,height output width,height in pixels or \code{NULL} (the default)
#'        which inputes the size from the SVG
#' @param css path/url to external css file or raw vector with css data.
#'        This requires your system has a recent version of librsvg.See
#'        documentation for \code{rsvg::rsvg()}.
#' @param ... other arguments passed to `grid::rasterGrob()`. A common extra
#'        argument is \code{interpolate = FALSE}.
#'
#' @return rasterGrob object wrapping the nativeraster returned from \code{rsvg}
#'
#' @importFrom utils packageVersion
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
svg_to_rasterGrob <- function(svg_text, width=NULL, height=NULL, css=NULL, ...) {

  nr <- rsvg::rsvg_nativeraster(
    svg    = charToRaw(svg_text),
    width  = width,
    height = height,
    css    = css
  )

  grid::rasterGrob(nr, ...)
}

