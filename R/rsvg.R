
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert SVG to a \code{grid} \code{rasterGrob} object
#'
#' @param svg_text character string containing valid SVG
#' @param width,height output width,height in pixels or \code{NULL} (the default)
#'        which inputes the size from the SVG
#' @param css character string containing CSS text.
#'        This requires your system has a recent version of librsvg.
#' @param ... other arguments passed to `grid::rasterGrob()`.
#'
#' @return \code{grid::rasterGrob()} object containing the
#'         \code{nativeRaster} returned from \code{rsvg}
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
svg_to_rasterGrob <- function(svg_text, width=NULL, height=NULL, css=NULL, ...) {

  if (!is.null(css)) {
    css <- charToRaw(css)
  }

  nr <- rsvg::rsvg_nativeraster(
    svg    = charToRaw(svg_text),
    width  = width,
    height = height,
    css    = css
  )

  grid::rasterGrob(nr, ...)
}
