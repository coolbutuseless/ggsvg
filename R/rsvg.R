

# Old manual way
# bitmap <- rsvg::rsvg(charToRaw(svg_text))
# im     <- magick::image_read(bitmap)
# ras    <- as.raster(im)
# grob   <- grid::rasterGrob(ras, ...)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert SVG to rasterGrob via rsvg
#'
#' Note: Waiting on built-in nativeraster export from rsvg. PR submitted 2022-04-14
#'
#' Note: Calling into the \code{rsvg} package C code is not really allowed by
#' CRAN, but it works for now to get it released for testing.
#'
#' @param svg_text charcter string containing valid SVG
#' @param width output width in pixels or \code{NULL} for default size derived from
#'        SVG size if possible
#' @param height output height in pixels or \code{NULL} for default size derived from
#'        SVG size if possible
#' @param css see documentation for \code{rsvg::rsvg()}.
#' @param ... other arguments passed to `grdi::rasterGrob()`. A common extra argument is
#'        \code{interpolate = FALSE}.
#'
#' @return rasterGrob
#'
#' @importFrom utils packageVersion
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
svg_to_rasterGrob <- function(svg_text, width=NULL, height=NULL, css=NULL, ...) {

  if (packageVersion('rsvg') < '2.3.0') {
    # bitmap result is in RGBA packed pixel format.
    bitmap <- .Call(rsvg:::R_rsvg, charToRaw(svg_text), width, height, 0L, NULL, PACKAGE = 'rsvg');

    # Convert to Native Raster ARGB format in C
    nr <- .Call(rsvg_bitmap_to_native_raster_, bitmap, dim(bitmap));
  } else {
    nr <- rsvg::rsvg_nativeraster(charToRaw(svg_text), width = width,
                                  height = height, css = NULL)
  }

  grid::rasterGrob(nr, ...)
}

