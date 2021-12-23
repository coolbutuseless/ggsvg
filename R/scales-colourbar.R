



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Continuous scales for colour and fill aesthetics for \code{ggsvg}
#'
#'All these colour/fill scales use \code{guide_colourbar()} but by default,
#'this guide will only accept aesthetics of \code{fill} and \code{colour}.
#'
#'The two key changes to these scales compared to their \code{ggplot2} originals are:
#'
#'\itemize{
#'\item{Default \code{guide} argument is now a \code{guide_colourbar()} object
#'       which explicitly supports the current \code{aesthetics}}
#'\item{\code{aesthetics} is now a required argument}
#'}
#'
#'
#' @param aesthetics name of the aesthetic e.g. \code{fill_rect}
#'
#' @param ...,guide,low,high,space,na.value,type,palette,direction,values,mid,midpoint,colours,colors,alpha,begin,end,option see \code{ggplot2} documentation
#'
#' @import ggplot2
#' @import scales
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_colour_gradient <- function(aesthetics,
                                      ...,
                                      low        = "#132B43",
                                      high       = "#56B1F7",
                                      space      = "Lab",
                                      na.value   = "grey50",
                                      guide      = ggplot2::guide_colorbar(available_aes = aesthetics)) {
  ggplot2::continuous_scale(
    aesthetics,
    scale_name = "gradient",
    palette    = scales::seq_gradient_pal(low, high, space),
    na.value   = na.value,
    guide      = guide,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_colour_gradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_colour_distiller <- function (aesthetics,
                                        ...,
                                        type       = "seq",
                                        palette    = 1,
                                        direction  = -1,
                                        values     = NULL,
                                        space      = "Lab",
                                        na.value   = "grey50",
                                        guide      = ggplot2::guide_colorbar(available_aes = aesthetics))
{
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warning("Using a discrete colour palette in a continuous scale.\n  Consider using type = \"seq\" or type = \"div\" instead")
  }

  ggplot2::continuous_scale(
    aesthetics,
    "distiller",
    scales::gradient_n_pal(scales::brewer_pal(type, palette, direction)(7), values, space),
    na.value = na.value,
    guide = guide,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_colour_gradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_colour_gradient2 <- function (aesthetics,
                                        ...,
                                        low        = muted("red"),
                                        mid        = "white",
                                        high       = muted("blue"),
                                        midpoint   = 0,
                                        space      = "Lab",
                                        na.value   = "grey50",
                                        guide      = ggplot2::guide_colorbar(available_aes = aesthetics))
{
  ggplot2::continuous_scale(
    aesthetics,
    "gradient2",
    scales::div_gradient_pal(low, mid, high, space),
    na.value = na.value,
    guide = guide,
    ...,
    rescaler = mid_rescaler(mid = midpoint)
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_colour_gradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_colour_gradientn <- function (aesthetics,
                                        ...,
                                        colours,
                                        values     = NULL,
                                        space      = "Lab",
                                        na.value   = "grey50",
                                        guide      = ggplot2::guide_colorbar(available_aes = aesthetics),
                                        colors)
{
  colours <- if (missing(colours))
    colors
  else colours

  ggplot2::continuous_scale(
    aesthetics,
    "gradientn",
    scales::gradient_n_pal(colours, values, space),
    na.value = na.value,
    guide = guide,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_colour_gradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_colour_viridis_c <- function (aesthetics,
                                        ...,
                                        alpha      = 1,
                                        begin      = 0,
                                        end        = 1,
                                        direction  = 1,
                                        option     = "D",
                                        values     = NULL,
                                        space      = "Lab",
                                        na.value   = "grey50",
                                        guide      = ggplot2::guide_colorbar(available_aes = aesthetics))
{
  ggplot2::continuous_scale(
    aesthetics,
    "viridis_c",
    scales::gradient_n_pal(scales::viridis_pal(alpha, begin, end, direction, option)(6), values, space),
    na.value = na.value,
    guide = guide,
    ...
  )
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_colour_gradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_fill_distiller <- function (aesthetics,
                                      ...,
                                      type       = "seq",
                                      palette    = 1,
                                      direction  = -1,
                                      values     = NULL,
                                      space      = "Lab",
                                      na.value   = "grey50",
                                      guide      = ggplot2::guide_colorbar(available_aes = aesthetics))
{
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warning("Using a discrete colour palette in a continuous scale.\n  Consider using type = \"seq\" or type = \"div\" instead")
  }

  ggplot2::continuous_scale(
    aesthetics,
    "distiller",
    scales::gradient_n_pal(scales::brewer_pal(type, palette, direction)(7), values, space),
    na.value = na.value,
    guide = guide,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_colour_gradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_fill_gradient  <- function (aesthetics,
                                      ...,
                                      low        = "#132B43",
                                      high       = "#56B1F7",
                                      space      = "Lab",
                                      na.value   = "grey50",
                                      guide      = ggplot2::guide_colorbar(available_aes = aesthetics))
{
  ggplot2::continuous_scale(
    aesthetics,
    "gradient",
    scales::seq_gradient_pal(low, high, space),
    na.value = na.value,
    guide = guide, ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_colour_gradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_fill_gradient2 <- function (aesthetics,
                                      ...,
                                      low        = muted("red"),
                                      mid        = "white",
                                      high       = muted("blue"),
                                      midpoint   = 0,
                                      space      = "Lab",
                                      na.value   = "grey50",
                                      guide      = ggplot2::guide_colorbar(available_aes = aesthetics))
{
  ggplot2::continuous_scale(
    aesthetics,
    "gradient2",
    scales::div_gradient_pal(low,  mid, high, space),
    na.value = na.value,
    guide = guide,
    ...,
    rescaler = mid_rescaler(mid = midpoint)
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_colour_gradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_fill_gradientn <- function (aesthetics,
                                      ...,
                                      colours,
                                      values     = NULL,
                                      space      = "Lab",
                                      na.value   = "grey50",
                                      guide      = ggplot2::guide_colorbar(available_aes = aesthetics),
                                      colors)
{
  colours <- if (missing(colours))
    colors
  else colours

  ggplot2::continuous_scale(
    aesthetics,
    "gradientn",
    scales::gradient_n_pal(colours, values, space),
    na.value = na.value,
    guide = guide,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_colour_gradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_fill_viridis_c <- function (aesthetics,
                                      ...,
                                      alpha      = 1,
                                      begin      = 0,
                                      end        = 1,
                                      direction  = 1,
                                      option     = "D",
                                      values     = NULL,
                                      space      = "Lab",
                                      na.value   = "grey50",
                                      guide      = ggplot2::guide_colorbar(available_aes = aesthetics))
{
  ggplot2::continuous_scale(
    aesthetics,
    "viridis_c",
    scales::gradient_n_pal(viridis_pal(alpha, begin, end, direction, option)(6), values, space),
    na.value = na.value,
    guide = guide,
    ...
  )
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Clone of ggplot2 function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mid_rescaler <- function (mid) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to, from, mid)
  }
}





if (FALSE) {


  nn <- ls(asNamespace('ggplot2'))
  nn <- grep("^scale_", nn, value = TRUE)

  for (i in seq_along(nn)) {
    f <- get(nn[i], envir = asNamespace('ggplot2'))

    args <- formals(f)
    if ('guide' %in% names(args) && as.character(args[['guide']]) %in% c('colorbar', 'colourbar')) {
      cat(nn[i],  "\n")
    }
  }


}
