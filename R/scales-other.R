
# scale_svg_alpha
# scale_svg_alpha_binned
# scale_svg_alpha_continuous
# scale_svg_alpha_discrete
# scale_svg_alpha_identity
# scale_svg_alpha_manual
# scale_svg_alpha_ordinal
# scale_svg_colour_binned
# scale_svg_colour_brewer
# scale_svg_colour_continuous
# scale_svg_colour_discrete
# scale_svg_colour_fermenter
# scale_svg_colour_grey
# scale_svg_colour_hue
# scale_svg_colour_identity
# scale_svg_colour_manual
# scale_svg_colour_ordinal
# scale_svg_colour_qualitative
# scale_svg_colour_steps
# scale_svg_colour_steps2
# scale_svg_colour_stepsn
# scale_svg_colour_viridis_b
# scale_svg_colour_viridis_d
# scale_svg_continuous_identity
# scale_svg_discrete_identity
# scale_svg_discrete_manual
# scale_svg_fill_binned
# scale_svg_fill_brewer
# scale_svg_fill_continuous
# scale_svg_fill_discrete
# scale_svg_fill_fermenter
# scale_svg_fill_grey
# scale_svg_fill_hue
# scale_svg_fill_identity
# scale_svg_fill_manual
# scale_svg_fill_ordinal
# scale_svg_fill_qualitative
# scale_svg_fill_steps
# scale_svg_fill_steps2
# scale_svg_fill_stepsn
# scale_svg_fill_viridis_b
# scale_svg_fill_viridis_d
# scale_svg_linetype
# scale_svg_linetype_binned
# scale_svg_linetype_continuous
# scale_svg_linetype_discrete
# scale_svg_linetype_identity
# scale_svg_linetype_manual
# scale_svg_radius
# scale_svg_size
# scale_svg_size_area
# scale_svg_size_binned
# scale_svg_size_binned_area
# scale_svg_size_continuous
# scale_svg_size_discrete
# scale_svg_size_identity
# scale_svg_size_manual
# scale_svg_size_ordinal


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Scales for working with arbitrary named aesthetics
#'
#' See the corresponding \code{scale_*()} function in \code{ggplot2} for
#' more information on how scales operate.
#'
#' @param aesthetics names of aesthethics to apply this scale to. e.g.
#'        \code{css("circle .big", fill = mpg)}, \code{'fill_rect'}
#' @param ...,range,guide,values,breaks,na.value,type,palette See \code{ggplot2} documentation
#' @param direction,start,end,h,c,l,h.start,low,high,space See \code{ggplot2} documentation
#' @param mid,midpoint,colours,colors,alpha,begin,option,name See \code{ggplot2} documentation
#' @param labels,limits,trans,max_size,n.breaks,nice.breaks See \code{ggplot2} documentation
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_alpha <- function (aesthetics,
                             ...,
                             range = c(0.1, 1))
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::continuous_scale(
    aesthetics,
    "alpha_c",
    scales::rescale_pal(range),
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_alpha_binned <- function (aesthetics,
                                    ...,
                                    range = c(0.1, 1))
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::binned_scale(
    aesthetics,
    "alpha_b",
    scales::rescale_pal(range),
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_alpha_continuous <- function (aesthetics,
                                        ...,
                                        range = c(0.1, 1))
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::continuous_scale(
    aesthetics,
    "alpha_c",
    scales::rescale_pal(range),
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_alpha_discrete <- function (aesthetics,
                                      ...)
{

  aesthetics <- prepare_aesthetics(aesthetics)

  warning("Using alpha for a discrete variable is not advised.", call. = FALSE)
  scale_svg_alpha_ordinal(...)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_alpha_identity <- function (aesthetics,
                                      ...,
                                      guide = "none")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::continuous_scale(
    aesthetics,
    "identity",
    scales::identity_pal(),
    ...,
    guide = guide,
    super = ScaleContinuousIdentity
  )

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_alpha_manual <- function (aesthetics,
                                    ...,
                                    values,
                                    breaks   = waiver(),
                                    na.value = NA)
{

  aesthetics <- prepare_aesthetics(aesthetics)

  manual_scale(
    aesthetics,
    values,
    breaks,
    ...,
    na.value = na.value
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_alpha_ordinal <- function (aesthetics,
                                     ...,
                                     range = c(0.1, 1))
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::discrete_scale(
    aesthetics,
    "alpha_d",
    function(n) seq(range[1], range[2], length.out = n),
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_colour_binned <- function (aesthetics,
                                     ...,
                                     type = getOption("ggplot2.binned.colour"))
{

  aesthetics <- prepare_aesthetics(aesthetics)

  type_fallback <- getOption("ggplot2.continuous.colour",
                             default = "gradient")
  if (is.function(type_fallback)) {
    type_fallback <- "gradient"
  }
  type <- type %||% type_fallback
  if (identical(type, "gradient")) {
    scale_svg_colour_steps(aesthetics = aesthetics, ...)
  }
  else if (identical(type, "viridis")) {
    scale_svg_colour_viridis_b(aesthetics = aesthetics, ...)
  }
  else {
    stop("Unknown scale type")
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_colour_brewer <- function (aesthetics,
                                     ...,
                                     type      = "seq",
                                     palette   = 1,
                                     direction = 1)
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::discrete_scale(
    aesthetics,
    "brewer",
    scales::brewer_pal(type, palette, direction),
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_colour_continuous <- function (aesthetics,
                                         ...,
                                         type = getOption("ggplot2.continuous.colour"))
{

  aesthetics <- prepare_aesthetics(aesthetics)

  type <- type %||% "gradient"
  if (identical(type, "gradient")) {
    scale_svg_colour_gradient(aesthetics = aesthetics, ...)
  }
  else if (identical(type, "viridis")) {
    scale_svg_colour_viridis_c(aesthetics = aesthetics, ...)
  }
  else {
    stop("Unknown scale type")
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_colour_discrete <- function (aesthetics,
                                       ...,
                                       type = getOption("ggplot2.discrete.colour"))
{

  aesthetics <- prepare_aesthetics(aesthetics)

  # maybe try: scale_svg_colour_hue   mikefc 2021-12-22
  scale_svg_colour_qualitative(aesthetics = aesthetics, ..., type = type)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_colour_fermenter <- function (aesthetics,
                                        ...,
                                        type      = "seq",
                                        palette   = 1,
                                        direction = -1,
                                        na.value  = "grey50",
                                        guide     = "coloursteps")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warning("Using a discrete colour palette in a binned scale.\n  Consider using type = \"seq\" or type = \"div\" instead", call. = FALSE)
  }
  ggplot2::binned_scale(
    aesthetics,
    "fermenter",
    binned_pal(scales::brewer_pal(type, palette, direction)),
    na.value = na.value,
    guide    = guide,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_colour_grey <- function (aesthetics,
                                   ...,
                                   start    = 0.2,
                                   end      = 0.8,
                                   na.value = "red")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::discrete_scale(
    aesthetics,
    "grey",
    scales::grey_pal(start, end),
    na.value = na.value,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_colour_hue <- function (aesthetics,
                                  ...,
                                  h         = c(0, 360) + 15,
                                  c         = 100,
                                  l         = 65,
                                  h.start   = 0,
                                  direction = 1,
                                  na.value  = "grey50")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::discrete_scale(
    aesthetics,
    "hue",
    scales::hue_pal(h, c, l, h.start, direction),
    na.value = na.value,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_colour_identity <- function (aesthetics,
                                       ...,
                                       guide = "none")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::discrete_scale(
    aesthetics,
    "identity",
    scales::identity_pal(),
    ...,
    guide = guide,
    super = ScaleDiscreteIdentity
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_colour_manual <- function (aesthetics,
                                     ...,
                                     values,
                                     breaks   = waiver(),
                                     na.value = "grey50")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  manual_scale(
    aesthetics,
    values,
    breaks,
    ...,
    na.value = na.value
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_colour_ordinal <- function (aesthetics,
                                      ...,
                                      type = getOption("ggplot2.ordinal.colour", getOption("ggplot2.ordinal.fill")))
{

  aesthetics <- prepare_aesthetics(aesthetics)

  type <- type %||% scale_svg_colour_viridis_d
  if (is.function(type)) {
    type(...)
  } else {
    ggplot2::discrete_scale(
      aesthetics,
      "ordinal",
      ordinal_pal(type),
      ...
    )
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_colour_qualitative <- function (aesthetics,
                                          ...,
                                          type      = NULL,
                                          h         = c(0, 360) + 15,
                                          c         = 100,
                                          l         = 65,
                                          h.start   = 0,
                                          direction = 1,
                                          na.value  = "grey50")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::discrete_scale(
    aesthetics,
    "qualitative",
    qualitative_pal(type, h, c, l, h.start, direction),
    na.value = na.value,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_colour_steps <- function (aesthetics,
                                    ...,
                                    low      = "#132B43",
                                    high     = "#56B1F7",
                                    space    = "Lab",
                                    na.value = "grey50",
                                    guide    = "coloursteps")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::binned_scale(
    aesthetics,
    "steps",
    scales::seq_gradient_pal(low, high, space),
    na.value = na.value,
    guide    = guide,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_colour_steps2 <- function (aesthetics,
                                     ...,
                                     low      = muted("red"),
                                     mid      = "white",
                                     high     = muted("blue"),
                                     midpoint = 0,
                                     space    = "Lab",
                                     na.value = "grey50",
                                     guide    = "coloursteps")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::binned_scale(
    aesthetics,
    "steps2",
    scales::div_gradient_pal(low, mid, high, space),
    na.value = na.value,
    guide    = guide,
    rescaler = mid_rescaler(mid = midpoint),
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_colour_stepsn <- function (aesthetics,
                                     ...,
                                     colours,
                                     values   = NULL,
                                     space    = "Lab",
                                     na.value = "grey50",
                                     guide    = "coloursteps",
                                     colors)
{

  aesthetics <- prepare_aesthetics(aesthetics)

  colours <- if (missing(colours))
    colors
  else colours

  ggplot2::binned_scale(
    aesthetics,
    "stepsn",
    scales::gradient_n_pal(colours, values, space),
    na.value = na.value,
    guide    = guide,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_colour_viridis_b <- function (aesthetics,
                                        ...,
                                        alpha     = 1,
                                        begin     = 0,
                                        end       = 1,
                                        direction = 1,
                                        option    = "D",
                                        values    = NULL,
                                        space     = "Lab",
                                        na.value  = "grey50",
                                        guide     = "coloursteps")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::binned_scale(
    aesthetics,
    "viridis_b",
    scales::gradient_n_pal(scales::viridis_pal(alpha, begin, end, direction, option)(6), values, space),
    na.value = na.value,
    guide    = guide,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_colour_viridis_d <- function (aesthetics,
                                        ...,
                                        alpha = 1,
                                        begin = 0,
                                        end = 1,
                                        direction = 1,
                                        option = "D")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::discrete_scale(
    aesthetics,
    "viridis_d",
    scales::viridis_pal(alpha, begin, end, direction, option),
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_continuous_identity <- function (aesthetics,
                                           ...,
                                           guide = "none")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::continuous_scale(
    aesthetics,
    "identity",
    scales::identity_pal(),
    ...,
    guide = guide,
    super = ScaleContinuousIdentity
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_discrete_identity <- function (aesthetics, ..., guide = "none")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::discrete_scale(
    aesthetics,
    "identity",
    identity_pal(),
    ...,
    guide = guide,
    super = ScaleDiscreteIdentity
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_discrete_manual <- function (aesthetics,
                                       ...,
                                       values,
                                       breaks = waiver())
{

  aesthetics <- prepare_aesthetics(aesthetics)

  manual_scale(
    aesthetics,
    values,
    breaks,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_fill_binned <- function (aesthetics,
                                   ..., type = getOption("ggplot2.binned.fill"))
{

  aesthetics <- prepare_aesthetics(aesthetics)

    type_fallback <- getOption("ggplot2.continuous.fill",
                               default = "gradient")
    if (is.function(type_fallback)) {
      type_fallback <- "gradient"
    }
    type <- type %||% type_fallback
    if (identical(type, "gradient")) {
      scale_svg_fill_steps(aesthetics = aesthetics, ...)
    }
    else if (identical(type, "viridis")) {
      scale_svg_fill_viridis_b(aesthetics = aesthetics, ...)
    }
    else {
      stop("Unknown scale type")
    }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_fill_brewer <- function (aesthetics,
                                   ...,
                                   type      = "seq",
                                   palette   = 1,
                                   direction = 1)
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::discrete_scale(
    aesthetics,
    "brewer",
    scales::brewer_pal(type, palette, direction),
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_fill_continuous <- function (aesthetics,
                                       ..., type = getOption("ggplot2.continuous.fill"))
{

  aesthetics <- prepare_aesthetics(aesthetics)

  type <- type %||% "gradient"
  if (identical(type, "gradient")) {
    scale_svg_fill_gradient(aesthetics = aesthetics, ...)
  }
  else if (identical(type, "viridis")) {
    scale_svg_fill_viridis_c(aesthetics = aesthetics, ...)
  }
  else {
    stop("Unknown scale type")
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_fill_discrete <- function (aesthetics,
                                     ...,
                                     type = getOption("ggplot2.discrete.fill"))
{

  aesthetics <- prepare_aesthetics(aesthetics)

  # type <- type %||% scale_fill_hue
  # scale_svg_fill_qualitative(aesthetics = aesthetics, ..., type = type)
  scale_svg_fill_hue(aesthetics = aesthetics, ...)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_fill_fermenter <- function (aesthetics,
                                      ...,
                                      type      = "seq",
                                      palette   = 1,
                                      direction = -1,
                                      na.value  = "grey50",
                                      guide     = "coloursteps")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warning("Using a discrete colour palette in a binned scale.\n  Consider using type = \"seq\" or type = \"div\" instead", call. = FALSE)
  }
  ggplot2::binned_scale(
    aesthetics, "fermenter",
    binned_pal(scales::brewer_pal(type, palette, direction)),
    na.value = na.value,
    guide    = guide,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_fill_grey <- function (aesthetics,
                                 ...,
                                 start    = 0.2,
                                 end      = 0.8,
                                 na.value = "red")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::discrete_scale(
    aesthetics,
    "grey",
    grey_pal(start, end),
    na.value = na.value,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_fill_hue <- function (aesthetics,
                                ...,
                                h         = c(0, 360) + 15,
                                c         = 100,
                                l         = 65,
                                h.start   = 0,
                                direction = 1,
                                na.value  = "grey50")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::discrete_scale(
    aesthetics,
    "hue",
    hue_pal(h, c, l, h.start, direction),
    na.value = na.value,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_fill_identity <- function (aesthetics,
                                     ...,
                                     guide = "none")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::discrete_scale(
    aesthetics,
    "identity",
    identity_pal(),
    ...,
    guide = guide,
    super = ScaleDiscreteIdentity
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_fill_manual <- function (aesthetics,
                                   ..., values,
                                   breaks   = waiver(),
                                   na.value = "grey50")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  manual_scale(
    aesthetics,
    values,
    breaks,
    ...,
    na.value = na.value
    )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_fill_ordinal <- function (aesthetics,
                                    ...,
                                    type = getOption("ggplot2.ordinal.fill", getOption("ggplot2.ordinal.colour")))
{

  aesthetics <- prepare_aesthetics(aesthetics)

  type <- type %||% scale_svg_fill_viridis_d
  if (is.function(type)) {
    type(...)
  }
  else {
    ggplot2::discrete_scale(
      aesthetics,
      "ordinal",
      ordinal_pal(type),
      ...
    )
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_fill_qualitative <- function (aesthetics,
                                        ...,
                                        type      = NULL,
                                        h         = c(0, 360) + 15,
                                        c         = 100,
                                        l         = 65,
                                        h.start   = 0,
                                        direction = 1,
                                        na.value  = "grey50")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::discrete_scale(
    aesthetics,
    "qualitative",
    qualitative_pal(type, h, c, l, h.start, direction),
    na.value = na.value,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_fill_steps <- function (aesthetics,
                                  ...,
                                  low      = "#132B43",
                                  high     = "#56B1F7",
                                  space    = "Lab",
                                  na.value = "grey50",
                                  guide    = "coloursteps")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::binned_scale(
    aesthetics,
    "steps",
    seq_gradient_pal(low, high, space),
    na.value = na.value,
    guide    = guide,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_fill_steps2 <- function (aesthetics,
                                   ...,
                                   low      = muted("red"),
                                   mid      = "white",
                                   high     = muted("blue"),
                                   midpoint = 0,
                                   space    = "Lab",
                                   na.value = "grey50",
                                   guide    = "coloursteps")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::binned_scale(
    aesthetics,
    "steps2",
    scales::div_gradient_pal(low, mid, high, space),
    na.value = na.value,
    guide    = guide,
    rescaler = mid_rescaler(mid = midpoint),
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_fill_stepsn <- function (aesthetics,
                                   ...,
                                   colours,
                                   values   = NULL,
                                   space    = "Lab",
                                   na.value = "grey50",
                                   guide    = "coloursteps",
                                   colors)
{

  aesthetics <- prepare_aesthetics(aesthetics)

  colours <- if (missing(colours))
    colors
  else colours

  ggplot2::binned_scale(
    aesthetics,
    "stepsn",
    scales::gradient_n_pal(colours, values, space),
    na.value = na.value,
    guide    = guide,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_fill_viridis_b <- function (aesthetics,
                                      ...,
                                      alpha     = 1,
                                      begin     = 0,
                                      end       = 1,
                                      direction = 1,
                                      option    = "D",
                                      values    = NULL,
                                      space     = "Lab",
                                      na.value  = "grey50",
                                      guide     = "coloursteps")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::binned_scale(
    aesthetics,
    "viridis_b",
    gradient_n_pal(viridis_pal(alpha, begin, end, direction, option)(6), values, space),
    na.value = na.value,
    guide    = guide,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_fill_viridis_d <- function (aesthetics,
                                      ...,
                                      alpha     = 1,
                                      begin     = 0,
                                      end       = 1,
                                      direction = 1,
                                      option    = "D")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::discrete_scale(
    aesthetics,
    "viridis_d",
    viridis_pal(alpha,  begin, end, direction, option),
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_linetype <- function (aesthetics,
                                ...,
                                na.value = "blank")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::discrete_scale(
    aesthetics,
    "linetype_d",
    linetype_pal(),
    na.value = na.value,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_linetype_binned <- function (aesthetics,
                                       ...,
                                       na.value = "blank")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::binned_scale(
    aesthetics,
    "linetype_b",
    binned_pal(scales::linetype_pal()),
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_linetype_continuous <- function (aesthetics,
                                           ...)
{
  stop("A continuous variable can not be mapped to linetype")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_linetype_discrete <- function (aesthetics,
                                         ...,
                                         na.value = "blank")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::discrete_scale(
    aesthetics,
    "linetype_d",
    linetype_pal(),
    na.value = na.value,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_linetype_identity <- function (aesthetics,
                                         ...,
                                         guide = "none")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::discrete_scale(
    aesthetics,
    "identity",
    identity_pal(),
    ...,
    guide = guide,
    super = ScaleDiscreteIdentity
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_linetype_manual <- function (aesthetics,
                                       ...,
                                       values,
                                       breaks   = waiver(),
                                       na.value = "blank")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  manual_scale(
    aesthetics,
    values,
    breaks,
    ...,
    na.value = na.value
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_radius <- function (aesthetics,
                              name   = waiver(),
                              breaks = waiver(),
                              labels = waiver(),
                              limits = NULL,
                              range  = c(1, 6),
                              trans  = "identity",
                              guide  = "legend")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::continuous_scale(
    aesthetics,
    "radius",
    scales::rescale_pal(range),
    name   = name,
    breaks = breaks,
    labels = labels,
    limits = limits,
    trans  = trans,
    guide  = guide
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_size <- function (aesthetics,
                            name   = waiver(),
                            breaks = waiver(),
                            labels = waiver(),
                            limits = NULL,
                            range  = c(5, 10),
                            trans  = "identity",
                            guide  = "legend")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::continuous_scale(
    aesthetics,
    "area",
    area_pal(range),
    name   = name,
    breaks = breaks,
    labels = labels,
    limits = limits,
    trans  = trans,
    guide  = guide
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_size_area <- function (aesthetics,
                                 ...,
                                 max_size = 6)
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::continuous_scale(
    aesthetics,
    "area",
    palette  = abs_area(max_size),
    rescaler = rescale_max,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_size_binned <- function (aesthetics,
                                   name        = waiver(),
                                   breaks      = waiver(),
                                   labels      = waiver(),
                                   limits      = NULL,
                                   range       = c(5, 10),
                                   n.breaks    = NULL,
                                   nice.breaks = TRUE,
                                   trans       = "identity",
                                   guide       = "bins")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::binned_scale(
    aesthetics,
    "area_b",
    area_pal(range),
    name        = name,
    breaks      = breaks,
    labels      = labels,
    limits      = limits,
    trans       = trans,
    n.breaks    = n.breaks,
    nice.breaks = nice.breaks,
    guide       = guide
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_size_binned_area <- function (aesthetics,
                                        ...,
                                        max_size = 6)
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::binned_scale(
    aesthetics,
    "area_b",
    palette = abs_area(max_size),
    rescaler = rescale_max,
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_size_continuous <- function (aesthetics,
                                       name   = waiver(),
                                       breaks = waiver(),
                                       labels = waiver(),
                                       limits = NULL,
                                       range  = c(5, 10),
                                       trans  = "identity",
                                       guide  = "legend")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::continuous_scale(
    aesthetics,
    "area",
    scales::area_pal(range),
    name = name,
    breaks = breaks,
    labels = labels,
    limits = limits,
    trans = trans,
    guide = guide)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_size_discrete <- function (aesthetics,
                                     ...)
{
  warning("Using size for a discrete variable is not advised.", call. = FALSE)
  scale_svg_size_ordinal(aesthetics = aesthetics, ...)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_size_identity <- function (aesthetics,
                                     ...,
                                     guide = "none")
{

  aesthetics <- prepare_aesthetics(aesthetics)

  ggplot2::continuous_scale(
    aesthetics,
    "identity",
    scales::identity_pal(),
    ...,
    guide = guide,
    super = ScaleContinuousIdentity
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_size_manual <- function (aesthetics,
                                   ...,
                                   values,
                                   breaks   = waiver(),
                                   na.value = NA)
{

  aesthetics <- prepare_aesthetics(aesthetics)

  manual_scale(
    aesthetics,
    values,
    breaks,
    ...,
    na.value = na.value
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_svg_alpha
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_size_ordinal <- function (aesthetics,
                                    ...,
                                    range = c(5, 10))
{

  aesthetics <- prepare_aesthetics(aesthetics)

  force(range)

  ggplot2::discrete_scale(
    aesthetics,
    "size_d",
    function(n) {
      area <- seq(range[1]^2, range[2]^2, length.out = n)
      sqrt(area)
    },
    ...
  )
}

