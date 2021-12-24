

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Key SVG
#'
#' @param data,params,size key stuff
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
draw_key_PointSVG <- function(data, params, size) {

  svg      <- data$svg[[1]]
  svg      <- glue::glue_data(data[1,], svg, .open = "{{", .close = "}}")
  svg_grob <- svgparser::read_svg(svg)


  w <- data$size[[1]] / size[[1]]
  h <- data$size[[1]] / size[[1]]

  svg_grob$vp <- grid::viewport(
    width  = w,
    height = h
  )


  svg_grob
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Draw SVG
#'
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes see
#'        documentation for \code{ggplot2::geom_point()}
#' @param defaults named list of default values for new aesthetics
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_point_svg <- function(mapping     = NULL,
                           data        = NULL,
                           stat        = "identity",
                           position    = "identity",
                           ...,
                           na.rm       = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           defaults    = list()) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Figure out which aesthetics are unknown
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # user named all these aesthetics
  this_aes <- union(names(mapping), names(list(...)))

  # The geom knows all these aesthetics
  known_aes <- sort(unique(c(
    GeomPointSVG$required_aes,
    GeomPointSVG$non_missing_aes,
    names(GeomPointSVG$default_aes)
  )))

  unknown_aes <- setdiff(this_aes, known_aes)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # User must give a default value for all unknown aes!
  # Throw an error if none given
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  unhandled_aes <- unknown_aes[!unknown_aes %in% names(defaults)]
  if (length(unhandled_aes) > 0) {
    stop("Please set a `defaults` value for: ", deparse1(unhandled_aes))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add all the default values for the aesthetics
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq_along(defaults)) {
    aes_name    <- names(defaults)[i]
    aes_default <- defaults[[i]]

    GeomPointSVG$default_aes[[aes_name]] <- aes_default
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the layer
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p <- layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomPointSVG,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm = na.rm,
      ...
    )
  )


  p
}


svg_text <- '
  <svg viewBox="0 0 100 100 ">
    <circle cx="50" cy="50" r="50" stroke="{colour}" fill="{fill}" />
  </svg>
  '

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' GeomSVG
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomPointSVG <- ggplot2::ggproto(
  "GeomPointSVG",
  ggplot2::GeomPoint,
  required_aes = c("x", "y"),
  non_missing_aes = c("size"),
  default_aes = ggplot2::aes(
    shape  = 19,
    colour = "black",
    size   = 1.5,
    fill   = 'black',
    alpha  = 1,
    stroke = 0.5,
    svg    = svg_text,
    scale  = 5
  ),


  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {

    debug  <- isTRUE(getOption("GGSVG_DEBUG", FALSE))
    coords <- coord$transform(data, panel_params)

    is_static_svg <- length(unique(coords$svg)) == 1 && !grepl("\\{\\{", coords$svg[[1]])

    if (is_static_svg) {
      svg_grob <- svgparser::read_svg(coords$svg[[1]])
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Function to create a grob for a row in coords
    # @param i row number
    # @return grob for this row
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    create_grob_for_plot <- function(i) {

      # if SVG changes, then need to re-parse it for every row
      if (!is_static_svg) {
        svg         <- coords$svg[[i]]
        svg         <- glue::glue_data(coords[i,], svg, .open = "{{", .close = "}}")
        svg_grob    <- svgparser::read_svg(svg)
      }

      if (debug) print(svg)

      svg_grob$vp <- grid::viewport(
        width  = grid::unit(coords$size[[i]] * 3, 'pt'),
        height = grid::unit(coords$size[[i]] * 3, 'pt'),
        x      = coords$x[i],
        y      = coords$y[i]
      )
      svg_grob$name <- strftime(Sys.time(), "%H%M%OS6") # Enforce unique name per grob.
      svg_grob
    }


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create a grob for each row in coords and return a grobTree
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    svg_grobs <- lapply(seq_len(nrow(coords)), create_grob_for_plot)
    do.call(grobTree, svg_grobs)
  },

  draw_key = draw_key_PointSVG
)

