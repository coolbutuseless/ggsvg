

.pt <- 2.845276  # ggplot2::.pt

# This will be default size value used when the user hasn't specified.
# In `draw_key_PointSVG` if I see this value, this means I can be pretty
# sure that the user hasn't mapped a variable to the size aesthetic.
# This is an ugly hack. Please fix.
sentinel_default_size <- 7.999


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Key for SVG points
#'
#' @param data,params,size key stuff
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
draw_key_PointSVG <- function(data, params, size) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # SVG
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  svg      <- data$svg[[1]]
  svg      <- glue::glue_data(data[1,], svg, .open = "{{", .close = "}}")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Map any CSS aesthetics
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  css <- NULL

  css_names_idx <- vapply(colnames(data), is_valid_css_aes, logical(1))
  css_names     <- colnames(data)[css_names_idx]
  has_css_aes   <- any(css_names_idx)

  if (any(css_names_idx)) {
    for (css_name in css_names) {
      glue_string <- css_aes_to_glue_string(css_name)
      this_css <- glue::glue_data(data, glue_string, .open = "{{", .close = "}}")
      css <- c(css, this_css)
    }
    css <- paste(css, collapse="\n")
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create GROB
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  svg_grob <- svg_to_rasterGrob(svg, css = css)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Figure out display size in legend.
  #
  # This is tricky because an SVG has no natural size like points in
  # R.  So it's difficult to display them at an absolute size which would make
  # sense.
  #
  # A bit of a heuristic to determine what size the SVG should be:
  # If the actual size is the same as the sentinel size of 7.999, then
  # it means that the SVG should displayed as unscaled.
  #
  # Alternately, if the user is not doing any size mapping in 'aes',
  # then the SVG should also be displayed as unscaled.
  #
  # Otherwise scale the SVG size by the size of the legend box it is to
  # displayed in.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (data$size[[1]] == sentinel_default_size || !isTRUE(params$mapped_size)) {
    w <- h <- grid::unit(0.9, 'npc')
  } else {
    w <- grid::unit(data$size[[1]] * .pt, 'pt')  #0.9 * data$size[[1]] / size[[1]]
    h <- grid::unit(data$size[[1]] * .pt, 'pt')  #0.9 * data$size[[1]] / size[[1]]
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set size in viewport
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  svg_grob$vp <- grid::viewport(
    width  = w,
    height = h
  )


  svg_grob
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Use SVG images as glyphs for points
#'
#'
#' \emph{Aesthetics}
#' \describe{
#' \item{svg}{SVG as a character string}
#' \item{svg_width,svg_height}{Specify rendered width and/or height.  If only one of these values
#'       is specified, then the other will be scaled to keep he aspect ratio.
#'       If neither value is specified (the default) then dimensions will be taken from the
#'       SVG itself.  This value could be used to increase the resolution of the SVG so it
#'       does not appear blurry once rendered to an element in the plot e.g. \code{svg_width = 1000}}
#' \item{hjust,vjust}{The justification of the SVG's bounding rectangle relative to
#'       its (x,y) position. Default value of 0.5 mean to centre the SVG at
#'       the specified location. Standard values for these variables are in the range [0,1]}
#' \item{x_abs,y_abs}{Absolute positioning within the panel. Default: NULL means
#'       that the standard x,y positioning is to be used.  Standard values
#'       for these variables are in the range [0,1]}
#' }
#'
#'
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes see
#'        documentation for \code{ggplot2::geom_point()}
#' @param defaults Advanced option.  A named list of default values for new aesthetics.
#'        In general this is not necessary when using \code{css()} aesthetics, as a
#'        default value will be determined based upon the CSS property e.g. \code{stroke}
#'        property will have a default value of "black"
#'
#' Set `options(GGSVG_DEBUG = TRUE)` for some verbose debugging which will
#' cause ggsvg to output (to the console) the final SVG for each and every element
#' in the plot.
#'
#'
#' @return ggplot2 layer object
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_point_svg <- function(mapping     = NULL,
                           ...,
                           data        = NULL,
                           stat        = "identity",
                           position    = "identity",
                           na.rm       = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           defaults    = list()) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Resolve mapping with the internal my_aes() function (defined in this pkg)
  # instead of ggplot's 'aes()' mapping.
  # This is so I can manually deal with the css() aesthetics which
  # would otherwise not make sense to ggplot internals
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  aes_call <- rlang::enquo(mapping)

  if (rlang::is_call(aes_call) &&
      !is.null(rlang::quo_get_expr(aes_call)) &&
      rlang::call_name(aes_call) == 'aes') {
    # Put 'my_aes()' into the quosure environment
    en <- rlang::quo_get_env(aes_call)
    this_env <- new.env(parent = en)
    this_env[['my_aes']] <- my_aes
    aes_call <- rlang::quo_set_env(aes_call, this_env)

    # replace the call to "aes()" with a call to "my_aes()"
    ex <- rlang::quo_get_expr(aes_call)
    ex[[1]] <- as.name("my_aes")
    aes_call <- rlang::quo_set_expr(aes_call, ex)
    mapping <- rlang::eval_tidy(aes_call)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sometimes we don't have a mapping because we are using absolute positioning,
  # so x and y can be ignored completely.
  # We can't set them to 'NA' of they will be filtered out.
  # We can't set them to '0', as then ggplot wants to include (0,0) in the plot
  # so set them to (Inf, Inf) which doesn't affect plot size/extents
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(mapping)) {
    mapping <- aes(x=Inf, y=Inf)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ggsvg has to dynamically add new aesthetics to the Geom
  # If I keep adding them to "GeomPointSVG" then they are attached to
  # the ggproto object in the global env for the duration of the R session.
  # This can lead to aesthetics hanging around which are actively an issue
  # for different SVG - especially CSS Aesthetics.
  # Instead, create a new GeomPointSVG Geom for every plot, so it can
  # be adapted and updated, and have its own environment independent of
  # other GeomPointSVG Geoms
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_geom <- create_new_GeomPointSVG()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Figure out which aesthetics are unknown
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # user named all these aesthetics
  this_aes <- union(names(mapping), names(list(...)))

  # The geom knows all these aesthetics
  known_aes <- sort(unique(c(
    this_geom$required_aes,
    this_geom$non_missing_aes,
    names(this_geom$default_aes)
  )))

  unknown_aes <- setdiff(this_aes, known_aes)
  unknown_aes <- setdiff(unknown_aes, "") # css() calls will be unnamed. ignore here. handle later

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # User can give a default value for all unknown aes!
  # but if they used the preferred "blah_type" naming, then I can infer
  # whether the default value should be a colour or a number.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  unhandled_aes <- unknown_aes[!unknown_aes %in% names(defaults)]
  css_aes       <- unhandled_aes[ startsWith(unhandled_aes, "css_")]
  unhandled_aes <- unhandled_aes[!startsWith(unhandled_aes, "css_")]

  colour_types <- c(
    'color', 'colour', 'stroke', 'fill', 'stop-color', 'flood-color',
    'lighting-color', 'background-color'
  )


  for (ua in unhandled_aes) {
    # message("Handle the unhandled: ", ua)
    bits <- parse_aes_type(ua)
    if (bits$type == 'unknown') {
      warning("No `defaults` value set for: ", deparse1(ua), "using '1'")
      this_geom$default_aes[[ua]] <- 1
    } else if (bits$property %in% colour_types) {
      this_geom$default_aes[[ua]] <- 'black'
    } else {
      this_geom$default_aes[[ua]] <- 1
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add in defaults for CSS aesthetics
  # If we specify them as 'NULL', when the CSS aes is converted to a glue
  # string, the NULL value will cause the glue string to produce an
  # empty string. Thus the CSS will not be created and the built-in
  # style in the SVG will be used.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (css_name in css_aes) {
    this_geom$default_aes[[css_name]] <-list(NULL)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add all the default values for the aesthetics as specified by the user
  # in the 'defaults' argument
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq_along(defaults)) {
    aes_name    <- names(defaults)[i]
    aes_default <- defaults[[i]]

    this_geom$default_aes[[aes_name]] <- aes_default
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # These are the named parameters
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  params <- list(
    na.rm = na.rm,
    mapped_size  = 'size' %in% names(mapping)
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The user may also specify other 'static' aesthetics like 'size = 10'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ll <- list(...)
  ll <- ll[names(ll) != '']

  params <- c(params, ll)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Static CSS aesthetics i.e. *not* within an aes.
  # e.g.  geom_point_svg(aes(...), css("circle .big", fill = 'green'))
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dts <- rlang::exprs(...)
  dts <- dts[!rlang::have_name(dts)]
  for (i in seq_along(dts)) {
    dt <- dts[[i]]
    nn <- names(dt)
    this_geom$default_aes[[nn]] <- dt[[1]]

    # Add both 'color' and 'colour' versions of the defaults.
    # this was a workaround (introduced 2022-04-23) and may no longer
    # be required.
    if (grepl('color', nn)) {
      nn <- gsub('color', 'colour', nn)
      this_geom$default_aes[[nn]] <- dt[[1]]
    }
    params <- c(params, dt)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the layer
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p <- layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = this_geom,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = params
  )


  p
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# If the user doesn't specify an 'svg', argument to the geom,
# then this is used as the default
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
svg_text <- '
  <svg viewBox="0 0 100 100 ">
    <circle cx="50" cy="50" r="50" stroke="{colour}" fill="{fill}" />
  </svg>
  '


global <- new.env()
global$count <- 0L

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a fresh instance of a GeomPointSVG ggproto object
#'
#' These Geoms are created dynamically as each time \code{geom_point_svg()}
#' is called, it wants to customize the \code{$default_aes} on this
#' ggproto.  Because ggproto objects are environments, then setting a default
#' on the global copy would set the default for all references. This will
#' get messy as things like CSS Aesthetics should not be shared between
#' plots.
#'
#' So every geom_point_svg() gets a fresh GeomPointSVG by calling this function.
#'
#'
#' @return ggproto object for GeomPointSVG
#'
#' @import grid
#' @import ggplot2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_new_GeomPointSVG <- function() {
  global$count <- global$count + 1L

  geom_point_idx <- global$count

  GeomPointSVG <- ggplot2::ggproto(
    "GeomPointSVG",
    ggplot2::GeomPoint,
    required_aes = c("x", "y"),
    non_missing_aes = c("size"),
    default_aes = ggplot2::aes(
      shape      = 19,
      colour     = "black",
      size       = sentinel_default_size,  # this is a sentinel value used during draw_key
      fill       = 'black',
      alpha      = 1,
      stroke     = 0.5,
      svg        = svg_text,
      svg_width  = NULL,
      svg_height = NULL,
      hjust      = 0.5,
      vjust      = 0.5,
      x_abs      = NULL,
      y_abs      = NULL,
    ),

    draw_panel = function(data, panel_params, coord, na.rm = FALSE, mapped_size = NULL) {

      debug  <- isTRUE(getOption("GGSVG_DEBUG", FALSE))
      coords <- coord$transform(data, panel_params)


      if (debug) {
        message("GeomPointSVG$draw_panel() 'coords' data.frame names")
        # print(names(coords))
        print(coords)
      }

      if (FALSE) {
        zz <- coords
        zz$svg <- NULL
        print(zz)
      }

      is_static_svg <- length(unique(coords$svg)) == 1 && !grepl("\\{\\{", coords$svg[[1]])

      css_names_idx <- vapply(colnames(coords), is_valid_css_aes, logical(1))
      css_names     <- colnames(coords)[css_names_idx]
      has_css_aes   <- any(css_names_idx)

      if (is_static_svg && !has_css_aes) {
        # Parse the SVG just once
        svg_grob_orig <- svg_to_rasterGrob(coords$svg[[1]],
                                           width  = coords$svg_width,
                                           height = coords$svg_height)
      }


      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Function to create a grob for a row in coords
      # @param i row number
      # @return grob for this row
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      create_grob_for_plot <- function(i) {

        # if SVG changes, then need to re-parse it for every row
        if (is_static_svg && !has_css_aes) {
          # Copy the original grob and add a new suffix so that it is guaranteed
          # that all grobs have a unique name.
          # Ensure that SVGs across multiple `geom_point_svg()` in the same
          # plot are globally unique
          svg_grob <- add_suffix(svg_grob_orig, paste(geom_point_idx, i, sep="."))
        } else {
          svg <- coords$svg[[i]]

          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Carefully glue() and trap any errors in the process so we can
          # give good feedback to the user on what variables might
          # be an issue
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          svg <- tryCatch(
            glue::glue_data(coords[i,], svg, .open = "{{", .close = "}}"),
            error = function(e) {
              msg <- e$message
              missing_obj <- stringr::str_match(msg, "object '(.*)' not found")
              if (nrow(missing_obj) == 1) {
                var <- missing_obj[1, 2]
                stop("Variable '", var, "' required for SVG has not been found\n",
                     "Please assign a default value using 'geom_point_svg(..., defaults = list(`", var, "` = ...))'\n",
                     "You may also include `", var, "` as a mapped or static aesthetic",
                     call. = FALSE)
              } else {
                stop(msg)
              }
            }
          )

          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          if (debug) print(svg)

          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Unpack CSS if any
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          css <- NULL
          if (has_css_aes) {
            for (css_name in css_names) {
              glue_string <- css_aes_to_glue_string(css_name)
              this_css <- glue::glue_data(coords[i,], glue_string, .open = "{{", .close = "}}")
              css <- c(css, this_css)
            }
            css <- paste(css, collapse="\n")

            if (debug && i == 1) {
              cat(css, "\n")
            }
          }


          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Render grob
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          svg_grob <- svg_to_rasterGrob(
            svg,
            width  = coords$svg_width[i],
            height = coords$svg_height[i],
            css    = css
          )

        }

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Adjust viewport of grob so it appears in the correct location on plot
        # Use absolute position if specified, other use the scaled x coordinate
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        x <- coords$x_abs[i] %||% coords$x[i]
        y <- coords$y_abs[i] %||% coords$y[i]

        dims <- dim(svg_grob$raster)

        svg_grob$vp <- grid::viewport(
          width  = grid::unit(coords$size[[i]] * .pt, 'pt'),
          height = grid::unit(coords$size[[i]] * .pt * dims[1]/dims[2], 'pt'),
          x      = grid::unit(x, 'npc'),
          y      = grid::unit(y, 'npc'),
          just   = c(coords$hjust[i], coords$vjust[i])
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
}



# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #' GeomPointSVG
# #'
# #' @rdname ggplot2-ggproto
# #' @format NULL
# #' @usage NULL
# #' @export
# #' @import ggplot2
# #' @import grid
# #' @export
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GeomPointSVG <- create_new_GeomPointSVG()
