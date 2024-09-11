

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# What type of values goes with each CSS property?
#
# Use this information to have a good guess on the appropriate scale
# to add to the plot when using `scale_svg_default()`
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
css_colour_properties <- c(
  'color', 'colour', 'stroke', 'fill', 'stop-color', 'flood-color',
  'lighting-color', 'background-color'
)

css_numeric_properties <- c(
  'stroke-width', 'radius', 'angle', 'rotation',
  'accent-height', 'opacity', 'stop-opacity', 'fill-opacity',
  'cx', 'cy', 'r', # circle
  'rx', 'ry', # ellipse
  'height', 'width',
  'x', 'y'
)

# Pixel properties are numeric, but need a "px" appended in order to be
# valid CSS
css_pixel_properties <- c(
  'letter-spacing'
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Automatically add a default scale for SVG CSS aesthetics
#'
#' @section Technical Details:
#'
#' If a scale for a particular aesthetic is not explicitly given, then
#' \code{ggplot2} guesses which scales to use based upon two things:
#'
#' \itemize{
#'   \item{The aesthetic being mapped e.g. \code{fill}}
#'   \item{The type of data that is being mapped e.g. \code{continuous}, \code{discrete},
#'         \code{date} etc}
#' }
#'
#' For regular \code{ggplot2} plots, the combination of all these factors have
#' scales included in the \code{ggplot2} package e.g. \code{scale_fill_continous},
#' \code{scale_linetype_discrete}.
#'
#' However, for \code{ggsvg}, using \code{css()} aesthetics means that there
#' are a very large number of aesthetics being mapped to i.e not just \code{fill}
#' and \code{colour}, but also \code{stroke-width}, \code{angle} and many other
#' CSS parameters.
#'
#' Furthermore,  CSS selectors are used to target the aesthetic, and since
#' there are an infinite number of valid CSS selectors, there will be an
#' infinite number of scales needed to be provided by \code{ggsvg}.
#'
#' Fortunately, the majority of things we want to map to in SVG are either
#' colours or numbers, which means that we can create a single \code{colour} scale
#' for use with \code{ggsvg} and just change its name, then add it to the plot
#' environment so that \code{ggplot2} can find it when it renders the plot.
#'
#'
#'
#' @param p \code{ggplot2} object
#' @param verbose Be verbose about what scales are being created? Logical.
#'        Default: FALSE
#'
#' @return \code{ggplot2} object with default \code{scale_svg_*()} added to
#'         the plot environment so that \code{ggplot2} will find these
#'         scales.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_ggsvg_default <- function(p, verbose = FALSE) {

  # Override the plot environment to be a totally fresh environment
  # that we can write stuff to.
  # But still set the parent to be the original envrionment so that
  # anything on the old search path can still be found
  orig_env <- p$plot_env
  p$plot_env <- new.env(parent = orig_env)

  # All the complex aesthetics in ggsvg have an underscore in them.
  # If they were created by `css()` (which should usually be the case),
  # then they will be of the form  "css_[selector]_[type]"
  # Otherwise if using the {glue} syntax for aesthetics, they may be "[label]_[type]
  # in this block of code, find all the aesthetic names with underscores
  all_aes <- lapply(p$layers, function(layer) {
    names(layer$geom$default_aes)
  })
  all_aes <- unlist(all_aes)
  all_aes <- grep("_", all_aes, value = TRUE)


  # To understand this section, remember
  #   css("circle", stroke = cyl)   =>   aesthetic named "css_circle_stroke"
  #  parse_aes_type("css_circle_stroke") => list of components
  # $type     = "css"
  # $selector = "circle"
  # $property = "stroke"
  # $format   = "[x]"      # A glue string of how to format the value e.g. "[x]px"
  # $aes_name = "css_circle_stroke"
  for (this_aes in all_aes) {
    aes_bits <- parse_aes_type(this_aes)

    # For CSS aesthetics, or glue-aesthetics following the preferred name, then
    # a type is usually extracted.   If parse_aes_type() couldn't find a type,
    # then don't try and do anything and let the user sort it out.
    if (aes_bits$type == 'unknown') {
      if (isTRUE(verbose)) message("Don't know what scale to use for: ", aes_bits$aes_name)
      next
    }

    # What is the CSS type that is being targetted? e.g. 'stroke'?
    target_type <- aes_bits$property

    # Canonicalise the target-type into 'fill' or 'size'
    # There's not much else that the value could be mapped to for a sensible
    # aesthetic.   But there could be!  Raise a 'github' issue if
    # you want to do something fancy here!
    if (target_type %in% css_colour_properties) {
      target_type <- 'fill'
    } else if (target_type %in% css_numeric_properties) {
      target_type <- 'size'
    } else {
      if (verbose) {
        message("target_type unknown: ", target_type)
      }
      target_type <- ''
    }

    # If a canonical target type could not be determined, then there's no
    # way I can add a scale to this plot
    if (target_type == '') next

    # Create a new scale function that shadows one of the included
    # scale functions in this package.
    #
    # @param ggsvg_scale_name the name of the ggsvg scale
    #        e.g. `scale_circle .big_fill_discrete`
    # @param the aesthetic that this is for
    #        e.g. `css=circle .big:fill`
    create_scale_func <- function(ggsvg_scale_name, this_aes) {
      ggsvg_scale <- get(ggsvg_scale_name)
      ggsvg_aes   <- this_aes
      function() {
        ggsvg_scale(aesthetics = ggsvg_aes)
      }
    }

    # Generate all possible scales for the possible input types and add them
    # to this plots environment.
    # By using this shotgun approach and adding every possible datatype,
    # it means I do not have to pre-determine what the datatype is for this
    # variable in the data.frame passed in by the user.
    for (input_type in c('continuous', 'discrete', 'binned', 'identity', 'date', 'datetime', 'ordinal')) {
      ggplot_scale_name <- paste("scale", this_aes, input_type, sep="_")
      ggsvg_scale_name <- paste("scale_svg", target_type, input_type, sep="_")

      if (exists(ggsvg_scale_name)) {
        if (verbose) {
          message(this_aes, " :: ", ggsvg_scale_name, "  -->>  ", ggplot_scale_name)
        }
        p$plot_env[[ggplot_scale_name]] <- create_scale_func(ggsvg_scale_name, this_aes)
      } else {
        if (verbose) {
          message("ggsvg scale not found: ", ggsvg_scale_name)
        }
      }
    }

  }

  p
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Public facing method
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_svg_default <- function() {
  structure(list(), class = "ScaleSVGDefault")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' S3 method
#' @param object,plot,object_name see ggplot2 docs
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot_add.ScaleSVGDefault <- function(object, plot, object_name) {
  scale_ggsvg_default(plot)
}






