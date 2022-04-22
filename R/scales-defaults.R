
css_colour_properties <- c(
  'color', 'colour', 'stroke', 'fill', 'stop-color', 'flood-color',
  'lighting-color'
)
css_numeric_properties <- c(
  'stroke-width', 'radius', 'angle', 'rotation',
  'accent-height', 'opacity', 'stop-opacity', 'fill-opacity',
  'cx', 'cy', 'r', # circle
  'rx', 'ry', # ellipse
  'height', 'width',
  'x', 'y'
)

# needs a "px"
css_pixel_properties <- c(
  'letter-spacing'
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Auto aesthetics for those marked with a ":"
#'
#' @param p ggplot object
#' @param verbose logical.  Be verbose about what scales are being created.
#'        default: FALSE
#'
#' @return ggplot object with default scales added to the plot environment
#'         for any aesthetic which nominates its type
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


  all_aes <- lapply(p$layers, function(layer) {
    names(layer$geom$default_aes)
  })
  all_aes <- unlist(all_aes)
  all_aes <- grep("_", all_aes, value = TRUE)

  # have a good guess on what the target type is e.g. fill/scale/linetype
  #    based upon the last :type
  # find the input data
  #    - might need to do some calculations apply the 'stat' ?
  #    - if it is 'waiver', then get data from parent.
  #    - follow the first part of 'ggplot_build.ggplot' to resolve data?
  # apply the mapping
  #    - rlang::eval_tidy(RHS_AES, data = data)
  # determine the 'type' of the data
  # create a function that returns a scale
  # add it to the plot_env
  #
  # Turn this into a function that supports the "+" operator with a gpplot
  #
  #  Write "aes_css" to replace "aes()"
  #  Write a "css(selector, property)" helper with syntax:
  #     css("circle .big", fill = as.factor(cyl))
  # maybe:  css("circle", fill=as.factor(cyl), `stroke_width`=disp)


  # this_aes <- all_aes[[1]]

  for (this_aes in all_aes) {
    # message("TODO: auto gen scale in plot_env: ", this_aes)

    target_type <- css_aes_property(this_aes)

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

    if (target_type == '') next

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

    # message(this_aes)

    # Generate all possible scales for the possible input types and add them
    # to this plots environment
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
#' @param object,plot,objectname see ggplot2 docs
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot_add.ScaleSVGDefault <- function(object, plot, objectname) {
  scale_ggsvg_default(plot)
}






