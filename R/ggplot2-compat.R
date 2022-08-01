
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Included here as it is not exported from ggplot2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
binned_pal <- function (palette) {
  function(x) {
    palette(length(x))
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Included here as it is not exported from ggplot2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ordinal_pal <- function (colours, na.color = "grey50", alpha = TRUE) {
  pal <- scales::colour_ramp(colours, na.color = na.color,
                             alpha = alpha)
  function(n) {
    pal(seq(0, 1, length.out = n))
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Included here as it is not exported from ggplot2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
qualitative_pal <- function (type, h, c, l, h.start, direction) {
  function(n) {
    type_list <- if (!is.list(type))
      list(type)
    else type
    if (!all(vapply(type_list, is.character, logical(1)))) {
      stop("`type` must be a character vector or a list of character vectors",
            call. = FALSE)
    }
    type_lengths <- vapply(type_list, length, integer(1))
    if (max(type_lengths) < n) {
      return((scales::hue_pal(h, c, l, h.start, direction))(n))
    }
    type_list <- type_list[order(type_lengths)]
    i <- 1
    while (length(type_list[[i]]) < n) {
      i <- i + 1
    }
    type_list[[i]][seq_len(n)]
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Included here as it is not exported from ggplot2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
manual_scale <- function (aesthetic, values = NULL, breaks = waiver(), ..., limits = NULL) {
  if (missing(values)) { # changed rlang::is_missing() to missing(). Mikefc 2021-12-31
    values <- NULL
  }
  else {
    force(values)
  }
  if (is.null(limits)) {
    limits <- names(values)
  }
  if (is.vector(values) && is.null(names(values)) && !is.waive(breaks) &&
      !is.null(breaks) && !is.function(breaks)) {
    if (length(breaks) <= length(values)) {
      names(values) <- breaks
    }
    else {
      names(values) <- breaks[1:length(values)]
    }
  }
  pal <- function(n) {
    if (n > length(values)) {
      stop(glue("Insufficient values in manual scale. {n} needed but only {length(values)} provided."))
    }
    values
  }
  discrete_scale(aesthetic, "manual", pal, breaks = breaks,
                 limits = limits, ...)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Included here as it is not exported from ggplot2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is.waive <- function (x) {
  inherits(x, "waiver")
}


