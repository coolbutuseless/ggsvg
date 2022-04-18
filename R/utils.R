

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NULL operator
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'%||%' <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Recursively update all names in a grobTree by adding a suffix to the name
#'
#' If two grobs in a plot have the same name, then it is not guaranteed that
#' both will be drawn. This function ensures that every grob in a tree has
#' a unique name.
#'
#' @param x grobTree
#' @param suffix string
#'
#' @return new grobTree
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
add_suffix <- function(x, suffix) {
  x$name <- paste(x$name, suffix, sep=".")

  child_grobs <- lapply(x$children, add_suffix, suffix = suffix)
  x$children <- do.call(grid::gList, child_grobs)
  x
}
