
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggsvg - Use SVG as points in ggplot <img src="man/figures/logo-ggsvg.png" align="right" width="230"/>

### With the novel capability of aesthetic mappings to *any* SVG feature

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg)
[![R-CMD-check](https://github.com/coolbutuseless/ggsvg/workflows/R-CMD-check/badge.svg)](https://github.com/coolbutuseless/ggsvg/actions)
<!-- badges: end -->

`ggsvg` is an extension to ggplot to use SVG for points.

The SVG may be customised to respond to aesthetics e.g. element colours
can changes in response to fill and/or colour scales.

Note that aesthetics are not limited to colour - any other SVG
parameter/value can be linked to any aesthetic which makes sense e.g. an
aesthetic may be used to control the corner radius on a rounded
rectangle.

## What’s in the box

-   `geom_point_svg()` is equivalent to `geom_point()` except it also
    requires SVG text to be set (via the `svg` argument or aesthetic
    mapping)
-   `scale_svg_*` a complete set of compatible scale functions for
    controlling the mapping of values to arbitrary named aesthetics.

## Installation

Install from [GitHub](https://github.com/coolbutuseless/ggsvg).

The [rsvg](https://github.com/ropensci/rsvg) package (\>= 2.3.0) is used
to convert SVG into an R raster object.

``` r
# install.package('remotes')
remotes::install_github('coolbutuseless/ggsvg')
```

## Using an existing SVG

In the simplest case where the user just wants to use an SVG as a
plotting glyph, only two changes are needed over a basic ggplot:

1.  Use `geom_point_svg()`
2.  specify the `svg =` argument

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read SVG from the web
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
car_url <- 'https://www.svgrepo.com/download/114837/car.svg'
car_svg <- paste(readLines(car_url), collapse = "\n")
```

``` r
grid::grid.draw(svg_to_rasterGrob(car_svg))
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

``` r
ggplot(mtcars) + 
  geom_point_svg(aes(mpg, wt), svg = car_svg, size = 8) + 
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

## Create simple SVG Image

The following simple SVG constructed by hand is just a square and a
ciricle.

``` r
library(ggplot2)
library(ggsvg)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define simple SVG
#   - Square with rounded corners and a circle inside it.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
simple_text <- '
  <svg viewBox="0 0 100 100 ">
    <rect width="100" height="100" fill="#88ccaa" />
    <circle cx="50" cy="50" r="40" fill="white" />
  </svg>
  '


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Render SVG to a rasterGrob
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
grob <- svg_to_rasterGrob(simple_text)
grid::grid.newpage()
grid::grid.draw(grob)
```

<img src="man/figures/README-simple_svg-1.png" width="100%" />

## Use SVG as point

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use 'geom_point_svg' to plot SVG image at each point
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(mtcars) +
  geom_point_svg(
    mapping  = aes(mpg, wt),
    svg      = simple_text,
    size     = 10
  ) +
  theme_bw() + 
  labs(
    title = "{ggsvg} Using SVG as points"
  )
```

<img src="man/figures/README-simple-1.png" width="100%" />

## Parameterise the SVG

Introduce parameters in the SVG using [glue]() syntax with double curly
braces i.e. `{{}}`

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define simple SVG
#   - Square with rounded corners and a circle inside it.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parameterised_text <- '
  <svg viewBox="0 0 100 100 ">
    <rect width="100" height="100" fill="{{rect_colour}}" />
    <circle cx="50" cy="50" r="{{radius}}" fill="white" />
  </svg>
  '

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test the glue parameters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rect_colour <- 'green'
radius      <- 10

final_text <- glue::glue(parameterised_text, .open = "{{", .close = "}}")

grob <- svg_to_rasterGrob(final_text)
grid::grid.newpage()
grid::grid.draw(grob)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

## Map ggplot2 aesthetics to the parameterised SVG

``` r
ggplot(mtcars) +
  geom_point_svg(
    mapping  = aes(mpg, wt, radius=as.factor(cyl), rect_colour = disp),
    svg      = parameterised_text,
    size     = 10,
    defaults = list(rect_colour = 'black', radius = 40)
  ) +
  theme_bw() + 
  labs(title = "{ggsvg} Using SVG as points") + 
  scale_svg_size_discrete(
    aesthetics = 'radius', 
    range = c(10, 45),
    guide = guide_legend(override.aes = list(size = 7))
  ) + 
  scale_svg_colour_continuous(
    aesthetics = 'rect_colour'
  )
#> Warning: Using size for a discrete variable is not advised.
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

## Simple aesthetics

This is a basic example showing how ggplot aesthetics may be used to
control SVG image properties.

Some key things to note:

-   Locations for ggplot to insert aesthetics via the `glue` package are
    marked with **double** curly braces i.e. `{{...}}`.
-   One variable has been added to the SVG - `{{fill_tree}}`. This is
    used in place of the static colour that was defined in the original
    SVG
-   `fill_tree` must now appear as an aesthetic in the call to
    `geom_point_svg()`
-   We need to inform `ggplot2` of the default value for each new
    aesthetic by setting the `defaults` argument
-   For each aesthetic, you will also need to add a scale with
    `scale_svg_*()` to let ggplot know how it should turn the mapped
    variable into a value to insert in the SVG.
-   In this case, the variables is a `fill` variable, so use one of
    `scale_svg_fill_*()` family to ensure that the value is mapped to a
    colour.
-   The new `scale_svg_*()` scales are mostly identical to their
    `ggplot` counterparts except:
    -   The `aesthetics` argument no longer has a default value
    -   `colourbar` guides have been tweaked to allow for non-standard
        aesthetics.

## Acknowledgements

-   R Core for developing and maintaining the language.
-   CRAN maintainers, for patiently shepherding packages onto CRAN and
    maintaining the repository
