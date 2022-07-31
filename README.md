
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggsvg - Use SVG as points in ggplot <img src="man/figures/logo-ggsvg.png" align="right" width="230"/>

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg)
[![R-CMD-check](https://github.com/coolbutuseless/ggsvg/workflows/R-CMD-check/badge.svg)](https://github.com/coolbutuseless/ggsvg/actions)
<!-- badges: end -->

`ggsvg` is an extension to ggplot to use SVG images for points.

Variables may be aesthetically mapped to features within the SVG using
CSS selectors via the `css()` helper function.

## What’s in the box

-   `geom_point_svg()` for plotting points with SVG as the glyph (This
    is a direct analogue to `geom_point()`)
-   `scale_svg*()` functions for controlling the aesthetic mapping.
    -   `scale_svg_default()` is a sensible default for most plots.
    -   `scale_svg_*` are a shadow set of `ggplot2::scale_*()` functions
        with adaptations needed for `css()` selectors as aesthetics.
    -   E.g. `scale_svg_fill_brewer()` is a direct analogue for
        `ggplot2::scale_fill_brewer()`
-   `css(selector, property = value)` is a helper function for aesthetic
    mapping to CSS Selector targets within SVGs

## Installation

Install from [GitHub](https://github.com/coolbutuseless/ggsvg).

The [`{rsvg}`](https://github.com/ropensci/rsvg) package is used to
convert SVG into an R raster object. This requires at least rsvg(\>=
2.3.0).

``` r
# install.package('remotes')
install.packages('rsvg')
remotes::install_github('coolbutuseless/ggsvg')
```

# Simple plot

``` r
svg_url <- 'https://www.svgrepo.com/download/289000/jellyfish.svg'
svg_txt <- paste(readLines(svg_url), collapse = "\n")
```

``` r
grid::grid.draw( svg_to_rasterGrob(svg_txt) )
```

![](man/figures/README-unnamed-chunk-4-1.png)<!-- -->

``` r
test_df <- data.frame(
  x = runif(10), 
  y = runif(10), 
  count = sample(3:5, 10, T),
  type  = sample(c('a', 'b', 'c'), 10, T))

test_df
#>             x         y count type
#> 1  0.26550866 0.2059746     3    b
#> 2  0.37212390 0.1765568     5    b
#> 3  0.57285336 0.6870228     3    a
#> 4  0.90820779 0.3841037     3    c
#> 5  0.20168193 0.7698414     3    a
#> 6  0.89838968 0.4976992     3    c
#> 7  0.94467527 0.7176185     4    b
#> 8  0.66079779 0.9919061     3    b
#> 9  0.62911404 0.3800352     3    b
#> 10 0.06178627 0.7774452     4    b

ggplot(test_df) + 
  geom_point_svg(aes(x, y), svg = svg_txt) + 
  theme_bw()
```

![](man/figures/README-unnamed-chunk-5-1.png)<!-- -->

# Simple plot with mapped `size` aesthetic

``` r
ggplot(test_df) + 
  geom_point_svg(aes(x, y, size = type), svg = svg_txt) + 
  theme_bw()
```

![](man/figures/README-unnamed-chunk-6-1.png)<!-- -->

## Mapping Aesthetics to SVG features with CSS Selectors

Aesthetic values are mapped to SVG features with [CSS
Selectors](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Selectors).

#### Snowman SVG

Here is a simple SVG consisting of 2 stacked circles - a big circle on
the bottom and a small circle resting on top.

``` r
snowman_txt <- '
  <svg viewBox="0 0 100 100 ">
    <circle id="top" cx="50" cy="20" r="20" fill="brown" stroke="black" />
    <circle id="bot" cx="50" cy="70" r="30" fill="brown" stroke="black" />
  </svg>
  '

grid::grid.draw( svg_to_rasterGrob(snowman_txt, width=800, height=800) )
```

![](man/figures/README-unnamed-chunk-7-1.png)<!-- -->

#### `css()` helper function

Use the `css()` helper function to target aesthetics at selected
elements within and SVG using `css(selector, property = value)`

E.g.

-   **`css("rect.big", stroke = x)`**
    -   Targets `<rect>` elements with `class = "big"`
    -   Map values in `x` in data.frame to the SVG `stroke` property for
        these targetted elements.

#### Example

In the following example, two `css()` selectors are used within the
`geom_point_svg()` call:

-   **`css("circle#top", fill=type)`**
    -   Targets `<circle>` elements with `id = "top"`
    -   Map values in `type` in data.frame to the SVG `fill` property
        for these targetted elements.
-   **`css("circle#top", fill='#aaaaaa")`**
    -   Targets `<circle>` elements with `id = "bot"`
    -   Set a constant value of `#aaaaaa` for the SVG `fill` property
        for these targetted elements.

To configure how the variable is mapped to the property on the selected
target, you can either use:

-   `scale_svg_default()` for reasonable defaults
-   `scale_svg_*()` family of functions
    -   Note: the `aesthetic` argument must match exactly the `css(...)`
        call used in the `geom_point_svg()` call.

``` r
snowman_txt <- '
  <svg viewBox="0 0 100 100 ">
    <circle id="top" cx="50" cy="20" r="20" fill="white" stroke="black" />
    <circle id="bot" cx="50" cy="70" r="30" fill="white" stroke="black" />
  </svg>
  '


ggplot(test_df) + 
  geom_point_svg(
    aes(x, y, css("circle#top", fill = type)),
    css("circle#bot", fill = '#aaaaaa'),
    svg = snowman_txt
  ) +
  theme_bw() + 
  scale_svg_fill_brewer(css("circle#top", fill = type), palette = 'Dark2')
```

![](man/figures/README-unnamed-chunk-8-1.png)<!-- -->

## Acknowledgements

-   R Core for developing and maintaining the language.
-   CRAN maintainers, for patiently shepherding packages onto CRAN and
    maintaining the repository
