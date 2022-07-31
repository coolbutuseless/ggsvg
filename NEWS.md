
# ggsvg 0.1.12 2022-07-31

* Fix issue with `css()` selectors in some `scale_svg_*()` functions

# ggsvg 0.1.11 2022-04-25

* Absolute legend sizing when size is an aesthetic.  Size in legend now matches 
  size on plots
* Improved default SVG sizing (prior sizing was from ggplots point sizing which is
  too small to be legible)
* Add `hjust` and `vjust` 
* Add absolute positioning with `x_abs` and `y_abs` which override any 
  `x` and `y`

# ggsvg 0.1.10 2022-04-24

* CSS and bespoke aesthetics both now support static assignment (in addition to the dynamic mapping).

# ggsvg 0.1.9 2022-04-23

* Initial support for formatting the value for CSS aesthetics e.g. if it 
  needs a 'px' or '%' suffix, or some other bespoke formatting.
* Infer default values for parameterised aesthetics if they are given in the 
  preferred format.

# ggsvg 0.1.8 2022-04-22

* Improved sizing of SVG in legends.  Drastically reduced the number of 
  situations that need to use `guides()` to adjust the size of legend
  elements
* Improved the override of `aes()` parsing within `geom_point_svg()` which 
  means that `my_aes()` is no longer dumped into the global environment.
* Expanded the number of CSS properties that get a reasonable default scale

# ggsvg 0.1.7 2022-04-21

* Unify the preferred naming for glue-parameterised SVG with naming required for
  CSS aesthetics.   i.e.  `[blah]_[type]`.  This is so ggsvg can make better guesses on 
  the default scale for any novel aesthetic.

# ggsvg 0.1.6 2022-04-21

* Wrapping CSS aesthetics a little with a `css()` helper
* Added `scale_svg_default()` which will automatically determine default
  scales for CSS aesthetics and add them to the plot

# ggsvg 0.1.5 2022-04-18

* Use `rsvg` v2.3.0+ for all SVG conversion
* Remove all C wrappers now that `rsvg` 2.3.0 on CRAN has nativeraster support
* Introduce CSS aesthetics

# ggsvg 0.1.4 2022-04-15

* call `rsvg::rsvg_nativeraster()` if user has version >= 2.3.0 of `rsvg`
* include `css` argument in `svg_to_nativeRaster()`

# ggsvg 0.1.3 2022-04-14

* Dropping `{svgparser}` as a dependency
* Use `{rsvg}` for parsing SVG and then massaging its output to a rasterGrob

# ggsvg 0.1.2 2021-12-31

* Adjust fastpath such that grob names are always unique

# ggsvg 0.1.1 2021-12-24 

* Add fastpath when SVG is static i.e. reduce number of parsing attempts

# ggsvg 0.1.0 2021-12-24

* Initial release
