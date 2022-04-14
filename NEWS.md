
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
