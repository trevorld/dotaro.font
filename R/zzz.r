#' @importFrom affiner as_coord2d
#' @import dee
#' @importFrom glue glue
#' @importFrom omsvg SVG svg_attrs_pres svg_path
#' @importFrom rlang abort check_dots_empty local_options warn
NULL

glyph_dir <- function(font = "square", create = TRUE) {
    dir <- tools::R_user_dir("dotaro.font", "cache") |> file.path(font)
    if (create && !dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
    }
    invisible(dir)
}

glyph_file <- function(font, hex) {
    hex <- as.character(hex)
    glyph_dir(font) |> file.path(paste0(hex, ".svg"))
}

# `fontforge` seems to recommend svg files in pixels
# By CSS standards a "px" unit is exactly equal to 1/96th of an "in" unit.
svg_square <- function(d, file) {
    attrs <- svg_attrs_pres(fill_rule = "evenodd")
    s <- SVG(width = SQUARE_WIDTH, height = SQUARE_HEIGHT, viewbox = TRUE)
    for (i in seq_along(d)) {
        s <- svg_path(s, d[[i]], fill = "black", stroke = "none", stroke_width = 0, attrs = attrs)
    }
    writeLines(as.character(s), file)
}

as_hex <- function(x) as.character(as.hexmode(x))

glyph_dee_options <- function(width, height) {
    stopifnot(is.numeric(width), is.numeric(height))
    attrs <- svg_attrs_pres(fill_rule = "evenodd")
    dee_options(dee.width = width, dee.height = height,
                dee.background_color = "grey90",
                dee.origin_at_bottom = TRUE, dee.fill = "black",
                dee.stroke = "none", dee.stroke_width = 0, dee.attrs = attrs)
}

plot_glyph <- function(x, font = "square") {
    stopifnot(requireNamespace("svgparser", quietly = TRUE))
    if (inherits(x, "dee")) {
        plot(x)
    } else if (is.character(x)) { # hex of svg already drawn
        f <- glyph_file(font, x)
        grid::grid.newpage()
        svgparser::read_svg(f) |> grid::grid.draw()
    } else {
        stop("Don't know how to plot this object")
    }
}

# paths try outer clockwise, inner counter-clockwise?

SQUARE_HEIGHT <- 2048L
SQUARE_WIDTH <- 2048L

NARROW_HEIGHT <- 2048L
NARROW_WIDTH <- 1024L

dotaro_height <- function(font = c("square", "narrow")) {
    font <- match.arg(font)
    switch(font,
           square = SQUARE_HEIGHT,
           narrow = NARROW_HEIGHT)
}

dotaro_width <- function(font = c("square", "narrow")) {
    font <- match.arg(font)
    switch(font,
           square = SQUARE_WIDTH,
           narrow = NARROW_WIDTH)
}
