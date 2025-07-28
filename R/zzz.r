#' @importFrom affiner as_coord2d degrees isotoxal_2ngon_inner_radius
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
    if (nchar(hex) < 4L) {
        hex <- paste0(strrep("0", 4L - nchar(hex)), hex)
    }
    glyph_dir(font) |> file.path(paste0(hex, ".svg"))
}

# `fontforge` seems to recommend svg files in pixels
# By CSS standards a "px" unit is exactly equal to 1/96th of an "in" unit.
write_svg <- function(d, hex,
                      font = getOption("dotaro.font", "square"),
                      width = getOption("dee.width", SQUARE_WIDTH),
                      height = getOption("dee.height", SQUARE_HEIGHT)) {
    file <- glyph_file(font, hex)
    attrs <- svg_attrs_pres(fill_rule = "evenodd")
    s <- SVG(width = width, height = height, viewbox = TRUE)
    for (i in seq_along(d)) {
        s <- svg_path(s, d[[i]], fill = "black", stroke = "none", stroke_width = 0, attrs = attrs)
    }
    writeLines(as.character(s), file)
}

as_hex <- function(x) as.hexmode(x) |> as.character() |> tolower()
as_int <- function(x) as.hexmode(x) |> as.integer()

# Some codepoint integers to jump to in Font Forge with <CTRL>-<SHIFT>-E
# Box Drawing            9472
# Geometric Shapes       9632
# PUA                   62895

glyph_options <- function(width, height, font) {
    stopifnot(is.numeric(width), is.numeric(height))
    attrs <- svg_attrs_pres(fill_rule = "evenodd")
    l <- dee_options(dee.attrs = attrs,
                     dee.background_color = "grey90",
                     dee.digits = 0,
                     dee.fill = "black",
                     dee.height = height,
                     dee.origin_at_bottom = TRUE,
                     dee.stroke = "none",
                     dee.stroke_width = 0,
                     dee.width = width)
    l[["dotaro.font"]] <- font
    l
}

plot_glyph <- function(x, font = getOption("dotaro.font", "square")) {
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

OW <- 60L # Outline Stroke Width
CH <- 1600 # Cap Height
SW <- 180 # Letter Stroke Width

BDL <- OW # Box Drawing Light
BDH <- 4 * BDL # Box Drawing Heavy

NHGM <- 0.25 # Narrow Horizontal Gap Multiplier

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
