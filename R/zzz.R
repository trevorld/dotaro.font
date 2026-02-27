#' @importFrom affiner as_coord2d degrees isotoxal_2ngon_inner_radius
#' @import dee
#' @importFrom glue glue
#' @importFrom grid fillStrokeGrob gList gTree gpar grid.draw popViewport pushViewport rectGrob roundrectGrob textGrob unit viewport
#' @importFrom omsvg SVG svg_attrs_pres svg_path
#' @importFrom rlang abort check_dots_empty local_options warn
#' @importFrom utils hasName packageVersion
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
write_svg <- function(
	d,
	hex,
	font = getOption("dotaro.font", "square"),
	width = getOption("dee.width", SQUARE_WIDTH),
	height = getOption("dee.height", SQUARE_HEIGHT)
) {
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

# Some codepoint integers to jump to in Font Forge with <CTRL>-<SHIFT>->
# Box Drawing            9472
# Geometric Shapes       9632
# PUA                   62895

glyph_options <- function(width, height, font) {
	stopifnot(is.numeric(width), is.numeric(height))
	attrs <- svg_attrs_pres(fill_rule = "evenodd")
	l <- dee_options(
		dee.attrs = attrs,
		dee.background_color = "grey90",
		dee.digits = 0,
		dee.fill = "black",
		dee.height = height,
		dee.origin_at_bottom = TRUE,
		dee.stroke = "none",
		dee.stroke_width = 0,
		dee.width = width
	)
	l[["dotaro.font"]] <- font
	l
}

# paths try outer clockwise, inner counter-clockwise?

SQUARE_HEIGHT <- 2048L
SQUARE_WIDTH <- 2048L

NARROW_HEIGHT <- 2048L
NARROW_WIDTH <- 1024L

dotaro_height <- function(font = c("square", "narrow")) {
	font <- match.arg(font)
	switch(font, square = SQUARE_HEIGHT, narrow = NARROW_HEIGHT)
}

dotaro_width <- function(font = c("square", "narrow")) {
	font <- match.arg(font)
	switch(font, square = SQUARE_WIDTH, narrow = NARROW_WIDTH)
}

dotaro_cap_width <- function(font = c("square", "narrow")) {
	font <- match.arg(font)
	(25 / 32) * dotaro_width(font)
}

dotaro_cap_height <- function(font = c("square", "narrow")) {
	font <- match.arg(font)
	switch(font, square = dotaro_cap_width(font), narrow = (3 / 2) * dotaro_cap_width(font))
}

dotaro_x_height <- function(font = c("square", "narrow")) {
	font <- match.arg(font)
	cap_width <- dotaro_cap_width(font)
	switch(font, square = 0.7 * cap_width, narrow = cap_width)
}

dotaro_vertical_gap <- function(font = c("square", "narrow")) {
	font <- match.arg(font)
	(dotaro_height(font) - dotaro_cap_height(font)) / 2
}

dotaro_horizontal_gap <- function(font = c("square", "narrow")) {
	font <- match.arg(font)
	(dotaro_width(font) - dotaro_cap_width(font)) / 2
}

dotaro_stroke_width <- function(font = c("square", "narrow")) {
	font <- match.arg(font)
	270L * dotaro_width(font) / SQUARE_WIDTH
}

dotaro_stroke_width_short <- function(font = c("square", "narrow")) {
	font <- match.arg(font)
	0.5 * dotaro_stroke_width(font)
}

dotaro_terminal_radius <- function(font = c("square", "narrow")) {
	font <- match.arg(font)
	(5 / 3) * dotaro_stroke_width(font)
}

dotaro_outline_stroke_width <- function(font = c("square", "narrow")) {
	font <- match.arg(font)
	60L * dotaro_width(font) / SQUARE_WIDTH
}

box_drawing_light <- function(font = c("square", "narrow")) {
	font <- match.arg(font)
	dotaro_outline_stroke_width(font)
}

box_drawing_heavy <- function(font = c("square", "narrow")) {
	font <- match.arg(font)
	4L * dotaro_outline_stroke_width(font)
}

box_drawing_radius <- function(font = c("square", "narrow")) {
	font <- match.arg(font)
	300L
}

d_rect2 <- function(yt, xr, yb, xl, ...) {
	x <- c(xl, xl, xr, xr)
	y <- c(yb, yt, yt, yb)
	d_polygon(x, y, ...)
}

# Constants from {dotaro.deck}
BLEED <- 1 / 8
CARD_HEIGHT <- 3.5
CARD_WIDTH <- 2.25 # Bridge Card
# INDEX_HEIGHT <- 1.25
INDEX_HEIGHT <- 0.75
INDEX_WIDTH <- 0.25
