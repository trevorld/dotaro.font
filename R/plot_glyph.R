# `plot_glyph()` plots a single character in both "narrow" and "square" fonts
# with {svgparser} based on the **svg** files
# In contrast `indexGrob()` plots glyphs in {grid} based on the **fonts**
plot_glyph <- function(x) {
	stopifnot(requireNamespace("svgparser", quietly = TRUE))
	if (inherits(x, "dee")) {
		plot(x)
	} else if (is.character(x)) {
		if (nchar(x) == 1L) {
			stopifnot(requireNamespace("Unicode", quietly = TRUE))
			x <- as_hex(as.integer(Unicode::as.u_char(utf8ToInt(x))))
		}
		mult <- dotaro_width("square") / dotaro_width("narrow")
		denom <- mult * dotaro_height("narrow") + dotaro_height("square")
		h_narrow <- grid::unit(mult * dotaro_height("narrow") / denom, "snpc")
		w_narrow <- grid::unit(mult * dotaro_width("narrow") / denom, "snpc")
		h_square <- grid::unit(dotaro_height("square") / denom, "snpc")
		w_square <- grid::unit(dotaro_width("square") / denom, "snpc")

		f_square <- glyph_file("square", x)
		f_narrow <- glyph_file("narrow", x)

		grid::grid.newpage()

		if (file.exists(f_square)) {
			grid::pushViewport(grid::viewport(
				y = h_square * 0.5,
				width = w_square,
				height = h_square
			))
			grid::grid.rect(gp = gpar(col = NA, fill = "cyan"))
			svgparser::read_svg(f_square) |> grid::grid.draw()
			grid::popViewport()
		}

		if (file.exists(f_narrow)) {
			grid::pushViewport(grid::viewport(
				y = h_square + h_narrow * 0.5,
				width = w_narrow,
				height = h_narrow
			))
			grid::grid.rect(gp = gpar(col = NA, fill = "magenta"))
			grid::popViewport()

			# Hack for https://github.com/coolbutuseless/svgparser/issues/10
			grid::pushViewport(grid::viewport(
				x = grid::unit(1, "npc") - 0.5 * w_narrow,
				y = h_square + h_narrow * 0.5,
				height = h_narrow,
				just = "center"
			))
			svgparser::read_svg(f_narrow) |> grid::grid.draw()
			grid::popViewport()
		}
	} else {
		stop("Don't know how to plot this object")
	}
}
