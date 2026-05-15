# `plot_glyph()` plots a single character in both "ranks" and "suits" fonts
# with {nanosvgr} based on the **svg** files
# In contrast `indexGrob()` plots glyphs in {grid} based on the **fonts**
plot_glyph <- function(x) {
	stopifnot(requireNamespace("nanosvgr", quietly = TRUE))
	if (inherits(x, "dee")) {
		plot(x)
	} else if (is.character(x)) {
		if (nchar(x) == 1L) {
			stopifnot(requireNamespace("Unicode", quietly = TRUE))
			x <- as_hex(as.integer(Unicode::as.u_char(utf8ToInt(x))))
		}
		mult <- dotaro_width("suits") / dotaro_width("ranks")
		denom <- mult * dotaro_height("ranks") + dotaro_height("suits")
		h_narrow <- grid::unit(mult * dotaro_height("ranks") / denom, "snpc")
		w_narrow <- grid::unit(mult * dotaro_width("ranks") / denom, "snpc")
		h_square <- grid::unit(dotaro_height("suits") / denom, "snpc")
		w_square <- grid::unit(dotaro_width("suits") / denom, "snpc")

		f_square <- glyph_file("suits", x)
		f_narrow <- glyph_file("ranks", x)

		grid::grid.newpage()

		if (file.exists(f_square)) {
			grid::pushViewport(grid::viewport(
				y = h_square * 0.5,
				width = w_square,
				height = h_square
			))
			grid::grid.rect(gp = gpar(col = NA, fill = "cyan"))
			nsvg_grob_npc(f_square, dotaro_width("suits"), dotaro_height("suits")) |>
				grid::grid.draw()
			grid::popViewport()
		}

		if (file.exists(f_narrow)) {
			grid::pushViewport(grid::viewport(
				y = h_square + h_narrow * 0.5,
				width = w_narrow,
				height = h_narrow
			))
			grid::grid.rect(gp = gpar(col = NA, fill = "magenta"))
			nsvg_grob_npc(f_narrow, dotaro_width("ranks"), dotaro_height("ranks")) |>
				grid::grid.draw()
			grid::popViewport()
		}
	} else {
		stop("Don't know how to plot this object")
	}
}

nsvg_grob_npc <- function(f, w, h) {
	# nsvg_to_grob(inverty=TRUE) inverts around max(y_svg) — the glyph's bounding
	# box bottom — not the SVG canvas height, losing the bottom margin. Correct by
	# adding back (h - max_y_svg) so the full canvas proportions are preserved.
	nsvg <- nanosvgr::nsvg_read(f)
	max_y <- max(do.call(rbind, nsvg$points)$y)
	grob <- nanosvgr::nsvg_to_grob(nsvg)
	for (i in seq_along(grob$children)) {
		child <- grob$children[[i]]
		child$x <- grid::unit(as.numeric(child$x) / w, "npc")
		child$y <- grid::unit((as.numeric(child$y) + h - max_y) / h, "npc")
		grob$children[[i]] <- child
	}
	grob
}
