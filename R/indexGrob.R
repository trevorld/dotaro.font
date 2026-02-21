#' Draw playing card indices using Dotaro Font glyphs
#'
#' @param df A data frame with the following columns:
#'
#'           * **card** Integer of card index
#'           * **type** Either `"rank"` or `"suit"`
#'           * **glyph** Character string of glyph to draw
#'           * **col** Boundary color of glyph
#'           * **fill** Fill color of glyph
#'
#' @param ... Should be empty
#' @param bleed Bleed zone fill color
#' @return A grid grob
#' @noRd
indexGrob <- function(df, ..., bleed = "white") {
	check_dots_empty()
	n_cards <- max(df$card)
	corner_width <- INDEX_WIDTH + 2 * BLEED
	corner_height <- INDEX_HEIGHT + 2 * BLEED
	lwd_card <- 2.0
	lwd_glyph <- 2.0

	gl_cards <- gList()
	for (i in seq_len(n_cards)) {
		x_offset <- (i - 0.5 - n_cards / 2) * corner_width
		x <- unit(0.5, "npc") + unit(x_offset, "in")
		gl_card <- gList()
		#### Look up the `roundrectGrob()` `r` value we use in `{piecepackr}`
		gl_card[[1L]] <- roundrectGrob(
			x = x - unit(corner_width / 2, "in"),
			y = unit(0.5, "npc") + unit(corner_height / 2, "in"),
			width = unit(CARD_WIDTH, "in"),
			height = unit(CARD_HEIGHT, "in"),
			just = c("left", "top"),
			gp = gpar(col = "black", fill = bleed, lwd = lwd_card),
			name = paste0("border.", i)
		)
		gl_card[[2L]] <- rectGrob(
			x = x,
			width = unit(INDEX_WIDTH, "in"),
			height = unit(INDEX_HEIGHT, "in"),
			gp = gpar(col = NA, fill = "white"),
			name = paste0("index.", i)
		)
		df_rank <- df[df$card == i & df$type == "rank", ]
		y_rank <- unit(0.5, "npc") + unit(0.5 * corner_height - BLEED - 0.12, "in")
		for (j in seq_len(nrow(df_rank))) {
			tg <- textGrob(
				label = df_rank$glyph[j],
				x = x,
				y = y_rank,
				gp = gpar(fontsize = 42, fontfamily = "Dotaro Narrow")
			)
			gl_card[[2L + j]] <- fillStrokeGrob(
				tg,
				gp = gpar(col = df_rank$col[j], fill = df_rank$fill[j], lwd = lwd_glyph),
				name = paste0("rank.", j)
			)
		}

		df_suit <- df[df$card == i & df$type == "suit", ]
		y_suit <- unit(0.5, "npc") +
			unit(0.5 * corner_height - BLEED - 0.5 - 0.08, "in")
		n_rank <- nrow(df_rank)
		for (j in seq_len(nrow(df_suit))) {
			tg <- textGrob(
				label = df_suit$glyph[j],
				x = x,
				y = y_suit,
				gp = gpar(fontsize = 22, fontfamily = "Dotaro Square")
			)
			gl_card[[2L + n_rank + j]] <- fillStrokeGrob(
				tg,
				gp = gpar(col = df_suit$col[j], fill = df_suit$fill[j], lwd = lwd_glyph),
				name = paste0("suit.", j)
			)
		}

		gl_cards[[i]] <- gTree(
			children = gl_card,
			name = paste0("card.", i)
		)
	}
	vp <- viewport(
		width = unit(n_cards * corner_width + 0.125, "in"),
		height = unit(corner_height + 0.125, "in"),
		x = unit(0.1, "in"),
		just = "left",
		mask = rectGrob(gp = gpar(col = NA, fill = "white"))
	)
	gTree(children = gl_cards, cl = "dotaro_index", vp = vp)
}

grid.index <- function(...) {
	grid.draw(indexGrob(...))
}
