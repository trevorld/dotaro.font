LWD_GLYPH <- 2.0

#' Create grobs for rank glyphs at a single position
#'
#' @param glyph Character vector of glyphs to draw
#' @param x,y Position (unit objects)
#' @param col,fill Boundary and fill colors (recycled to length of `glyph`)
#' @param center Whether to add a center indicator circle
#' @return A grid gTree
#' @noRd
rankGrob <- function(
	glyph,
	x = unit(0.5, "npc"),
	y = unit(0.5, "npc"),
	...,
	col = "black",
	fill = "black",
	center = FALSE,
	name = NULL
) {
	check_dots_empty()
	n <- max(lengths(list(glyph, x, y, col, fill)))
	glyph <- rep_len(glyph, n)
	x <- rep_len(x, n)
	y <- rep_len(y, n)
	col <- rep_len(col, n)
	fill <- rep_len(fill, n)
	# cap height = 3/8" = 27pt
	# em height = 2048px, cap height = 1200px => 46pt = 27pt / (1200px/2048px)
	fontsize <- 27 / (dotaro_cap_height("ranks") / dotaro_height("ranks"))
	gl <- gList()
	for (j in seq_len(n)) {
		tg <- textGrob(
			label = glyph[j],
			x = x[j],
			y = y[j],
			gp = gpar(fontsize = fontsize, fontfamily = "Dotaro Ranks")
		)
		gl[[j]] <- fillStrokeGrob(
			tg,
			name = paste0("rank_glyph.", j),
			gp = gpar(col = col[j], fill = fill[j], lwd = LWD_GLYPH)
		)
	}
	if (isTRUE(center)) {
		gl[[n + 1L]] <- circleGrob(
			x = x,
			y = y,
			r = 0.04,
			name = "rank_center",
			gp = gpar(col = "black", fill = "yellow")
		)
	}
	gTree(children = gl, name = name)
}

#' Create grobs for suit glyphs at a single position
#'
#' @param glyph Character vector of glyphs to draw
#' @param x,y Position (unit objects)
#' @param col,fill Boundary and fill colors (recycled to length of `glyph`)
#' @param center Whether to add a center indicator circle
#' @return A grid gTree
#' @noRd
suitGrob <- function(
	glyph,
	x = unit(0.5, "npc"),
	y = unit(0.5, "npc"),
	...,
	col = "black",
	fill = "black",
	center = FALSE,
	name = NULL
) {
	check_dots_empty()
	n <- max(lengths(list(glyph, x, y, col, fill)))
	glyph <- rep_len(glyph, n)
	x <- rep_len(x, n)
	y <- rep_len(y, n)
	col <- rep_len(col, n)
	fill <- rep_len(fill, n)
	# cap height = 1/4" = 18pt
	# em height = 2048px, cap height = 1600px => 23pt = 18pt / (1600px/2048px)
	fontsize <- 18 / (dotaro_cap_height("suits") / dotaro_height("suits"))
	gl <- gList()
	for (j in seq_len(n)) {
		tg <- textGrob(
			label = glyph[j],
			x = x[j],
			y = y[j],
			gp = gpar(fontsize = fontsize, fontfamily = "Dotaro Suits")
		)
		gl[[j]] <- fillStrokeGrob(
			tg,
			name = paste0("suit_glyph.", j),
			gp = gpar(col = col[j], fill = fill[j], lwd = LWD_GLYPH)
		)
	}
	if (isTRUE(center)) {
		gl[[n + 1L]] <- circleGrob(
			x = x,
			y = y,
			r = 0.04,
			name = "suit_center",
			gp = gpar(col = "black", fill = "yellow")
		)
	}
	gTree(children = gl, name = name)
}

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
#' @param center Should we add circles at the glyph "centers"
#' @return A grid grob
#' @noRd
indexGrob <- function(df, ..., bleed = "white", center = FALSE) {
	check_dots_empty()
	n_cards <- max(df$card)
	corner_width <- INDEX_WIDTH + 2 * BLEED
	corner_height <- INDEX_HEIGHT + 2 * BLEED
	lwd_card <- 2.0

	gl_cards <- gList()
	for (i in seq_len(n_cards)) {
		x_offset <- (i - 0.5 - n_cards / 2) * corner_width
		x <- unit(0.5, "npc") + unit(x_offset, "in")
		gl_card <- gList()
		vp_card <- viewport(
			x = x - unit(corner_width / 2, "in"),
			y = unit(0.5, "npc") + unit(corner_height / 2, "in"),
			width = unit(CARD_WIDTH, "in"),
			height = unit(CARD_HEIGHT, "in"),
			just = c("left", "top")
		)
		gl_card[[1L]] <- roundrectGrob(
			r = unit(0.06, "snpc"), # default for a card in {piecepackr}
			name = "border_and_bleed",
			gp = gpar(col = "black", fill = bleed, lwd = lwd_card),
			vp = vp_card
		)
		gl_card[[2L]] <- rectGrob(
			x = x,
			width = unit(INDEX_WIDTH, "in"),
			height = unit(INDEX_HEIGHT, "in"),
			name = "index_background",
			gp = gpar(col = NA, fill = "white")
		)
		df_rank <- df[df$card == i & df$type == "rank", ]
		y_rank <- unit(0.5, "npc") + unit(0.5 * corner_height - BLEED - 0.5 * 3 / 8, "in")
		gl_card[[3L]] <- rankGrob(
			glyph = df_rank$glyph,
			x = x,
			y = y_rank,
			col = df_rank$col,
			fill = df_rank$fill,
			center = center,
			name = paste0("rank.", "i")
		)

		df_suit <- df[df$card == i & df$type == "suit", ]
		y_suit <- unit(0.5, "npc") +
			unit(0.5 * corner_height - BLEED - 3 / 8 - 0.10 - 0.5 * 0.25, "in")
		gl_card[[4L]] <- suitGrob(
			glyph = df_suit$glyph,
			x = x,
			y = y_suit,
			col = df_suit$col,
			fill = df_suit$fill,
			center = center,
			name = paste0("suit.", i)
		)

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

# Helper function for quick rank tests where
# we simply cycle through the French suits or circled digits
df_index <- function(ranks, suits = "french", fill = NULL) {
	n_cards <- length(ranks)
	if (suits == "french") {
		french_suits <- c("\u2660", "\u2665", "\u2666", "\u2663") # ♠ ♥ ♦ ♣
		if (is.null(fill)) {
			suit_fills <- c("#009E73", "#D55E00", "#D55E00", "#009E73")
		} else {
			suit_fills <- rep_len(fill, 4L)
		}
		suit_idx <- ((seq_len(n_cards) - 1L) %% 4L) + 1L
		df_ranks <- data.frame(
			card = seq_len(n_cards),
			type = "rank",
			glyph = ranks,
			col = "black",
			fill = suit_fills[suit_idx]
		)
		df_suits <- data.frame(
			card = seq_len(n_cards),
			type = "suit",
			glyph = french_suits[suit_idx],
			col = "black",
			fill = suit_fills[suit_idx]
		)
		rbind(df_ranks, df_suits)
	} else if (suits == "circles") {
		circled <- c("\u24ea", "\u2460", "\u2461", "\u2462", "\u2463") # ⓪①②③④
		neg_circled <- c("\u24ff", "\u2776", "\u2777", "\u2778", "\u2779") # ⓿❶❷❸❹
		if (is.null(fill)) {
			oi_fills <- c("#F0E442", "#0072B2") # Okabe-Ito yellow, dark blue
		} else {
			oi_fills <- rep_len(fill, 2L)
		}
		suit_idx <- ((seq_len(n_cards) - 1L) %% 5L) + 1L
		fill_idx <- ((seq_len(n_cards) - 1L) %% 2L) + 1L
		df_ranks <- data.frame(
			card = seq_len(n_cards),
			type = "rank",
			glyph = ranks,
			col = "black",
			fill = oi_fills[fill_idx]
		)
		df_suits_base <- data.frame(
			card = seq_len(n_cards),
			type = "suit",
			glyph = circled[suit_idx],
			col = "black",
			fill = "black"
		)
		df_suits_neg <- data.frame(
			card = seq_len(n_cards),
			type = "suit",
			glyph = neg_circled[suit_idx],
			col = "black",
			fill = oi_fills[fill_idx]
		)
		rbind(df_ranks, df_suits_base, df_suits_neg)
	} else if (suits == "numbers") {
		number_shapes <- c("⓪", "", "", "", "") # ⓪, droplet+1, arch+2, shield+3, square+4
		neg_number_shapes <- c("⓿", "", "", "", "") # ⓿, negatives
		if (is.null(fill)) {
			oi_fills <- c("#F0E442", "#0072B2")
		} else {
			oi_fills <- rep_len(fill, 2L)
		}
		suit_idx <- ((seq_len(n_cards) - 1L) %% 5L) + 1L
		fill_idx <- ((seq_len(n_cards) - 1L) %% 2L) + 1L
		df_ranks <- data.frame(
			card = seq_len(n_cards),
			type = "rank",
			glyph = ranks,
			col = "black",
			fill = oi_fills[fill_idx]
		)
		df_suits_base <- data.frame(
			card = seq_len(n_cards),
			type = "suit",
			glyph = number_shapes[suit_idx],
			col = "black",
			fill = "black"
		)
		df_suits_neg <- data.frame(
			card = seq_len(n_cards),
			type = "suit",
			glyph = neg_number_shapes[suit_idx],
			col = "black",
			fill = oi_fills[fill_idx]
		)
		rbind(df_ranks, df_suits_base, df_suits_neg)
	} else {
		abort(glue('unknown suits value "{suits}"'))
	}
}
