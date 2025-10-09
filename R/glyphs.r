create_glyphs <- function(font = "square") {
	latin <- create_basic_latin(font)
	block <- create_block_elements(font)
	box <- create_box_drawing(font)
	shapes <- create_geometric_shapes(font)
	misc_symbols <- create_miscellaneous_symbols(font)
	c(box, block, latin, shapes, misc_symbols) |> sort()
}

# Playing Card Suit has similar location as the Catrinity Font but different names
GLYPH_NAMES <- list(
	`f590` = "NUMBER TEN",
	`f5b0` = "GERMAN ACORN SUIT",
	`f5b1` = "GERMAN LEAF SUIT",
	`f5b2` = "GERMAN HEART SUIT",
	`f5b3` = "GERMAN BELL SUIT",
	`f5b4` = "SWISS SHIELD SUIT",
	`f5b5` = "SWISS ROSE SUIT",
	`f5b6` = "LATIN SWORD SUIT",
	`f5b7` = "LATIN CUP SUIT",
	`f5b8` = "LATIN COIN SUIT",
	`f5b9` = "LATIN CLUB SUIT"
)

# Copy glyph from and use just the outline
# https://fontforge.org/docs/scripting/python/fontforge.html#fontforge.glyph.stroke
OUTLINE_FROM_TO <- list(
	`265a` = "2654", # Chess King
	`2660` = "2664", # Spade Suit
	`2665` = "2661", # Heart Suit
	`2663` = "2667" # Club Suit
)
