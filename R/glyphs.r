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
	`265d` = "2657", # Chess Bishop
	`265f` = "2659", # Chess Pawn
	`2660` = "2664", # Spade Suit
	`2665` = "2661", # Heart Suit
	`2663` = "2667" # Club Suit
)

TURNED_FROM_TO <- list(
	`265a` = "1fa24", # Black Chess King
	`265d` = "1fa27", # Black Chess Bishop
	`265e` = "1fa28", # Black Chess Knight
	`265f` = "1fa29", # Black Chess Pawn
	`2654` = "1fa1e", # White Chess King
	`2657` = "1fa21", # White Chess Bishop
	`2658` = "1fa22", # White Chess Knight
	`2659` = "1fa23" # White Chess Pawn
)

LEFT_FROM_TO <- list(
	`2654` = "1fa33", # White Chess King
	`2655` = "1fa34", # White Chess Queen
	`2656` = "1fa35", # White Chess Rook
	`2657` = "1fa36", # White Chess Bishop
	`2658` = "1fa37", # White Chess Knight
	`2659` = "1fa38", # White Chess Pawn
	`265a` = "1fa39", # Black Chess King
	`265b` = "1fa3a", # Black Chess Queen
	`265c` = "1fa3b", # Black Chess Rook
	`265d` = "1fa3c", # Black Chess Bishop
	`265e` = "1fa3d", # Black Chess Knight
	`265f` = "1fa3e" # Black Chess Pawn
)

RIGHT_FROM_TO <- list(
	`2654` = "1fa09", # White Chess King
	`2655` = "1fa0a", # White Chess Queen
	`2656` = "1fa0b", # White Chess Rook
	`2657` = "1fa0c", # White Chess Bishop
	`2658` = "1fa0d", # White Chess Knight
	`2659` = "1fa0e", # White Chess Pawn
	`265a` = "1fa0f", # Black Chess King
	`265b` = "1fa10", # Black Chess Queen
	`265c` = "1fa11", # Black Chess Rook
	`265d` = "1fa12", # Black Chess Bishop
	`265e` = "1fa13", # Black Chess Knight
	`265f` = "1fa14" # Black Chess Pawn
)
