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
	`f5b9` = "LATIN CLUB SUIT",
	`fc431` = "BLACK MEEPLE",
	`fc432` = "WHITE MEEPLE"
)

# Copy glyph from and use just the outline
# https://fontforge.org/docs/scripting/python/fontforge.html#fontforge.glyph.stroke
OUTLINE_FROM_TO <- list(
	`265a` = "2654", # Chess King
	`265c` = "2656", # Chess Rook
	`265d` = "2657", # Chess Bishop
	`265f` = "2659", # Chess Pawn
	`2660` = "2664", # Spade Suit
	`2665` = "2661", # Heart Suit
	`2663` = "2667", # Club Suit
	`1d7ce` = "1d7d8", # Bold Digit Zero -> Double-Struck Digit Zero
	`1d7cf` = "1d7d9", # Bold Digit One -> Double-Struck Digit One
	# `1d7d0` = "1d7da", # Bold Digit Two -> Double-Struck Digit Two
	# `1d7d1` = "1d7db", # Bold Digit Three -> Double-Struck Digit Three
	`1d7d2` = "1d7dc", # Bold Digit Four -> Double-Struck Digit Four
	`1d7d3` = "1d7dd", # Bold Digit Five -> Double-Struck Digit Five
	# `1d7d4` = "1d7de", # Bold Digit Six -> Double-Struck Digit Six
	`1d7d5` = "1d7df", # Bold Digit Seven -> Double-Struck Digit Seven
	`1d7d6` = "1d7e0", # Bold Digit Eight -> Double-Struck Digit Eight
	# `1d7d7` = "1d7e1", # Bold Digit Nine -> Double-Struck Digit Nine
	`0030` = "1ccf0", # Digit Zero -> Outline Digit Zero
	`0031` = "1ccf1", # Digit One -> Outline Digit One
	`0032` = "1ccf2", # Digit Two -> Outline Digit Two
	`0033` = "1ccf3", # Digit Three -> Outline Digit Three
	`0034` = "1ccf4", # Digit Four -> Outline Digit Four
	`0035` = "1ccf5", # Digit Five -> Outline Digit Five
	`0036` = "1ccf6", # Digit Six -> Outline Digit Six
	`0037` = "1ccf7", # Digit Seven -> Outline Digit Seven
	`0038` = "1ccf8", # Digit Eight -> Outline Digit Eight
	`0039` = "1ccf9", # Digit Nine -> Outline Digit Nine
	`0041` = "1ccd6", # Latin Capital Letter A -> Outlined Latin Capital Letter A
	`0042` = "1ccd7", # Latin Capital Letter B -> Outlined Latin Capital Letter B
	`0043` = "1ccd8", # Latin Capital Letter C -> Outlined Latin Capital Letter C
	`0044` = "1ccd9", # Latin Capital Letter D -> Outlined Latin Capital Letter D
	`0045` = "1ccda", # Latin Capital Letter E -> Outlined Latin Capital Letter E
	`0046` = "1ccdb", # Latin Capital Letter F -> Outlined Latin Capital Letter F
	`0047` = "1ccdc", # Latin Capital Letter G -> Outlined Latin Capital Letter G
	`0048` = "1ccdd", # Latin Capital Letter H -> Outlined Latin Capital Letter H
	`0049` = "1ccde", # Latin Capital Letter I -> Outlined Latin Capital Letter I
	`004a` = "1ccdf", # Latin Capital Letter J -> Outlined Latin Capital Letter J
	`004b` = "1cce0", # Latin Capital Letter K -> Outlined Latin Capital Letter K
	`004c` = "1cce1", # Latin Capital Letter L -> Outlined Latin Capital Letter L
	`004d` = "1cce2", # Latin Capital Letter M -> Outlined Latin Capital Letter M
	`004e` = "1cce3", # Latin Capital Letter N -> Outlined Latin Capital Letter N
	`004f` = "1cce4", # Latin Capital Letter O -> Outlined Latin Capital Letter O
	`0050` = "1cce5", # Latin Capital Letter P -> Outlined Latin Capital Letter P
	`0051` = "1cce6", # Latin Capital Letter Q -> Outlined Latin Capital Letter Q
	`0052` = "1cce7", # Latin Capital Letter R -> Outlined Latin Capital Letter R
	`0053` = "1cce8", # Latin Capital Letter S -> Outlined Latin Capital Letter S
	`0054` = "1cce9", # Latin Capital Letter T -> Outlined Latin Capital Letter T
	`0055` = "1ccea", # Latin Capital Letter U -> Outlined Latin Capital Letter U
	`0056` = "1cceb", # Latin Capital Letter V -> Outlined Latin Capital Letter V
	`0057` = "1ccec", # Latin Capital Letter W -> Outlined Latin Capital Letter W
	`0058` = "1cced", # Latin Capital Letter X -> Outlined Latin Capital Letter X
	`0059` = "1ccee", # Latin Capital Letter Y -> Outlined Latin Capital Letter Y
	`005a` = "1ccef" # Latin Capital Letter Z -> Outlined Latin Capital Letter Z
)

TURNED_FROM_TO <- list(
	`265a` = "1fa24", # Black Chess King
	`265c` = "1fa26", # Black Chess Rook
	`265d` = "1fa27", # Black Chess Bishop
	`265e` = "1fa28", # Black Chess Knight
	`265f` = "1fa29", # Black Chess Pawn
	`2654` = "1fa1e", # White Chess King
	`2656` = "1fa20", # White Chess Rook
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
