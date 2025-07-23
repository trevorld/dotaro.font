create_glyphs <- function(font = "square") {
    block <- create_block_elements(font)
    box <- create_box_drawing(font)
    shapes <- create_geometric_shapes(font)
    misc_symbols <- create_miscellaneous_symbols(font)
    c(box, block, shapes, misc_symbols) |> sort()
}

# Playing Card Suit has similar location as the Catrinity Font but different names
GLYPH_NAMES <- list(
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
    `25c6` = "25c7", # Diamond (Shape)
    `2660` = "2664", # Spade Suit
    `2665` = "2661", # Heart Suit
    `2666` = "2662", # Diamond Suit
    `2663` = "2667", # Club Suit
    `2b25` = "2b26", # Medium Diamond (Shape)
    `25e2` = "25ff", # Lower Right Triangle
    `25e3` = "25fa", # Lower Left Triangle
    `25e4` = "25f8", # Upper Left Triangle
    `25e5` = "25f9"  # Upper Right Triangle
)
