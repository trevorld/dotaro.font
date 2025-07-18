create_glyphs <- function(font = "square") {
    block <- create_block_elements(font)
    shapes <- create_geometric_shapes(font)
    c(block, shapes)
}
