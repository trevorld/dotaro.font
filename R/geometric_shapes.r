create_geometric_shapes <- function(font = "square") {
    if (font != "square") {
        return(character(0L))
    }

    glyph_height <- dotaro_height(font)
    glyph_width <- dotaro_width(font)
    do.call(local_options, glyph_dee_options(glyph_width, glyph_height))

    # 25a0 black square
    xl <- 0
    xr <- glyph_width
    yb <- 0
    yt <- glyph_height
    p <- as_coord2d(x = c(xl, xl, xr, xr),
                    y = c(yb, yt, yt, yb))
    d <- MZ(p)
    svg_square(d, glyph_file(font, "25a0"))

    # 25c6 black diamond
    xl <- 0
    xc <- 0.5 * glyph_width |> round()
    xr <- glyph_width
    yb <- 0
    yc <- 0.5 * glyph_height |> round()
    yt <- glyph_height
    p <- as_coord2d(x = c(xl, xc, xr, xc),
                    y = c(yc, yt, yc, yb))
    d <- MZ(p)
    svg_square(d, glyph_file(font, "25c6"))

    # 25cf black circle
    d <- M(0.5 * glyph_width, glyph_height) +
        A(0.5 * glyph_width, 0.5 * glyph_width, 0, 0, 0, 0.5 * glyph_width, 0) +
        AZ(0.5 * glyph_width, 0.5 * glyph_width, 0, 0, 0, 0.5 * glyph_width, glyph_height)
    svg_square(d, glyph_file(font, "25cf"))

    as_hex(c("25a0", "25c6", "25cf"))
}
