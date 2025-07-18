# All but the three "shade" glyphs
create_block_elements <- function(font = "square") {
    glyph_height <- dotaro_height(font)
    glyph_width <- dotaro_width(font)
    do.call(local_options, glyph_dee_options(glyph_width, glyph_height))

    # 2580 upper half block
    xl <- 0
    xr <- glyph_width
    yb <- glyph_height / 2
    yt <- glyph_height
    p <- as_coord2d(x = c(xl, xl, xr, xr),
                    y = c(yb, yt, yt, yb))
    d <- MZ(p)
    svg_square(d, glyph_file(font, "2580"))

    # 2581--2587 lower 1/8 -- lower 7/8 block
    for (i in 1:7) {
        hex <- as.character(2580 + i)
        xl <- 0
        xr <- glyph_width
        yb <- 0
        yt <- (i / 8) * glyph_height
        p <- as_coord2d(x = c(xl, xl, xr, xr),
                        y = c(yb, yt, yt, yb))
        d <- MZ(p)
        svg_square(d, glyph_file(font, hex))
    }

    # 2588 full block
    xl <- 0
    xr <- glyph_width
    yb <- 0
    yt <- glyph_height
    p <- as_coord2d(x = c(xl, xl, xr, xr),
                    y = c(yb, yt, yt, yb))
    d <- MZ(p)
    svg_square(d, glyph_file(font, "2588"))

    # 2589 -- 258f left 7/8 -- left 1/8 block
    for (i in 1:7) {
        hex <- as.hexmode("2588") + i
        xl <- 0
        xr <- ((8 - i) / 8) * glyph_width
        yb <- 0
        yt <- glyph_height
        p <- as_coord2d(x = c(xl, xl, xr, xr),
                        y = c(yb, yt, yt, yb))
        d <- MZ(p)
        svg_square(d, glyph_file(font, hex))
    }

    # 2590 right half block
    xl <- glyph_width / 2
    xr <- glyph_width
    yb <- 0
    yt <- glyph_height
    p <- as_coord2d(x = c(xl, xl, xr, xr),
                    y = c(yb, yt, yt, yb))
    d <- MZ(p)
    svg_square(d, glyph_file(font, "2590"))

    #### 2591--2593 LIGHT SHADE--HEAVY SHADE

    # 2594 upper 1/8
    xl <- 0
    xr <- glyph_width
    yb <- (7/8) * glyph_height
    yt <- glyph_height
    p <- as_coord2d(x = c(xl, xl, xr, xr),
                    y = c(yb, yt, yt, yb))
    d <- MZ(p)
    svg_square(d, glyph_file(font, "2594"))

    # 2595 right 1/8
    xl <- (7/8) * glyph_width
    xr <- glyph_width
    yb <- 0
    yt <- glyph_height
    p <- as_coord2d(x = c(xl, xl, xr, xr),
                    y = c(yb, yt, yt, yb))
    d <- MZ(p)
    svg_square(d, glyph_file(font, "2595"))

    # 2596 quadrant lower left
    xl <- 0
    xr <- glyph_width / 2
    yb <- 0
    yt <- glyph_height / 2
    p <- as_coord2d(x = c(xl, xl, xr, xr),
                    y = c(yb, yt, yt, yb))
    dll <- MZ(p)
    svg_square(dll, glyph_file(font, "2596"))

    # 2597 quadrant lower right
    xl <- glyph_width / 2
    xr <- glyph_width
    yb <- 0
    yt <- glyph_height / 2
    p <- as_coord2d(x = c(xl, xl, xr, xr),
                    y = c(yb, yt, yt, yb))
    dlr <- MZ(p)
    svg_square(dlr, glyph_file(font, "2597"))

    # 2598 quadrant upper left
    xl <- 0
    xr <- glyph_width / 2
    yb <- glyph_height / 2
    yt <- glyph_height
    p <- as_coord2d(x = c(xl, xl, xr, xr),
                    y = c(yb, yt, yt, yb))
    dul <- MZ(p)
    svg_square(dul, glyph_file(font, "2598"))

    # 259d quadrant upper right
    xl <- glyph_width / 2
    xr <- glyph_width
    yb <- glyph_height / 2
    yt <- glyph_height
    p <- as_coord2d(x = c(xl, xl, xr, xr),
                    y = c(yb, yt, yt, yb))
    dur <- MZ(p)
    svg_square(dur, glyph_file(font, "259d"))

    # 259a quadrant upper left and lower right
    svg_square(dul + dlr, glyph_file(font, "259a"))

    # 259e quadrant upper right and lower left
    svg_square(dur + dll, glyph_file(font, "259e"))

    xl <- 0
    xc <- glyph_width / 2
    xr <- glyph_width
    yb <- 0
    yc <- glyph_height / 2
    yt <- glyph_height

    # 2599 quadrant upper left, lower left, and lower right
    p <- as_coord2d(x = c(xl, xl, xc, xc, xr, xr),
                    y = c(yb, yt, yt, yc, yc, yb))
    svg_square(MZ(p), glyph_file(font, "2599"))

    # 259b quadrant upper left, upper right, lower left
    p <- as_coord2d(x = c(xl, xl, xr, xr, xc, xc),
                    y = c(yb, yt, yt, yc, yc, yb))
    svg_square(MZ(p), glyph_file(font, "259b"))

    # 259c quadrant upper left, upper right, lower right
    p <- as_coord2d(x = c(xl, xl, xr, xr, xc, xc),
                    y = c(yc, yt, yt, yb, yb, yc))
    svg_square(MZ(p), glyph_file(font, "259c"))

    # 259f quadrant upper right, lower left, lower right
    p <- as_coord2d(x = c(xl, xl, xc, xc, xr, xr),
                    y = c(yb, yc, yc, yt, yt, yb))
    svg_square(MZ(p), glyph_file(font, "259f"))

    as_hex(c(as.hexmode("2580"):as.hexmode("2590"),
             as.hexmode("2594"):as.hexmode("259f")))
}
