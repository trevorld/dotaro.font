# All but the three "shade" glyphs
create_block_elements <- function(font = "square") {
    h <- dotaro_height(font) # glyph height
    w <- dotaro_width(font) # glyph width
    do.call(local_options, glyph_dee_options(w, h))

    xc <- w / 2
    yc <- h / 2

    # 2580 upper half block
    d <- d_rect(xl = 0, xr = w, yb = yc, yt = h)
    svg_square(d, glyph_file(font, "2580"))

    # 2581--2587 lower 1/8 -- lower 7/8 block
    for (i in 1:7) {
        hex <- as.character(2580 + i)
        d <- d_rect(xl = 0, xr = w, yb = 0, yt = (i / 8) * h)
        svg_square(d, glyph_file(font, hex))
    }

    # 2588 full block
    d <- d_rect(xl = 0, xr = w, yb = 0, yt = h)
    svg_square(d, glyph_file(font, "2588"))

    # 2589 -- 258f left 7/8 -- left 1/8 block
    for (i in 1:7) {
        hex <- as.hexmode("2588") + i
        d <- d_rect(xl = 0, xr = ((8 - i) / 8) * w, yb = 0, yt = h)
        svg_square(d, glyph_file(font, hex))
    }

    # 2590 right half block
    d <- d_rect(xl = xc, xr = w, yb = 0, yt = h)
    svg_square(d, glyph_file(font, "2590"))

    #### 2591--2593 LIGHT SHADE--HEAVY SHADE

    # 2594 upper 1/8
    d <- d_rect(xl = 0, xr = w, yb = (7/8) * h, yt = h)
    svg_square(d, glyph_file(font, "2594"))

    # 2595 right 1/8
    d <- d_rect(xl = (7/8) * w, xr = w, yb = 0, yt = h)
    svg_square(d, glyph_file(font, "2595"))

    # 2596 quadrant lower left
    dll <- d_rect(xl = 0, xr = xc, yb = 0, yt = yc)
    svg_square(dll, glyph_file(font, "2596"))

    # 2597 quadrant lower right
    dlr <- d_rect(xl = xc, xr = w, yb = 0, yt = yc)
    svg_square(dlr, glyph_file(font, "2597"))

    # 2598 quadrant upper left
    dul <- d_rect(xl = 0, xr = xc, yb = yc, yt = h)
    svg_square(dul, glyph_file(font, "2598"))

    # 259d quadrant upper right
    dur <- d_rect(xl = xc, xr = w, yb = yc, yt = h)
    svg_square(dur, glyph_file(font, "259d"))

    # 259a quadrant upper left and lower right
    svg_square(dul + dlr, glyph_file(font, "259a"))

    # 259e quadrant upper right and lower left
    svg_square(dur + dll, glyph_file(font, "259e"))

    # 2599 quadrant upper left, lower left, and lower right
    p <- as_coord2d(x = c(0, 0, xc, xc, w, w),
                    y = c(0, h, h, yc, yc, 0))
    svg_square(MZ(p), glyph_file(font, "2599"))

    # 259b quadrant upper left, upper right, lower left
    p <- as_coord2d(x = c(0, 0, w, w, xc, xc),
                    y = c(0, h, h, yc, yc, 0))
    svg_square(MZ(p), glyph_file(font, "259b"))

    # 259c quadrant upper left, upper right, lower right
    p <- as_coord2d(x = c(0, 0, w, w, xc, xc),
                    y = c(yc, h, h, 0, 0, yc))
    svg_square(MZ(p), glyph_file(font, "259c"))

    # 259f quadrant upper right, lower left, lower right
    p <- as_coord2d(x = c(0, 0, xc, xc, w, w),
                    y = c(0, yc, yc, h, h, 0))
    svg_square(MZ(p), glyph_file(font, "259f"))

    as_hex(c(as.hexmode("2580"):as.hexmode("2590"),
             as.hexmode("2594"):as.hexmode("259f")))
}

d_rect <- function(xl, xr, yb, yt) {
    xl <- round(xl)
    xr <- round(xr)
    yb <- round(yb)
    yt <- round(yt)
    p <- as_coord2d(x = c(xl, xl, xr, xr),
                    y = c(yb, yt, yt, yb))
    MZ(p)
}
