create_geometric_shapes <- function(font = "square") {
    if (font != "square") {
        return(character(0L))
    }

    h <- dotaro_height(font)
    w <- dotaro_width(font)
    do.call(local_options, glyph_dee_options(w, h))

    xc <- w / 2
    yc <- h / 2

    # 25a0 black square
    d_25a0 <- d_rect(xl = 0, xr = w, yb = 0, yt = h)
    svg_square(d_25a0, glyph_file(font, "25a0"))

    # 25a1 white square
    d_25a1 <- d_rect(xl = SW, xr = w - SW, yb = SW, yt = h - SW)
    svg_square(d_25a0 + d_25a1, glyph_file(font, "25a1"))

    # 25a3 white square containing black small square
    d_25a3 <- d_rect(xl = w / 4, xr = (3 / 4) * w,
                     yb = h / 4, yt = (3 / 4) * h)
    svg_square(d_25a0 + d_25a1 + d_25a3, glyph_file(font, "25a3"))

    # 25c6 black diamond
    p <- as_coord2d(x = c(0, xc, w, xc),
                    y = c(yc, h, yc, 0))
    d <- MZ(p)
    svg_square(d, glyph_file(font, "25c6"))

    d_25cf <- M(0.5 * w, h) +
        A(0.5 * w, 0.5 * w, 0, 0, 0, 0.5 * w, 0) +
        AZ(0.5 * w, 0.5 * w, 0, 0, 0, 0.5 * w, h)

    r_inner <- round(0.5 * w - SW)
    d_25cb <- M(0.5 * w, h - SW) +
        A(r_inner, r_inner, 0, 0, 0, 0.5 * w, SW) +
        AZ(r_inner, r_inner, 0, 0, 0, 0.5 * w, h - SW)

    # 25cb white circle
    svg_square(d_25cf + d_25cb, glyph_file(font, "25cb"))

    # 25cf black circle
    svg_square(d_25cf, glyph_file(font, "25cf"))

    # 25d0 circle with left half black
    d_25d0 <- M(0.5 * w, h - SW) +
        AZ(r_inner, r_inner, 0, 0, 0, 0.5 * w, SW)
    svg_square(d_25cf + d_25d0, glyph_file(font, "25d0"))

    # 25d1 circle with right half black
    d_25d1 <- M(0.5 * w, h - SW) +
        V(SW) +
        AZ(r_inner, r_inner, 0, 0, 0, 0.5 * w, h - SW)
    svg_square(d_25cf + d_25d1, glyph_file(font, "25d1"))

    as_hex(c("25a0", "25a1", "25a3", "25c6", "25cb", "25cf", "25d0", "25d1"))
}
