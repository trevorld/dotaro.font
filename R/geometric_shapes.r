# Geometric Shapes
# Miscellaneous Symbols and Arrows
create_geometric_shapes <- function(font = "square") {
    h <- dotaro_height(font)
    w <- dotaro_width(font)
    do.call(local_options, glyph_options(w, h, font))

    vg <- (h - CH) / 2
    hg <- vg
    if (font == "narrow")
        hg <- 0.25 * hg
    cw <- w - 2 * hg

    xc <- w / 2
    yc <- h / 2

    # 25a0 black square
    d_25a0 <- RECT(x = xc, y = yc, w = w, h = w)
    write_svg(d_25a0, "25a0")
    # 2b1b black large square
    write_svg(d_25a0, "2b1b")

    d_25fc <- RECT(x = xc, y = yc, w = cw, h = cw)
    d_25fb <- RECT(x = xc, y = yc, w = cw - 2 * OW, h = cw - 2 * OW)
    # 25fb white medium square
    write_svg(d_25fb + d_25fc, "25fb")
    # 25fc black medium square
    write_svg(d_25fc, "25fc")

    # 25a1 white square
    d_25a1 <- RECT(x = xc, y = yc, w = w - 2 * OW, h = w - 2 * OW)
    white_square <- d_25a0 + d_25a1
    write_svg(white_square, "25a1")
    # 2b1c white large square
    write_svg(white_square, "2b1c")

    # 25a3 white square containing black small square
    d_25a3 <- RECT(x = xc, y = yc, w = 0.5 * w, h = 0.5 * w)
    write_svg(white_square + d_25a3, "25a3")

    # 25c6 black diamond
    d_25c6 <- d_diamond(xc, yc, w, w)
    write_svg(d_25c6, "25c6")
    # 25c7 white diamond
    d_25c7 <- d_diamond(xc, yc, w, w, offset = -OW)
    write_svg(d_25c6 + d_25c7, "25c7")

    # 2b25 black medium diamond
    d_2b25 <- d_diamond(xc, yc, w = cw, h = cw)
    write_svg(d_2b25, "2b25")
    # 2b26 black medium diamond
    d_2b26 <- d_diamond(xc, yc, w = cw, h = cw, offset = -OW)
    write_svg(d_2b25 + d_2b26, "2b26")

    d_25cf <- CIRCLE(xc, yc, 0.5 * w)

    r_inner <- 0.5 * w - OW
    d_25cb <- CIRCLE(xc, yc, r_inner)

    # 25cb white circle
    write_svg(d_25cf + d_25cb, "25cb")
    # 25ef large circle
    write_svg(d_25cf + d_25cb, "25ef")

    # 25cf black circle
    write_svg(d_25cf, "25cf")
    # 2b24 black large circle
    write_svg(d_25cf, "2b24")

    # 25d0 circle with left half black
    d_25d0 <- M(xc, yc + r_inner) +
        AZ(r_inner, x = xc, y = yc - r_inner)
    write_svg(d_25cf + d_25d0, "25d0")

    # 25d1 circle with right half black
    d_25d1 <- M(xc, yc + r_inner) +
        V(yc - r_inner) +
        AZ(r_inner, x = xc, y = yc + r_inner)
    write_svg(d_25cf + d_25d1, "25d1")

    # 25d2 circle with lower half black
    d_25d2 <- M(xc + r_inner, yc) +
        H(xc - r_inner) +
        AZ(r_inner, x = xc + r_inner, y = yc)
    write_svg(d_25cf + d_25d2, "25d2")

    # 25d3 circle with upper half black
    d_25d3 <- M(xc + r_inner, yc) +
        AZ(r_inner, x = xc - r_inner, y = yc)
    write_svg(d_25cf + d_25d3, "25d3")

    # 25e2 black lower right triangle
    l <- list(x = c(0, w, w),
              y = c(yc - 0.5 * w, yc + 0.5 * w, yc - 0.5 * w))
    d_25e2 <- POLYGON(l)
    write_svg(d_25e2, "25e2")
    # 25ff white lower right triangle
    d_25ff <- POLYGON(l, offset = -OW)
    write_svg(d_25e2 + d_25ff, "25ff")
    # 25e3 black lower left triangle
    l <- list(x = c(0, 0, w),
              y = c(yc - 0.5 * w, yc + 0.5 * w, yc - 0.5 * w))
    d_25e3 <- POLYGON(l)
    write_svg(d_25e3, "25e3")
    # 25fa white lower left triangle
    d_25fa <- POLYGON(l, offset = -OW)
    write_svg(d_25e3 + d_25fa, "25fa")
    # 25e4 black upper left triangle
    l <- list(x = c(0, 0, w),
              y = c(yc - 0.5 * w, yc + 0.5 * w, yc + 0.5 * w))
    d_25e4 <- POLYGON(l)
    write_svg(d_25e4, "25e4")
    # 25f8 white upper left triangle
    d_25f8 <- POLYGON(l, offset = -OW)
    write_svg(d_25e4 + d_25f8, "25f8")
    # 25e5 black upper right triangle
    l <- list(x = c(0, w, w),
              y = c(yc + 0.5 * w, yc + 0.5 * w, yc - 0.5 * w))
    d_25e5 <- POLYGON(l)
    write_svg(d_25e5, "25e5")
    # 25f9 white upper right triangle
    d_25f9 <- POLYGON(l, offset = -OW)
    write_svg(d_25e5 + d_25f9, "25f9")

    # 25e7 square with left half black
    write_svg(c(white_square, RECT(0.25 * w, yc, 0.5 * w, w)), "25e7")
    # 25e8 square with right half black
    write_svg(c(white_square, RECT(0.75 * w, yc, 0.5 * w, w)), "25e8")
    # 25e9 square with upper left diagonal half black
    write_svg(c(white_square, d_25e4), "25e9")
    # 25ea square with lower right diagonal half black
    write_svg(c(white_square, d_25e2), "25ea")

    as_hex(c(as.hexmode(c("25a0", "25a1", "25a3", "25c6", "25c7", "25cb")),
             as.hexmode("25cf"):as.hexmode("25d3"),
             as.hexmode("25ef"),
             as.hexmode("25fb"):as.hexmode("25fc"),
             as.hexmode("25e2"):as.hexmode("25e5"),
             as.hexmode("25e7"):as.hexmode("25ea"),
             as.hexmode(c("25fa", "25f8", "25f9", "25ff")),
             as.hexmode("2b1b"):as.hexmode("2b1c"),
             as.hexmode("2b24"):as.hexmode("2b26")
             ))
}

d_diamond <- function(xc, yc, w, h, offset = 0) {
    x <- c(xc - 0.5 * w, xc, xc + 0.5 * w, xc)
    y <- c(yc, yc + 0.5 * h, yc, yc - 0.5 * h)
    d <- POLYGON(x, y, offset = offset)
}
