create_geometric_shapes <- function(font = "square") {
    h <- dotaro_height(font)
    w <- dotaro_width(font)
    do.call(local_options, glyph_options(w, h, font))

    xc <- w / 2
    yc <- h / 2

    # 25a0 black square
    d_25a0 <- RECT(x = xc, y = yc, w = w, h = w)
    write_svg(d_25a0, "25a0")

    # 25a1 white square
    d_25a1 <- RECT(x = xc, y = yc, w = w - 2 * SW, h = w - 2 * SW)
    write_svg(d_25a0 + d_25a1, "25a1")

    # 25a3 white square containing black small square
    d_25a3 <- RECT(x = xc, y = yc, w = 0.5 * w, h = 0.5 * w)
    write_svg(d_25a0 + d_25a1 + d_25a3, "25a3")

    # 25c6 black diamond
    x <- c(0, xc, w, xc)
    y <- c(yc, yc + 0.5 * w, yc, yc - 0.5 * w)
    d <- MZ(x, y)
    write_svg(d, "25c6")

    d_25cf <- CIRCLE(xc, yc, 0.5 * w)

    r_inner <- 0.5 * w - SW
    d_25cb <- CIRCLE(xc, yc, r_inner)

    # 25cb white circle
    write_svg(d_25cf + d_25cb, "25cb")

    # 25cf black circle
    write_svg(d_25cf, "25cf")

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

    as_hex(c(as.hexmode(c("25a0", "25a1", "25a3", "25c6", "25cb")),
             as.hexmode("25cf"):as.hexmode("25d3")))
}
