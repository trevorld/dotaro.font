create_miscellaneous_symbols <- function(font = "square") {
    h <- dotaro_height(font) # glyph height
    w <- dotaro_width(font) # glyph width
    do.call(local_options, glyph_options(w, h, font))

    vg <- (h - CH) / 2
    hg <- vg
    if (font == "narrow")
        hg <- 0.25 * hg
    cw <- w - 2 * hg

    xc <- w / 2
    yc <- h / 2

    #

    # 2663 Black Club Suit
    #### Add triangle at base?
    if (font == "square") {
        r <- 280
        rt <- r
        ycl <- yc
    } else {
        r <- 240
        rt <- 320
        ycl <- yc - 40
    }
    l1 <- RECT(x = xc, y = yc - 0.5 * r, w = SW, h = CH - r)
    l2 <- RECT(x = xc, y = ycl, h = SW, w = cw - 2 * r)
    c1 <- CIRCLE(x = hg + r, y = ycl, r = r)
    c2 <- CIRCLE(x = xc, y = h - vg - rt, r = rt)
    c3 <- CIRCLE(x = w - hg - r, y = ycl, r = r)
    write_svg(c(l1, l2, c1, c2, c3), "2663")

    # 2660 Black Spade Suit
    hr <- cw / 4
    vr <- cw / 5
    if (font == "narrow") vr <- 2 * vr
    bso <- 180
    d <- M(xc, vg + vr + bso) +
        A(hr, vr, 0, 0, 0, xc - 2 * hr, vg + vr + bso) +
        Q(hg, h - 5 * vg, xc, h - vg) +
        Q(w - hg, h - 5 * vg, xc + 2 * hr, vg + vr + bso) +
        AZ(hr, vr, 0, 0, 0, xc, vg + vr + bso)
    write_svg(d + l1, "2660")
    # 2665 Black Heart Suit
    hr <- cw / 4
    vr <- cw / 4
    if (font == "narrow") vr <- 2 * vr
    d <- M(xc, h - vg - vr) +
        A(hr, vr, 0, 0, 0, xc + 2 * hr, h - vg - vr) +
        Q(w - hg, 5 * vg, xc, vg) +
        Q(hg, 5 * vg, xc - 2 * hr, h - vg - vr) +
        AZ(hr, vr, 0, 0, 0, xc, h - vg - vr)
    write_svg(d, "2665")

    # 2666 Black Diamond Suit
    x <- c(xc, w - hg, xc, hg)
    y <- c(h - vg, yc, vg, yc)
    d_2666 <- MZ(x, y)
    write_svg(d_2666, "2666")

    # Thickness not quite right especially for narrow version
    # # 2662 White Diamond Suit
    # x <- c(xc, w - hg - OW, xc, hg + OW)
    # y <- c(h - vg - OW, yc, vg + OW, yc)
    # d_2662 <- MZ(x, y)
    # write_svg(d_2666 + d_2662, "2662")

    d_26aa <- CIRCLE(xc, yc, 0.5 * cw - OW)
    d_26ab <- CIRCLE(xc, yc, 0.5 * cw)
    # 26aa Medium White Circle
    write_svg(d_26ab + d_26aa, "26aa")
    # 26ab Medium Black Circle
    write_svg(d_26ab, "26ab")

    # PUA f5b8 Latin Coin Suit
    d_diam <- d_diamond(xc, yc, w = 0.5 * cw, h = 0.5 * cw)
    write_svg(d_26ab + d_diam, "f5b8")


    as_hex(c(as.hexmode("2660"),
             as.hexmode("2663"),
             as.hexmode("2665"):as.hexmode("2666"),
             as.hexmode("26aa"):as.hexmode("26ab"),
             as.hexmode("f5b8")
             ))
}
