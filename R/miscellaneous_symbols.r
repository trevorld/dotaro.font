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

    # 2605 Black Star
    ro <- 0.5 * cw
    xyo <- as_coord2d(degrees(seq(90, by = 72, length.out = 5L)),
                      radius = ro)$
        translate(x = xc, y = yc)
    ri <- 0.2 * cw
    xyi <- as_coord2d(degrees(seq(90 + 36, by = 72, length.out = 5L)),
                      radius = ri)$
        translate(x = xc, y = yc)
    x <- as.numeric(rbind(xyo$x, xyi$x))
    y <- as.numeric(rbind(xyo$y, xyi$y))
    d_2605 <- POLYGON(x, y)
    write_svg(d_2605, "2605")

    # 2606 White Star
    d_2606 <- POLYGON(x, y, offset = -OW)
    write_svg(d_2605 + d_2606, "2606")

    # 272a circled white star
    write_svg(CIRCLE(xc, yc, ro) + d_2605, "272a")
    # 272b open center black star
    write_svg(d_2605 + CIRCLE(xc, yc, ri), "272b")
    # 272c black center white star
    write_svg(d_2605 + d_2606 + CIRCLE(xc, yc, ri - OW), "272c")
    # 272d outlined black star
    write_svg(d_2605 + d_2606 + POLYGON(x, y, offset = -2 * OW), "272d")
    # Throws an error if I let `offset` get bigger than -2.4 * OW
    # # 272e heavy outlined black star
    # write_svg(d_2605 + d_2606 + POLYGON(x, y, offset = -2.4 * OW), "272e")

    # 2736 six-pointed black star
    ro <- 0.5 * cw
    xyo <- as_coord2d(degrees(seq(90, by = 60, length.out = 6L)),
                      radius = ro)$
        translate(x = xc, y = yc)
    ri <- 0.2 * cw
    xyi <- as_coord2d(degrees(seq(90 + 30, by = 60, length.out = 6L)),
                      radius = ri)$
        translate(x = xc, y = yc)
    x <- as.numeric(rbind(xyo$x, xyi$x))
    y <- as.numeric(rbind(xyo$y, xyi$y))
    d_2736 <- POLYGON(x, y)
    write_svg(d_2736, "2736")

    # 2734 eight-pointed black star
    ro <- 0.5 * cw
    xyo <- as_coord2d(degrees(seq(90, by = 45, length.out = 8L)),
                      radius = ro)$
        translate(x = xc, y = yc)
    ri <- 0.2 * cw
    xyi <- as_coord2d(degrees(seq(90 + 22.5, by = 45, length.out = 8L)),
                      radius = ri)$
        translate(x = xc, y = yc)
    x <- as.numeric(rbind(xyo$x, xyi$x))
    y <- as.numeric(rbind(xyo$y, xyi$y))
    d_2734 <- POLYGON(x, y)
    write_svg(d_2734, "2734")

    # 2742 circled open center eight-pointed black star
    ri <- 0.3 * cw
    xyi <- as_coord2d(degrees(seq(90 + 22.5, by = 45, length.out = 8L)),
                      radius = ri)$
        translate(x = xc, y = yc)
    x <- as.numeric(rbind(xyo$x, xyi$x))
    y <- as.numeric(rbind(xyo$y, xyi$y))
    d_2742 <- CIRCLE(xc, yc, ro) + POLYGON(x, y) + CIRCLE(xc, yc, ri) + CIRCLE(xc, yc, ri - OW)
    write_svg(d_2742, "2742")

    # 2739 twelve-pointed black star
    ro <- 0.5 * cw
    xyo <- as_coord2d(degrees(seq(90, by = 30, length.out = 12L)),
                      radius = ro)$
        translate(x = xc, y = yc)
    ri <- 0.3 * cw
    xyi <- as_coord2d(degrees(seq(90 + 15, by = 30, length.out = 12L)),
                      radius = ri)$
        translate(x = xc, y = yc)
    x <- as.numeric(rbind(xyo$x, xyi$x))
    y <- as.numeric(rbind(xyo$y, xyi$y))
    d_2739 <- POLYGON(x, y)
    write_svg(d_2739, "2739")


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

    # 2662 White Diamond Suit
    d_2662 <- POLYGON(x, y, offset = -OW)
    write_svg(d_2666 + d_2662, "2662")

    d_26aa <- CIRCLE(xc, yc, 0.5 * cw - OW)
    d_26ab <- CIRCLE(xc, yc, 0.5 * cw)
    # 26aa Medium White Circle
    write_svg(d_26ab + d_26aa, "26aa")
    # 26ab Medium Black Circle
    write_svg(d_26ab, "26ab")


    # PUA f5b8 Latin Coin Suit
    d_diam <- d_diamond(xc, yc, w = 0.5 * cw, h = 0.5 * cw)
    write_svg(d_26ab + d_diam, "f5b8")

    as_hex(c(as.hexmode(c("2605", "2606")),
             as.hexmode("2660"),
             as.hexmode("2662"):as.hexmode("2663"),
             as.hexmode("2665"):as.hexmode("2666"),
             as.hexmode("26aa"):as.hexmode("26ab"),
             as.hexmode("272a"):as.hexmode("272d"),
             as.hexmode(c("2734", "2736", "2739", "2742")),
             as.hexmode("f5b8")
             ))
}
