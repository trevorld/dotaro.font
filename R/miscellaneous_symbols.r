create_miscellaneous_symbols <- function(font = "square") {
    h <- dotaro_height(font) # glyph height
    w <- dotaro_width(font) # glyph width
    do.call(local_options, glyph_options(w, h, font))

    vg <- (h - CH) / 2
    hg <- vg
    if (font == "narrow")
        hg <- NHGM * hg
    cw <- w - 2 * hg
    ch <- w - 2 * vg

    xc <- w / 2
    yc <- h / 2

    # 2605 Black Star
    ro <- 0.5 * cw
    s <- isotoxal_2ngon_inner_radius(n = 5, d = 2)
    d_2605 <- d_isotoxal_2ngon(xc, yc, ro, s, 5)
    write_svg(d_2605, "2605")

    # 2606 White Star
    d_2606 <- d_isotoxal_2ngon(xc, yc, ro, s, 5, offset = -OW)
    write_svg(d_2605 + d_2606, "2606")

    # 272a circled white star
    write_svg(d_circle(xc, yc, ro) + d_2605, "272a")
    # 272b open center black star
    ri <- s * ro
    write_svg(d_2605 + d_circle(xc, yc, ri), "272b")
    # 272c black center white star
    write_svg(d_2605 + d_2606 + d_circle(xc, yc, ri - OW), "272c")
    # 272d outlined black star
    write_svg(d_2605 + d_2606 + d_isotoxal_2ngon(xc, yc, ro, s, 5, offset = -2 * OW), "272d")
    # Throws an error if I let `offset` get bigger than -2.4 * OW
    # # 272e heavy outlined black star
    # write_svg(d_2605 + d_2606 + d_polygon(x, y, offset = -2.4 * OW), "272e")

    # 2736 six-pointed black star
    s <- 0.4
    d_2736 <- d_isotoxal_2ngon(xc, yc, ro, s, 6)
    write_svg(d_2736, "2736")

    # 2734 eight pointed black star
    s <- 0.30
    d_2734 <- d_isotoxal_2ngon(xc, yc, ro, s, 8)
    write_svg(d_2734, "2734")

    # 2737 eight pointed rectilinear black star
    s <- 0.40
    d_2737 <- d_isotoxal_2ngon(xc, yc, ro, s, 8, 22.5)
    write_svg(d_2737, "2737")

    # 2738 heavy eight pointed rectilinear black star
    # (in unicode example the "heavy" one actually looks like it)
    s <- isotoxal_2ngon_inner_radius(n = 8, beta_ext = 90) # 0.5411
    d_2738 <- d_isotoxal_2ngon(xc, yc, ro, s, 8, 22.5)
    write_svg(d_2738, "2738")

    # 2742 circled open center eight-pointed black star
    s <- 0.6
    ri <- s * ro
    d_2742 <- d_circle(xc, yc, c(ro, ri, ri - OW)) + d_isotoxal_2ngon(xc, yc, ro, s, 8)
    write_svg(d_2742, "2742")

    # 2739 twelve-pointed black star
    s <- isotoxal_2ngon_inner_radius(n = 12, d = 5)
    d_2739 <- d_isotoxal_2ngon(xc, yc, ro, s, 12)
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
    l1 <- d_rect(x = xc, y = yc - 0.5 * r, w = STW, h = CH - r)
    l2 <- d_rect(x = xc, y = ycl, h = STW, w = cw - 2 * r)
    c1 <- d_circle(x = hg + r, y = ycl, r = r)
    c2 <- d_circle(x = xc, y = h - vg - rt, r = rt)
    c3 <- d_circle(x = w - hg - r, y = ycl, r = r)
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
    d_2662 <- d_polygon(x, y, offset = -OW)
    write_svg(d_2666 + d_2662, "2662")

    d_26aa <- d_circle(xc, yc, 0.5 * cw - OW)
    d_26ab <- d_circle(xc, yc, 0.5 * cw)
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
             as.hexmode(c("2734", "2736", "2737", "2738", "2739", "2742")),
             as.hexmode("f5b8")
             ))
}
