# Basic Latin
create_basic_latin <- function(font = "square") {
    h <- dotaro_height(font)
    w <- dotaro_width(font)
    do.call(local_options, glyph_options(w, h, font))

    vg <- (h - CH) / 2
    hg <- vg
    srw <- STW # serif width
    srw2 <- STW # serif width (short)
    if (font == "narrow") {
        hg <- NHGM * hg
        srw2 <- 0.5 * STW
    }
    cw <- w - 2 * hg
    ch <- h - 2 * vg
    
    xc <- w / 2
    yc <- h / 2

    rp <- 0.75 * STW # period radius
    ah <- 3 * STW # apostrophe height

    # 0022 quotation mark
    d <- d_rect(xc + c(-0.25, 0.25) * cw, h - vg - 0.5 * ah, STW, ah)
    write_svg(d, "0022")

    # 0023 number sign
    d <- c(d_rect(xc, yc + c(-0.25, 0.25) * cw, cw, STW),
           d_rect(xc + c(-0.25, 0.25) * cw, yc, STW, cw))
    write_svg(d, "0023")

    # 0025 percent sign
    if (font == "square") {
        rps <- 300
    } else {
        rps <- 250
    }
    d_0025 <- d_fslash(h - vg, w - 2 * hg, vg, 2 * hg, STW) +
        d_ellipse(hg + rps,
                  h - vg - rps,
                  c(rps, rps - STW), c(rps, rps - STW)) +
        d_ellipse(w - hg - rps,
                  vg + rps,
                  c(rps, rps - STW), c(rps, rps - STW))
    write_svg(d_0025, "0025")

    # 002e full stop
    d_002e <- d_circle(xc, vg + 0.5 * rp, rp)
    write_svg(d_002e, "002e")

    # 002f Solidus (Slash)
    d_002f <- d_fslash(h - vg, w - hg, vg, hg, STW)
    write_svg(d_002f, "002f")

    # 005c Reverse Solidus (Backslash)
    d_005c <- d_bslash(h - vg, w - hg, vg, hg, STW)
    write_svg(d_005c, "005c")


    # 0021 exclamation mark
    d <- d_002e + d_rect(xc, yc + 1.5 * rp, STW, ch - 3 * rp)
    write_svg(d, "0021")

    # 00a1 inverted exclamation mark
    d <- d_circle(xc, h - vg - 0.5 * rp, rp) + 
        d_rect(xc, yc - 1.5 * rp, STW, ch - 3 * rp)
    write_svg(d, "00a1")

    # 0027 apostrophe
    d <- d_rect(xc, h - vg - 0.5 * ah, STW, ah)
    write_svg(d, "0027")

    # 002b plus sign
    d <- d_rect(xc, yc, STW, cw) + d_rect(xc, yc, cw, STW)
    write_svg(d, "002b")

    # 002d hyphen-minus
    d_002d <- d_rect(xc, yc, cw, STW)
    write_svg(d_002d, "002d")

    # 0041 latin capital letter a

    # 0042 latin capital letter b
    ds <- c(d_rect2(h - vg, hg + srw + STW, vg, hg + srw), # stem
            d_rect2(h - vg, xc, h - vg - STW, hg), # u bar
            d_rect2(yc + 0.5 * STW, xc, yc - 0.5 * STW, hg + STW), # m bar
            d_rect2(vg + STW, xc, vg, hg), # b bar
            d_arc41(h - vg, w - hg, yc - 0.5 * STW, xc, STW), # u bowl
            d_arc41(yc + 0.5 * STW, w - hg, vg, xc, STW)) # b bowl
    write_svg(ds, "0042")

    # 0044 latin capital letter d
    ds <- c(d_rect2(h - vg, hg + srw + STW, vg, hg + srw), # stem
            d_rect2(h - vg, xc, h - vg - STW, hg), # u bar
            d_rect2(vg + STW, xc, vg, hg), # b bar
            d_arc41(h - vg, w - hg, vg, xc, STW)) # bowl
    write_svg(ds, "0044")

    # 0045 latin capital letter e
    ds <- c(d_rect2(h - vg, hg + srw + STW, vg, hg + srw), # stem
            d_rect2(h - vg, w - hg, h - vg - STW, hg), # t bar
            d_rect2(yc + 0.5 * STW, 0.7 * w, yc - 0.5 * STW, hg + srw), # m bar
            d_rect2(vg + STW, w - hg, vg, hg), # b bar
            d_rect2(h - vg, w - hg, h - vg - STW - srw, w - hg - STW), # ur serif
            d_rect2(vg + STW + srw, w - hg, vg, w - hg - STW)) # lr serif
    write_svg(ds, "0045")

    # 0046 latin capital letter f
    ds <- c(d_rect2(h - vg, hg + srw + STW, vg, hg + srw), # stem
            d_rect2(h - vg, w - hg, h - vg - STW, hg), # t bar
            d_rect2(yc + 0.5 * STW, 0.7 * w, yc - 0.5 * STW, hg + STW), # m bar
            d_rect2(vg + STW, hg + 2 * srw + STW, vg, hg), # b serif
            d_rect2(h - vg, w - hg, h - vg - STW - srw, w - hg - STW)) # t serif
    write_svg(ds, "0046")

    # 0048 latin capital letter h
    ds <- c(d_rect2(h - vg, hg + srw2 + STW, vg, hg + srw2), # left stem
            d_rect2(yc + 0.5 * STW, w - hg - srw2, yc - 0.5 * STW, hg + srw2), # crossbar
            d_rect2(h - vg, w - hg - srw2, vg, w - hg - STW - srw2), # right stem
            d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg), # ll serif
            d_rect2(h - vg, hg + 2 * srw2 + STW, h - vg - STW, hg), # ul serif
            d_rect2(vg + STW, w - hg, vg, w - hg - 2 * srw2 - STW), # lr serif
            d_rect2(h - vg, w - hg, h - vg - STW, w - hg - 2 * srw2 - STW)) # ur serif
    write_svg(ds, "0048")

    # 0049 latin capital letter i
    ds <- c(d_rect2(h - vg, xc + 0.5 * STW, vg, xc - 0.5 * STW), # bar
            d_rect2(h - vg, xc + 0.5 * STW + srw, h - vg - STW, xc - 0.5 * STW - srw), # t serif
            d_rect2(vg + STW, xc + 0.5 * STW + srw, vg, xc - 0.5 * STW - srw)) # b serif
    write_svg(ds, "0049")

    # 004c latin capital letter l
    ds <- c(d_rect2(h - vg, hg + srw + STW, vg, hg + srw), # stem
            d_rect2(h - vg, hg + 2 * srw + STW, h - vg - STW, hg), # t serif
            d_rect2(vg + srw + STW, w - hg, vg, w - hg - STW), # lr serif
            d_rect2(vg + STW, w - hg, vg, hg)) # bar
    write_svg(ds, "004c")

    # 004f latin capital letter o
    d <- d_ellipse(xc, yc, 0.5 * cw - c(0, STW), 0.5 * ch - c(0, STW))
    write_svg(d, "004f")

    # 0050 latin capital letter p
    ds <- c(d_rect2(h - vg, hg + srw + STW, vg, hg + srw), # stem
            d_rect2(h - vg, xc, h - vg - STW, hg), # t bar
            d_rect2(vg + STW, hg + 2 * srw + STW, vg, hg), # b serif
            d_rect2(yc + 0.5 * STW, xc, yc - 0.5 * STW, hg + srw), # b bar
            d_arc41(h - vg, w - hg, yc - 0.5 * STW, xc, STW)) # bowl
    write_svg(ds, "0050")

    # 0054 latin capital letter t
    ds <- c(d_rect2(h - vg, w - hg, h - vg - STW, hg), # bar
            d_rect2(h - vg, xc + 0.5 * STW, vg, xc - 0.5 * STW), # stem
            d_rect2(h - vg, w - hg, h - vg - 2 * STW, w - hg - STW), # r serif
            d_rect2(h - vg, hg + STW, h - vg - 2 * STW, hg), # l serif
            d_rect2(vg + STW, xc + 1.5 * STW, vg, xc - 1.5 * STW)) # b serif
    write_svg(ds, "0054")

    # 0055 latin capital letter u
    yu <- 0.4 * h
    ds <- c(d_rect2(h - vg, hg + srw2 + STW, yu, hg + srw2), # l stem
            d_rect2(h - vg, w - hg - srw2, yu, w - hg - srw2 - STW), # r stem
            d_arc34(yu, w - hg - srw2, vg, hg + srw2, STW), # bottom
            d_rect2(h - vg, hg + 2 * srw2 + STW, h - vg - STW, hg), # l serif
            d_rect2(h - vg, w - hg, h - vg - STW, w - hg - 2 * srw2 - STW)) # r serif
    write_svg(ds, "0055")

    # 0056 latin capital letter v
    ds <- c(d_bslash(h - vg - STW, xc, vg, hg + srw2, STW,
                     left = "horizontal", right = "vertical"), # l stroke
            d_fslash(h - vg - STW, w - hg - srw2, vg, xc, STW,
                     left = "vertical", right = "horizontal"), # r stroke
            d_rect2(h - vg, hg + 2 * srw2 + STW, h - vg - STW, hg), # l serif
            d_rect2(h - vg, w - hg, h - vg - STW, w - hg - 2 * srw2 - STW)) # r serif
    write_svg(ds, "0056")

    # # 0057 latin capital letter w
    # ds <- c(d_bslash(h - vg - STW, 0.5 * (hg + xc), vg, hg + srw2, STW,
    #                  left = "horizontal", right = "vertical"), # l stroke
    #         d_fslash(h - vg - STW, xc, vg, 0.5 * (hg + xc), STW,
    #                  left = "vertical", right = "horizontal"), # r stroke
    #         d_rect2(h - vg, hg + 2 * srw2 + STW, h - vg - STW, hg), # l serif
    #         d_rect2(h - vg, xc + srw2, h - vg - STW, xc - srw2), # m serif
    #         d_rect2(h - vg, w - hg, h - vg - STW, w - hg - 2 * srw2 - STW)) # r serif
    # write_svg(ds, "0057")

    # 0058 latin capital letter x
    ds <- c(d_bslash(h - vg - STW, w - hg - srw2, vg + STW, hg + srw2, STW), # l stroke
            d_fslash(h - vg - STW, w - hg - srw2, vg + STW, hg + srw2, STW), # r stroke
            d_rect2(h - vg, hg + 2 * srw2 + STW, h - vg - STW, hg), # ul serif
            d_rect2(h - vg, w - hg, h - vg - STW, w - hg - 2 * srw2 - STW), # ur serif
            d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg), # bl serif
            d_rect2(vg + STW, w - hg, vg, w - hg - 2 * srw2 - STW)) # br serif
    write_svg(ds, "0058")

    # 005a latin capital letter z
    ds <- c(d_rect2(h - vg, w - hg, h - vg - STW, hg), # t bar
            d_rect2(vg + STW, w - hg, vg, hg), # b bar
            d_fslash(h - vg - STW, w - hg, vg + STW, hg, STW), # stroke
            d_rect2(vg + 2 * STW, w - hg, vg, w - hg - STW), # b serif
            d_rect2(h - vg, hg + STW, h - vg - 2 * STW, hg)) # t serif
    write_svg(ds, "005a")

    # 005f low line
    d <- d_rect(xc, vg + 0.5 * STW, cw, STW)
    write_svg(d, "005f")

    # 00af macron
    d <- d_rect(xc, h - vg - 0.5 * STW, cw, STW)
    write_svg(d, "00af")

    # 007c vertical line
    d <- d_rect(xc, yc, STW, ch)
    write_svg(d, "007c")

    # 003a colon
    d_003a <- d_circle(xc, yc + c(-0.25, 0.25) * cw, rp)
    write_svg(d_003a, "003a")

    # 00f7 division sign
    if (font == "narrow")
        d_003a <- d_circle(xc, yc + c(-0.33, 0.33) * cw, rp)
    write_svg(d_002d + d_003a, "00f7")

    # 003d equals sign
    d <- d_rect(xc, yc + c(-0.25, 0.25) * cw, cw, STW)
    write_svg(d, "003d")

    # 00d7 multiplication sign
    ms <- 0.8 * 0.5
    d <- c(d_fslash(yc + ms * cw, xc + ms * cw, yc - ms * cw, xc - ms * cw, STW, nib = "diagonal"),
           d_bslash(yc + ms * cw, xc + ms * cw, yc - ms * cw, xc - ms * cw, STW, nib = "diagonal"))
    write_svg(d, "00d7")

    # 00a8 diaeresis
    write_svg(d_circle(xc + c(-2, 2) * rp, h - vg - rp, rp), "00a8")

    # 00b7 middle dot
    write_svg(d_circle(xc, yc, rp), "00b7")

    # 2026 horizontal ellipsis
    d <- d_circle(c(hg + rp, xc, h - hg - rp), vg + rp, rp)
    write_svg(d, "2026")

    # 22ee vertical ellipsis
    d <- d_circle(xc, c(vg + rp, yc, h - vg - rp), rp)
    write_svg(d, "22ee")

    # 22ef midline horizontal ellipsis
    d <- d_circle(c(hg + rp, xc, h - hg - rp), yc, rp)
    write_svg(d, "22ef")

    # 22f0 up right diagonal ellipsis
    d <- d_circle(c(hg + rp, xc, h - hg - rp), c(vg + rp, yc, h - vg - rp), rp)
    write_svg(d, "22f0")

    # 22f1 down right diagonal ellipsis
    d <- d_circle(c(hg + rp, xc, h - hg - rp), c(h - vg - rp, yc, vg + rp), rp)
    write_svg(d, "22f1")

    c(as.hexmode("0021"):as.hexmode("0023"),
      as.hexmode("002d"):as.hexmode("002f"),
      as.hexmode("0044"):as.hexmode("0046"),
      as.hexmode("0048"):as.hexmode("0049"),
      as.hexmode("004f"):as.hexmode("0050"),
      as.hexmode("0054"):as.hexmode("0056"),
      as.hexmode("22ee"):as.hexmode("22f1"),
      as.hexmode(c("0025", "0027", "002b", "003a", "003d",
                   "0042", "004c", "0058", "005a", "005c", "005f", "007c",
                   "00a1", "00a8", "00af", "00b7", "00d7", "00f7",
                   "2026"))
    ) |> as_hex()
}
