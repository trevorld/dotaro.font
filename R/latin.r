# Basic Latin
create_basic_latin <- function(font = "square") {
    h <- dotaro_height(font)
    w <- dotaro_width(font)
    do.call(local_options, glyph_options(w, h, font))

    vg <- (h - CH) / 2
    hg <- vg
    srw <- STW # serif width
    srw2 <- STW # serif width (short)
    srw3 <- 0.5 * STW # serif width (extra short)
    if (font == "narrow") {
        hg <- NHGM * hg
        srw2 <- 0.5 * STW
        srw3 <- 0.25 * STW
    }
    cw <- w - 2 * hg
    ch <- h - 2 * vg
    ncw <- 912
    yxh <- vg + ncw # x-height = narrow cw
    ych <- h - vg

    xc <- w / 2
    yc <- h / 2

    rp <- 0.75 * STW # period radius
    ah <- 3 * STW # apostrophe height

    # 0022 quotation mark
    d <- d_rect(xc + c(-0.25, 0.25) * cw, ych - 0.5 * ah, STW, ah)
    write_svg(d, "0022")

    # 0023 number sign
    d <- c(d_rect(xc, yc + c(-0.20, 0.20) * cw, cw, STW),
           d_rect(xc + c(-0.20, 0.20) * cw, yc, STW, cw))
    write_svg(d, "0023")

    # 0025 percent sign
    if (font == "square") {
        rps <- 300
    } else {
        rps <- 250
    }
    d_0025 <- d_fslash(ych, w - 2 * hg, vg, 2 * hg, STW) +
        d_ellipse(hg + rps,
                  ych - rps,
                  c(rps, rps - STW), c(rps, rps - STW)) +
        d_ellipse(w - hg - rps,
                  vg + rps,
                  c(rps, rps - STW), c(rps, rps - STW))
    write_svg(d_0025, "0025")

    # 002e full stop
    d_002e <- d_circle(xc, vg + 0.5 * rp, rp)
    write_svg(d_002e, "002e")

    # 002f Solidus (Slash)
    d_002f <- d_fslash(ych, w - hg, vg, hg, STW)
    write_svg(d_002f, "002f")

    # 005b left square bracket
    ds <- c(d_rect2(ych - STW, xc, vg + STW, xc - STW), # stem
            d_rect2(ych, xc + STW, ych - STW, xc - STW), # t serif
            d_rect2(vg + STW, xc + STW, vg, xc - STW)) # b serif
    write_svg(ds, "005b")

    # 005c Reverse Solidus (Backslash)
    d_005c <- d_bslash(ych, w - hg, vg, hg, STW)
    write_svg(d_005c, "005c")

    # 005d right square bracket
    ds <- c(d_rect2(ych - STW, xc + STW, vg + STW, xc), # stem
            d_rect2(ych, xc + STW, ych - STW, xc - STW), # t serif
            d_rect2(vg + STW, xc + STW, vg, xc - STW)) # b serif
    write_svg(ds, "005d")

    # 0021 exclamation mark
    d <- d_002e + d_rect(xc, yc + 1.5 * rp, STW, ch - 3 * rp)
    write_svg(d, "0021")

    # 00a1 inverted exclamation mark
    d <- d_circle(xc, ych - 0.5 * rp, rp) +
        d_rect(xc, yc - 1.5 * rp, STW, ch - 3 * rp)
    write_svg(d, "00a1")

    # 0027 apostrophe
    d <- d_rect(xc, ych - 0.5 * ah, STW, ah)
    write_svg(d, "0027")

    # 0028 left parenthesis
    w_par <- 2 * STW
    d <- d_arc23(ych, xc + 0.5 * w_par, vg, xc - 0.5 * w_par, STW)
    write_svg(d, "0028")
    # 0029 right parenthesis
    d <- d_arc41(ych, xc + 0.5 * w_par, vg, xc - 0.5 * w_par, STW)
    write_svg(d, "0029")

    # 002b plus sign
    d <- d_rect(xc, yc, STW, cw) + d_rect(xc, yc, cw, STW)
    write_svg(d, "002b")

    # 002d hyphen-minus
    d_002d <- d_rect(xc, yc, cw, STW)
    write_svg(d_002d, "002d")

    # # 007e tilde
    # ry <- 300
    # ds <- c(d_arc12(yc + ry, xc + 0.5 * STW, yc, hg, STW),
    #         d_arc34(yc, w - hg, yc - ry, xc - 0.5 * STW, STW))
    # write_svg(ds, "007e")

    # 0030 digit 0
    ds <- c(d_ellipse(x = xc, y = yc, rx = xc - c(hg, hg + STW), ry = yc - c(vg, vg + STW)), # loop
            d_rect(x = xc, y = yc, w = STW, h = 0.6 * yc) # inside "dot/slash"
            )
    write_svg(ds, "0030")
    # 0031 digit 1
    ds <- c(d_rect(x = xc, y = yc, w = STW, h = h - 2 * vg - 2 * STW), # stem,
            d_rect2(ych, xc + 0.5 * STW, ych - STW, xc - 1.5 * STW), # t serif,
            d_rect2(vg + STW, xc + 1.5 * STW, vg, xc - 1.5 * STW)) # b serif
    write_svg(ds, "0031")
    # 0032 digit 2
    # 0033 digit 3
    # 0034 digit 4
    # 0035 digit 5
    # 0036 digit 6
    # 0037 digit 7
    ds <- c(d_rect2(ych, hg + STW, ych - STW - STW, hg), # ul serif
            d_rect2(ych, w - hg, ych - STW, hg + STW), # bar
            d_fslash(ych - STW, w - hg, vg + STW, xc - 0.5 * STW, STW), # stroke
            d_rect2(vg + STW, xc + 0.5 * STW + STW, vg, xc - 0.5 * STW - STW) # b serif
            )
    write_svg(ds, "0037")
    # 0038 digit 8
    ov8 <- 50 # overlap amount
    ds <- c(d_ellipse(x = xc, y = 0.5 * (yc + ych - ov8), rx = xc - c(hg, hg + STW), ry = 0.5 * (ov8 + yc - vg) - c(0, STW)), # top
            d_ellipse(x = xc, y = 0.5 * (yc + vg + ov8), rx = xc - c(hg, hg + STW), ry = 0.5 * (ov8 + yc - vg) - c(0, STW)) # bottom
            )
    write_svg(ds, "0038")
    # 0039 digit 9

    # 0041 latin capital letter a
    if (font == "square")
        yca <- 0.35 * h
    else
        yca <- 0.31 * h
    ds <- c(d_fslash(ych, xc, vg + STW, hg + srw2, STW,
                     left = "horizontal", right = "vertical"), # l stroke
            d_bslash(ych, w - hg - srw2, vg + STW, xc, STW,
                     left = "vertical", right = "horizontal"), # r stroke
            d_rect2(yca + 0.5 * STW, 0.7 * w, yca - 0.5 * STW, 0.3 * w), # crossbar
            d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg), # ll serif
            d_rect2(vg + STW, w - hg, vg, w - hg - 2 * srw2 - STW)) # lr serif
    write_svg(ds, "0041")

    # 0042 latin capital letter b
    ds <- c(d_rect2(ych, hg + srw + STW, vg, hg + srw), # stem
            d_rect2(ych, xc, ych - STW, hg), # u bar
            d_rect2(yc + 0.5 * STW, xc, yc - 0.5 * STW, hg + STW), # m bar
            d_rect2(vg + STW, xc, vg, hg), # b bar
            d_arc41(ych, w - hg, yc - 0.5 * STW, xc, STW), # u bowl
            d_arc41(yc + 0.5 * STW, w - hg, vg, xc, STW)) # b bowl
    write_svg(ds, "0042")

    # 0043 latin capital letter c
    cvo <- 2.5 * STW
    ds <- c(d_arc23(ych, xc, vg, hg, STW), # l curve
            d_arc1(ych, w - hg, ych - cvo, xc, STW), # ur curve
            d_circle(w - hg - rp, ych - cvo, rp), # ball
            d_arc4(vg + cvo, w - hg, vg, xc, STW)) # lr curve
    write_svg(ds, "0043")

    # 0186 latin capital letter open o "turned c"
    ds <- c(d_arc41(ych, w - hg, vg, xc, STW), # r curve
            d_arc2(ych, xc, ych - cvo, hg, STW), # ul curve
            d_circle(hg + rp, ych - cvo, rp), # ball
            d_arc3(vg + cvo, xc, vg, hg, STW)) # ll curve
    write_svg(ds, "0186")

    # 0044 latin capital letter d
    ds <- c(d_rect2(ych, hg + srw + STW, vg, hg + srw), # stem
            d_rect2(ych, xc, ych - STW, hg), # u bar
            d_rect2(vg + STW, xc, vg, hg), # b bar
            d_arc41(ych, w - hg, vg, xc, STW)) # bowl
    write_svg(ds, "0044")

    # 0045 latin capital letter e
    ds <- c(d_rect2(ych, hg + srw + STW, vg, hg + srw), # stem
            d_rect2(ych, w - hg, ych - STW, hg), # t bar
            d_rect2(yc + 0.5 * STW, 0.7 * w, yc - 0.5 * STW, hg + srw), # m bar
            d_rect2(vg + STW, w - hg, vg, hg), # b bar
            d_rect2(ych, w - hg, ych - STW - srw, w - hg - STW), # ur serif
            d_rect2(vg + STW + srw, w - hg, vg, w - hg - STW)) # lr serif
    write_svg(ds, "0045")

    # 0046 latin capital letter f
    ds <- c(d_rect2(ych, hg + srw + STW, vg, hg + srw), # stem
            d_rect2(ych, w - hg, ych - STW, hg), # t bar
            d_rect2(yc + 0.5 * STW, 0.7 * w, yc - 0.5 * STW, hg + STW), # m bar
            d_rect2(vg + STW, hg + 2 * srw + STW, vg, hg), # b serif
            d_rect2(ych, w - hg, ych - STW - srw, w - hg - STW)) # t serif
    write_svg(ds, "0046")

    # 0047 latin capital letter g
    if (font == "narrow")
        ghw <- 2.5 * STW
    else
        ghw <- 4.0 * STW
    ds <- c(d_arc23(ych, xc, vg, hg, STW), # l curve
            d_arc1(ych, w - hg, ych - cvo, xc, STW), # ur curve
            d_circle(w - hg - rp, ych - cvo, rp), # ball
            d_arc4(vg + cvo, w - hg, vg, xc, STW), # lr curve
            d_rect2(vg + cvo + STW, w - hg, vg + cvo, w - hg - STW), # hook 1
            d_rect2(vg + cvo + 2 * STW, w - hg, vg + cvo + STW, w - hg - ghw)) # hook 2
    write_svg(ds, "0047")

    # 0048 latin capital letter h
    ds <- c(d_rect2(ych, hg + srw2 + STW, vg, hg + srw2), # left stem
            d_rect2(yc + 0.5 * STW, w - hg - srw2, yc - 0.5 * STW, hg + srw2), # crossbar
            d_rect2(ych, w - hg - srw2, vg, w - hg - STW - srw2), # right stem
            d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg), # ll serif
            d_rect2(ych, hg + 2 * srw2 + STW, ych - STW, hg), # ul serif
            d_rect2(vg + STW, w - hg, vg, w - hg - 2 * srw2 - STW), # lr serif
            d_rect2(ych, w - hg, ych - STW, w - hg - 2 * srw2 - STW)) # ur serif
    write_svg(ds, "0048")

    # 0049 latin capital letter i
    ds <- c(d_rect2(ych, xc + 0.5 * STW, vg, xc - 0.5 * STW), # bar
            d_rect2(ych, xc + 0.5 * STW + srw, ych - STW, xc - 0.5 * STW - srw), # t serif
            d_rect2(vg + STW, xc + 0.5 * STW + srw, vg, xc - 0.5 * STW - srw)) # b serif
    write_svg(ds, "0049")

    # 004a latin capital letter j
    ds <- c(d_rect2(ych, w - hg, ych - STW, w - hg - 3* STW), # t serif
            d_rect2(ych - STW, w - hg - STW, 0.4 * h, w - hg - 2 * STW), # bar
            d_arc34(0.4 * h, w - hg - STW, vg, hg, STW)) # hook
    write_svg(ds, "004a")

    # 004b latin capital letter k
    ds <- c(d_rect2(ych, hg + srw2 + STW, vg, hg + srw2), # l stem
            d_rect2(ych, hg + 2 * srw2 + STW, ych - STW, hg), # ul serif
            d_rect2(ych, w - hg, ych - STW, w - hg - 2 * srw2 - STW), # ur serif
            d_fslash(ych - STW, w - hg - srw2, yc, hg + srw2 + STW, STW,
                     left = "square", right = "horizontal"), # t stroke
            d_bslash(yc, w - hg - srw2, vg + STW, hg + srw2 + STW, STW,
                     left = "square", right = "horizontal"), # b stroke
            d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg), # ll serif
            d_rect2(vg + STW, w - hg, vg, w - hg - 2 * srw2 - STW)) # lr serif
    write_svg(ds, "004b")

    # 004c latin capital letter l
    ds <- c(d_rect2(ych, hg + srw + STW, vg, hg + srw), # stem
            d_rect2(ych, hg + 2 * srw + STW, ych - STW, hg), # t serif
            d_rect2(vg + srw + STW, w - hg, vg, w - hg - STW), # lr serif
            d_rect2(vg + STW, w - hg, vg, hg)) # bar
    write_svg(ds, "004c")

    # 004d latin capital letter m
    ds <- c(d_rect2(ych - STW, hg + srw2 + STW, vg + STW, hg + srw2), # l stem
            d_rect2(ych - STW, w - hg - srw2, vg, w - hg - srw2 - STW), # r stem
            d_bslash(ych, xc, vg, hg + srw2 + STW, STW, nib = "vertical"), # l stroke
            d_fslash(ych, w - hg - srw2 - STW, vg, xc, STW, nib = "vertical"), # r stroke
            d_rect2(ych, hg + srw2 + STW, ych - STW, hg), # ul serif
            d_rect2(ych, w - hg, ych - STW, w - hg - srw2 - STW), # ur serif
            d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg), # ll serif
            d_rect2(vg + STW, w - hg, vg, w - hg - 2 * srw2 - STW)) # lr serif
    write_svg(ds, "004d")

    # 004e latin capital letter n
    ds <- c(d_rect2(ych - STW, hg + srw2 + STW, vg + STW, hg + srw2), # l stem
            d_rect2(ych - STW, w - hg - srw2, vg, w - hg - srw2 - STW), # r stem
            d_bslash(ych, w - hg - srw2 - STW, vg, hg + srw2 + STW, STW, nib = "vertical"), # stroke
            d_rect2(ych, hg + srw2 + STW, ych - STW, hg), # ul serif
            d_rect2(ych, w - hg, ych - STW, w - hg - 2 * srw2 - STW), # ur serif
            d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg)) # ll serif
    write_svg(ds, "004e")

    # 004f latin capital letter o
    d <- d_ellipse(xc, yc, 0.5 * cw - c(0, STW), 0.5 * ch - c(0, STW))
    write_svg(d, "004f")

    # 0050 latin capital letter p
    ds <- c(d_rect2(ych, hg + srw + STW, vg, hg + srw), # stem
            d_rect2(ych, xc, ych - STW, hg), # t bar
            d_rect2(vg + STW, hg + 2 * srw + STW, vg, hg), # b serif
            d_rect2(yc + 0.5 * STW, xc, yc - 0.5 * STW, hg + srw), # b bar
            d_arc41(ych, w - hg, yc - 0.5 * STW, xc, STW)) # bowl
    write_svg(ds, "0050")

    # 0051 latin capital letter q
    ylclq <- 0.40 * h
    ds <- c(d_ellipse(xc, yc, 0.5 * cw - c(0, STW), 0.5 * ch - c(0, STW)),
            d_arc1(ylclq, xc + 0.5 * STW, 0.5 * (vg + ylclq), hg, STW),
            d_arc3(0.5 * (vg + ylclq), w - hg, vg, xc - 0.5 * STW, STW))
    write_svg(ds, "0051")

    # 0052 latin capital letter r
    ds <- c(d_rect2(ych, hg + srw2 + STW, vg, hg + srw2), # stem
            d_rect2(ych, xc, ych - STW, hg), # t bar
            d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg), # ll serif
            d_rect2(vg + STW, w - hg, vg, w - hg - 2 * srw2 - STW), # lr serif
            d_bslash(yc, w - hg - srw2, vg + STW, hg + srw2 + STW, STW), # stroke
            d_rect2(yc + 0.5 * STW, xc, yc - 0.5 * STW, hg + srw2), # b bar
            d_arc41(ych, w - hg, yc - 0.5 * STW, xc, STW)) # bowl
    write_svg(ds, "0052")

    # 0053 latin capital letter s
    d_lcls <- c(d_arc23(ych, xc, yc - 0.5 * STW, hg, STW), # l curve
            d_arc41(yc + 0.5 * STW, w - hg, vg, xc, STW), # r curve
            d_arc1(ych, w - hg, ych - cvo, xc, STW), # ur curve
            d_circle(w - hg - rp, ych - cvo, rp), # ball
            d_arc3(vg + cvo, xc, vg, hg, STW)) # lr curve
    write_svg(d_lcls, "0053")

    # 0024 dollar sign
    ds <- c(d_lcls, d_rect(xc, yc, STW, h - 2 * vg + 2 * STW))
    write_svg(ds, "0024")

    # 0054 latin capital letter t
    ds <- c(d_rect2(ych, w - hg, ych - STW, hg), # bar
            d_rect2(ych, xc + 0.5 * STW, vg, xc - 0.5 * STW), # stem
            d_rect2(ych, w - hg, ych - 2 * STW, w - hg - STW), # r serif
            d_rect2(ych, hg + STW, ych - 2 * STW, hg), # l serif
            d_rect2(vg + STW, xc + 1.5 * STW, vg, xc - 1.5 * STW)) # b serif
    write_svg(ds, "0054")

    # 0055 latin capital letter u
    yu <- 0.4 * h
    ds <- c(d_rect2(ych, hg + srw2 + STW, yu, hg + srw2), # l stem
            d_rect2(ych, w - hg - srw2, yu, w - hg - srw2 - STW), # r stem
            d_arc34(yu, w - hg - srw2, vg, hg + srw2, STW), # bottom
            d_rect2(ych, hg + 2 * srw2 + STW, ych - STW, hg), # l serif
            d_rect2(ych, w - hg, ych - STW, w - hg - 2 * srw2 - STW)) # r serif
    write_svg(ds, "0055")

    # 0056 latin capital letter v
    ds <- c(d_bslash(ych - STW, xc, vg, hg + srw2, STW,
                     left = "horizontal", right = "vertical"), # l stroke
            d_fslash(ych - STW, w - hg - srw2, vg, xc, STW,
                     left = "vertical", right = "horizontal"), # r stroke
            d_rect2(ych, hg + 2 * srw2 + STW, ych - STW, hg), # l serif
            d_rect2(ych, w - hg, ych - STW, w - hg - 2 * srw2 - STW)) # r serif
    write_svg(ds, "0056")

    # 0057 latin capital letter w
    if (font == "narrow")
        wstw <- 0.85 * STW
    else
        wstw <- STW
    ds <- c(d_bslash(ych - STW, 0.35 * w, vg, hg + srw3, wstw,
                     left = "horizontal", right = "vertical"), # ll stroke
            d_fslash(ych, xc, vg, 0.35 * w, wstw,
                     left = "vertical", right = "vertical"), # lr stroke
            d_bslash(ych, 0.65 * w, vg, xc, wstw,
                     left = "vertical", right = "vertical"), # rl stroke
            d_fslash(ych - STW, w - hg - srw3, vg, 0.65 * w, wstw,
                     left = "vertical", right = "horizontal"), # rr stroke
            d_rect2(ych, hg + 2 * srw3 + wstw, ych - STW, hg), # l serif
            # d_rect2(ych, xc + srw3, ych - STW, xc - srw3), # m serif
            d_rect2(ych, w - hg, ych - STW, w - hg - 2 * srw3 - wstw)) # r serif
    write_svg(ds, "0057")

    # 0058 latin capital letter x
    ds <- c(d_bslash(ych - STW, w - hg - srw2, vg + STW, hg + srw2, STW), # l stroke
            d_fslash(ych - STW, w - hg - srw2, vg + STW, hg + srw2, STW), # r stroke
            d_rect2(ych, hg + 2 * srw2 + STW, ych - STW, hg), # ul serif
            d_rect2(ych, w - hg, ych - STW, w - hg - 2 * srw2 - STW), # ur serif
            d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg), # bl serif
            d_rect2(vg + STW, w - hg, vg, w - hg - 2 * srw2 - STW)) # br serif
    write_svg(ds, "0058")

    # 0059 latin capital letter y
    d_lcly <- c(d_rect2(ych, hg + 2 * srw2 + STW, ych - STW, hg), # ul serif
            d_rect2(ych, w - hg, ych - STW, w - hg - 2 * srw2 - STW), # ur serif
            d_bslash(ych - STW, xc, yc, hg + srw2, STW,
                     left = "horizontal", right = "square"), # left stroke
            d_fslash(ych - STW, w - hg - srw2, yc, xc, STW,
                     left = "square", right = "horizontal"), # right stroke
            d_rect2(yc, xc + 0.5 * STW, vg, xc - 0.5 * STW), # stem
            d_rect2(vg + STW, xc + 1.5 * STW, vg, xc - 1.5 * STW)) # b serif
    write_svg(d_lcly, "0059")

    # 00a5 yen sign
    ds <- c(d_lcly, 
            d_rect2(yc + 0.8 * STW, w - hg - srw2, yc - 0.2 * STW, hg + srw2), # t bar
            d_rect2(yc - 0.7 * STW, w - hg - srw2, yc - 1.7 * STW, hg + srw2)) # b bar
    write_svg(ds, "00a5")

    # 005a latin capital letter z
    ds <- c(d_rect2(ych, w - hg, ych - STW, hg), # t bar
            d_rect2(vg + STW, w - hg, vg, hg), # b bar
            d_fslash(ych - STW, w - hg, vg + STW, hg, STW), # stroke
            d_rect2(vg + 2 * STW, w - hg, vg, w - hg - STW), # b serif
            d_rect2(ych, hg + STW, ych - 2 * STW, hg)) # t serif
    write_svg(ds, "005a")

    rt <- 300

    # 0063 latin small letter c
    cvo <- 1.7 * STW
    d_slc <- c(d_arc23(yxh, xc, vg, xc - 0.5 * ncw, STW), # l curve
               d_arc1(yxh, xc + 0.5 * ncw, yxh - cvo, xc, STW), # ur curve
               d_circle(xc + 0.5 * ncw - rp, yxh - cvo, rp), # ball
               d_arc4(vg + cvo, xc + 0.5 * ncw, vg, xc, STW)) # lr curve
    write_svg(d_slc, "0063")

    ####  00a9 copyright sign

    # 0254 latin small letter open o "turned c"
    ds <- c(d_arc41(yxh, xc + 0.5 * ncw, vg, xc, STW), # r curve
            d_arc2(yxh, xc, yxh - cvo, xc - 0.5 * ncw, STW), # ul curve
            d_circle(xc - 0.5 * ncw + rp, yxh - cvo, rp), # ball
            d_arc3(vg + cvo, xc, vg, xc - 0.5 * ncw, STW)) # ll curve
    write_svg(ds, "0254")

    #### 1f12f copyleft symbol

    # 00a2 cent sign
    ds <- c(d_slc, d_rect(xc, 0.5 * (vg + yxh), STW, ncw + 2 * STW))
    write_svg(ds, "00a2")

    # 0064 latin small letter d
    xl_ds <- w - hg - rt - srw2
    ds <- c(d_rect2(ych, xl_ds + STW, vg + rt, xl_ds), # stem
            d_rect2(ych, xl_ds, ych - STW, xl_ds - srw2), # t serif
            d_ellipse(0.5 * (hg + xl_ds + STW),
                      0.5 * (vg + yxh),
                      0.5 * (xl_ds + STW - hg) + c(0, -STW),
                      0.5 * (yxh - vg) + c(0, -STW)), # ellipse
            d_arc3(vg + rt, xl_ds + rt, vg, xl_ds, STW), # terminal..
            d_rect2(vg + STW, xl_ds + rt + srw2, vg, xl_ds + rt)) # terminal
    write_svg(ds, "0064")

    # 0065 latin small letter e
    ds <- c(d_arc23(yxh, xc, vg, xc - 0.5 * ncw, STW), # l curve
            d_arc1(yxh, xc + 0.5 * ncw, yxh - cvo - STW, xc, STW), # ur curve
            d_rect2(yxh - cvo - STW + STW, xc + 0.5 * ncw - 30, yxh - cvo - STW, xc - 0.5 * ncw + 30), # bar
            d_arc4(vg + cvo, xc + 0.5 * ncw, vg, xc, STW)) # lr curve
    write_svg(ds, "0065")

    # 0066 latin small letter f
    # yslt <- 0.7 * h
    yslt <- yxh
    ds <- c(d_rect2(ych - 0.2 * h, xc + 0.5 * STW, vg + STW, xc - 0.5 * STW), # stem
            d_arc2(ych, w - hg - rp, ych - 0.2 * h, xc - 0.5 * STW, STW), # ur curve
            d_circle(w - hg - rp, ych - rp, rp), # ball
            d_rect2(yslt, xc + 2.0 * STW, yslt - STW, xc - 2.0 * STW), # bar
            d_rect2(vg + STW, xc + 1.5 * STW, vg, xc - 1.5 * STW)) # b serif
    write_svg(ds, "0066")

    # 0131 latin small letter dotless i
    d_dli <- c(d_rect2(yxh - STW, xc + 0.5 * STW, vg + STW, xc - 0.5 * STW), # stem
               d_rect2(yxh, xc + 0.5 * STW, yxh - STW, xc - 1.5 * STW), # t serif
               d_rect2(vg + STW, xc + 1.5 * STW, vg, xc - 1.5 * STW)) # b serif
    write_svg(d_dli, "0131")

    # 0069 latin small letter i
    d_i <- c(d_dli, d_circle(xc, yxh + 0.7 * STW + rp, rp))
    write_svg(d_i, "0069")

    # 0237 latin small letter dotless j
    d_dlj <- c(d_rect2(yxh - STW, xc + 0.5 * STW, vg + STW, xc - 0.5 * STW), # stem
               d_rect2(yxh, xc + 0.5 * STW, yxh - STW, xc - 1.5 * STW), # t serif
               d_arc4(vg + STW, xc + 0.5 * STW, 0, xc - 2.0 * STW, STW)) # hook
    write_svg(d_dlj, "0237")

    # 006a latin small letter j
    d_j <- c(d_dlj, d_circle(xc, yxh + 0.7 * STW + rp, rp))
    write_svg(d_j, "006a")

    # 006c latin small letter l
    ds <- c(d_rect2(ych - STW, xc + 0.5 * STW, vg + rt, xc - 0.5 * STW), # stem
            d_rect2(ych, xc + 0.5 * STW, ych - STW, xc - 1.5 * STW), # t serif
            d_arc3(vg + rt, xc - 0.5 * STW + rt, vg, xc - 0.5 * STW, STW), # terminal..
            d_rect2(vg + STW, xc + 0.5 * STW + rt, vg, xc - 0.5 * STW + rt)) # terminal
    write_svg(ds, "006c")

    # 006f latin small letter o
    write_svg(d_ellipse(xc, 0.5 * (vg + yxh),
                        0.5 * ncw + c(0, -STW), 0.5 * ncw + c(0, -STW)), "006f")

    # 0072 latin small letter r
    d_lr <- c(d_rect2(yxh - STW, xc - 0.5 * ncw + STW + STW, vg + STW, xc - 0.5 * ncw + STW), # stem
              d_rect2(yxh, xc - 0.5 * ncw + STW + STW, yxh - STW, xc - 0.5 * ncw), # t serif
              d_rect2(vg + STW, xc - 0.5 * ncw + 2 * STW + STW, vg, xc - 0.5 * ncw), # b serif
              d_arc2(yxh, xc + 0.5 * ncw - rp, yxh - 2 * STW, xc - 0.5 * ncw + STW, STW), # curve
              d_circle(xc + 0.5 * ncw - rp, yxh - rp, rp)) # ball
    write_svg(d_lr, "0072")

    # 0073 latin small letter s
    ssvo <- 1.3 * STW
    d_lsls <- c(d_arc23(yxh, xc, 0.5 * (yxh + vg) - 0.5 * STW, xc - 0.5 * ncw, STW), # l curve
            d_arc41(0.5 * (yxh + vg) + 0.5 * STW, xc + 0.5 * ncw, vg, xc, STW), # r curve
            d_arc1(yxh, xc + 0.5 * ncw, yxh - ssvo, xc, STW), # ur curve
            d_circle(xc + 0.5 * ncw - rp, yxh - ssvo, rp), # ball
            d_arc3(vg + ssvo, xc, vg, xc - 0.5 * ncw, STW)) # lr curve
    write_svg(d_lsls, "0073")

    # 00a7 section sign (aka double-s)
    h_s <- 0.56 * h
    ssvo <- 1.6 * STW
    d_lss <- c(
            d_arc1(ych, xc + 0.5 * ncw, ych - ssvo, xc, STW), # t ur curve
            d_arc23(ych, xc, ych - ch / 3 - 0.5 * STW,
                    xc - 0.5 * ncw, STW), # t l curve
            d_arc41(ych - ch / 3 + 0.5 * STW, xc + 0.5 * ncw,
                    vg + ch / 3 - 0.5 * STW, xc, STW), # t r curve
            d_arc3(vg + ch / 3 - 0.5 * STW + ssvo, xc,
                   vg + ch / 3 - 0.5 * STW, xc - 0.5 * ncw, STW), # t lr curve
            d_circle(xc + 0.5 * ncw - rp, ych - ssvo, rp), # ball
            d_arc23(ych - ch / 3 + 0.5 * STW, xc,
                    vg + ch / 3 - 0.5 * STW, xc - 0.5 * ncw, STW), # b l curve
            d_arc41(vg + ch / 3 + 0.5 * STW, xc + 0.5 * ncw, vg, xc, STW), # b r curve
            d_arc1(ych - ch / 3 + 0.5 * STW, xc + 0.5 * ncw, ych - ch / 3 + 0.5 * STW - ssvo, xc, STW), # b ur curve
            d_arc3(vg + ssvo, xc, vg, xc - 0.5 * ncw, STW)) # b lr curve
    write_svg(d_lss, "00a7")

    # 0074 latin small letter t
    # yslt <- 0.7 * h
    yslt <- yxh
    ds <- c(d_rect2(ych, xc + 0.5 * STW, vg + rt, xc - 0.5 * STW), # stem
            d_rect2(ych, xc - 0.5 * STW, ych - STW, xc - 0.5 * STW - STW), # t serif
            d_rect2(yslt, xc + 2.0 * STW, yslt - STW, xc - 2.0 * STW), # bar
            d_arc3(vg + rt, xc - 0.5 * STW + rt, vg, xc - 0.5 * STW, STW), # terminal..
            d_rect2(vg + STW, xc + 0.5 * STW + rt, vg, xc - 0.5 * STW + rt)) # terminal
    write_svg(ds, "0074")

    # 0076 latin small letter v
    ds <- c(d_bslash(yxh - STW, xc, vg, hg + srw2, STW,
                     left = "horizontal", right = "vertical"), # l stroke
            d_fslash(yxh - STW, w - hg - srw2, vg, xc, STW,
                     left = "vertical", right = "horizontal"), # r stroke
            d_rect2(yxh, hg + 2 * srw2 + STW, yxh - STW, hg), # l serif
            d_rect2(yxh, w - hg, yxh - STW, w - hg - 2 * srw2 - STW)) # r serif
    write_svg(ds, "0076")

    # 0077 latin small letter w
    if (font == "narrow")
        wstw <- 0.85 * STW
    else
        wstw <- STW
    ds <- c(d_bslash(yxh - STW, 0.35 * w, vg, hg + srw3, wstw,
                     left = "horizontal", right = "vertical"), # ll stroke
            d_fslash(yxh, xc, vg, 0.35 * w, wstw,
                     left = "vertical", right = "vertical"), # lr stroke
            d_bslash(yxh, 0.65 * w, vg, xc, wstw,
                     left = "vertical", right = "vertical"), # rl stroke
            d_fslash(yxh - STW, w - hg - srw3, vg, 0.65 * w, wstw,
                     left = "vertical", right = "horizontal"), # rr stroke
            d_rect2(yxh, hg + 2 * srw3 + wstw, yxh - STW, hg), # l serif
            # d_rect2(yxh, xc + srw3, yxh - STW, xc - srw3), # m serif
            d_rect2(yxh, w - hg, yxh - STW, w - hg - 2 * srw3 - wstw)) # r serif
    write_svg(ds, "0077")

    # 0078 latin small letter x
    ds <- c(d_bslash(yxh - STW, w - hg - srw2, vg + STW, hg + srw2, STW), # l stroke
            d_fslash(yxh - STW, w - hg - srw2, vg + STW, hg + srw2, STW), # r stroke
            d_rect2(yxh, hg + 2 * srw2 + STW, yxh - STW, hg), # ul serif
            d_rect2(yxh, w - hg, yxh - STW, w - hg - 2 * srw2 - STW), # ur serif
            d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg), # bl serif
            d_rect2(vg + STW, w - hg, vg, w - hg - 2 * srw2 - STW)) # br serif
    write_svg(ds, "0078")

    # 007a latin small letter z
    ds <- c(d_rect2(yxh, xc + 0.5 * ncw, yxh - STW, xc - 0.5 * ncw), # t bar
            d_rect2(vg + STW, xc + 0.5 * ncw, vg, xc - 0.5 * ncw), # b bar
            d_fslash(yxh - STW, xc + 0.5 * ncw, vg + STW, xc - 0.5 * ncw, STW), # stroke
            d_rect2(vg + 2 * STW, xc + 0.5 * ncw, vg, xc + 0.5 * ncw - STW), # b serif
            d_rect2(yxh, xc - 0.5 * ncw + STW, yxh - 2 * STW, xc - 0.5 * ncw)) # t serif
    write_svg(ds, "007a")

    # 005f low line
    d <- d_rect(xc, vg + 0.5 * STW, cw, STW)
    write_svg(d, "005f")

    # 00af macron
    d <- d_rect(xc, ych - 0.5 * STW, cw, STW)
    write_svg(d, "00af")

    # 007c vertical line
    d <- d_rect(xc, yc, STW, ch)
    write_svg(d, "007c")

    # 00a6 broken bar
    bbg <- 0.8 * STW
    d <- d_rect2(ych, xc + 0.5 * STW, yc + bbg, xc - 0.5 * STW) +
         d_rect2(yc - bbg, xc + 0.5 * STW, vg, xc - 0.5 * STW)
    write_svg(d, "00a6")

    # 003a colon
    d_003a <- d_circle(xc, yc + c(-0.25, 0.25) * cw, rp)
    write_svg(d_003a, "003a")

    # 00f7 division sign
    if (font == "narrow")
        d_003a <- d_circle(xc, yc + c(-0.33, 0.33) * cw, rp)
    write_svg(d_002d + d_003a, "00f7")

    # 003c less-than sign
    ds <- c(d_fslash(yc + 0.50 * cw, w - hg, yc, hg, STW,
                     left = "horizontal", right = "vertical"), # t stroke
            d_bslash(yc, w - hg, yc - 0.50 * cw, hg, STW,
                     left = "horizontal", right = "vertical")) # b stroke
    write_svg(ds, "003c")

    waqm <- 0.44 * w

    # 2039 single left-pointing angle quotation mark
    ds <- c(d_fslash(yc + 0.50 * cw, xc + 0.5 * waqm, yc, xc - 0.5 * waqm, STW,
                     left = "horizontal", right = "vertical"), # t stroke
            d_bslash(yc, xc + 0.5 * waqm, yc - 0.50 * cw, xc - 0.5 * waqm, STW,
                     left = "horizontal", right = "vertical")) # b stroke
    write_svg(ds, "2039")

    # 203a single right-pointing angle quotation mark
    ds <- c(d_bslash(yc + 0.50 * cw, xc + 0.5 * waqm, yc, xc - 0.5 * waqm, STW,
                     left = "vertical", right = "horizontal"), # t stroke
            d_fslash(yc, xc + 0.5 * waqm, yc - 0.50 * cw, xc - 0.5 * waqm, STW,
                     left = "vertical", right = "horizontal")) # b stroke
    write_svg(ds, "203a")

    # 00ab left-pointing double angle quotation mark
    ds <- c(d_fslash(yc + 0.50 * cw, hg + waqm, yc, hg, STW,
                     left = "horizontal", right = "vertical"), # l t stroke
            d_bslash(yc, hg + waqm, yc - 0.50 * cw, hg, STW,
                     left = "horizontal", right = "vertical"), # l b stroke
            d_fslash(yc + 0.50 * cw, w - hg, yc, w - hg - waqm, STW,
                     left = "horizontal", right = "vertical"), # r t stroke
            d_bslash(yc, w - hg, yc - 0.50 * cw, w - hg - waqm, STW,
                     left = "horizontal", right = "vertical")) # r b stroke
    write_svg(ds, "00ab")

    # 00bb right-pointing double angle quotation mark
    ds <- c(d_bslash(yc + 0.50 * cw, hg + waqm, yc, hg, STW,
                     left = "vertical", right = "horizontal"), # l t stroke
            d_fslash(yc, hg + waqm, yc - 0.50 * cw, hg, STW,
                     left = "vertical", right = "horizontal"), # l b stroke
            d_bslash(yc + 0.50 * cw, w - hg, yc, w - hg - waqm, STW,
                     left = "vertical", right = "horizontal"), # r t stroke
            d_fslash(yc, w - hg, yc - 0.50 * cw, w - hg - waqm, STW,
                     left = "vertical", right = "horizontal")) # r b stroke
    write_svg(ds, "00bb")

    # 003d equals sign
    d <- d_rect(xc, yc + c(-0.25, 0.25) * cw, cw, STW)
    write_svg(d, "003d")

    # 003e greater-than sign
    ds <- c(d_bslash(yc + 0.50 * cw, w - hg, yc, hg, STW,
                     left = "vertical", right = "horizontal"), # t stroke
            d_fslash(yc, w - hg, yc - 0.50 * cw, hg, STW,
                     left = "vertical", right = "horizontal")) # b stroke
    write_svg(ds, "003e")

    # 00d7 multiplication sign
    ms <- 0.8 * 0.5
    d <- c(d_fslash(yc + ms * cw, xc + ms * cw, yc - ms * cw, xc - ms * cw, STW, nib = "diagonal"),
           d_bslash(yc + ms * cw, xc + ms * cw, yc - ms * cw, xc - ms * cw, STW, nib = "diagonal"))
    write_svg(d, "00d7")

    # 00a8 diaeresis
    write_svg(d_circle(xc + c(-2, 2) * rp, ych - rp, rp), "00a8")

    # 00b7 middle dot
    write_svg(d_circle(xc, yc, rp), "00b7")

    # 2026 horizontal ellipsis
    d <- d_circle(c(hg + rp, xc, h - hg - rp), vg + rp, rp)
    write_svg(d, "2026")

    # 22ee vertical ellipsis
    d <- d_circle(xc, c(vg + rp, yc, ych - rp), rp)
    write_svg(d, "22ee")

    # 22ef midline horizontal ellipsis
    d <- d_circle(c(hg + rp, xc, h - hg - rp), yc, rp)
    write_svg(d, "22ef")

    # 22f0 up right diagonal ellipsis
    d <- d_circle(c(hg + rp, xc, h - hg - rp), c(vg + rp, yc, ych - rp), rp)
    write_svg(d, "22f0")

    # 22f1 down right diagonal ellipsis
    d <- d_circle(c(hg + rp, xc, h - hg - rp), c(ych - rp, yc, vg + rp), rp)
    write_svg(d, "22f1")

    c(as.hexmode("0021"):as.hexmode("0025"),
      as.hexmode("0027"):as.hexmode("0029"),
      as.hexmode("002d"):as.hexmode("0031"),
      as.hexmode("0037"):as.hexmode("0038"),
      as.hexmode("003c"):as.hexmode("003e"),
      as.hexmode("0041"):as.hexmode("005d"),
      as.hexmode("0063"):as.hexmode("0066"),
      as.hexmode("0069"):as.hexmode("006a"),
      as.hexmode("0072"):as.hexmode("0074"),
      as.hexmode("0076"):as.hexmode("0078"),
      as.hexmode("00a1"):as.hexmode("00a2"),
      as.hexmode("00a5"):as.hexmode("00a8"),
      as.hexmode("22ee"):as.hexmode("22f1"),
      as.hexmode(c("002b", "003a",
                   "005f", 
                   "006c", "006f",
                   "007a", "007c",
                   "00ab", "00af",
                   "00b7", "00bb", "00d7", "00f7",
                   "0131", "0186",
                   "0237", "0254",
                   "2026", "2039", "203a"))
    ) |> as_hex()
}
