# Basic Latin
create_basic_latin <- function(font = "square") {
    h <- dotaro_height(font)
    w <- dotaro_width(font)
    do.call(local_options, glyph_options(w, h, font))

    vg <- (h - CH) / 2
    hg <- vg
    if (font == "narrow")
        hg <- NHGM * hg
    cw <- w - 2 * hg
    ch <- h - 2 * vg

    xc <- w / 2
    yc <- h / 2

    rp <- 0.75 * SW # period radius
    ah <- 3 * SW # apostrophe height

    # 0022 quotation mark
    d <- d_rect(xc + c(-0.25, 0.25) * cw, h - vg - 0.5 * ah, SW, ah)
    write_svg(d, "0022")

    # 0023 number sign
    d <- c(d_rect(xc, yc + c(-0.25, 0.25) * cw, cw, SW),
           d_rect(xc + c(-0.25, 0.25) * cw, yc, SW, cw))
    write_svg(d, "0023")

    # 0025 percent sign
    if (font == "square") {
        rps <- 300
    } else {
        rps <- 250
    }
    d_0025 <- d_fslash(h - vg, w - 2 * hg, vg, 2 * hg, SW) +
        d_ellipse(hg + rps,
                  h - vg - rps,
                  c(rps, rps - SW), c(rps, rps - SW)) +
        d_ellipse(w - hg - rps,
                  vg + rps,
                  c(rps, rps - SW), c(rps, rps - SW))
    write_svg(d_0025, "0025")

    # 002e full stop
    d_002e <- d_circle(xc, vg + 0.5 * rp, rp)
    write_svg(d_002e, "002e")

    # 002f Solidus (Slash)
    d_002f <- d_fslash(h - vg, w - hg, vg, hg, SW)
    write_svg(d_002f, "002f")

    # 005c Reverse Solidus (Backslash)
    d_005c <- d_bslash(h - vg, w - hg, vg, hg, SW)
    write_svg(d_005c, "005c")


    # 0021 exclamation mark
    d <- d_002e + d_rect(xc, yc + 1.5 * rp, SW, ch - 3 * rp)
    write_svg(d, "0021")

    # 0027 apostrophe
    d <- d_rect(xc, h - vg - 0.5 * ah, SW, ah)
    write_svg(d, "0027")

    # 002b plus sign
    d <- d_rect(xc, yc, SW, cw) + d_rect(xc, yc, cw, SW)
    write_svg(d, "002b")

    # 002d hyphen-minus
    d <- d_rect(xc, yc, cw, SW)
    write_svg(d, "002d")

    # 004f latin capital letter o
    d <- d_ellipse(xc, yc, 0.5 * cw - c(0, SW), 0.5 * ch - c(0, SW))
    write_svg(d, "004f")

    # 005f low line
    d <- d_rect(xc, vg + 0.5 * SW, cw, SW)
    write_svg(d, "005f")

    # 007c vertical line
    d <- d_rect(xc, yc, SW, ch)
    write_svg(d, "007c")

    # 003a colon
    d <- d_circle(xc, yc + c(-0.25, 0.25) * cw, rp)
    write_svg(d, "003a")

    # 003d equals sign
    d <- d_rect(xc, yc + c(-0.25, 0.25) * cw, cw, SW)
    write_svg(d, "003d")

    c(as.hexmode("0021"):as.hexmode("0023"),
      as.hexmode("002d"):as.hexmode("002f"),
      as.hexmode(c("0025", "0027", "002b", "003a", "003d",
                   "004f", "005c", "005f", "007c"))
    ) |> as_hex()
}
