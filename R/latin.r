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
    d <- RECT(xc + c(-0.25, 0.25) * cw, h - vg - 0.5 * ah, SW, ah)
    write_svg(d, "0022")

    # 0023 number sign
    d <- c(RECT(xc, yc + c(-0.25, 0.25) * cw, cw, SW),
           RECT(xc + c(-0.25, 0.25) * cw, yc, SW, cw))
    write_svg(d, "0023")

    # 002e full stop
    d_002e <- CIRCLE(xc, vg + 0.5 * rp, rp)
    write_svg(d_002e, "002e")

    # 0021 exclamation mark
    d <- d_002e + RECT(xc, yc + 1.5 * rp, SW, ch - 3 * rp)
    write_svg(d, "0021")

    # 0027 apostrophe
    d <- RECT(xc, h - vg - 0.5 * ah, SW, ah)
    write_svg(d, "0027")

    # 002b plus sign
    d <- RECT(xc, yc, SW, cw) + RECT(xc, yc, cw, SW)
    write_svg(d, "002b")

    # 002d hyphen-minus
    d <- RECT(xc, yc, cw, SW)
    write_svg(d, "002d")

    # 004f latin capital letter o
    d <- ELLIPSE(xc, yc, 0.5 * cw, 0.5 * ch) +
        ELLIPSE(xc, yc, 0.5 * cw - SW, 0.5 * ch - SW)
    write_svg(d, "004f")

    # 005f low line
    d <- RECT(xc, vg + 0.5 * SW, cw, SW)
    write_svg(d, "005f")

    # 007c vertical line
    d <- RECT(xc, yc, SW, ch)
    write_svg(d, "007c")

    # 003a colon
    d <- CIRCLE(xc, yc + c(-0.25, 0.25) * cw, rp)
    write_svg(d, "003a")

    # 003d equals sign
    d <- RECT(xc, yc + c(-0.25, 0.25) * cw, cw, SW)
    write_svg(d, "003d")

    c(as.hexmode("0021"):as.hexmode("0023"),
      as.hexmode(c("0027", "002b", "002d", "002e",
                   "004f", "005f", "007c", "003a", "003d"))
    ) |> as_hex()
}
