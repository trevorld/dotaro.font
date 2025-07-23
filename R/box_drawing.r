create_box_drawing <- function(font = "square") {
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

    # 2500 box drawings light horizontal
    d_2500 <- RECT(xc, yc, w, BDL)
    write_svg(d_2500, "2500")
    # 2501 box drawings heavy horizontal
    d_2501 <- RECT(xc, yc, w, BDH)
    write_svg(d_2501, "2501")
    # 2502 box drawings light vertical
    d_2502 <- RECT(xc, yc, BDL, h)
    write_svg(d_2502, "2502")
    # 2503 box drawings heavy vertical
    d_2503 <- RECT(xc, yc, BDH, h)
    write_svg(d_2503, "2503")

    #### 2504...250b light and heavy dashed lines

    # 250c box drawings light down and right
    d_250c <- MZ(x = c(xc - 0.5 * BDL |> rep(2L), w, w, xc + 0.5 * BDL |> rep(2L)),
                 y = c(0, yc + 0.5 * BDL |> rep(2L), yc - 0.5 * BDL |> rep(2L), 0))
    write_svg(d_250c, "250c")
    # 250d box drawings down light and right heavy
    d_250d <- MZ(x = c(xc - 0.5 * BDL |> rep(2L), w, w, xc + 0.5 * BDL |> rep(2L)),
                 y = c(0, yc + 0.5 * BDH |> rep(2L), yc - 0.5 * BDH |> rep(2L), 0))
    write_svg(d_250d, "250d")
    # 250e box drawings down heavy and right light
    d_250e <- MZ(x = c(xc - 0.5 * BDH |> rep(2L), w, w, xc + 0.5 * BDH |> rep(2L)),
                 y = c(0, yc + 0.5 * BDL |> rep(2L), yc - 0.5 * BDL |> rep(2L), 0))
    write_svg(d_250e, "250e")
    # 250f box drawings heavy down and right
    d_250f <- MZ(x = c(xc - 0.5 * BDH |> rep(2L), w, w, xc + 0.5 * BDH |> rep(2L)),
                 y = c(0, yc + 0.5 * BDH |> rep(2L), yc - 0.5 * BDH |> rep(2L), 0))
    write_svg(d_250f, "250f")
    # 2510 box drawings light down and left
    d_2510 <- MZ(x = c(0, 0, xc + 0.5 * BDL |> rep(2L), xc - 0.5 * BDL |> rep(2L)), 
                 y = c(yc - 0.5 * BDL, yc + 0.5 * BDL |> rep(2L), 0, 0, yc - 0.5 * BDL))
    write_svg(d_2510, "2510")
    # 2511 box drawings down light and left heavy
    d_2511 <- MZ(x = c(0, 0, xc + 0.5 * BDL |> rep(2L), xc - 0.5 * BDL |> rep(2L)), 
                 y = c(yc - 0.5 * BDH, yc + 0.5 * BDH |> rep(2L), 0, 0, yc - 0.5 * BDH))
    write_svg(d_2511, "2511")
    # 2512 box drawings down heavy and left light
    d_2512 <- MZ(x = c(0, 0, xc + 0.5 * BDH |> rep(2L), xc - 0.5 * BDH |> rep(2L)), 
                 y = c(yc - 0.5 * BDL, yc + 0.5 * BDL |> rep(2L), 0, 0, yc - 0.5 * BDL))
    write_svg(d_2512, "2512")
    # 2513 box drawings heavy down and left
    d_2513 <- MZ(x = c(0, 0, xc + 0.5 * BDH |> rep(2L), xc - 0.5 * BDH |> rep(2L)), 
                 y = c(yc - 0.5 * BDH, yc + 0.5 * BDH |> rep(2L), 0, 0, yc - 0.5 * BDH))
    write_svg(d_2513, "2513")
    # 2514 box drawings light up and right
    d_2514 <- MZ(x = c(xc - 0.5 * BDL |> rep(2L), xc + 0.5 * BDL |> rep(2L), w, w),
                 y = c(yc - 0.5 * BDL, h, h, yc + 0.5 * BDL |> rep(2L), yc - 0.5 * BDL))
    write_svg(d_2514, "2514")
    # 2515 box drawings up light and right heavy
    d_2515 <- MZ(x = c(xc - 0.5 * BDL |> rep(2L), xc + 0.5 * BDL |> rep(2L), w, w),
                 y = c(yc - 0.5 * BDH, h, h, yc + 0.5 * BDH |> rep(2L), yc - 0.5 * BDH))
    write_svg(d_2515, "2515")
    # 2516 box drawings up heavy and right light
    d_2516 <- MZ(x = c(xc - 0.5 * BDH |> rep(2L), xc + 0.5 * BDH |> rep(2L), w, w),
                 y = c(yc - 0.5 * BDL, h, h, yc + 0.5 * BDL |> rep(2L), yc - 0.5 * BDL))
    write_svg(d_2516, "2516")
    # 2517 box drawings heavy up and right
    d_2517 <- MZ(x = c(xc - 0.5 * BDH |> rep(2L), xc + 0.5 * BDH |> rep(2L), w, w),
                 y = c(yc - 0.5 * BDH, h, h, yc + 0.5 * BDH |> rep(2L), yc - 0.5 * BDH))
    write_svg(d_2517, "2517")
    # 2518 box drawings light up and left
    d_2518 <- MZ(x = c(0, 0, xc - 0.5 * BDL |> rep(2L), xc + 0.5 * BDL |> rep(2L)),
                 y = c(yc - 0.5 * BDL, yc + 0.5 * BDL |> rep(2L), h, h, yc - 0.5 * BDL))
    write_svg(d_2518, "2518")
    # 2519 box drawings up light and left heavy
    d_2519 <- MZ(x = c(0, 0, xc - 0.5 * BDL |> rep(2L), xc + 0.5 * BDL |> rep(2L)),
                 y = c(yc - 0.5 * BDH, yc + 0.5 * BDH |> rep(2L), h, h, yc - 0.5 * BDH))
    write_svg(d_2519, "2519")
    # 251a box drawings up heavy and left light
    d_251a <- MZ(x = c(0, 0, xc - 0.5 * BDH |> rep(2L), xc + 0.5 * BDH |> rep(2L)),
                 y = c(yc - 0.5 * BDL, yc + 0.5 * BDL |> rep(2L), h, h, yc - 0.5 * BDL))
    write_svg(d_251a, "251a")
    # 251b box drawings heavy up and left
    d_251b <- MZ(x = c(0, 0, xc - 0.5 * BDH |> rep(2L), xc + 0.5 * BDH |> rep(2L)),
                 y = c(yc - 0.5 * BDH, yc + 0.5 * BDH |> rep(2L), h, h, yc - 0.5 * BDH))
    write_svg(d_251b, "251b")

    # 2574 box drawings light left
    d_2574 <- RECT(0.25 * w, yc, 0.5 * w, BDL)
    write_svg(d_2574, "2574")
    # 2575 box drawings light up
    d_2575 <- RECT(xc, 0.75 * h, BDL, 0.5 * h)
    write_svg(d_2575, "2575")
    # 2576 box drawings light right
    d_2576 <- RECT(0.75 * w, yc, 0.5 * w, BDL)
    write_svg(d_2576, "2576")
    # 2577 box drawings light down
    d_2577 <- RECT(xc, 0.25 * h, BDL, 0.5 * h)
    write_svg(d_2577, "2577")
    # 2578 box drawings heavy left
    d_2578 <- RECT(0.25 * w, yc, 0.5 * w, BDH)
    write_svg(d_2578, "2578")
    # 2579 box drawings heavy up
    d_2579 <- RECT(xc, 0.75 * h, BDH, 0.5 * h)
    write_svg(d_2579, "2579")
    # 257a box drawings heavy right
    d_257a <- RECT(0.75 * w, yc, 0.5 * w, BDH)
    write_svg(d_257a, "257a")
    # 257b box drawings heavy down
    d_257b <- RECT(xc, 0.25 * h, BDH, 0.5 * h)
    write_svg(d_257b, "257b")

    # 251c box drawings light vertical and right
    d_251c <- c(d_2502, d_2576)
    write_svg(d_251c, "251c")
    # 251d box drawings vertical light and right heavy
    write_svg(c(d_2502, d_257a), "251d")
    # 251e box drawings up heavy and right down light
    write_svg(c(d_2516, d_2577), "251e")
    # 251f box drawings down heavy and right up light
    write_svg(c(d_250e, d_2577), "251f")
    # 2520 box drawings vertical heavy and right light
    write_svg(c(d_2503, d_2576), "2520")
    # 2521 box drawings down light and right up heavy
    write_svg(c(d_2517, d_2577), "2521")
    # 2522 box drawings up light and right down heavy
    write_svg(c(d_250f, d_2575), "2522")
    # 2523 box drawings heavy vertical and right
    write_svg(c(d_2503, d_257a), "2523")
    # 2524 box drawings light vertical and left
    write_svg(c(d_2502, d_2574), "2524")
    # 2525 box drawings vertical light and left heavy
    write_svg(c(d_2502, d_2578), "2525")
    # 2526 box drawings up heavy and left down light
    write_svg(c(d_251a, d_2577), "2526")
    # 2527 box drawings down heavy and left up light
    write_svg(c(d_2512, d_2575), "2527")
    # 2528 box drawings vertical heavy and left light
    write_svg(c(d_2503, d_2574), "2528")
    # 2529 box drawings down light and left up heavy
    write_svg(c(d_251b, d_2577), "2529")
    # 252a box drawings up light and left down heavy
    write_svg(c(d_2513, d_2575), "252a")
    # 252b box drawings heavy vertical and left
    write_svg(c(d_2513, d_2579), "252b")
    # 252c box drawings light down and horizontal
    write_svg(c(d_2500, d_2577), "252c")
    # 252d box drawings left heavy and right down light
    write_svg(c(d_2511, d_2576), "252d")
    # 252e box drawings right heavy and left down light
    write_svg(c(d_250d, d_2574), "252e")
    # 252f box drawings down light and horizontal heavy
    write_svg(c(d_2501, d_2577), "252f")
    # 2530 box drawings down heavy and horizontal light
    write_svg(c(d_2500, d_257b), "2530")
    # 2531 box drawings right light and left down heavy
    write_svg(c(d_2513, d_2576), "2531")
    # 2532 box drawings left light and right down heavy
    write_svg(c(d_250f, d_2574), "2532")
    # 2533 box drawings heavy down and horizontal
    write_svg(c(d_2501, d_257b), "2533")
    # 2534 box drawings light up and horizontal
    write_svg(c(d_2500, d_2575), "2534")
    # 2535 box drawings left heavy and right up light
    write_svg(c(d_2519, d_2576), "2535")
    # 2536 box drawings right heavy and left up light
    write_svg(c(d_2515, d_2574), "2536")
    # 2537 box drawings up light and horizontal heavy
    write_svg(c(d_2501, d_2575), "2537")
    # 2538 box drawings up heavy and horizontal light
    write_svg(c(d_2500, d_2579), "2538")
    # 2539 box drawings right light and left up heavy
    write_svg(c(d_251b, d_2576), "2539")
    # 253a box drawings left light and right up heavy
    write_svg(c(d_2517, d_2574), "253a")
    # 253b box drawings heavy up and horizontal
    write_svg(c(d_2501, d_2579), "253b")
    # 253d box drawings left heavy and right vertical light
    write_svg(c(d_2502, d_2578, d_2576), "253d")
    # 253e box drawings right heavy and left vertical light
    write_svg(c(d_2502, d_257a, d_2574), "253e")

    # 253c box drawings light vertical and horizontal
    write_svg(c(d_2500, d_2502), "253c")
    # 253f box drawings vertical light and horizontal heavy
    write_svg(c(d_2501, d_2502), "253f")
    # 2542 box drawings vertical heavy and horizontal light
    write_svg(c(d_2500, d_2503), "2542")
    # 254b box drawings heavy vertical and horizontal
    write_svg(c(d_2501, d_2503), "254b")

    # 2540 box drawings up heavy and down horizontal light
    write_svg(c(d_2577, d_2500, d_2579), "2540")
    # 2541 box drawings down heavy and up horizontal light
    write_svg(c(d_2575, d_2500, d_257b), "2541")

    # 2543 box drawings left up heavy and right down light
    write_svg(c(d_250c, d_251b), "2543")
    # 2544 box drawings right up heavy and left down light
    write_svg(c(d_2510, d_2517), "2544")
    # 2545 box drawings left down heavy and right up light
    write_svg(c(d_2513, d_2514), "2545")
    # 2546 box drawings right down heavy and left up light
    write_svg(c(d_250f, d_2518), "2546")

    # 2547 box drawings down light and up horizontal heavy
    write_svg(c(d_2577, d_2501, d_2579), "2547")
    # 2548 box drawings up light and down horizontal heavy
    write_svg(c(d_2575, d_2501, d_257b), "2548")
    # 2549 box drawings right light and left vertical heavy
    write_svg(c(d_2576, d_2503, d_2578), "2549")
    # 254a box drawings left light and right vertical heavy
    write_svg(c(d_2574, d_2503, d_257a), "254a")

    # 257c box drawings light left and heavy right
    write_svg(c(d_2574, d_257a), "257c")
    # 257d box drawings light up and heavy down
    write_svg(c(d_2575, d_257b), "257d")
    # 257e box drawings heavy left and light right
    write_svg(c(d_2578, d_2576), "257e")
    # 257f box drawings heavy up and light down
    write_svg(c(d_2579, d_2577), "257f")

    as_hex(c(as.hexmode("2500"):as.hexmode("2503"),
             as.hexmode("250c"):as.hexmode("254b"),
             as.hexmode("2574"):as.hexmode("257f")
             ))
}
