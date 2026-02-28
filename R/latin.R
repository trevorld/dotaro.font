# Basic Latin
create_basic_latin <- function(font = "square") {
	h <- dotaro_height(font)
	w <- dotaro_width(font)
	do.call(local_options, glyph_options(w, h, font))

	ch <- dotaro_cap_height(font)
	cw <- dotaro_cap_width(font)
	vg <- dotaro_vertical_gap(font)
	hg <- dotaro_horizontal_gap(font)
	rt <- dotaro_terminal_radius(font)

	ow <- dotaro_outline_stroke_width(font)
	srw <- dotaro_stroke_width(font)
	srw2 <- dotaro_stroke_width_short(font)
	xh <- dotaro_x_height(font)
	yxh <- vg + xh # x-height
	ych <- h - vg
	xcw <- w - hg

	xc <- w / 2
	yc <- h / 2

	rp <- 0.75 * srw # period radius
	ah <- 3 * srw # apostrophe height
	cdw <- cw / sqrt(2)

	# Bottom y coordinate on the border of an ellipse centered at (cx, cy) given x
	ellipse_y_bottom <- function(x, cx = xc, cy = yc, rx = 0.5 * cw, ry = 0.5 * ch) {
		cy - ry * sqrt(pmax(0, 1 - ((x - cx) / rx)^2))
	}

	# 0022 quotation mark
	d <- d_rect(xc + c(-0.25, 0.25) * cw, ych - 0.5 * ah, srw, ah)
	write_svg(d, "0022")

	#### use vertical slashes?
	# 0023 number sign
	d <- c(
		d_rect(xc, yc + c(-0.20, 0.20) * cw, cw, srw),
		d_rect(xc + c(-0.20, 0.20) * cw, yc, srw, cw)
	)
	write_svg(d, "0023")

	# 0025 percent sign
	rps <- 1.5 * srw
	d_0025 <- d_fslash(ych, w - 2 * hg, vg, 2 * hg, srw) +
		d_ellipse(hg + rps, ych - rps, c(rps, rps - srw), c(rps, rps - srw)) +
		d_ellipse(xcw - rps, vg + rps, c(rps, rps - srw), c(rps, rps - srw))
	write_svg(d_0025, "0025")

	# 002e full stop
	d_full_stop <- d_circle(xc, vg + rp, rp)
	write_svg(d_full_stop, "002e")

	# 002f Solidus (Slash)
	d_002f <- d_fslash(ych, xcw, vg, hg, srw)
	write_svg(d_002f, "002f")

	# 005b left square bracket
	ds <- c(
		d_rect2(ych - srw, xc, vg + srw, xc - srw), # stem
		d_rect2(ych, xc + srw, ych - srw, xc - srw), # t serif
		d_rect2(vg + srw, xc + srw, vg, xc - srw)
	) # b serif
	write_svg(ds, "005b")

	# 005c Reverse Solidus (Backslash)
	d_005c <- d_bslash(ych, xcw, vg, hg, srw)
	write_svg(d_005c, "005c")

	# 005d right square bracket
	ds <- c(
		d_rect2(ych - srw, xc + srw, vg + srw, xc), # stem
		d_rect2(ych, xc + srw, ych - srw, xc - srw), # t serif
		d_rect2(vg + srw, xc + srw, vg, xc - srw)
	) # b serif
	write_svg(ds, "005d")

	# 0021 exclamation mark
	d <- d_full_stop + d_rect(xc, yc + 1.5 * rp, srw, ch - 3 * rp)
	write_svg(d, "0021")

	# 00a1 inverted exclamation mark
	d <- d_circle(xc, ych - rp, rp) +
		d_rect(xc, yc - 1.5 * rp, srw, ch - 3 * rp)
	write_svg(d, "00a1")

	# 003f question mark
	ds <- c(
		d_full_stop, # period
		d_rect2(yc, xc + 0.5 * srw, vg + 3 * rp, xc - 0.5 * srw), # stem
		d_arc412(ych, xcw, yc - srw, hg, srw), # curve
		d_circle(hg + rp, 0.5 * (ych + yc - srw), rp)
	) # ball
	write_svg(ds, "003f")

	# 00bf inverted question mark
	ds <- c(
		d_circle(xc, ych - rp, rp), # period
		d_rect2(ych - 3 * rp, xc + 0.5 * srw, yc, xc - 0.5 * srw), # stem
		d_arc234(yc + srw, xcw, vg, hg, srw)
	) # curve
	write_svg(ds, "00bf")

	# 0027 apostrophe
	d <- d_rect(xc, ych - 0.5 * ah, srw, ah)
	write_svg(d, "0027")

	# 002a asterisk
	if (font == "narrow") {
		astl <- 0.8 * ah
	} else {
		astl <- ah
	}
	astf <- 1.0 * astl
	ds <- c(
		d_rect(xc, yc, srw, 2 * astl),
		d_fslash(yc + 0.66 * astf, xc + astf, yc - 0.66 * astf, xc - astf, srw, nib = "diagonal"),
		d_bslash(yc + 0.66 * astf, xc + astf, yc - 0.66 * astf, xc - astf, srw, nib = "diagonal")
	)
	write_svg(ds, "002a")

	# 002c comma
	d_comma <- c(d_full_stop, d_arc4(vg + rp, xc + rp, vg - rp, xc - rp, 0.5 * srw)) # hook
	write_svg(d_comma, "002c")

	# 003a colon
	d <- d_full_stop + d_circle(xc, yxh - rp, rp)
	write_svg(d, "003a")

	# 003b semi-colon
	d <- d_comma + d_circle(xc, yxh - rp, rp)
	write_svg(d, "003b")

	# 002b plus sign
	d <- d_rect(xc, yc, srw, cw) + d_rect(xc, yc, cw, srw)
	write_svg(d, "002b")

	# 002d hyphen-minus
	d_hyphen <- d_rect(xc, yc, cw, srw)
	write_svg(d_hyphen, "002d")

	# 00f7 division sign
	if (font == "narrow") {
		# need to spread them out
		d_dots <- d_circle(xc, yc + c(-0.33, 0.33) * cw, rp)
	} else {
		d_dots <- d_circle(xc, yc + c(-0.25, 0.25) * cw, rp)
	}
	write_svg(d_hyphen + d_dots, "00f7")

	# 0060 grave accent
	d <- d_bslash(ych, xc + 1.5 * srw, ych - ah, xc - 1.5 * srw, srw)
	write_svg(d, "0060")

	# 00ac not sign
	ds <- c(
		d_rect2(yc + 0.5 * ah, xcw - srw, yc + 0.5 * ah - srw, hg),
		d_rect2(yc + 0.5 * ah, xcw, yc - 0.5 * ah, xcw - srw)
	)
	write_svg(ds, "00ac")

	# 00b0 degree sign
	d <- d_circle(xc, ych - 0.5 * ah, 0.5 * ah + c(0, -srw))
	write_svg(d, "00b0")

	# 00b4 acute accent
	d <- d_fslash(ych, xc + 1.5 * srw, ych - ah, xc - 1.5 * srw, srw)
	write_svg(d, "00b4")

	# 005e circumflex accent
	ds <- c(
		d_fslash(ych, xc, ych - ah, xc - 2 * srw, srw, left = "horizontal", right = "vertical"),
		d_bslash(ych, xc + 2 * srw, ych - ah, xc, srw, left = "vertical", right = "horizontal")
	)
	write_svg(ds, "005e")

	# 02c7 caron
	ds <- c(
		d_bslash(ych, xc, ych - ah, xc - 2 * srw, srw, left = "horizontal", right = "vertical"),
		d_fslash(ych, xc + 2 * srw, ych - ah, xc, srw, left = "vertical", right = "horizontal")
	)
	write_svg(ds, "02c7")

	# 0028 left parenthesis
	w_par <- 2 * srw
	d <- d_arc23(ych, xc + 0.5 * w_par, vg, xc - 0.5 * w_par, srw)
	write_svg(d, "0028")
	# 0029 right parenthesis
	d <- d_arc41(ych, xc + 0.5 * w_par, vg, xc - 0.5 * w_par, srw)
	write_svg(d, "0029")

	# # 007e tilde
	ry <- (5 / 6) * srw
	ds <- c(
		d_arc2(yc + ry, hg + 0.25 * cw, yc - ry, hg, srw),
		d_arc4(yc + ry, xcw, yc - ry, xcw - 0.25 * cw, srw)
	)
	dx1 <- 0.36 * cw
	dy1 <- 0.95 * srw
	dy <- 0.5 * srw
	d_middle <- M(hg + 0.25 * cw, yc + ry) +
		Q(hg + dx1, yc + dy1, xc, yc + dy) +
		T(xcw - 0.25 * cw, yc - ry + srw) +
		L(xcw - 0.25 * cw, yc - ry) +
		Q(xcw - dx1, yc - dy1, xc, yc - dy) +
		TZ(hg + 0.25 * cw, yc + ry - srw)
	write_svg(c(ds, d_middle), "007e")

	# 0030 digit zero
	ds <- c(
		d_ellipse(x = xc, y = yc, rx = xc - c(hg, hg + srw), ry = yc - c(vg, vg + srw)), # loop
		d_rect(x = xc, y = yc, w = srw, h = 0.6 * yc) # inside "dot/slash"
	)
	write_svg(ds, "0030")
	# 1ccf0 mathematical outline digit zero (derived via OUTLINE_FROM_TO)

	# 1d7ce mathematical bold digit zero
	xbd0 <- xc + 0.5 * srw
	ybd0 <- ellipse_y_bottom(xbd0)
	ds <- c(
		d_ellipse(xc, yc, 0.5 * cw + c(0, -2), 0.5 * ch + c(0, -2)),
		M(xbd0, h - ybd0) + AZ(0.5 * cw, 0.5 * ch, x = xbd0, y = ybd0),
		M(w - xbd0, ybd0) + AZ(0.5 * cw, 0.5 * ch, x = w - xbd0, y = h - ybd0)
	)
	write_svg(ds, "1d7ce")
	# 1d7d8 mathematical double-struck digit zero (derived via OUTLINE_FROM_TO)

	# 0031 digit one
	ds <- c(
		d_rect(x = xc, y = yc, w = srw, h = h - 2 * vg - 2 * srw), # stem,
		d_rect2(ych, xc + 0.5 * srw, ych - srw, xc - 1.5 * srw), # t serif,
		d_rect2(vg + srw, xc + 1.5 * srw, vg, xc - 1.5 * srw)
	) # b serif
	write_svg(ds, "0031")
	# 1ccf1 mathematical outline digit one (derived via OUTLINE_FROM_TO)
	# 1d7cf mathematical bold digit one
	ds <- c(
		d_rect(x = xc, y = yc, w = srw, h = h - 2 * vg - 2), # stem,
		d_rect2(ych, xc + 0.5 * srw, ych - 1, xc - 1.5 * srw), # t serif,
		d_rect2(vg + 1, xc + 1.5 * srw, vg, xc - 1.5 * srw)
	) # b serif
	write_svg(ds, "1d7cf")
	# 1d7d9 mathematical double-struck digit one (derived via OUTLINE_FROM_TO)

	# 0032 digit two
	d2ry <- 0.30 * ch
	# multiplier for the quadratic bezier curve
	if (font == "narrow") {
		d2_m <- 1.5
		d2_yq <- yc
	} else {
		d2_m <- 2.0
		d2_yq <- yc
	}
	ds <- c(
		d_arc12(ych, xcw, ych - d2ry, hg, srw), # top curve
		d_circle(hg + rp, ych - d2ry, rp), # ball
		M(xcw, ych - d2ry) + # curved stroke
			Q(xcw, d2_yq, hg + d2_m * srw, vg + srw) +
			H(hg) +
			Q(xcw - d2_m * srw, d2_yq, xcw - srw, ych - d2ry) +
			Z(),
		d_rect2(vg + srw, xcw, vg, hg), # bar
		d_rect2(vg + 2 * srw, xcw, vg + srw, xcw - srw)
	) # lr serif
	write_svg(ds, "0032")
	# 1ccf2 mathematical outline digit two (derived via OUTLINE_FROM_TO)
	#### 1d7d0 mathematical bold digit two
	# write_svg(???, "1d7d0")
	# 1d7da mathematical double-struck digit two (derived via OUTLINE_FROM_TO)

	#### 218a turned digit two

	# 0033 digit three
	if (font == "narrow") {
		d3o <- 0.6 * srw
	} else {
		d3o <- 0.8 * srw
	}
	ds <- c(
		d_rect2(ych, hg + srw, ych - srw - srw, hg), # ul serif
		d_rect2(ych, xcw, ych - srw, hg + srw), # bar
		d_fslash(ych - srw, xcw, yc, xc - d3o, srw, right = "horizontal", left = "diagonal"), # stroke
		d_arc41(yc + 1.0 * srw, xcw, vg, xc, srw), # b curve 1
		d_arc3(vg + 3 * srw, xc, vg, hg, srw)
	) # b curve 2
	write_svg(ds, "0033")
	# 1ccf3 mathematical outline digit three (derived via OUTLINE_FROM_TO)
	#### 1d7d1 mathematical bold digit three
	# write_svg(???, "1d7d1")
	# 1d7db mathematical double-struck digit three (derived via OUTLINE_FROM_TO)

	# 218b turned digit three
	ds <- c(
		d_rect2(vg + srw + srw, xcw - srw, vg, xcw), # lr serif
		d_rect2(vg + srw, xcw - srw, vg, hg), # bar
		d_fslash(yc, xc + d3o, vg + srw, hg, srw, right = "diagonal", left = "horizontal"), # stroke
		d_arc23(ych, xc, yc - 1.0 * srw, hg, srw), # t curve 1
		d_arc1(ych, xcw, ych - 3 * srw, xc, srw), # t curve 2
		d_circle(xcw - rp, ych - 3 * srw, rp)
	) # ball
	write_svg(ds, "218b")

	# 0034 digit four
	yd4b <- yc - 1.0 * srw
	ds <- c(
		d_rect2(yd4b + 1 * srw, xcw - srw, vg + srw, xcw - srw - srw), # stem
		d_rect2(vg + srw, xcw, vg, xcw - srw - 2 * srw), # b serif
		d_rect2(yd4b, xcw, yd4b - 1.0 * srw, hg), # bar
		d_fslash(ych, xcw - srw, yd4b, hg, srw, left = "horizontal", right = "vertical")
	) # stroke
	write_svg(ds, "0034")
	# 1ccf4 mathematical outline digit four (derived via OUTLINE_FROM_TO)
	# 1d7d2 mathematical bold digit four
	ds <- c(
		d_rect2(yd4b + srw, xcw - srw, vg + 2, xcw - srw - srw), # stem
		d_rect2(vg + 2, xcw, vg, xcw - srw - 2 * srw), # b serif
		d_rect2(yd4b, xcw, yd4b - 2, hg), # bar
		d_fslash(ych, xcw - srw, yd4b, hg, srw, left = "horizontal", right = "vertical")
	) # stroke
	write_svg(ds, "1d7d2")
	# 1d7dc mathematical double-struck digit four (derived via OUTLINE_FROM_TO)

	# 0035 digit five
	ds <- c(
		d_rect2(ych, xcw, ych - 2 * srw, xcw - srw), # ur serif
		d_rect2(ych, xcw - srw, ych - srw, hg + 0.5 * srw), # bar
		d_rect2(ych - srw, hg + srw + 0.5 * srw, yc - 0.0 * srw, hg + 0.5 * srw), # stem
		d_rect2(yc + 1.0 * srw, xc, yc - 0.0 * srw, hg + srw + 0.5 * srw), # curve 1
		d_arc41(yc + 1.0 * srw, xcw, vg, xc, srw), # curve 2
		d_arc3(0.5 * (vg + yc + 1.0 * srw), xc, vg, hg, srw)
	) # curve 3
	write_svg(ds, "0035")
	# 1ccf5 mathematical outline digit five (derived via OUTLINE_FROM_TO)
	# 1d7d3 mathematical bold digit five
	cy5b <- 0.5 * (yc + 2 + vg)
	rx5b <- xcw - xc
	ry5b <- 0.5 * (yc + 2 - vg)
	xbd5 <- xc + 0.5 * srw
	yb5 <- ellipse_y_bottom(xbd5, cy = cy5b, rx = rx5b, ry = ry5b)
	ds <- c(
		d_rect2(ych, xcw, ych - 4, xcw - 2), # ur serif
		d_rect2(ych, xcw - 2, ych - 2, hg + 0.5 * srw), # bar
		d_rect2(ych - 2, hg + srw + 0.5 * srw, yc, hg + 0.5 * srw), # stem
		d_rect2(yc + 2, xc, yc, hg + srw + 0.5 * srw), # curve 1
		d_arc41(yc + 2, xcw, vg, xc, 2), # curve 2
		d_arc3(0.5 * (vg + yc + 2), xc, vg, hg, 2), # curve 3
		M(xbd5, 2 * cy5b - yb5) + AZ(rx5b, ry5b, x = xbd5, y = yb5) # partial ellipse
	)
	write_svg(ds, "1d7d3")
	# 1d7dd mathematical double-struck digit five (derived via OUTLINE_FROM_TO)

	# 0036 digit six
	d6yf <- 0.3
	ds <- c(
		d_ellipse(xc, vg + d6yf * ch, 0.5 * cw + c(0, -srw), d6yf * ch + c(0, -srw)), # loop
		d_arc2(ych, xcw - rp, vg + d6yf * ch, hg, srw), # upper curve
		d_circle(xcw - rp, ych - rp, rp)
	) # ball
	write_svg(ds, "0036")
	# 1ccf6 mathematical outline digit six (derived via OUTLINE_FROM_TO)
	#### 1d7d4 mathematical bold digit six
	# write_svg(???, "1d7d4")
	# 1d7de mathematical double-struck digit six (derived via OUTLINE_FROM_TO)

	# 0037 digit seven
	d7stw <- width_slash_left(xcw - (xc - 0.5 * srw), (ych - srw) - (vg + srw), srw)
	ds <- c(
		d_rect2(ych, hg + srw, ych - srw - srw, hg), # ul serif
		d_rect2(ych, xcw, ych - srw, hg + srw), # bar
		d_fslash(ych - srw, xcw, vg + srw, xc - 0.5 * srw, srw), # stroke
		d_rect2(vg + srw, xc + d7stw + 0.5 * srw, vg, xc - 0.5 * srw - srw) # b serif
	)
	write_svg(ds, "0037")
	# 1ccf7 mathematical outline digit seven (derived via OUTLINE_FROM_TO)
	# 1d7d5 mathematical bold digit seven
	d7stw <- width_slash_left(xcw - (xc - 0.5 * srw), (ych - 2) - (vg + 2), srw)
	ds <- c(
		d_rect2(ych, hg + 2, ych - srw, hg), # ul serif
		d_rect2(ych, xcw, ych - 2, hg + 2), # bar
		d_fslash(ych - 2, xcw, vg + 2, xc - 0.5 * srw, srw), # stroke
		d_rect2(vg + 2, xc + d7stw + 0.5 * srw, vg, xc - 0.5 * srw - srw) # b serif
	)
	write_svg(ds, "1d7d5")
	# 1d7df mathematical double-struck digit seven (derived via OUTLINE_FROM_TO)

	# 0038 digit eight
	ov8 <- (5 / 18) * srw # overlap amount
	ds <- c(
		d_ellipse(
			x = xc,
			y = 0.5 * (yc + ych - ov8),
			rx = 0.5 * cw - c(0, srw),
			ry = 0.5 * (ov8 + yc - vg) - c(0, srw)
		), # top
		d_ellipse(
			x = xc,
			y = 0.5 * (yc + vg + ov8),
			rx = 0.5 * cw - c(0, srw),
			ry = 0.5 * (ov8 + yc - vg) - c(0, srw)
		) # bottom
	)
	write_svg(ds, "0038")
	# 1ccf8 mathematical outline digit eight (derived via OUTLINE_FROM_TO)
	# 1d7d6 mathematical bold digit eight
	rx8b <- 0.5 * cw
	ry8b <- 0.25 * ch
	cy_top8b <- 0.5 * (yc + ych)
	cy_bot8b <- 0.5 * (yc + vg)
	xbd8 <- xc + 0.5 * srw
	yb_top8b <- ellipse_y_bottom(xbd8, cy = cy_top8b, rx = rx8b, ry = ry8b)
	yb_bot8b <- ellipse_y_bottom(xbd8, cy = cy_bot8b, rx = rx8b, ry = ry8b)
	ds <- c(
		d_ellipse(
			x = xc,
			y = cy_top8b,
			rx = rx8b - c(0, 2),
			ry = ry8b - c(0, 2)
		), # top
		d_ellipse(
			x = xc,
			y = cy_bot8b,
			rx = rx8b - c(0, 2),
			ry = ry8b - c(0, 2)
		), # bottom
		M(xbd8, 2 * cy_top8b - yb_top8b) + AZ(rx8b, ry8b, x = xbd8, y = yb_top8b),
		M(w - xbd8, yb_top8b) + AZ(rx8b, ry8b, x = w - xbd8, y = 2 * cy_top8b - yb_top8b),
		M(xbd8, 2 * cy_bot8b - yb_bot8b) + AZ(rx8b, ry8b, x = xbd8, y = yb_bot8b),
		M(w - xbd8, yb_bot8b) + AZ(rx8b, ry8b, x = w - xbd8, y = 2 * cy_bot8b - yb_bot8b)
	)
	write_svg(ds, "1d7d6")
	# 1d7e0 mathematical double-struck digit eight (derived via OUTLINE_FROM_TO)

	# 0039 digit nine
	d6yf <- 0.3
	d9so <- srw / 3
	d9stw <- width_slash_left(
		(xcw - d9so) - (xc - 0.5 * srw),
		(ych - d6yf * ch - srw) - (vg + srw),
		srw
	)
	ds <- c(
		d_ellipse(xc, ych - d6yf * ch, 0.5 * cw + c(0, -srw), d6yf * ch + c(0, -srw)), # loop
		d_fslash(ych - d6yf * ch - srw, xcw - d9so, vg + srw, xc - 0.5 * srw, srw), # lower stroke
		d_rect2(vg + srw, xc + d9stw + 0.5 * srw, vg, xc - 0.5 * srw - srw) # b serif
	)
	write_svg(ds, "0039")
	# 1ccf9 mathematical outline digit nine (derived via OUTLINE_FROM_TO)
	#### 1d7d7 mathematical bold digit nine
	# write_svg(???, "1d7d7")
	# 1d7e1 mathematical double-struck digit nine (derived via OUTLINE_FROM_TO)

	# number ten (private use area since doesn't exist in Unicode)
	# PUA f590 (for now)
	ten_rx <- 0.5 * (w - 2 * hg - 2.00 * srw)
	ds <- c(
		d_rect2(ych, hg + srw, vg, hg), # one
		d_ellipse(xcw - ten_rx, yc, ten_rx + c(0, -srw), 0.5 * ch + c(0, -srw))
	)
	write_svg(ds, "f590")

	# 0041 latin capital letter a
	if (font == "square") {
		yca <- 0.37 * h
	} else {
		yca <- 0.35 * h
	}
	Asw <- width_slash_left(
		xc - (hg + srw2),
		ych - (vg + srw),
		srw,
		left = "horizontal",
		right = "vertical"
	)
	ds_A_base <- c(
		d_fslash(ych, xc, vg + srw, hg + srw2, srw, left = "horizontal", right = "vertical"), # l stroke
		d_bslash(ych, xcw - srw2, vg + srw, xc, srw, left = "vertical", right = "horizontal"), # r stroke
		d_rect2(vg + srw, hg + Asw + 2 * srw2, vg, hg), # ll serif
		d_rect2(vg + srw, xcw, vg, xcw - Asw - 2 * srw2)
	) # lr serif
	ds <- c(
		ds_A_base,
		d_rect2(yca + 0.5 * srw, 0.7 * w, yca - 0.5 * srw, 0.3 * w) # crossbar
	)
	write_svg(ds, "0041")
	# 1ccd6 outlined latin capital letter a (derived via OUTLINE_FROM_TO)

	# 20b3 austral sign
	ds <- c(
		ds_A_base,
		d_rect2(yca + 1.6 * srw, xcw, yca + 0.6 * srw, hg), # t. crossbar
		d_rect2(yca + 0.1 * srw, xcw, yca - 0.9 * srw, hg) # b. crossbar
	)
	write_svg(ds, "20b3")

	# 0042 latin capital letter b
	d_lclb <- c(
		d_rect2(ych, hg + srw + srw, vg, hg + srw), # stem
		d_rect2(ych, xc, ych - srw, hg), # u bar
		d_rect2(yc + 0.5 * srw, xc, yc - 0.5 * srw, hg + srw), # m bar
		d_rect2(vg + srw, xc, vg, hg), # b bar
		d_arc41(ych, xcw, yc - 0.5 * srw, xc, srw), # u bowl
		d_arc41(yc + 0.5 * srw, xcw, vg, xc, srw)
	) # b bowl
	write_svg(d_lclb, "0042")
	# 1ccd7 outlined latin capital letter b (derived via OUTLINE_FROM_TO)

	# 0e3f thai currency symbol baht
	ds <- c(
		d_rect2(ych, hg + srw + srw, vg, hg + srw), # stem
		d_rect2(ych, xc, ych - srw, hg), # u bar
		d_rect2(yc + 0.5 * srw, xc, yc - 0.5 * srw, hg + srw), # m bar
		d_rect2(vg + srw, xc, vg, hg), # b bar
		d_arc41(ych, xcw, yc - 0.5 * srw, xc, srw), # u bowl
		d_arc41(yc + 0.5 * srw, xcw, vg, xc, srw), # b bowl
		d_rect(xc, yc, srw, ch + 2 * srw)
	)
	write_svg(ds, "0e3f")

	# 20bf bitcoin sign
	xbs <- xc + 0.84 * srw
	ds <- c(
		d_rect2(ych, hg + srw + srw, vg, hg + srw), # stem
		d_rect2(ych, xbs, ych - srw, hg), # u bar
		d_rect2(yc + 0.5 * srw, xbs, yc - 0.5 * srw, hg + srw), # m bar
		d_rect2(vg + srw, xbs, vg, hg), # b bar
		d_arc41(ych, xcw, yc - 0.5 * srw, xbs, srw), # u bowl
		d_arc41(yc + 0.5 * srw, xcw, vg, xbs, srw), # b bowl
		d_rect2(h - vg + srw, xc + 0.9 * srw, h - vg, xc + -0.1 * srw),
		d_rect2(h - vg + srw, xc - 0.5 * srw, h - vg, xc - 1.5 * srw),
		d_rect2(vg, xc + 0.9 * srw, vg - srw, xc + -0.1 * srw),
		d_rect2(vg, xc - 0.5 * srw, vg - srw, xc - 1.5 * srw)
	)
	write_svg(ds, "20bf")

	# 0043 latin capital letter c
	Cvo <- 0.25 * ch
	d_clc <- c(
		d_arc23(ych, xc, vg, hg, srw), # l curve
		d_arc1(ych, xcw, ych - Cvo, xc, srw), # ur curve
		d_circle(xcw - rp, ych - Cvo, rp), # ball
		d_arc4(vg + Cvo, xcw, vg, xc, srw)
	) # lr curve
	write_svg(d_clc, "0043")
	# 1ccd8 outlined latin capital letter c (derived via OUTLINE_FROM_TO)

	# 20b5 cedi sign
	ds <- c(d_clc, d_rect(xc, yc, srw, ch + 2 * srw))
	write_svg(ds, "20b5")

	#### 20a1 colon sign
	#### 20a2 cruzeiro sign

	# 20ac euro sign
	euvo <- 2.0 * srw
	ds <- c(
		d_arc23(ych, xc + 0.5 * srw, vg, hg + srw, srw), # l curve
		d_arc1(ych, xcw, ych - euvo, xc + 0.5 * srw, srw), # ur curve
		d_circle(xcw - rp, ych - euvo, rp), # ball
		d_arc4(vg + euvo, xcw, vg, xc + 0.5 * srw, srw), # lr curve
		d_rect(0.5 * (hg + xcw - srw), yc + c(-0.7, 0.7) * srw, cw - srw, srw)
	) # bars
	write_svg(ds, "20ac")

	# 0186 latin capital letter open o "turned c"
	ds <- c(
		d_arc41(ych, xcw, vg, xc, srw), # r curve
		d_arc2(ych, xc, ych - Cvo, hg, srw), # ul curve
		d_circle(hg + rp, ych - Cvo, rp), # ball
		d_arc3(vg + Cvo, xc, vg, hg, srw)
	) # ll curve
	write_svg(ds, "0186")

	# 0044 latin capital letter d
	ds <- c(
		d_rect2(ych, hg + srw + srw, vg, hg + srw), # stem
		d_rect2(ych, xc, ych - srw, hg), # u bar
		d_rect2(vg + srw, xc, vg, hg), # b bar
		d_arc41(ych, xcw, vg, xc, srw)
	) # bowl
	write_svg(ds, "0044")
	# 1ccd9 outlined latin capital letter d (derived via OUTLINE_FROM_TO)

	# 0045 latin capital letter e
	ds <- c(
		d_rect2(ych, hg + srw + srw, vg, hg + srw), # stem
		d_rect2(ych, xcw, ych - srw, hg), # t bar
		d_rect2(yc + 0.5 * srw, 0.7 * w, yc - 0.5 * srw, hg + srw), # m bar
		d_rect2(vg + srw, xcw, vg, hg), # b bar
		d_rect2(ych, xcw, ych - srw - srw, xcw - srw), # ur serif
		d_rect2(vg + srw + srw, xcw, vg, xcw - srw)
	) # lr serif
	write_svg(ds, "0045")
	# 1ccda outlined latin capital letter e (derived via OUTLINE_FROM_TO)

	# 0046 latin capital letter f
	ds <- c(
		d_rect2(ych, hg + srw + srw, vg, hg + srw), # stem
		d_rect2(ych, xcw, ych - srw, hg), # t bar
		d_rect2(yc + 0.5 * srw, 0.7 * w, yc - 0.5 * srw, hg + srw), # m bar
		d_rect2(vg + srw, hg + 2 * srw + srw, vg, hg), # b serif
		d_rect2(ych, xcw, ych - srw - srw, xcw - srw)
	) # t serif
	write_svg(ds, "0046")
	# 1ccdb outlined latin capital letter f (derived via OUTLINE_FROM_TO)

	# 20a3 french franc sign
	ds <- c(
		d_rect2(ych, hg + srw + srw, vg, hg + srw), # stem
		d_rect2(ych, xcw, ych - srw, hg), # t bar
		d_rect2(yc + 0.5 * srw, 0.7 * w, yc - 0.5 * srw, hg + srw), # m bar
		d_rect2(0.5 * (vg + yc) + 0.75 * srw, hg + 2 * srw + srw, 0.5 * (vg + yc) - 0.25 * srw, hg), # l bar
		d_rect2(vg + srw, hg + 2 * srw + srw, vg, hg), # b serif
		d_rect2(ych, xcw, ych - srw - srw, xcw - srw)
	) # t serif
	write_svg(ds, "20a3")

	# 0047 latin capital letter g
	ghw <- 3.0 * srw
	d_clg <- c(
		d_arc23(ych, xc, vg, hg, srw), # l curve
		d_arc1(ych, xcw, ych - Cvo, xc, srw), # ur curve
		d_circle(xcw - rp, ych - Cvo, rp), # ball
		d_arc4(vg + Cvo, xcw, vg, xc, srw), # lr curve
		d_rect2(vg + Cvo + srw, xcw, vg + Cvo, xcw - srw), # hook 1
		d_rect2(vg + Cvo + 2 * srw, xcw, vg + Cvo + srw, xcw - ghw)
	) # hook 2
	write_svg(d_clg, "0047")
	# 1ccdc outlined latin capital letter g (derived via OUTLINE_FROM_TO)

	# 20b2 guarani sign
	if (font == "narrow") {
		ghw <- 1.7 * srw
	} else {
		ghw <- 3.0 * srw
	}
	ds <- c(
		d_arc23(ych, xc, vg, hg, srw), # l curve
		d_arc1(ych, xcw, ych - Cvo, xc, srw), # ur curve
		d_circle(xcw - rp, ych - Cvo, rp), # ball
		d_arc4(vg + Cvo, xcw, vg, xc, srw), # lr curve
		d_rect2(vg + Cvo + srw, xcw, vg + Cvo, xcw - srw), # hook 1
		d_rect2(vg + Cvo + 2 * srw, xcw, vg + Cvo + srw, xcw - ghw), # hook 2
		d_rect(xc, yc, srw, ch + 2 * srw)
	) # stem
	write_svg(ds, "20b2")

	# 0048 latin capital letter h
	ds <- c(
		d_rect2(ych, hg + srw2 + srw, vg, hg + srw2), # left stem
		d_rect2(yc + 0.5 * srw, xcw - srw2, yc - 0.5 * srw, hg + srw2), # crossbar
		d_rect2(ych, xcw - srw2, vg, xcw - srw2 - srw), # right stem
		d_rect2(vg + srw, hg + srw + 2 * srw2, vg, hg), # ll serif
		d_rect2(ych, hg + srw + 2 * srw2, ych - srw, hg), # ul serif
		d_rect2(vg + srw, xcw, vg, xcw - srw - 2 * srw2), # lr serif
		d_rect2(ych, xcw, ych - srw, xcw - srw - 2 * srw2)
	) # ur serif
	write_svg(ds, "0048")
	# 1ccdd outlined latin capital letter h (derived via OUTLINE_FROM_TO)

	# 0049 latin capital letter i
	ds <- c(
		d_rect2(ych, xc + 0.5 * srw, vg, xc - 0.5 * srw), # bar
		d_rect2(ych, xc + 0.5 * srw + srw, ych - srw, xc - 0.5 * srw - srw), # t serif
		d_rect2(vg + srw, xc + 0.5 * srw + srw, vg, xc - 0.5 * srw - srw)
	) # b serif
	write_svg(ds, "0049")
	# 1ccde outlined latin capital letter i (derived via OUTLINE_FROM_TO)

	# 004a latin capital letter j
	ds <- c(
		d_rect2(ych, xcw, ych - srw, xcw - 3 * srw), # t serif
		d_rect2(ych - srw, xcw - srw, 0.4 * h, xcw - 2 * srw), # bar
		d_arc34(0.4 * h, xcw - srw, vg, hg, srw)
	) # hook
	write_svg(ds, "004a")
	# 1ccdf outlined latin capital letter j (derived via OUTLINE_FROM_TO)

	# 004b latin capital letter k
	Ksw <- width_slash_right(
		(xcw - srw2) - (hg + srw2 + srw),
		ych - srw - yc,
		srw,
		left = "square",
		right = "horizontal"
	)
	d_clk <- c(
		d_rect2(ych, hg + srw2 + srw, vg, hg + srw2), # l stem
		d_rect2(ych, hg + srw + 2 * srw2, ych - srw, hg), # ul serif
		d_rect2(ych, xcw, ych - srw, xcw - Ksw - 2 * srw2), # ur serif
		d_fslash(
			ych - srw,
			xcw - srw2,
			yc,
			hg + srw2 + srw,
			srw,
			left = "square",
			right = "horizontal"
		), # t stroke
		d_bslash(
			yc,
			xcw - srw2,
			vg + srw,
			hg + srw2 + srw,
			srw,
			left = "square",
			right = "horizontal"
		), # b stroke
		d_rect2(vg + srw, hg + srw + 2 * srw2, vg, hg), # ll serif
		d_rect2(vg + srw, xcw, vg, xcw - Ksw - 2 * srw2)
	) # lr serif
	write_svg(d_clk, "004b")
	# 1cce0 outlined latin capital letter k (derived via OUTLINE_FROM_TO)

	# 20ad kip sign
	if (font == "narrow") {
		xks <- 0.8 * w
	} else {
		xks <- 0.7 * w
	}
	ds <- c(d_clk, d_rect2(yc + 0.5 * srw, xks, yc - 0.5 * srw, hg))
	write_svg(ds, "20ad")

	# 004c latin capital letter l
	ds <- c(
		d_rect2(ych, hg + srw + srw, vg, hg + srw), # stem
		d_rect2(ych, hg + 2 * srw + srw, ych - srw, hg), # t serif
		d_rect2(vg + srw + srw, xcw, vg, xcw - srw), # lr serif
		d_rect2(vg + srw, xcw, vg, hg)
	) # bar
	write_svg(ds, "004c")
	# 1cce1 outlined latin capital letter l (derived via OUTLINE_FROM_TO)

	# 004d latin capital letter m
	ds <- c(
		d_rect2(ych - srw, hg + srw + srw2, vg + srw, hg + srw2), # l stem
		d_rect2(ych - srw, xcw - srw2, vg, xcw - srw - srw2), # r stem
		d_bslash(ych, xc, vg, hg + srw + srw2, srw, nib = "vertical"), # l stroke
		d_fslash(ych, xcw - srw - srw2, vg, xc, srw, nib = "vertical"), # r stroke
		d_rect2(ych, hg + srw + srw2, ych - srw, hg), # ul serif
		d_rect2(ych, xcw, ych - srw, xcw - srw - srw2), # ur serif
		d_rect2(vg + srw, hg + 2 * srw2 + srw, vg, hg), # ll serif
		d_rect2(vg + srw, xcw, vg, xcw - 2 * srw2 - srw)
	) # lr serif
	write_svg(ds, "004d")
	# 1cce2 outlined latin capital letter m (derived via OUTLINE_FROM_TO)

	# 004e latin capital letter n
	d_cln <- c(
		d_rect2(ych - srw, hg + srw + srw, vg + srw, hg + srw), # l stem
		d_rect2(ych - srw, xcw - srw, vg, xcw - srw - srw), # r stem
		d_bslash(ych, xcw - srw - srw, vg, hg + srw + srw, srw, nib = "vertical"), # stroke
		d_rect2(ych, hg + srw + srw, ych - srw, hg), # ul serif
		d_rect2(ych, xcw, ych - srw, xcw - 2 * srw - srw), # ur serif
		d_rect2(vg + srw, hg + 2 * srw + srw, vg, hg)
	) # ll serif
	write_svg(d_cln, "004e")
	# 1cce3 outlined latin capital letter n (derived via OUTLINE_FROM_TO)

	# 20a6 naira sign
	if (font == "square") {
		nsg <- 0.7
	} else {
		nsg <- 0.6
	}
	ds <- c(d_cln, d_rect(xc, yc + c(-nsg, nsg) * srw, cw, srw)) # bars
	write_svg(ds, "20a6")

	# 004f latin capital letter o
	d <- d_ellipse(xc, yc, 0.5 * cw - c(0, srw), 0.5 * ch - c(0, srw))
	write_svg(d, "004f")
	# 1cce4 outlined latin capital letter o (derived via OUTLINE_FROM_TO)

	# 0050 latin capital letter p
	ds <- c(
		d_rect2(ych, hg + srw + srw, vg, hg + srw), # stem
		d_rect2(ych, xc, ych - srw, hg), # t bar
		d_rect2(vg + srw, hg + 2 * srw + srw, vg, hg), # b serif
		d_rect2(yc + 0.5 * srw, xc, yc - 0.5 * srw, hg + srw), # b bar
		d_arc41(ych, xcw, yc - 0.5 * srw, xc, srw)
	) # bowl
	write_svg(ds, "0050")
	# 1cce5 outlined latin capital letter p (derived via OUTLINE_FROM_TO)

	# 20b1 peso sign
	ds <- c(
		d_rect2(ych, hg + srw + srw, vg, hg + srw), # stem
		d_rect2(ych, xc, ych - srw, hg), # t bar
		d_rect2(vg + srw, hg + 2 * srw + srw, vg, hg), # b serif
		d_rect2(yc + 0.5 * srw, xc, yc - 0.5 * srw, hg + srw), # b bar
		d_arc41(ych, xcw - srw, yc - 0.5 * srw, xc, srw), # bowl
		d_rect2(0.63 * ych + 0.33 * yc + 0.5 * srw, xcw, 0.63 * ych + 0.33 * yc - 0.5 * srw, hg), # t strikethrough
		d_rect2(0.32 * ych + 0.66 * yc + 0.5 * srw, xcw, 0.32 * ych + 0.66 * yc - 0.5 * srw, hg)
	) # b strikethrough
	write_svg(ds, "20b1")

	# 0051 latin capital letter q
	ylclq <- 0.40 * h
	ds <- c(
		d_ellipse(xc, yc, 0.5 * cw - c(0, srw), 0.5 * ch - c(0, srw)),
		d_arc1(ylclq, xc + 0.5 * srw, 0.5 * (vg + ylclq), hg, srw),
		d_arc3(0.5 * (vg + ylclq), xcw, vg, xc - 0.5 * srw, srw)
	)
	write_svg(ds, "0051")
	# 1cce6 outlined latin capital letter q (derived via OUTLINE_FROM_TO)

	# 0052 latin capital letter r
	ds <- c(
		d_rect2(ych, hg + srw2 + srw, vg, hg + srw2), # stem
		d_rect2(ych, xc, ych - srw, hg), # t bar
		d_rect2(vg + srw, hg + srw + 2 * srw2, vg, hg), # ll serif
		d_rect2(vg + srw, xcw, vg, xcw - srw - 2 * srw2), # lr serif
		d_bslash(yc, xcw - srw2, vg + srw, hg + srw2 + srw, srw), # stroke
		d_rect2(yc + 0.5 * srw, xc, yc - 0.5 * srw, hg + srw2), # b bar
		d_arc41(ych, xcw, yc - 0.5 * srw, xc, srw)
	) # bowl
	write_svg(ds, "0052")
	# 1cce7 outlined latin capital letter r (derived via OUTLINE_FROM_TO)

	# 0053 latin capital letter s
	d_lcls <- c(
		d_arc23(ych, xc, yc - 0.5 * srw, hg, srw), # l curve
		d_arc41(yc + 0.5 * srw, xcw, vg, xc, srw), # r curve
		d_arc1(ych, xcw, ych - Cvo, xc, srw), # ur curve
		d_circle(xcw - rp, ych - Cvo, rp), # ball
		d_arc3(vg + Cvo, xc, vg, hg, srw)
	) # lr curve
	write_svg(d_lcls, "0053")
	# 1cce8 outlined latin capital letter s (derived via OUTLINE_FROM_TO)

	# 0024 dollar sign
	ds <- c(d_lcls, d_rect(xc, yc, srw, h - 2 * vg + 2 * srw))
	write_svg(ds, "0024")

	#### 20b4 Hryvnia sign
	#### 20b7 Spesmilo sign

	# 0054 latin capital letter t
	ds <- c(
		d_rect2(ych, xcw, ych - srw, hg), # bar
		d_rect2(ych, xc + 0.5 * srw, vg, xc - 0.5 * srw), # stem
		d_rect2(ych, xcw, ych - 2 * srw, xcw - srw), # r serif
		d_rect2(ych, hg + srw, ych - 2 * srw, hg), # l serif
		d_rect2(vg + srw, xc + 1.5 * srw, vg, xc - 1.5 * srw)
	) # b serif
	write_svg(ds, "0054")
	# 1cce9 outlined latin capital letter t (derived via OUTLINE_FROM_TO)

	#### 20ae tugrik sign
	#### 20b8 tenge sign

	# 0055 latin capital letter u
	yu <- 0.4 * h
	ds <- c(
		d_rect2(ych, hg + srw2 + srw, yu, hg + srw2), # l stem
		d_rect2(ych, xcw - srw2, yu, xcw - srw2 - srw), # r stem
		d_arc34(yu, xcw - srw2, vg, hg + srw2, srw), # bottom
		d_rect2(ych, hg + srw + 2 * srw2, ych - srw, hg), # l serif
		d_rect2(ych, xcw, ych - srw, xcw - srw - 2 * srw2)
	) # r serif
	write_svg(ds, "0055")
	# 1ccea outlined latin capital letter u (derived via OUTLINE_FROM_TO)

	# 0056 latin capital letter v
	Vsw <- width_slash_left(
		xc - (hg + srw2),
		(ych - srw) - vg,
		srw,
		left = "horizontal",
		right = "vertical"
	)
	ds <- c(
		d_bslash(ych - srw, xc, vg, hg + srw2, srw, left = "horizontal", right = "vertical"), # l stroke
		d_fslash(ych - srw, xcw - srw2, vg, xc, srw, left = "vertical", right = "horizontal"), # r stroke
		d_rect2(ych, hg + 2 * srw2 + Vsw, ych - srw, hg), # l serif
		d_rect2(ych, xcw, ych - srw, xcw - 2 * srw2 - Vsw)
	) # r serif
	write_svg(ds, "0056")
	# 1cceb outlined latin capital letter v (derived via OUTLINE_FROM_TO)

	# 0057 latin capital letter w
	wstw <- width_slash_left(
		0.5 * (xc + 0.5 * srw - (hg + srw2)),
		(ych - srw) - vg,
		srw,
		left = "horizontal",
		right = "vertical"
	)
	wxll <- hg + srw2
	wxlr <- xc + 0.5 * wstw
	wxrl <- xc - 0.5 * wstw
	wxrr <- xcw - srw2
	d_clw <- c(
		d_bslash(
			ych - srw,
			0.5 * (wxll + wxlr),
			vg,
			wxll,
			srw,
			left = "horizontal",
			right = "vertical"
		), # ll stroke
		d_fslash(
			ych - srw,
			wxlr,
			vg,
			0.5 * (wxll + wxlr),
			srw,
			left = "vertical",
			right = "horizontal"
		), # lr stroke
		d_bslash(
			ych - srw,
			0.5 * (wxrl + wxrr),
			vg,
			wxrl,
			srw,
			left = "horizontal",
			right = "vertical"
		), # rl stroke
		d_fslash(
			ych - srw,
			wxrr,
			vg,
			0.5 * (wxrl + wxrr),
			srw,
			left = "vertical",
			right = "horizontal"
		), # rr stroke
		d_rect2(ych, hg + 2 * srw2 + wstw, ych - srw, hg), # l serif
		# d_rect2(ych, xc + srw2, ych - srw, xc - srw2), # m serif
		d_rect2(ych, xcw, ych - srw, xcw - 2 * srw2 - wstw)
	) # r serif
	write_svg(d_clw, "0057")
	# 1ccec outlined latin capital letter w (derived via OUTLINE_FROM_TO)

	# 20a9 won sign
	ds <- c(d_clw, d_rect(x = xc, y = yc + 1.5 * srw, w = cw, h = srw))
	write_svg(ds, "20a9")

	# 0058 latin capital letter x
	Xsw <- width_slash_right(
		(xcw - srw2) - (hg + srw2),
		(ych - srw) - (vg + srw),
		srw
	)
	ds <- c(
		d_bslash(ych - srw, xcw - srw2, vg + srw, hg + srw2, srw), # l stroke
		d_fslash(ych - srw, xcw - srw2, vg + srw, hg + srw2, srw), # r stroke
		d_rect2(ych, hg + 2 * srw2 + Xsw, ych - srw, hg), # ul serif
		d_rect2(ych, xcw, ych - srw, xcw - 2 * srw2 - Xsw), # ur serif
		d_rect2(vg + srw, hg + 2 * srw2 + Xsw, vg, hg), # bl serif
		d_rect2(vg + srw, xcw, vg, xcw - 2 * srw2 - Xsw)
	) # br serif
	write_svg(ds, "0058")
	# 1cced outlined latin capital letter x (derived via OUTLINE_FROM_TO)

	# 0059 latin capital letter y
	Ysw <- width_slash_left(
		xc - (hg + srw2),
		(ych - srw) - yc,
		srw,
		left = "horizontal",
		right = "square"
	)
	d_lcly <- c(
		d_rect2(ych, hg + 2 * srw2 + Ysw, ych - srw, hg), # ul serif
		d_rect2(ych, xcw, ych - srw, xcw - 2 * srw2 - Ysw), # ur serif
		d_bslash(ych - srw, xc, yc, hg + srw2, srw, left = "horizontal", right = "square"), # left stroke
		d_fslash(ych - srw, xcw - srw2, yc, xc, srw, left = "square", right = "horizontal"), # right stroke
		d_rect2(yc, xc + 0.5 * srw, vg, xc - 0.5 * srw), # stem
		d_rect2(vg + srw, xc + 1.5 * srw, vg, xc - 1.5 * srw)
	) # b serif
	write_svg(d_lcly, "0059")
	# 1ccee outlined latin capital letter y (derived via OUTLINE_FROM_TO)

	# 00a5 yen sign
	ds <- c(
		d_lcly,
		d_rect2(yc + 0.8 * srw, xcw - srw, yc - 0.2 * srw, hg + srw), # t bar
		d_rect2(yc - 0.7 * srw, xcw - srw, yc - 1.7 * srw, hg + srw)
	) # b bar
	write_svg(ds, "00a5")

	# 005a latin capital letter z
	ds <- c(
		d_rect2(ych, xcw, ych - srw, hg), # t bar
		d_rect2(vg + srw, xcw, vg, hg), # b bar
		d_fslash(ych - srw, xcw, vg + srw, hg, srw), # stroke
		d_rect2(vg + 2 * srw, xcw, vg, xcw - srw), # b serif
		d_rect2(ych, hg + srw, ych - 2 * srw, hg)
	) # t serif
	write_svg(ds, "005a")
	# 1ccef outlined latin capital letter z (derived via OUTLINE_FROM_TO)

	# 0061 latin small letter a
	xl_ds <- xcw - rt - srw
	ds <- c(
		d_rect2(yxh, xl_ds + srw, vg + rt, xl_ds), # stem
		d_ellipse(
			0.5 * (hg + xl_ds + srw),
			0.5 * (vg + yxh),
			0.5 * (xl_ds + srw - hg) + c(0, -srw),
			0.5 * (yxh - vg) + c(0, -srw)
		), # ellipse
		d_arc3(vg + rt, xl_ds + rt, vg, xl_ds, srw), # terminal..
		d_rect2(vg + srw, xl_ds + rt + srw, vg, xl_ds + rt) # terminal
	)
	write_svg(ds, "0061")

	# 0040 commercial at (circled small letter a)
	xl_ds <- hg + srw
	if (font == "square") {
		at_d <- xh
	} else {
		at_d <- 0.5 * xh
	}
	at_d <- 0.5 * xh
	ds <- c(
		d_ellipse(xc, yc, 0.5 * at_d + c(0, -srw), 0.5 * at_d + c(0, -srw)),
		d_rect2(yc + 0.5 * at_d, xc + 0.5 * at_d, yc, xc + 0.5 * at_d - srw),
		d_arc34(yc + srw, xcw, yc - 0.5 * at_d, xc + 0.5 * at_d - srw, srw),
		d_arc1(ych, xcw, yc + srw, xc, srw),
		d_arc23(ych, xc, vg, hg, srw),
		d_arc4(vg + 1.8 * srw, xcw - srw, vg, xc, srw)
	)
	write_svg(ds, "0040")

	# 0062 latin small letter b
	xl_ds <- hg + srw
	ds <- c(
		d_rect2(ych, xl_ds + srw, 0.5 * (vg + yxh), xl_ds), # stem
		d_rect2(ych, xl_ds, ych - srw, xl_ds - srw), # t serif
		d_ellipse(
			0.5 * (hg + srw + xcw),
			0.5 * (vg + yxh),
			0.5 * (xcw - hg - srw) + c(0, -srw),
			0.5 * (yxh - vg) + c(0, -srw)
		) # ellipse
	) # terminal
	write_svg(ds, "0062")

	# 0063 latin small letter c
	cvo <- 1.7 * srw
	d_slc <- c(
		d_arc23(yxh, xc, vg, xc - 0.5 * xh, srw), # l curve
		d_arc1(yxh, xc + 0.5 * xh, yxh - cvo, xc, srw), # ur curve
		d_circle(xc + 0.5 * xh - rp, yxh - cvo, rp), # ball
		d_arc4(vg + cvo, xc + 0.5 * xh, vg, xc, srw)
	) # lr curve
	write_svg(d_slc, "0063")

	####  00a9 copyright sign

	# 0254 latin small letter open o "turned c"
	ds <- c(
		d_arc41(yxh, xc + 0.5 * xh, vg, xc, srw), # r curve
		d_arc2(yxh, xc, yxh - cvo, xc - 0.5 * xh, srw), # ul curve
		d_circle(xc - 0.5 * xh + rp, yxh - cvo, rp), # ball
		d_arc3(vg + cvo, xc, vg, xc - 0.5 * xh, srw)
	) # ll curve
	write_svg(ds, "0254")

	#### 1f12f copyleft symbol

	# 00a2 cent sign
	ds <- c(d_slc, d_rect(xc, 0.5 * (vg + yxh), srw, xh + 2 * srw))
	write_svg(ds, "00a2")

	# 0064 latin small letter d
	xl_ds <- xcw - rt - srw
	ds <- c(
		d_rect2(ych, xl_ds + srw, vg + rt, xl_ds), # stem
		d_rect2(ych, xl_ds, ych - srw, xl_ds - srw), # t serif
		d_ellipse(
			0.5 * (hg + xl_ds + srw),
			0.5 * (vg + yxh),
			0.5 * (xl_ds + srw - hg) + c(0, -srw),
			0.5 * (yxh - vg) + c(0, -srw)
		), # ellipse
		d_arc3(vg + rt, xl_ds + rt, vg, xl_ds, srw), # terminal..
		d_rect2(vg + srw, xl_ds + rt + srw, vg, xl_ds + rt)
	) # terminal
	write_svg(ds, "0064")

	#### 20ab dong sign

	# 0065 latin small letter e
	ds <- c(
		d_arc23(yxh, xc, vg, xc - 0.5 * xh, srw), # l curve
		d_arc1(yxh, xc + 0.5 * xh, yxh - cvo - srw, xc, srw), # ur curve
		d_rect2(yxh - cvo - srw + srw, xc + 0.5 * xh - 30, yxh - cvo - srw, xc - 0.5 * xh + 30), # bar
		d_arc4(vg + cvo, xc + 0.5 * xh, vg, xc, srw)
	) # lr curve
	write_svg(ds, "0065")

	# 0066 latin small letter f
	# yslt <- 0.7 * h
	yslt <- yxh
	ds <- c(
		d_rect2(ych - 0.2 * h, xc + 0.5 * srw, vg + srw, xc - 0.5 * srw), # stem
		d_arc2(ych, xcw - rp, ych - 0.2 * h, xc - 0.5 * srw, srw), # ur curve
		d_circle(xcw - rp, ych - rp, rp), # ball
		d_rect2(yslt, xc + 2.0 * srw, yslt - srw, xc - 2.0 * srw), # bar
		d_rect2(vg + srw, xc + 1.5 * srw, vg, xc - 1.5 * srw)
	) # b serif
	write_svg(ds, "0066")

	# 00a3 pound sign
	yslt <- yxh
	ds <- c(
		d_rect2(ych - 0.2 * h, hg + 2 * srw, vg + srw, hg + srw), # stem
		d_arc12(ych, xcw, ych - 0.2 * h, hg + srw, srw), # ur curve
		d_circle(xcw - rp, ych - 0.2 * h, rp), # ball
		d_rect2(yslt, hg + 3.0 * srw, yslt - srw, hg), # t. bar
		d_rect2(vg + srw + srw, xcw, vg, xcw - srw), # lr serif
		d_rect2(vg + srw, xcw, vg, hg)
	) # b. bar
	write_svg(ds, "00a3")

	# 20a4 lira sign (like a pound sign with two horizontal lines)
	yslt <- yxh
	ds <- c(
		d_rect2(ych - 0.2 * h, hg + 2 * srw, vg + srw, hg + srw), # stem
		d_arc12(ych, xcw, ych - 0.2 * h, hg + srw, srw), # ur curve
		d_circle(xcw - rp, ych - 0.2 * h, rp), # ball
		d_rect2(yslt + 0.0 * srw, hg + 3.0 * srw, yslt - 1.0 * srw, hg), # t. bar
		d_rect2(yslt - 1.5 * srw, hg + 3.0 * srw, yslt - 2.5 * srw, hg), # m. bar
		d_rect2(vg + srw + srw, xcw, vg, xcw - srw), # lr serif
		d_rect2(vg + srw, xcw, vg, hg) # b. bar
	)
	write_svg(ds, "20a4")

	# 0067 latin small letter g
	xl_ds <- xcw - srw
	ds <- c(
		d_rect2(yxh, xl_ds, srw + 0.5 * srw, xl_ds - srw), # stem
		d_ellipse(
			0.5 * (hg + xl_ds),
			0.5 * (vg + yxh),
			0.5 * (xl_ds - hg) + c(0, -srw),
			0.5 * (yxh - vg) + c(0, -srw)
		),
		d_arc4(srw + 0.5 * srw, xl_ds, 0, xc, srw),
		d_arc3(srw + 0.5 * srw, xc, 0, hg, srw)
	)
	write_svg(ds, "0067")

	# 0068 latin small letter h
	ds <- c(
		d_rect2(ych, hg + srw2 + srw, vg, hg + srw2), # l stem
		d_rect2(ych, hg + srw2 + srw, ych - srw, hg), # ul serif
		d_rect2(vg + srw, hg + srw + 2 * srw2, vg, hg), # ll serif
		d_arc12(yxh, xcw - srw2, yxh - 2 * srw, hg + srw2, srw), # curve
		d_rect2(yxh - 2 * srw, xcw - srw2, vg, xcw - srw2 - srw), # r stem
		d_rect2(vg + srw, xcw, vg, xcw - srw - 2 * srw2) # lr serif
	)
	write_svg(ds, "0068")

	# 0131 latin small letter dotless i
	d_dli <- c(
		d_rect2(yxh - srw, xc + 0.5 * srw, vg + srw, xc - 0.5 * srw), # stem
		d_rect2(yxh, xc + 0.5 * srw, yxh - srw, xc - 1.5 * srw), # t serif
		d_rect2(vg + srw, xc + 1.5 * srw, vg, xc - 1.5 * srw)
	) # b serif
	write_svg(d_dli, "0131")

	# 0069 latin small letter i
	d_i <- c(d_dli, d_circle(xc, yxh + 0.7 * srw + rp, rp))
	write_svg(d_i, "0069")

	# 0237 latin small letter dotless j
	d_dlj <- c(
		d_rect2(yxh - srw, xc + 0.5 * srw, vg + srw, xc - 0.5 * srw), # stem
		d_rect2(yxh, xc + 0.5 * srw, yxh - srw, xc - 1.5 * srw), # t serif
		d_arc4(vg + srw, xc + 0.5 * srw, 0, xc - 2.0 * srw, srw)
	) # hook
	write_svg(d_dlj, "0237")

	# 006a latin small letter j
	d_j <- c(d_dlj, d_circle(xc, yxh + 0.7 * srw + rp, rp))
	write_svg(d_j, "006a")

	# 006b latin small letter k
	ksw <- width_slash_right(
		(xcw - srw2) - (hg + srw2 + srw),
		yxh - srw - 0.5 * (yxh + vg),
		srw,
		left = "square",
		right = "horizontal"
	)
	d_slk <- c(
		d_rect2(ych, hg + srw + srw2, vg, hg + srw2), # l stem
		d_rect2(ych, hg + srw + srw2, ych - srw, hg), # ul serif
		d_rect2(yxh, xcw, yxh - srw, xcw - ksw - 2 * srw2), # ur serif
		d_fslash(
			yxh - srw,
			xcw - srw2,
			0.5 * (yxh + vg),
			hg + srw + srw2,
			srw,
			left = "square",
			right = "horizontal"
		), # t stroke
		d_bslash(
			0.5 * (yxh + vg),
			xcw - srw2,
			vg + srw,
			hg + srw + srw2,
			srw,
			left = "square",
			right = "horizontal"
		), # b stroke
		d_rect2(vg + srw, hg + 2 * srw2 + srw, vg, hg), # ll serif
		d_rect2(vg + srw, xcw, vg, xcw - ksw - 2 * srw2) # lr serif
	)
	write_svg(d_slk, "006b")

	# 006c latin small letter l
	ds <- c(
		d_rect2(ych - srw, xc + 0.5 * srw, vg + rt, xc - 0.5 * srw), # stem
		d_rect2(ych, xc + 0.5 * srw, ych - srw, xc - 1.5 * srw), # t serif
		d_arc3(vg + rt, xc - 0.5 * srw + rt, vg, xc - 0.5 * srw, srw), # terminal..
		d_rect2(vg + srw, xc + 0.5 * srw + rt, vg, xc - 0.5 * srw + rt)
	) # terminal
	write_svg(ds, "006c")

	# 006d latin small letter m
	r_ul_serif <- 0.5 * (xc + 0.5 * srw + hg + srw2)
	ds <- c(
		d_rect2(yxh - 2 * srw, hg + srw2 + srw, vg + srw, hg + srw2), # l stem
		d_rect2(yxh, r_ul_serif, yxh - srw, hg), # ul serif
		d_rect2(vg + srw, hg + srw + 2 * srw2, vg, hg), # ll serif
		d_arc12(yxh, xc + 0.5 * srw, yxh - 2 * srw, hg + srw2, srw), # l curve
		d_rect2(yxh - 2 * srw, xc + 0.5 * srw, vg + srw + srw, xc - 0.5 * srw), # m stem
		d_arc12(yxh, xcw - srw2, yxh - 2 * srw, xc - 0.5 * srw, srw), # r curve
		d_rect2(yxh - 2 * srw, xcw - srw2, vg + srw, xcw - srw2 - srw), # r stem
		d_rect2(vg + srw, xcw, vg, xcw - srw - 2 * srw2) # lr serif
	)
	write_svg(ds, "006d")

	#### 20a5 mill sign

	# 006e latin small letter n
	ds <- c(
		d_rect2(yxh, hg + srw2 + srw, vg, hg + srw2), # l stem
		d_rect2(yxh, hg + srw2 + srw, yxh - srw, hg), # ul serif
		d_rect2(vg + srw, hg + srw + 2 * srw2, vg, hg), # ll serif
		d_arc12(yxh, xcw - srw2, yxh - 2 * srw, hg + srw2, srw), # curve
		d_rect2(yxh - 2 * srw, xcw - srw2, vg, xcw - srw2 - srw), # r stem
		d_rect2(vg + srw, xcw, vg, xcw - srw - 2 * srw2) # lr serif
	)
	write_svg(ds, "006e")

	# 006f latin small letter o
	write_svg(
		d_ellipse(xc, 0.5 * (vg + yxh), 0.5 * xh + c(0, -srw), 0.5 * xh + c(0, -srw)),
		"006f"
	)

	# 0070 latin small letter p
	xl_ds <- hg + srw
	ds <- c(
		d_rect2(yxh, xl_ds + srw, srw, xl_ds), # stem
		d_rect2(yxh, xl_ds, yxh - srw, xl_ds - srw), # t serif
		d_rect2(srw, xl_ds + srw + srw, 0, xl_ds - srw), # b serif
		d_ellipse(
			0.5 * (hg + srw + xcw),
			0.5 * (vg + yxh),
			0.5 * (xcw - hg - srw) + c(0, -srw),
			0.5 * (yxh - vg) + c(0, -srw)
		)
	)
	write_svg(ds, "0070")

	# 0071 latin small letter q
	xl_ds <- xcw - srw
	ds <- c(
		d_rect2(yxh, xl_ds, srw, xl_ds - srw), # stem
		d_ellipse(
			0.5 * (hg + xl_ds),
			0.5 * (vg + yxh),
			0.5 * (xl_ds - hg) + c(0, -srw),
			0.5 * (yxh - vg) + c(0, -srw)
		), # ellipse
		d_rect2(srw, xcw, 0, xcw - srw - 2 * srw) # b serif
		# d_arc3(rt, xl_ds + rt, 0, xl_ds, srw), # terminal..
		# d_rect2(srw, xl_ds + rt + srw, 0, xl_ds + rt) # terminal
	)
	write_svg(ds, "0071")

	# 0072 latin small letter r
	d_lr <- c(
		d_rect2(yxh - srw, xc - 0.5 * xh + srw + srw, vg + srw, xc - 0.5 * xh + srw), # stem
		d_rect2(yxh, xc - 0.5 * xh + srw + srw, yxh - srw, xc - 0.5 * xh), # t serif
		d_rect2(vg + srw, xc - 0.5 * xh + 2 * srw + srw, vg, xc - 0.5 * xh), # b serif
		d_arc2(yxh, xc + 0.5 * xh - rp, yxh - 2 * srw, xc - 0.5 * xh + srw, srw), # curve
		d_circle(xc + 0.5 * xh - rp, yxh - rp, rp)
	) # ball
	write_svg(d_lr, "0072")

	# 0073 latin small letter s
	ssvo <- 1.3 * srw
	d_lsls <- c(
		d_arc23(yxh, xc, 0.5 * (yxh + vg) - 0.5 * srw, xc - 0.5 * xh, srw), # l curve
		d_arc41(0.5 * (yxh + vg) + 0.5 * srw, xc + 0.5 * xh, vg, xc, srw), # r curve
		d_arc1(yxh, xc + 0.5 * xh, yxh - ssvo, xc, srw), # ur curve
		d_circle(xc + 0.5 * xh - rp, yxh - ssvo, rp), # ball
		d_arc3(vg + ssvo, xc, vg, xc - 0.5 * xh, srw)
	) # lr curve
	write_svg(d_lsls, "0073")

	# 00a7 section sign (aka double-s)
	h_s <- 0.56 * h
	ssvo <- 1.6 * srw
	d_lss <- c(
		d_arc1(ych, xc + 0.5 * xh, ych - ssvo, xc, srw), # t ur curve
		d_arc23(ych, xc, ych - ch / 3 - 0.5 * srw, xc - 0.5 * xh, srw), # t l curve
		d_arc41(ych - ch / 3 + 0.5 * srw, xc + 0.5 * xh, vg + ch / 3 - 0.5 * srw, xc, srw), # t r curve
		d_arc3(vg + ch / 3 - 0.5 * srw + ssvo, xc, vg + ch / 3 - 0.5 * srw, xc - 0.5 * xh, srw), # t lr curve
		d_circle(xc + 0.5 * xh - rp, ych - ssvo, rp), # ball
		d_arc23(ych - ch / 3 + 0.5 * srw, xc, vg + ch / 3 - 0.5 * srw, xc - 0.5 * xh, srw), # b l curve
		d_arc41(vg + ch / 3 + 0.5 * srw, xc + 0.5 * xh, vg, xc, srw), # b r curve
		d_arc1(ych - ch / 3 + 0.5 * srw, xc + 0.5 * xh, ych - ch / 3 + 0.5 * srw - ssvo, xc, srw), # b ur curve
		d_arc3(vg + ssvo, xc, vg, xc - 0.5 * xh, srw)
	) # b lr curve
	write_svg(d_lss, "00a7")

	# 0074 latin small letter t
	# yslt <- 0.7 * h
	yslt <- yxh
	ds <- c(
		d_rect2(ych, xc + 0.5 * srw, vg + rt, xc - 0.5 * srw), # stem
		d_rect2(ych, xc - 0.5 * srw, ych - srw, xc - 0.5 * srw - srw), # t serif
		d_rect2(yslt, xc + 2.0 * srw, yslt - srw, xc - 2.0 * srw), # bar
		d_arc3(vg + rt, xc - 0.5 * srw + rt, vg, xc - 0.5 * srw, srw), # terminal..
		d_rect2(vg + srw, xc + 0.5 * srw + rt, vg, xc - 0.5 * srw + rt)
	) # terminal
	write_svg(ds, "0074")

	# 0026 ampersand is a stylized ligature of e t
	# https://en.wikipedia.org/wiki/Ampersand
	x_bar_r <- xcw - 1.0 * srw
	ds <- c(
		d_arc1(ych, x_bar_r, ych - Cvo, xc, srw), # ur curve
		d_arc23(ych, xc, yc - 0.5 * srw, hg, srw), # ul curve
		d_arc23(yc + 0.5 * srw, xc, vg, hg, srw), # ll curve
		d_arc4(vg + 1.5 * srw, x_bar_r, vg, xc, srw), # lr curve
		d_rect2(yc - 0.5 * srw, x_bar_r, vg + 1.5 * srw, x_bar_r - srw), # stem
		d_rect2(yc + 0.5 * srw, xcw, yc - 0.5 * srw, xc), # bar
		d_circle(x_bar_r - rp, ych - Cvo, rp) # ball
	)
	write_svg(ds, "0026")

	# 0075 latin small letter u
	xl_ds <- xcw - rt - srw2
	ds <- c(
		d_rect2(yxh - srw, hg + srw + srw2, vg + rt, hg + srw2), # l stem
		d_rect2(yxh, hg + srw + 2 * srw2, yxh - srw, hg), # l bar
		d_arc3(vg + rt, 0.5 * (hg + srw2 + xl_ds + srw), vg, hg + srw2, srw),
		d_arc4(vg + rt, xl_ds + srw, vg, 0.5 * (hg + srw2 + xl_ds + srw), srw),
		d_arc3(vg + rt, xl_ds + rt, vg, xl_ds, srw), # terminal..
		d_rect2(yxh, xl_ds + srw + srw2, yxh - srw, xl_ds - srw2), # r bar
		d_rect2(yxh - srw, xl_ds + srw, vg + rt, xl_ds), # r stem
		d_rect2(vg + srw, xl_ds + rt + srw, vg, xl_ds + rt) # terminal
	)
	write_svg(ds, "0075")

	# 0076 latin small letter v
	vsw <- width_slash_left(
		xc - (hg + srw2),
		(yxh - srw) - vg,
		srw,
		left = "horizontal",
		right = "vertical"
	)
	ds <- c(
		d_bslash(yxh - srw, xc, vg, hg + srw2, srw, left = "horizontal", right = "vertical"), # l stroke
		d_fslash(yxh - srw, xcw - srw2, vg, xc, srw, left = "vertical", right = "horizontal"), # r stroke
		d_rect2(yxh, hg + 2 * srw2 + vsw, yxh - srw, hg), # l serif
		d_rect2(yxh, xcw, yxh - srw, xcw - 2 * srw2 - vsw)
	) # r serif
	write_svg(ds, "0076")

	# 0077 latin small letter w
	wstw <- width_slash_left(
		0.5 * (xc + 0.5 * srw - (hg + srw2)),
		(yxh - srw) - vg,
		srw,
		left = "horizontal",
		right = "vertical"
	)
	wxll <- hg + srw2
	wxlr <- xc + 0.5 * wstw
	wxrl <- xc - 0.5 * wstw
	wxrr <- xcw - srw2
	d_slw <- c(
		d_bslash(
			yxh - srw,
			0.5 * (wxll + wxlr),
			vg,
			wxll,
			srw,
			left = "horizontal",
			right = "vertical"
		), # ll stroke
		d_fslash(
			yxh - srw,
			wxlr,
			vg,
			0.5 * (wxll + wxlr),
			srw,
			left = "vertical",
			right = "horizontal"
		), # lr stroke
		d_bslash(
			yxh - srw,
			0.5 * (wxrl + wxrr),
			vg,
			wxrl,
			srw,
			left = "horizontal",
			right = "vertical"
		), # rl stroke
		d_fslash(
			yxh - srw,
			wxrr,
			vg,
			0.5 * (wxrl + wxrr),
			srw,
			left = "vertical",
			right = "horizontal"
		), # rr stroke
		d_rect2(yxh, hg + 2 * srw2 + wstw, yxh - srw, hg), # l serif
		# d_rect2(yxh, xc + srw2, yxh - srw, xc - srw2), # m serif
		d_rect2(yxh, xcw, yxh - srw, xcw - 2 * srw2 - wstw)
	) # r serif
	write_svg(d_slw, "0077")

	# 0078 latin small letter x
	xsw <- width_slash_right(
		(xcw - srw2) - (hg + srw2),
		(yxh - srw) - (vg + srw),
		srw
	)
	ds <- c(
		d_bslash(yxh - srw, xcw - srw2, vg + srw, hg + srw2, srw), # l stroke
		d_fslash(yxh - srw, xcw - srw2, vg + srw, hg + srw2, srw), # r stroke
		d_rect2(yxh, hg + 2 * srw2 + xsw, yxh - srw, hg), # ul serif
		d_rect2(yxh, xcw, yxh - srw, xcw - 2 * srw2 - xsw), # ur serif
		d_rect2(vg + srw, hg + 2 * srw2 + xsw, vg, hg), # bl serif
		d_rect2(vg + srw, xcw, vg, xcw - 2 * srw2 - xsw)
	) # br serif
	write_svg(ds, "0078")

	# 0079 latin small letter y
	xl_ds <- xcw - srw2
	ds <- c(
		d_rect2(yxh - srw, hg + srw2 + srw, vg + rt, hg + srw2), # l stem
		d_rect2(yxh, hg + srw + 2 * srw2, yxh - srw, hg), # l bar
		d_arc3(vg + rt, 0.5 * (hg + srw2 + xl_ds), vg, hg + srw2, srw),
		d_arc4(vg + rt, xl_ds, vg, 0.5 * (hg + srw2 + xl_ds), srw),
		d_rect2(yxh, xl_ds + srw2, yxh - srw, xl_ds - srw - srw2), # r bar
		d_rect2(yxh - srw, xl_ds, srw + 0.4 * srw, xl_ds - srw), # r stem
		d_arc4(srw + 0.4 * srw, xl_ds, 0, xc, srw),
		d_arc3(srw + 0.4 * srw, xc, 0, hg + srw2, srw)
	)
	write_svg(ds, "0079")

	# 007a latin small letter z
	ds <- c(
		d_rect2(yxh, xc + 0.5 * xh, yxh - srw, xc - 0.5 * xh), # t bar
		d_rect2(vg + srw, xc + 0.5 * xh, vg, xc - 0.5 * xh), # b bar
		d_fslash(yxh - srw, xc + 0.5 * xh, vg + srw, xc - 0.5 * xh, srw), # stroke
		d_rect2(vg + 2 * srw, xc + 0.5 * xh, vg, xc + 0.5 * xh - srw), # b serif
		d_rect2(yxh, xc - 0.5 * xh + srw, yxh - 2 * srw, xc - 0.5 * xh)
	) # t serif
	write_svg(ds, "007a")

	# 005f low line (to go under baseline and reaches from side to side)
	d <- d_rect2(srw, w, 0, 0)
	write_svg(d, "005f")

	# 203e over line
	d <- d_rect2(h, w, h - srw, 0)
	write_svg(d, "203e")

	# 00af macron
	d <- d_rect(xc, ych - 0.5 * srw, cw, srw)
	write_svg(d, "00af")

	# 007c vertical line
	d <- d_rect(xc, yc, srw, ch)
	write_svg(d, "007c")

	# 00a6 broken bar
	bbg <- 0.8 * srw
	d <- d_rect2(ych, xc + 0.5 * srw, yc + bbg, xc - 0.5 * srw) +
		d_rect2(yc - bbg, xc + 0.5 * srw, vg, xc - 0.5 * srw)
	write_svg(d, "00a6")

	# 003c less-than sign
	ds <- c(
		d_fslash(yc + 0.50 * cw, xcw, yc, hg, srw, left = "horizontal", right = "vertical"), # t stroke
		d_bslash(yc, xcw, yc - 0.50 * cw, hg, srw, left = "horizontal", right = "vertical")
	) # b stroke
	write_svg(ds, "003c")

	waqm <- 0.44 * w

	# 2039 single left-pointing angle quotation mark
	ds <- c(
		d_fslash(
			yc + 0.50 * cw,
			xc + 0.5 * waqm,
			yc,
			xc - 0.5 * waqm,
			srw,
			left = "horizontal",
			right = "vertical"
		), # t stroke
		d_bslash(
			yc,
			xc + 0.5 * waqm,
			yc - 0.50 * cw,
			xc - 0.5 * waqm,
			srw,
			left = "horizontal",
			right = "vertical"
		)
	) # b stroke
	write_svg(ds, "2039")

	# 203a single right-pointing angle quotation mark
	ds <- c(
		d_bslash(
			yc + 0.50 * cw,
			xc + 0.5 * waqm,
			yc,
			xc - 0.5 * waqm,
			srw,
			left = "vertical",
			right = "horizontal"
		), # t stroke
		d_fslash(
			yc,
			xc + 0.5 * waqm,
			yc - 0.50 * cw,
			xc - 0.5 * waqm,
			srw,
			left = "vertical",
			right = "horizontal"
		)
	) # b stroke
	write_svg(ds, "203a")

	# 00ab left-pointing double angle quotation mark
	ds <- c(
		d_fslash(yc + 0.50 * cw, hg + waqm, yc, hg, srw, left = "horizontal", right = "vertical"), # l t stroke
		d_bslash(yc, hg + waqm, yc - 0.50 * cw, hg, srw, left = "horizontal", right = "vertical"), # l b stroke
		d_fslash(yc + 0.50 * cw, xcw, yc, xcw - waqm, srw, left = "horizontal", right = "vertical"), # r t stroke
		d_bslash(yc, xcw, yc - 0.50 * cw, xcw - waqm, srw, left = "horizontal", right = "vertical")
	) # r b stroke
	write_svg(ds, "00ab")

	# 00bb right-pointing double angle quotation mark
	ds <- c(
		d_bslash(yc + 0.50 * cw, hg + waqm, yc, hg, srw, left = "vertical", right = "horizontal"), # l t stroke
		d_fslash(yc, hg + waqm, yc - 0.50 * cw, hg, srw, left = "vertical", right = "horizontal"), # l b stroke
		d_bslash(yc + 0.50 * cw, xcw, yc, xcw - waqm, srw, left = "vertical", right = "horizontal"), # r t stroke
		d_fslash(yc, xcw, yc - 0.50 * cw, xcw - waqm, srw, left = "vertical", right = "horizontal")
	) # r b stroke
	write_svg(ds, "00bb")

	# 003d equals sign
	d <- d_rect(xc, yc + c(-0.25, 0.25) * cw, cw, srw)
	write_svg(d, "003d")

	# 003e greater-than sign
	ds <- c(
		d_bslash(yc + 0.50 * cw, xcw, yc, hg, srw, left = "vertical", right = "horizontal"), # t stroke
		d_fslash(yc, xcw, yc - 0.50 * cw, hg, srw, left = "vertical", right = "horizontal")
	) # b stroke
	write_svg(ds, "003e")

	# 007b left curly bracket
	ds <- c(
		d_arc3(yc - 0.25 * ch, xc + 1.5 * srw, vg, xc - 0.5 * srw, srw), # b b curve
		d_arc2(ych, xc + 1.5 * srw, yc + 0.25 * ch, xc - 0.5 * srw, srw), # t t curve
		d_arc4(yc + 0.25 * ch, xc + 0.5 * srw, yc - 0.5 * srw, xc - 1.5 * srw, srw), # t b curve
		d_arc1(yc + 0.5 * srw, xc + 0.5 * srw, yc - 0.25 * ch, xc - 1.5 * srw, srw)
	) # b t curve
	write_svg(ds, "007b")
	# 007d right curly bracket
	ds <- c(
		d_arc4(yc - 0.25 * ch, xc + 0.5 * srw, vg, xc - 1.5 * srw, srw), # b b curve
		d_arc1(ych, xc + 0.5 * srw, yc + 0.25 * ch, xc - 1.5 * srw, srw), # t t curve
		d_arc3(yc + 0.25 * ch, xc + 1.5 * srw, yc - 0.5 * srw, xc - 0.5 * srw, srw), # t b curve
		d_arc2(yc + 0.5 * srw, xc + 1.5 * srw, yc - 0.25 * ch, xc - 0.5 * srw, srw)
	) # b t curve
	write_svg(ds, "007d")

	# 00d7 multiplication sign
	ms <- 0.8 * 0.5
	d <- c(
		d_fslash(yc + ms * cw, xc + ms * cw, yc - ms * cw, xc - ms * cw, srw, nib = "diagonal"),
		d_bslash(yc + ms * cw, xc + ms * cw, yc - ms * cw, xc - ms * cw, srw, nib = "diagonal")
	)
	write_svg(d, "00d7")

	# 00a8 diaeresis
	write_svg(d_circle(xc + c(-2, 2) * rp, ych - rp, rp), "00a8")

	# 00b7 middle dot
	write_svg(d_circle(xc, yc, rp), "00b7")

	# 2022 bullet
	write_svg(d_circle(xc, yc, 2 * rp), "2022")

	# 2026 horizontal ellipsis
	d <- d_circle(c(hg + rp, xc, w - hg - rp), vg + rp, rp)
	write_svg(d, "2026")

	# 22ee vertical ellipsis
	d <- d_circle(xc, c(vg + rp, yc, ych - rp), rp)
	write_svg(d, "22ee")

	# 22ef midline horizontal ellipsis
	d <- d_circle(c(hg + rp, xc, w - hg - rp), yc, rp)
	write_svg(d, "22ef")

	# 22f0 up right diagonal ellipsis
	d <- d_circle(c(hg + rp, xc, w - hg - rp), c(vg + rp, yc, ych - rp), rp)
	write_svg(d, "22f0")

	# 22f1 down right diagonal ellipsis
	d <- d_circle(c(hg + rp, xc, w - hg - rp), c(ych - rp, yc, vg + rp), rp)
	write_svg(d, "22f1")

	# 00a4 (generic) currency sign
	gcso <- 0.2 * xh
	ds <- c(
		d_circle(xc, yc, 0.45 * xh + c(0, -srw)),
		d_fslash(yc - gcso, xc - gcso, yc - 0.5 * xh, hg, srw, nib = "diagonal"), # ll stroke
		d_bslash(yc - gcso, xcw, yc - 0.5 * xh, xc + gcso, srw, nib = "diagonal"), # lr stroke
		d_bslash(yc + 0.5 * xh, xc - gcso, yc + gcso, hg, srw, nib = "diagonal"), # ul stroke
		d_fslash(yc + 0.5 * xh, xcw, yc + gcso, xc + gcso, srw, nib = "diagonal")
	) # ur stroke
	write_svg(ds, "00a4")

	if (font == "square") {
		d_circ_white <- d_circle(xc, yc, 0.5 * c(cw, cw - ow))
		d_circ_black <- d_ellipse(xc, yc, 0.5 * cw, 0.5 * ch)

		# 24ea circled digit zero
		d_cd0 <- c(
			d_ellipse(x = xc, y = yc, rx = 0.35 * cdw + c(0, -srw), ry = 0.5 * cdw + c(0, -srw))
		)
		write_svg(c(d_circ_white, d_cd0), "24ea")
		# 24ff negative circled digit zero
		write_svg(c(d_circ_black + d_cd0), "24ff")
		# 2460 circled digit one
		d_cd1 <- d_rect(x = xc, y = yc, w = srw, h = cdw - 2 * srw) + # stem
			d_rect2(yc + 0.5 * cdw, xc + 0.5 * srw, yc + 0.5 * cdw - srw, xc - 0.5 * srw - srw2) + # t serif
			d_rect2(
				yc - 0.5 * cdw + srw,
				xc + 0.5 * srw + srw2,
				yc - 0.5 * cdw,
				xc - 0.5 * srw - srw2
			) # b serif
		write_svg(c(d_circ_white, d_cd1), "2460")
		# 2776 dingbat negative circled digit one
		write_svg(c(d_circ_black + d_cd1), "2776")
		# write_svg(c(d_cd1), "2776")
		#### 2461 circled digit two
		write_svg(d_circ_white, "2461")
		#### 2777 dingbat negative circled digit two
		write_svg(d_circ_black, "2777")
		#### 2462 circled digit three
		write_svg(d_circ_white, "2462")
		#### 2778 dingbat negative circled digit three
		write_svg(d_circ_black, "2778")
		#### 2463 circled digit four
		write_svg(d_circ_white, "2463")
		#### 2779 dingbat negative circled digit four
		write_svg(d_circ_black, "2779")
		#### 2464 circled digit five
		write_svg(d_circ_white, "2464")
		#### 277a dingbat negative circled digit five
		write_svg(d_circ_black, "277a")
		#### 2465 circled digit six
		write_svg(d_circ_white, "2465")
		#### 277b dingbat negative circled digit six
		write_svg(d_circ_black, "277b")
		#### 2466 circled digit seven
		write_svg(d_circ_white, "2466")
		#### 277c dingbat negative circled digit seven
		write_svg(d_circ_black, "277c")
		# 2467 circled digit eight
		ov8c <- (8 / 18) * srw
		he8c <- 0.5 * (cdw - ov8c)
		d_cd8 <- c(
			d_ellipse(
				x = xc,
				y = yc + 0.5 * (he8c - ov8c),
				rx = he8c - c(0, srw),
				ry = 0.5 * (ov8c + he8c) - c(0, srw)
			), # top
			d_ellipse(
				x = xc,
				y = yc - 0.5 * (he8c - ov8c),
				rx = he8c - c(0, srw),
				ry = 0.5 * (ov8c + he8c) - c(0, srw)
			) # bottom
		)
		write_svg(c(d_circ_white, d_cd8), "2467")
		# 277d dingbat negative circled digit eight
		# We need to add the trace of the outside of the eight
		# plus the two ellipses of the eight loops to the black circle
		# Claude Sonnet 4.6 gave us the following equation for
		# x-coordinate where the two ellipses meet
		x_left8c <- xc - he8c * sqrt(1 - ((he8c - ov8c) / (ov8c + he8c))^2)
		x_right8c <- 2 * xc - x_left8c
		d_cd8_outer <- M(x_left8c, yc) +
			A(he8c, 0.5 * (ov8c + he8c), big = TRUE, x = x_right8c, y = yc) +
			AZ(he8c, 0.5 * (ov8c + he8c), big = TRUE, x = x_left8c, y = yc)
		d_cd8_loop_top <- d_ellipse(
			x = xc,
			y = yc + 0.5 * (he8c - ov8c),
			rx = he8c - srw,
			ry = 0.5 * (ov8c + he8c) - srw
		)
		d_cd8_loop_bot <- d_ellipse(
			x = xc,
			y = yc - 0.5 * (he8c - ov8c),
			rx = he8c - srw,
			ry = 0.5 * (ov8c + he8c) - srw
		)

		write_svg(c(d_circ_black + d_cd8_outer + d_cd8_loop_top + d_cd8_loop_bot), "277d")
		#### 2468 circled digit nine
		write_svg(d_circ_white, "2468")
		#### 277e dingbat negative circled digit nine
		write_svg(d_circ_black, "277e")
	}

	# 1f4a7 droplet
	r_drop <- 0.5 * cw
	ycc_drop <- vg + r_drop
	ds <- M(xc, ych) +
		Q(xc + 0.8 * r_drop, vg + 0.75 * ch, xc + r_drop, ycc_drop) +
		A(r_drop, r_drop, x = xc - r_drop, y = ycc_drop) +
		QZ(xc - 0.8 * r_drop, vg + 0.75 * ch, xc, ych)
	write_svg(ds, "1f4a7")

	c(
		as.hexmode("0021"):as.hexmode("007e"),
		as.hexmode("00a1"):as.hexmode("00a8"),
		as.hexmode("20b1"):as.hexmode("20b3"),
		as.hexmode("22ee"):as.hexmode("22f1"),
		as.hexmode(c(
			"00ab",
			"00ac",
			"00af",
			"00b0",
			"00b4",
			"00b7",
			"00bb",
			"00bf",
			"00d7",
			"00f7",
			"0131",
			"0186",
			"02c7",
			"0237",
			"0254",
			"0e3f",
			"2022",
			"2026",
			"2039",
			"203a",
			"203e",
			"20a3",
			"20a4",
			"20a6",
			"20a9",
			"20ac",
			"20ad",
			"20b5",
			"20bf",
			"218b",
			"f590"
		)),
		if (font == "square") {
			c(
				as.hexmode(c("24ea", "24ff", "2460", "2467", "2776", "277d"))
				# as.hexmode("2460"):as.hexmode("2468"),
				# as.hexmode("2776"):as.hexmode("277e")
			)
		},
		as.hexmode("1f4a7"),
		c(as.hexmode("1d7ce"):as.hexmode("1d7cf")),
		as.hexmode(c("1d7d2", "1d7d3", "1d7d5", "1d7d6"))
		# as.hexmode("1d7da"):as.hexmode("1d7d7"), # mathematical bold (avec-serif) digits
	) |>
		as_hex()
}
