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
	xcw <- w - hg

	xc <- w / 2
	yc <- h / 2

	rp <- 0.75 * STW # period radius
	ah <- 3 * STW # apostrophe height

	# 0022 quotation mark
	d <- d_rect(xc + c(-0.25, 0.25) * cw, ych - 0.5 * ah, STW, ah)
	write_svg(d, "0022")

	#### use vertical slashes?
	# 0023 number sign
	d <- c(
		d_rect(xc, yc + c(-0.20, 0.20) * cw, cw, STW),
		d_rect(xc + c(-0.20, 0.20) * cw, yc, STW, cw)
	)
	write_svg(d, "0023")

	# 0025 percent sign
	if (font == "square") {
		rps <- 300
	} else {
		rps <- 250
	}
	d_0025 <- d_fslash(ych, w - 2 * hg, vg, 2 * hg, STW) +
		d_ellipse(hg + rps, ych - rps, c(rps, rps - STW), c(rps, rps - STW)) +
		d_ellipse(xcw - rps, vg + rps, c(rps, rps - STW), c(rps, rps - STW))
	write_svg(d_0025, "0025")

	# 002e full stop
	d_full_stop <- d_circle(xc, vg + rp, rp)
	write_svg(d_full_stop, "002e")

	# 002f Solidus (Slash)
	d_002f <- d_fslash(ych, xcw, vg, hg, STW)
	write_svg(d_002f, "002f")

	# 005b left square bracket
	ds <- c(
		d_rect2(ych - STW, xc, vg + STW, xc - STW), # stem
		d_rect2(ych, xc + STW, ych - STW, xc - STW), # t serif
		d_rect2(vg + STW, xc + STW, vg, xc - STW)
	) # b serif
	write_svg(ds, "005b")

	# 005c Reverse Solidus (Backslash)
	d_005c <- d_bslash(ych, xcw, vg, hg, STW)
	write_svg(d_005c, "005c")

	# 005d right square bracket
	ds <- c(
		d_rect2(ych - STW, xc + STW, vg + STW, xc), # stem
		d_rect2(ych, xc + STW, ych - STW, xc - STW), # t serif
		d_rect2(vg + STW, xc + STW, vg, xc - STW)
	) # b serif
	write_svg(ds, "005d")

	# 0021 exclamation mark
	d <- d_full_stop + d_rect(xc, yc + 1.5 * rp, STW, ch - 3 * rp)
	write_svg(d, "0021")

	# 00a1 inverted exclamation mark
	d <- d_circle(xc, ych - rp, rp) +
		d_rect(xc, yc - 1.5 * rp, STW, ch - 3 * rp)
	write_svg(d, "00a1")

	# 003f question mark
	ds <- c(
		d_full_stop, # period
		d_rect2(yc, xc + 0.5 * STW, vg + 3 * rp, xc - 0.5 * STW), # stem
		d_arc412(ych, xcw, yc - STW, hg, STW), # curve
		d_circle(hg + rp, 0.5 * (ych + yc - STW), rp)
	) # ball
	write_svg(ds, "003f")

	# 00bf inverted question mark
	ds <- c(
		d_circle(xc, ych - rp, rp), # period
		d_rect2(ych - 3 * rp, xc + 0.5 * STW, yc, xc - 0.5 * STW), # stem
		d_arc234(yc + STW, xcw, vg, hg, STW)
	) # curve
	write_svg(ds, "00bf")

	# 0027 apostrophe
	d <- d_rect(xc, ych - 0.5 * ah, STW, ah)
	write_svg(d, "0027")

	# 002a asterisk
	if (font == "narrow") {
		astl <- 0.8 * ah
	} else {
		astl <- ah
	}
	astf <- 1.0 * astl
	ds <- c(
		d_rect(xc, yc, STW, 2 * astl),
		d_fslash(yc + 0.66 * astf, xc + astf, yc - 0.66 * astf, xc - astf, STW, nib = "diagonal"),
		d_bslash(yc + 0.66 * astf, xc + astf, yc - 0.66 * astf, xc - astf, STW, nib = "diagonal")
	)
	write_svg(ds, "002a")

	# 002c comma
	d_comma <- c(d_full_stop, d_arc4(vg + rp, xc + rp, vg - rp, xc - rp, 0.5 * STW)) # hook
	write_svg(d_comma, "002c")

	# 003a colon
	d <- d_full_stop + d_circle(xc, yxh - rp, rp)
	write_svg(d, "003a")

	# 003b semi-colon
	d <- d_comma + d_circle(xc, yxh - rp, rp)
	write_svg(d, "003b")

	# 002b plus sign
	d <- d_rect(xc, yc, STW, cw) + d_rect(xc, yc, cw, STW)
	write_svg(d, "002b")

	# 002d hyphen-minus
	d_hyphen <- d_rect(xc, yc, cw, STW)
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
	d <- d_bslash(ych, xc + 1.5 * STW, ych - ah, xc - 1.5 * STW, STW)
	write_svg(d, "0060")

	# 00ac not sign
	ds <- c(
		d_rect2(yc + 0.5 * ah, xcw - STW, yc + 0.5 * ah - STW, hg),
		d_rect2(yc + 0.5 * ah, xcw, yc - 0.5 * ah, xcw - STW)
	)
	write_svg(ds, "00ac")

	# 00b0 degree sign
	d <- d_circle(xc, ych - 0.5 * ah, 0.5 * ah + c(0, -STW))
	write_svg(d, "00b0")

	# 00b4 acute accent
	d <- d_fslash(ych, xc + 1.5 * STW, ych - ah, xc - 1.5 * STW, STW)
	write_svg(d, "00b4")

	# 005e circumflex accent
	ds <- c(
		d_fslash(ych, xc, ych - ah, xc - 2 * STW, STW, left = "horizontal", right = "vertical"),
		d_bslash(ych, xc + 2 * STW, ych - ah, xc, STW, left = "vertical", right = "horizontal")
	)
	write_svg(ds, "005e")

	# 02c7 caron
	ds <- c(
		d_bslash(ych, xc, ych - ah, xc - 2 * STW, STW, left = "horizontal", right = "vertical"),
		d_fslash(ych, xc + 2 * STW, ych - ah, xc, STW, left = "vertical", right = "horizontal")
	)
	write_svg(ds, "02c7")

	# 0028 left parenthesis
	w_par <- 2 * STW
	d <- d_arc23(ych, xc + 0.5 * w_par, vg, xc - 0.5 * w_par, STW)
	write_svg(d, "0028")
	# 0029 right parenthesis
	d <- d_arc41(ych, xc + 0.5 * w_par, vg, xc - 0.5 * w_par, STW)
	write_svg(d, "0029")

	# # 007e tilde
	ry <- 150
	ds <- c(
		d_arc2(yc + ry, hg + 0.25 * cw, yc - ry, hg, STW),
		d_arc4(yc + ry, xcw, yc - ry, xcw - 0.25 * cw, STW)
	)
	dx1 <- 0.36 * cw
	dy1 <- 0.95 * STW
	dy <- 0.5 * STW
	d_middle <- M(hg + 0.25 * cw, yc + ry) +
		Q(hg + dx1, yc + dy1, xc, yc + dy) +
		T(xcw - 0.25 * cw, yc - ry + STW) +
		L(xcw - 0.25 * cw, yc - ry) +
		Q(xcw - dx1, yc - dy1, xc, yc - dy) +
		TZ(hg + 0.25 * cw, yc + ry - STW)
	write_svg(c(ds, d_middle), "007e")

	# 0030 digit 0
	ds <- c(
		d_ellipse(x = xc, y = yc, rx = xc - c(hg, hg + STW), ry = yc - c(vg, vg + STW)), # loop
		d_rect(x = xc, y = yc, w = STW, h = 0.6 * yc) # inside "dot/slash"
	)
	write_svg(ds, "0030")
	# 0031 digit 1
	ds <- c(
		d_rect(x = xc, y = yc, w = STW, h = h - 2 * vg - 2 * STW), # stem,
		d_rect2(ych, xc + 0.5 * STW, ych - STW, xc - 1.5 * STW), # t serif,
		d_rect2(vg + STW, xc + 1.5 * STW, vg, xc - 1.5 * STW)
	) # b serif
	write_svg(ds, "0031")

	# 0032 digit 2
	d2ry <- 0.26 * h
	if (font == "narrow") {
		d2xa <- 0.7 * STW
	} else {
		d2xa <- 0.9 * STW
	}
	ds <- c(
		d_arc412(ych, xcw, ych - 2 * d2ry, hg, STW), # top curve
		d_circle(hg + rp, ych - d2ry, rp), # ball
		d_fslash(
			ych - 2 * d2ry + 1.0 * STW,
			xc + d2xa,
			vg + STW,
			hg,
			STW,
			left = "horizontal",
			right = "diagonal"
		), # stroke
		d_rect2(vg + STW, xcw, vg, hg), # bar
		d_rect2(vg + 2 * STW, xcw, vg + STW, xcw - STW)
	) # lr serif
	write_svg(ds, "0032")

	#### 218a turned digit 2

	# 0033 digit 3
	if (font == "narrow") {
		d3o <- 0.6 * STW
	} else {
		d3o <- 0.8 * STW
	}
	ds <- c(
		d_rect2(ych, hg + STW, ych - STW - STW, hg), # ul serif
		d_rect2(ych, xcw, ych - STW, hg + STW), # bar
		d_fslash(ych - STW, xcw, yc, xc - d3o, STW, right = "horizontal", left = "diagonal"), # stroke
		d_arc41(yc + 1.0 * STW, xcw, vg, xc, STW), # b curve 1
		d_arc3(vg + 3 * STW, xc, vg, hg, STW)
	) # b curve 2
	write_svg(ds, "0033")

	# 218b turned digit 3
	ds <- c(
		d_rect2(vg + STW + STW, xcw - STW, vg, xcw), # lr serif
		d_rect2(vg + STW, xcw - STW, vg, hg), # bar
		d_fslash(yc, xc + d3o, vg + STW, hg, STW, right = "diagonal", left = "horizontal"), # stroke
		d_arc23(ych, xc, yc - 1.0 * STW, hg, STW), # t curve 1
		d_arc1(ych, xcw, ych - 3 * STW, xc, STW), # t curve 2
		d_circle(xcw - rp, ych - 3 * STW, rp)
	) # ball
	write_svg(ds, "218b")

	# 0034 digit 4
	yd4b <- yc - 1.0 * STW
	ds <- c(
		d_rect2(yd4b + 1 * STW, xcw - srw, vg + STW, xcw - srw - STW), # stem
		d_rect2(vg + STW, xcw, vg, xcw - STW - 2 * srw), # b serif
		d_rect2(yd4b, xcw, yd4b - 1.0 * STW, hg), # bar
		d_fslash(ych, xcw - srw, yd4b, hg, STW, left = "horizontal", right = "vertical")
	) # stroke
	write_svg(ds, "0034")

	# 0035 digit 5
	ds <- c(
		d_rect2(ych, xcw, ych - 2 * STW, xcw - STW), # ur serif
		d_rect2(ych, xcw - STW, ych - STW, hg + 0.5 * STW), # bar
		d_rect2(ych - STW, hg + STW + 0.5 * STW, yc - 0.0 * STW, hg + 0.5 * STW), # stem
		d_rect2(yc + 1.0 * STW, xc, yc - 0.0 * STW, hg + STW + 0.5 * STW), # curve 1
		d_arc41(yc + 1.0 * STW, xcw, vg, xc, STW), # curve 2
		d_arc3(0.5 * (vg + yc + 1.0 * STW), xc, vg, hg, STW)
	) # curve 3
	write_svg(ds, "0035")

	# 0036 digit 6
	d6yf <- 0.3
	ds <- c(
		d_ellipse(xc, vg + d6yf * ch, 0.5 * cw + c(0, -STW), d6yf * ch + c(0, -STW)), # loop
		d_arc2(ych, xcw - rp, vg + d6yf * ch, hg, STW), # upper curve
		d_circle(xcw - rp, ych - rp, rp)
	) # ball
	write_svg(ds, "0036")

	# 0037 digit 7
	ds <- c(
		d_rect2(ych, hg + STW, ych - STW - STW, hg), # ul serif
		d_rect2(ych, xcw, ych - STW, hg + STW), # bar
		d_fslash(ych - STW, xcw, vg + STW, xc - 0.5 * STW, STW), # stroke
		d_rect2(vg + STW, xc + 0.5 * STW + STW, vg, xc - 0.5 * STW - STW) # b serif
	)
	write_svg(ds, "0037")
	# 0038 digit 8
	ov8 <- 50 # overlap amount
	ds <- c(
		d_ellipse(
			x = xc,
			y = 0.5 * (yc + ych - ov8),
			rx = xc - c(hg, hg + STW),
			ry = 0.5 * (ov8 + yc - vg) - c(0, STW)
		), # top
		d_ellipse(
			x = xc,
			y = 0.5 * (yc + vg + ov8),
			rx = xc - c(hg, hg + STW),
			ry = 0.5 * (ov8 + yc - vg) - c(0, STW)
		) # bottom
	)
	write_svg(ds, "0038")
	# 0039 digit 9
	d6yf <- 0.3
	if (font == "narrow") {
		d9so <- 30
	} else {
		d9so <- 55
	}
	ds <- c(
		d_ellipse(xc, ych - d6yf * ch, 0.5 * cw + c(0, -STW), d6yf * ch + c(0, -STW)), # loop
		d_fslash(ych - d6yf * ch - STW, xcw - d9so, vg + STW, xc - 0.5 * STW, STW), # lower stroke
		d_rect(xc, vg + 0.5 * STW, 3 * STW, STW)
	) # b serif
	write_svg(ds, "0039")

	# number ten (private use area since doesn't exist in Unicode)
	# PUA f590 (for now)
	if (font == "narrow") {
		ten_rx <- 0.5 * (w - 2 * hg - 1.75 * STW)
		ds <- c(
			d_rect2(ych, hg + STW, vg, hg), # one
			d_ellipse(xcw - ten_rx, yc, ten_rx + c(0, -STW), 0.5 * ch + c(0, -STW))
		)
	} else {
		ten_rx <- 0.5 * (w - 2 * hg - 1.75 * STW - 2 * srw)
		ds <- c(
			d_rect2(ych, hg + srw + STW, vg + STW, hg + srw), # 1 stem
			d_rect2(ych, hg + srw, ych - STW, hg), # 1 t serif
			d_rect2(vg + STW, hg + 2 * srw + STW, vg, hg), # 1 b serif
			d_ellipse(xcw - ten_rx, yc, ten_rx + c(0, -STW), 0.5 * ch + c(0, -STW))
		)
	}
	write_svg(ds, "f590")

	# 0041 latin capital letter a
	if (font == "square") {
		yca <- 0.35 * h
	} else {
		yca <- 0.31 * h
	}
	ds <- c(
		d_fslash(ych, xc, vg + STW, hg + srw2, STW, left = "horizontal", right = "vertical"), # l stroke
		d_bslash(ych, xcw - srw2, vg + STW, xc, STW, left = "vertical", right = "horizontal"), # r stroke
		d_rect2(yca + 0.5 * STW, 0.7 * w, yca - 0.5 * STW, 0.3 * w), # crossbar
		d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg), # ll serif
		d_rect2(vg + STW, xcw, vg, xcw - 2 * srw2 - STW)
	) # lr serif
	write_svg(ds, "0041")

	# 20b3 austral sign
	ds <- c(
		d_fslash(ych, xc, vg + STW, hg + srw2, STW, left = "horizontal", right = "vertical"), # l stroke
		d_bslash(ych, xcw - srw2, vg + STW, xc, STW, left = "vertical", right = "horizontal"), # r stroke
		d_rect2(yca + 1.6 * STW, xcw, yca + 0.6 * STW, hg), # t. crossbar
		d_rect2(yca + 0.1 * STW, xcw, yca - 0.9 * STW, hg), # b. crossbar
		d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg), # ll serif
		d_rect2(vg + STW, xcw, vg, xcw - 2 * srw2 - STW)
	) # lr serif
	write_svg(ds, "20b3")

	# 0042 latin capital letter b
	d_lclb <- c(
		d_rect2(ych, hg + srw + STW, vg, hg + srw), # stem
		d_rect2(ych, xc, ych - STW, hg), # u bar
		d_rect2(yc + 0.5 * STW, xc, yc - 0.5 * STW, hg + STW), # m bar
		d_rect2(vg + STW, xc, vg, hg), # b bar
		d_arc41(ych, xcw, yc - 0.5 * STW, xc, STW), # u bowl
		d_arc41(yc + 0.5 * STW, xcw, vg, xc, STW)
	) # b bowl
	write_svg(d_lclb, "0042")

	# 0e3f thai currency symbol baht
	ds <- c(
		d_rect2(ych, hg + srw2 + STW, vg, hg + srw2), # stem
		d_rect2(ych, xc, ych - STW, hg), # u bar
		d_rect2(yc + 0.5 * STW, xc, yc - 0.5 * STW, hg + STW), # m bar
		d_rect2(vg + STW, xc, vg, hg), # b bar
		d_arc41(ych, xcw, yc - 0.5 * STW, xc, STW), # u bowl
		d_arc41(yc + 0.5 * STW, xcw, vg, xc, STW), # b bowl
		d_rect(xc, yc, STW, ch + 2 * STW)
	)
	write_svg(ds, "0e3f")

	# 20bf bitcoin sign
	xbs <- xc + 0.84 * STW
	ds <- c(
		d_rect2(ych, hg + srw2 + STW, vg, hg + srw2), # stem
		d_rect2(ych, xbs, ych - STW, hg), # u bar
		d_rect2(yc + 0.5 * STW, xbs, yc - 0.5 * STW, hg + STW), # m bar
		d_rect2(vg + STW, xbs, vg, hg), # b bar
		d_arc41(ych, xcw, yc - 0.5 * STW, xbs, STW), # u bowl
		d_arc41(yc + 0.5 * STW, xcw, vg, xbs, STW), # b bowl
		d_rect2(h - vg + STW, xc + 0.9 * STW, h - vg, xc + -0.1 * STW),
		d_rect2(h - vg + STW, xc - 0.5 * STW, h - vg, xc - 1.5 * STW),
		d_rect2(vg, xc + 0.9 * STW, vg - STW, xc + -0.1 * STW),
		d_rect2(vg, xc - 0.5 * STW, vg - STW, xc - 1.5 * STW)
	)
	write_svg(ds, "20bf")

	# 0043 latin capital letter c
	cvo <- 2.5 * STW
	d_clc <- c(
		d_arc23(ych, xc, vg, hg, STW), # l curve
		d_arc1(ych, xcw, ych - cvo, xc, STW), # ur curve
		d_circle(xcw - rp, ych - cvo, rp), # ball
		d_arc4(vg + cvo, xcw, vg, xc, STW)
	) # lr curve
	write_svg(d_clc, "0043")

	# 20b5 cedi sign
	ds <- c(d_clc, d_rect(xc, yc, STW, ch + 2 * STW))
	write_svg(ds, "20b5")

	#### 20a1 colon sign
	#### 20a2 cruzeiro sign

	# 20ac euro sign
	cvo <- 2.0 * STW
	ds <- c(
		d_arc23(ych, xc + 0.5 * STW, vg, hg + STW, STW), # l curve
		d_arc1(ych, xcw, ych - cvo, xc + 0.5 * STW, STW), # ur curve
		d_circle(xcw - rp, ych - cvo, rp), # ball
		d_arc4(vg + cvo, xcw, vg, xc + 0.5 * STW, STW), # lr curve
		d_rect(0.5 * (hg + xcw - STW), yc + c(-0.7, 0.7) * STW, cw - STW, STW)
	) # bars
	write_svg(ds, "20ac")

	# 0186 latin capital letter open o "turned c"
	ds <- c(
		d_arc41(ych, xcw, vg, xc, STW), # r curve
		d_arc2(ych, xc, ych - cvo, hg, STW), # ul curve
		d_circle(hg + rp, ych - cvo, rp), # ball
		d_arc3(vg + cvo, xc, vg, hg, STW)
	) # ll curve
	write_svg(ds, "0186")

	# 0044 latin capital letter d
	ds <- c(
		d_rect2(ych, hg + srw + STW, vg, hg + srw), # stem
		d_rect2(ych, xc, ych - STW, hg), # u bar
		d_rect2(vg + STW, xc, vg, hg), # b bar
		d_arc41(ych, xcw, vg, xc, STW)
	) # bowl
	write_svg(ds, "0044")

	# 0045 latin capital letter e
	ds <- c(
		d_rect2(ych, hg + srw + STW, vg, hg + srw), # stem
		d_rect2(ych, xcw, ych - STW, hg), # t bar
		d_rect2(yc + 0.5 * STW, 0.7 * w, yc - 0.5 * STW, hg + srw), # m bar
		d_rect2(vg + STW, xcw, vg, hg), # b bar
		d_rect2(ych, xcw, ych - STW - srw, xcw - STW), # ur serif
		d_rect2(vg + STW + srw, xcw, vg, xcw - STW)
	) # lr serif
	write_svg(ds, "0045")

	# 0046 latin capital letter f
	ds <- c(
		d_rect2(ych, hg + srw + STW, vg, hg + srw), # stem
		d_rect2(ych, xcw, ych - STW, hg), # t bar
		d_rect2(yc + 0.5 * STW, 0.7 * w, yc - 0.5 * STW, hg + STW), # m bar
		d_rect2(vg + STW, hg + 2 * srw + STW, vg, hg), # b serif
		d_rect2(ych, xcw, ych - STW - srw, xcw - STW)
	) # t serif
	write_svg(ds, "0046")

	# 20a3 french franc sign
	ds <- c(
		d_rect2(ych, hg + srw + STW, vg, hg + srw), # stem
		d_rect2(ych, xcw, ych - STW, hg), # t bar
		d_rect2(yc + 0.5 * STW, 0.7 * w, yc - 0.5 * STW, hg + STW), # m bar
		d_rect2(0.5 * (vg + yc) + 0.75 * STW, hg + 2 * srw + STW, 0.5 * (vg + yc) - 0.25 * STW, hg), # l bar
		d_rect2(vg + STW, hg + 2 * srw + STW, vg, hg), # b serif
		d_rect2(ych, xcw, ych - STW - srw, xcw - STW)
	) # t serif
	write_svg(ds, "20a3")

	# 0047 latin capital letter g
	if (font == "narrow") {
		ghw <- 2.5 * STW
	} else {
		ghw <- 4.0 * STW
	}
	d_clg <- c(
		d_arc23(ych, xc, vg, hg, STW), # l curve
		d_arc1(ych, xcw, ych - cvo, xc, STW), # ur curve
		d_circle(xcw - rp, ych - cvo, rp), # ball
		d_arc4(vg + cvo, xcw, vg, xc, STW), # lr curve
		d_rect2(vg + cvo + STW, xcw, vg + cvo, xcw - STW), # hook 1
		d_rect2(vg + cvo + 2 * STW, xcw, vg + cvo + STW, xcw - ghw)
	) # hook 2
	write_svg(d_clg, "0047")

	# 20b2 guarani sign
	if (font == "narrow") {
		ghw <- 1.7 * STW
	} else {
		ghw <- 3.0 * STW
	}
	ds <- c(
		d_arc23(ych, xc, vg, hg, STW), # l curve
		d_arc1(ych, xcw, ych - cvo, xc, STW), # ur curve
		d_circle(xcw - rp, ych - cvo, rp), # ball
		d_arc4(vg + cvo, xcw, vg, xc, STW), # lr curve
		d_rect2(vg + cvo + STW, xcw, vg + cvo, xcw - STW), # hook 1
		d_rect2(vg + cvo + 2 * STW, xcw, vg + cvo + STW, xcw - ghw), # hook 2
		d_rect(xc, yc, STW, ch + 2 * STW)
	) # stem
	write_svg(ds, "20b2")

	# 0048 latin capital letter h
	ds <- c(
		d_rect2(ych, hg + srw2 + STW, vg, hg + srw2), # left stem
		d_rect2(yc + 0.5 * STW, xcw - srw2, yc - 0.5 * STW, hg + srw2), # crossbar
		d_rect2(ych, xcw - srw2, vg, xcw - STW - srw2), # right stem
		d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg), # ll serif
		d_rect2(ych, hg + 2 * srw2 + STW, ych - STW, hg), # ul serif
		d_rect2(vg + STW, xcw, vg, xcw - 2 * srw2 - STW), # lr serif
		d_rect2(ych, xcw, ych - STW, xcw - 2 * srw2 - STW)
	) # ur serif
	write_svg(ds, "0048")

	# 0049 latin capital letter i
	ds <- c(
		d_rect2(ych, xc + 0.5 * STW, vg, xc - 0.5 * STW), # bar
		d_rect2(ych, xc + 0.5 * STW + srw, ych - STW, xc - 0.5 * STW - srw), # t serif
		d_rect2(vg + STW, xc + 0.5 * STW + srw, vg, xc - 0.5 * STW - srw)
	) # b serif
	write_svg(ds, "0049")

	# 004a latin capital letter j
	ds <- c(
		d_rect2(ych, xcw, ych - STW, xcw - 3 * STW), # t serif
		d_rect2(ych - STW, xcw - STW, 0.4 * h, xcw - 2 * STW), # bar
		d_arc34(0.4 * h, xcw - STW, vg, hg, STW)
	) # hook
	write_svg(ds, "004a")

	# 004b latin capital letter k
	d_clk <- c(
		d_rect2(ych, hg + srw2 + STW, vg, hg + srw2), # l stem
		d_rect2(ych, hg + 2 * srw2 + STW, ych - STW, hg), # ul serif
		d_rect2(ych, xcw, ych - STW, xcw - 2 * srw2 - STW), # ur serif
		d_fslash(
			ych - STW,
			xcw - srw2,
			yc,
			hg + srw2 + STW,
			STW,
			left = "square",
			right = "horizontal"
		), # t stroke
		d_bslash(
			yc,
			xcw - srw2,
			vg + STW,
			hg + srw2 + STW,
			STW,
			left = "square",
			right = "horizontal"
		), # b stroke
		d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg), # ll serif
		d_rect2(vg + STW, xcw, vg, xcw - 2 * srw2 - STW)
	) # lr serif
	write_svg(d_clk, "004b")

	# 20ad kip sign
	if (font == "narrow") {
		xks <- 0.8 * w
	} else {
		xks <- 0.7 * w
	}
	ds <- c(d_clk, d_rect2(yc + 0.5 * STW, xks, yc - 0.5 * STW, hg))
	write_svg(ds, "20ad")

	# 004c latin capital letter l
	ds <- c(
		d_rect2(ych, hg + srw + STW, vg, hg + srw), # stem
		d_rect2(ych, hg + 2 * srw + STW, ych - STW, hg), # t serif
		d_rect2(vg + srw + STW, xcw, vg, xcw - STW), # lr serif
		d_rect2(vg + STW, xcw, vg, hg)
	) # bar
	write_svg(ds, "004c")

	# 004d latin capital letter m
	ds <- c(
		d_rect2(ych - STW, hg + srw2 + STW, vg + STW, hg + srw2), # l stem
		d_rect2(ych - STW, xcw - srw2, vg, xcw - srw2 - STW), # r stem
		d_bslash(ych, xc, vg, hg + srw2 + STW, STW, nib = "vertical"), # l stroke
		d_fslash(ych, xcw - srw2 - STW, vg, xc, STW, nib = "vertical"), # r stroke
		d_rect2(ych, hg + srw2 + STW, ych - STW, hg), # ul serif
		d_rect2(ych, xcw, ych - STW, xcw - srw2 - STW), # ur serif
		d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg), # ll serif
		d_rect2(vg + STW, xcw, vg, xcw - 2 * srw2 - STW)
	) # lr serif
	write_svg(ds, "004d")

	# 004e latin capital letter n
	d_cln <- c(
		d_rect2(ych - STW, hg + srw2 + STW, vg + STW, hg + srw2), # l stem
		d_rect2(ych - STW, xcw - srw2, vg, xcw - srw2 - STW), # r stem
		d_bslash(ych, xcw - srw2 - STW, vg, hg + srw2 + STW, STW, nib = "vertical"), # stroke
		d_rect2(ych, hg + srw2 + STW, ych - STW, hg), # ul serif
		d_rect2(ych, xcw, ych - STW, xcw - 2 * srw2 - STW), # ur serif
		d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg)
	) # ll serif
	write_svg(d_cln, "004e")

	# 20a6 naira sign
	if (font == "square") {
		nsg <- 0.7
	} else {
		nsg <- 0.6
	}
	ds <- c(d_cln, d_rect(xc, yc + c(-nsg, nsg) * STW, cw, STW)) # bars
	write_svg(ds, "20a6")

	# 004f latin capital letter o
	d <- d_ellipse(xc, yc, 0.5 * cw - c(0, STW), 0.5 * ch - c(0, STW))
	write_svg(d, "004f")

	# 0050 latin capital letter p
	ds <- c(
		d_rect2(ych, hg + srw + STW, vg, hg + srw), # stem
		d_rect2(ych, xc, ych - STW, hg), # t bar
		d_rect2(vg + STW, hg + 2 * srw + STW, vg, hg), # b serif
		d_rect2(yc + 0.5 * STW, xc, yc - 0.5 * STW, hg + srw), # b bar
		d_arc41(ych, xcw, yc - 0.5 * STW, xc, STW)
	) # bowl
	write_svg(ds, "0050")

	# 20b1 peso sign
	ds <- c(
		d_rect2(ych, hg + srw2 + STW, vg, hg + srw2), # stem
		d_rect2(ych, xc, ych - STW, hg), # t bar
		d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg), # b serif
		d_rect2(yc + 0.5 * STW, xc, yc - 0.5 * STW, hg + srw2), # b bar
		d_arc41(ych, xcw - srw2, yc - 0.5 * STW, xc, STW), # bowl
		d_rect2(0.63 * ych + 0.33 * yc + 0.5 * STW, xcw, 0.63 * ych + 0.33 * yc - 0.5 * STW, hg), # t strikethrough
		d_rect2(0.32 * ych + 0.66 * yc + 0.5 * STW, xcw, 0.32 * ych + 0.66 * yc - 0.5 * STW, hg)
	) # b strikethrough
	write_svg(ds, "20b1")

	# 0051 latin capital letter q
	ylclq <- 0.40 * h
	ds <- c(
		d_ellipse(xc, yc, 0.5 * cw - c(0, STW), 0.5 * ch - c(0, STW)),
		d_arc1(ylclq, xc + 0.5 * STW, 0.5 * (vg + ylclq), hg, STW),
		d_arc3(0.5 * (vg + ylclq), xcw, vg, xc - 0.5 * STW, STW)
	)
	write_svg(ds, "0051")

	# 0052 latin capital letter r
	ds <- c(
		d_rect2(ych, hg + srw2 + STW, vg, hg + srw2), # stem
		d_rect2(ych, xc, ych - STW, hg), # t bar
		d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg), # ll serif
		d_rect2(vg + STW, xcw, vg, xcw - 2 * srw2 - STW), # lr serif
		d_bslash(yc, xcw - srw2, vg + STW, hg + srw2 + STW, STW), # stroke
		d_rect2(yc + 0.5 * STW, xc, yc - 0.5 * STW, hg + srw2), # b bar
		d_arc41(ych, xcw, yc - 0.5 * STW, xc, STW)
	) # bowl
	write_svg(ds, "0052")

	# 0053 latin capital letter s
	d_lcls <- c(
		d_arc23(ych, xc, yc - 0.5 * STW, hg, STW), # l curve
		d_arc41(yc + 0.5 * STW, xcw, vg, xc, STW), # r curve
		d_arc1(ych, xcw, ych - cvo, xc, STW), # ur curve
		d_circle(xcw - rp, ych - cvo, rp), # ball
		d_arc3(vg + cvo, xc, vg, hg, STW)
	) # lr curve
	write_svg(d_lcls, "0053")

	# 0024 dollar sign
	ds <- c(d_lcls, d_rect(xc, yc, STW, h - 2 * vg + 2 * STW))
	write_svg(ds, "0024")

	#### 20b4 Hryvnia sign
	#### 20b7 Spesmilo sign

	# 0054 latin capital letter t
	ds <- c(
		d_rect2(ych, xcw, ych - STW, hg), # bar
		d_rect2(ych, xc + 0.5 * STW, vg, xc - 0.5 * STW), # stem
		d_rect2(ych, xcw, ych - 2 * STW, xcw - STW), # r serif
		d_rect2(ych, hg + STW, ych - 2 * STW, hg), # l serif
		d_rect2(vg + STW, xc + 1.5 * STW, vg, xc - 1.5 * STW)
	) # b serif
	write_svg(ds, "0054")

	#### 20ae tugrik sign
	#### 20b8 tenge sign

	# 0055 latin capital letter u
	yu <- 0.4 * h
	ds <- c(
		d_rect2(ych, hg + srw2 + STW, yu, hg + srw2), # l stem
		d_rect2(ych, xcw - srw2, yu, xcw - srw2 - STW), # r stem
		d_arc34(yu, xcw - srw2, vg, hg + srw2, STW), # bottom
		d_rect2(ych, hg + 2 * srw2 + STW, ych - STW, hg), # l serif
		d_rect2(ych, xcw, ych - STW, xcw - 2 * srw2 - STW)
	) # r serif
	write_svg(ds, "0055")

	# 0056 latin capital letter v
	ds <- c(
		d_bslash(ych - STW, xc, vg, hg + srw2, STW, left = "horizontal", right = "vertical"), # l stroke
		d_fslash(ych - STW, xcw - srw2, vg, xc, STW, left = "vertical", right = "horizontal"), # r stroke
		d_rect2(ych, hg + 2 * srw2 + STW, ych - STW, hg), # l serif
		d_rect2(ych, xcw, ych - STW, xcw - 2 * srw2 - STW)
	) # r serif
	write_svg(ds, "0056")

	# 0057 latin capital letter w
	if (font == "narrow") {
		wstw <- 0.85 * STW
	} else {
		wstw <- STW
	}
	d_clw <- c(
		d_bslash(ych - STW, 0.35 * w, vg, hg + srw3, wstw, left = "horizontal", right = "vertical"), # ll stroke
		d_fslash(ych, xc, vg, 0.35 * w, wstw, left = "vertical", right = "vertical"), # lr stroke
		d_bslash(ych, 0.65 * w, vg, xc, wstw, left = "vertical", right = "vertical"), # rl stroke
		d_fslash(
			ych - STW,
			xcw - srw3,
			vg,
			0.65 * w,
			wstw,
			left = "vertical",
			right = "horizontal"
		), # rr stroke
		d_rect2(ych, hg + 2 * srw3 + wstw, ych - STW, hg), # l serif
		# d_rect2(ych, xc + srw3, ych - STW, xc - srw3), # m serif
		d_rect2(ych, xcw, ych - STW, xcw - 2 * srw3 - wstw)
	) # r serif
	write_svg(d_clw, "0057")

	# 20a9 won sign
	ds <- c(d_clw, d_rect(x = xc, y = yc + 1.5 * STW, w = cw, h = STW))
	write_svg(ds, "20a9")

	# 0058 latin capital letter x
	ds <- c(
		d_bslash(ych - STW, xcw - srw2, vg + STW, hg + srw2, STW), # l stroke
		d_fslash(ych - STW, xcw - srw2, vg + STW, hg + srw2, STW), # r stroke
		d_rect2(ych, hg + 2 * srw2 + STW, ych - STW, hg), # ul serif
		d_rect2(ych, xcw, ych - STW, xcw - 2 * srw2 - STW), # ur serif
		d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg), # bl serif
		d_rect2(vg + STW, xcw, vg, xcw - 2 * srw2 - STW)
	) # br serif
	write_svg(ds, "0058")

	# 0059 latin capital letter y
	d_lcly <- c(
		d_rect2(ych, hg + 2 * srw2 + STW, ych - STW, hg), # ul serif
		d_rect2(ych, xcw, ych - STW, xcw - 2 * srw2 - STW), # ur serif
		d_bslash(ych - STW, xc, yc, hg + srw2, STW, left = "horizontal", right = "square"), # left stroke
		d_fslash(ych - STW, xcw - srw2, yc, xc, STW, left = "square", right = "horizontal"), # right stroke
		d_rect2(yc, xc + 0.5 * STW, vg, xc - 0.5 * STW), # stem
		d_rect2(vg + STW, xc + 1.5 * STW, vg, xc - 1.5 * STW)
	) # b serif
	write_svg(d_lcly, "0059")

	# 00a5 yen sign
	ds <- c(
		d_lcly,
		d_rect2(yc + 0.8 * STW, xcw - srw2, yc - 0.2 * STW, hg + srw2), # t bar
		d_rect2(yc - 0.7 * STW, xcw - srw2, yc - 1.7 * STW, hg + srw2)
	) # b bar
	write_svg(ds, "00a5")

	# 005a latin capital letter z
	ds <- c(
		d_rect2(ych, xcw, ych - STW, hg), # t bar
		d_rect2(vg + STW, xcw, vg, hg), # b bar
		d_fslash(ych - STW, xcw, vg + STW, hg, STW), # stroke
		d_rect2(vg + 2 * STW, xcw, vg, xcw - STW), # b serif
		d_rect2(ych, hg + STW, ych - 2 * STW, hg)
	) # t serif
	write_svg(ds, "005a")

	rt <- 300
	# 0061 latin small letter a
	xl_ds <- xcw - rt - srw2
	ds <- c(
		d_rect2(yxh, xl_ds + STW, vg + rt, xl_ds), # stem
		d_ellipse(
			0.5 * (hg + xl_ds + STW),
			0.5 * (vg + yxh),
			0.5 * (xl_ds + STW - hg) + c(0, -STW),
			0.5 * (yxh - vg) + c(0, -STW)
		), # ellipse
		d_arc3(vg + rt, xl_ds + rt, vg, xl_ds, STW), # terminal..
		d_rect2(vg + STW, xl_ds + rt + srw2, vg, xl_ds + rt) # terminal
	)
	write_svg(ds, "0061")

	# 0040 commercial at (circled small letter a)
	xl_ds <- hg + srw2
	if (font == "square") {
		at_d <- ncw
	} else {
		at_d <- 0.5 * ncw
	}
	ds <- c(
		d_ellipse(xc, yc, 0.5 * at_d + c(0, -STW), 0.5 * at_d + c(0, -STW)),
		d_rect2(yc + 0.5 * at_d, xc + 0.5 * at_d, yc, xc + 0.5 * at_d - STW),
		d_arc34(yc + STW, xcw, yc - 0.5 * at_d, xc + 0.5 * at_d - STW, STW),
		d_arc1(ych, xcw, yc + STW, xc, STW),
		d_arc23(ych, xc, vg, hg, STW),
		d_arc4(vg + 1.8 * STW, xcw - STW, vg, xc, STW)
	)
	write_svg(ds, "0040")

	# 0062 latin small letter b
	xl_ds <- hg + srw2
	ds <- c(
		d_rect2(ych, xl_ds + STW, 0.5 * (vg + yxh), xl_ds), # stem
		d_rect2(ych, xl_ds, ych - STW, xl_ds - srw2), # t serif
		d_ellipse(
			0.5 * (hg + srw2 + xcw),
			0.5 * (vg + yxh),
			0.5 * (xcw - hg - srw2) + c(0, -STW),
			0.5 * (yxh - vg) + c(0, -STW)
		) # ellipse
	) # terminal
	write_svg(ds, "0062")

	# 0063 latin small letter c
	cvo <- 1.7 * STW
	d_slc <- c(
		d_arc23(yxh, xc, vg, xc - 0.5 * ncw, STW), # l curve
		d_arc1(yxh, xc + 0.5 * ncw, yxh - cvo, xc, STW), # ur curve
		d_circle(xc + 0.5 * ncw - rp, yxh - cvo, rp), # ball
		d_arc4(vg + cvo, xc + 0.5 * ncw, vg, xc, STW)
	) # lr curve
	write_svg(d_slc, "0063")

	####  00a9 copyright sign

	# 0254 latin small letter open o "turned c"
	ds <- c(
		d_arc41(yxh, xc + 0.5 * ncw, vg, xc, STW), # r curve
		d_arc2(yxh, xc, yxh - cvo, xc - 0.5 * ncw, STW), # ul curve
		d_circle(xc - 0.5 * ncw + rp, yxh - cvo, rp), # ball
		d_arc3(vg + cvo, xc, vg, xc - 0.5 * ncw, STW)
	) # ll curve
	write_svg(ds, "0254")

	#### 1f12f copyleft symbol

	# 00a2 cent sign
	ds <- c(d_slc, d_rect(xc, 0.5 * (vg + yxh), STW, ncw + 2 * STW))
	write_svg(ds, "00a2")

	# 0064 latin small letter d
	xl_ds <- xcw - rt - srw2
	ds <- c(
		d_rect2(ych, xl_ds + STW, vg + rt, xl_ds), # stem
		d_rect2(ych, xl_ds, ych - STW, xl_ds - srw2), # t serif
		d_ellipse(
			0.5 * (hg + xl_ds + STW),
			0.5 * (vg + yxh),
			0.5 * (xl_ds + STW - hg) + c(0, -STW),
			0.5 * (yxh - vg) + c(0, -STW)
		), # ellipse
		d_arc3(vg + rt, xl_ds + rt, vg, xl_ds, STW), # terminal..
		d_rect2(vg + STW, xl_ds + rt + srw2, vg, xl_ds + rt)
	) # terminal
	write_svg(ds, "0064")

	#### 20ab dong sign

	# 0065 latin small letter e
	ds <- c(
		d_arc23(yxh, xc, vg, xc - 0.5 * ncw, STW), # l curve
		d_arc1(yxh, xc + 0.5 * ncw, yxh - cvo - STW, xc, STW), # ur curve
		d_rect2(yxh - cvo - STW + STW, xc + 0.5 * ncw - 30, yxh - cvo - STW, xc - 0.5 * ncw + 30), # bar
		d_arc4(vg + cvo, xc + 0.5 * ncw, vg, xc, STW)
	) # lr curve
	write_svg(ds, "0065")

	# 0066 latin small letter f
	# yslt <- 0.7 * h
	yslt <- yxh
	ds <- c(
		d_rect2(ych - 0.2 * h, xc + 0.5 * STW, vg + STW, xc - 0.5 * STW), # stem
		d_arc2(ych, xcw - rp, ych - 0.2 * h, xc - 0.5 * STW, STW), # ur curve
		d_circle(xcw - rp, ych - rp, rp), # ball
		d_rect2(yslt, xc + 2.0 * STW, yslt - STW, xc - 2.0 * STW), # bar
		d_rect2(vg + STW, xc + 1.5 * STW, vg, xc - 1.5 * STW)
	) # b serif
	write_svg(ds, "0066")

	# 00a3 pound sign
	yslt <- yxh
	ds <- c(
		d_rect2(ych - 0.2 * h, hg + 2 * STW, vg + STW, hg + STW), # stem
		d_arc12(ych, xcw, ych - 0.2 * h, hg + STW, STW), # ur curve
		d_circle(xcw - rp, ych - 0.2 * h, rp), # ball
		d_rect2(yslt, hg + 3.0 * STW, yslt - STW, hg), # t. bar
		d_rect2(vg + srw + STW, xcw, vg, xcw - STW), # lr serif
		d_rect2(vg + STW, xcw, vg, hg)
	) # b. bar
	write_svg(ds, "00a3")

	# 20a4 lira sign (like a pound sign with two horizontal lines)
	yslt <- yxh
	ds <- c(
		d_rect2(ych - 0.2 * h, hg + 2 * STW, vg + STW, hg + STW), # stem
		d_arc12(ych, xcw, ych - 0.2 * h, hg + STW, STW), # ur curve
		d_circle(xcw - rp, ych - 0.2 * h, rp), # ball
		d_rect2(yslt + 0.0 * STW, hg + 3.0 * STW, yslt - 1.0 * STW, hg), # t. bar
		d_rect2(yslt - 1.5 * STW, hg + 3.0 * STW, yslt - 2.5 * STW, hg), # m. bar
		d_rect2(vg + srw + STW, xcw, vg, xcw - STW), # lr serif
		d_rect2(vg + STW, xcw, vg, hg) # b. bar
	)
	write_svg(ds, "20a4")

	# 0067 latin small letter g
	xl_ds <- xcw - srw2
	ds <- c(
		d_rect2(yxh, xl_ds, STW + 0.5 * STW, xl_ds - STW), # stem
		d_ellipse(
			0.5 * (hg + xl_ds),
			0.5 * (vg + yxh),
			0.5 * (xl_ds - hg) + c(0, -STW),
			0.5 * (yxh - vg) + c(0, -STW)
		),
		d_arc4(STW + 0.5 * STW, xl_ds, 0, xc, STW),
		d_arc3(STW + 0.5 * STW, xc, 0, hg, STW)
	)
	write_svg(ds, "0067")

	# 0068 latin small letter h
	ds <- c(
		d_rect2(ych, hg + srw2 + STW, vg, hg + srw2), # l stem
		d_rect2(ych, hg + srw2 + STW, ych - STW, hg), # ul serif
		d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg), # ll serif
		d_arc12(yxh, xcw - srw2, yxh - 2 * STW, hg + srw2, STW), # curve
		d_rect2(yxh - 2 * STW, xcw - srw2, vg, xcw - srw2 - STW), # r stem
		d_rect2(vg + STW, xcw, vg, xcw - 2 * srw2 - STW) # lr serif
	)
	write_svg(ds, "0068")

	# 0131 latin small letter dotless i
	d_dli <- c(
		d_rect2(yxh - STW, xc + 0.5 * STW, vg + STW, xc - 0.5 * STW), # stem
		d_rect2(yxh, xc + 0.5 * STW, yxh - STW, xc - 1.5 * STW), # t serif
		d_rect2(vg + STW, xc + 1.5 * STW, vg, xc - 1.5 * STW)
	) # b serif
	write_svg(d_dli, "0131")

	# 0069 latin small letter i
	d_i <- c(d_dli, d_circle(xc, yxh + 0.7 * STW + rp, rp))
	write_svg(d_i, "0069")

	# 0237 latin small letter dotless j
	d_dlj <- c(
		d_rect2(yxh - STW, xc + 0.5 * STW, vg + STW, xc - 0.5 * STW), # stem
		d_rect2(yxh, xc + 0.5 * STW, yxh - STW, xc - 1.5 * STW), # t serif
		d_arc4(vg + STW, xc + 0.5 * STW, 0, xc - 2.0 * STW, STW)
	) # hook
	write_svg(d_dlj, "0237")

	# 006a latin small letter j
	d_j <- c(d_dlj, d_circle(xc, yxh + 0.7 * STW + rp, rp))
	write_svg(d_j, "006a")

	# 006b latin small letter k
	d_slk <- c(
		d_rect2(ych, hg + srw2 + STW, vg, hg + srw2), # l stem
		d_rect2(ych, hg + srw2 + STW, ych - STW, hg), # ul serif
		d_rect2(yxh, xcw, yxh - STW, xcw - 3 * srw2 - STW), # ur serif
		d_fslash(
			yxh - STW,
			xcw - srw2,
			0.5 * (yxh + vg),
			hg + srw2 + STW,
			STW,
			left = "square",
			right = "horizontal"
		), # t stroke
		d_bslash(
			0.5 * (yxh + vg),
			xcw - srw2,
			vg + STW,
			hg + srw2 + STW,
			STW,
			left = "square",
			right = "horizontal"
		), # b stroke
		d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg), # ll serif
		d_rect2(vg + STW, xcw, vg, xcw - 3 * srw2 - STW) # lr serif
	)
	write_svg(d_slk, "006b")

	# 006c latin small letter l
	ds <- c(
		d_rect2(ych - STW, xc + 0.5 * STW, vg + rt, xc - 0.5 * STW), # stem
		d_rect2(ych, xc + 0.5 * STW, ych - STW, xc - 1.5 * STW), # t serif
		d_arc3(vg + rt, xc - 0.5 * STW + rt, vg, xc - 0.5 * STW, STW), # terminal..
		d_rect2(vg + STW, xc + 0.5 * STW + rt, vg, xc - 0.5 * STW + rt)
	) # terminal
	write_svg(ds, "006c")

	# 006d latin small letter m
	ds <- c(
		d_rect2(yxh, hg + srw2 + STW, vg, hg + srw2), # l stem
		d_rect2(yxh, hg + srw2 + STW, yxh - STW, hg), # ul serif
		d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg), # ll serif
		d_arc12(yxh, xc + 0.5 * STW, yxh - 2 * STW, hg + srw2, STW), # l curve
		d_rect2(yxh - 2 * STW, xc + 0.5 * STW, vg + STW + srw2, xc - 0.5 * STW), # m stem
		d_arc12(yxh, xcw - srw2, yxh - 2 * STW, xc - 0.5 * STW, STW), # r curve
		d_rect2(yxh - 2 * STW, xcw - srw2, vg, xcw - srw2 - STW), # r stem
		d_rect2(vg + STW, xcw, vg, xcw - 2 * srw2 - STW) # lr serif
	)
	write_svg(ds, "006d")

	#### 20a5 mill sign

	# 006e latin small letter n
	ds <- c(
		d_rect2(yxh, hg + srw2 + STW, vg, hg + srw2), # l stem
		d_rect2(yxh, hg + srw2 + STW, yxh - STW, hg), # ul serif
		d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg), # ll serif
		d_arc12(yxh, xcw - srw2, yxh - 2 * STW, hg + srw2, STW), # curve
		d_rect2(yxh - 2 * STW, xcw - srw2, vg, xcw - srw2 - STW), # r stem
		d_rect2(vg + STW, xcw, vg, xcw - 2 * srw2 - STW) # lr serif
	)
	write_svg(ds, "006e")

	# 006f latin small letter o
	write_svg(
		d_ellipse(xc, 0.5 * (vg + yxh), 0.5 * ncw + c(0, -STW), 0.5 * ncw + c(0, -STW)),
		"006f"
	)

	# 0070 latin small letter p
	xl_ds <- hg + srw2
	ds <- c(
		d_rect2(yxh, xl_ds + STW, STW, xl_ds), # stem
		d_rect2(yxh, xl_ds, yxh - STW, xl_ds - srw2), # t serif
		d_rect2(STW, xl_ds + STW + srw2, 0, xl_ds - srw2), # b serif
		d_ellipse(
			0.5 * (hg + srw2 + xcw),
			0.5 * (vg + yxh),
			0.5 * (xcw - hg - srw2) + c(0, -STW),
			0.5 * (yxh - vg) + c(0, -STW)
		)
	)
	write_svg(ds, "0070")

	# 0071 latin small letter q
	xl_ds <- xcw - srw2
	ds <- c(
		d_rect2(yxh, xl_ds, STW, xl_ds - STW), # stem
		d_ellipse(
			0.5 * (hg + xl_ds),
			0.5 * (vg + yxh),
			0.5 * (xl_ds - hg) + c(0, -STW),
			0.5 * (yxh - vg) + c(0, -STW)
		), # ellipse
		d_rect2(STW, xcw, 0, xcw - STW - 2 * srw2) # b serif
		# d_arc3(rt, xl_ds + rt, 0, xl_ds, STW), # terminal..
		# d_rect2(STW, xl_ds + rt + srw2, 0, xl_ds + rt) # terminal
	)
	write_svg(ds, "0071")

	# 0072 latin small letter r
	d_lr <- c(
		d_rect2(yxh - STW, xc - 0.5 * ncw + STW + STW, vg + STW, xc - 0.5 * ncw + STW), # stem
		d_rect2(yxh, xc - 0.5 * ncw + STW + STW, yxh - STW, xc - 0.5 * ncw), # t serif
		d_rect2(vg + STW, xc - 0.5 * ncw + 2 * STW + STW, vg, xc - 0.5 * ncw), # b serif
		d_arc2(yxh, xc + 0.5 * ncw - rp, yxh - 2 * STW, xc - 0.5 * ncw + STW, STW), # curve
		d_circle(xc + 0.5 * ncw - rp, yxh - rp, rp)
	) # ball
	write_svg(d_lr, "0072")

	# 0073 latin small letter s
	ssvo <- 1.3 * STW
	d_lsls <- c(
		d_arc23(yxh, xc, 0.5 * (yxh + vg) - 0.5 * STW, xc - 0.5 * ncw, STW), # l curve
		d_arc41(0.5 * (yxh + vg) + 0.5 * STW, xc + 0.5 * ncw, vg, xc, STW), # r curve
		d_arc1(yxh, xc + 0.5 * ncw, yxh - ssvo, xc, STW), # ur curve
		d_circle(xc + 0.5 * ncw - rp, yxh - ssvo, rp), # ball
		d_arc3(vg + ssvo, xc, vg, xc - 0.5 * ncw, STW)
	) # lr curve
	write_svg(d_lsls, "0073")

	# 00a7 section sign (aka double-s)
	h_s <- 0.56 * h
	ssvo <- 1.6 * STW
	d_lss <- c(
		d_arc1(ych, xc + 0.5 * ncw, ych - ssvo, xc, STW), # t ur curve
		d_arc23(ych, xc, ych - ch / 3 - 0.5 * STW, xc - 0.5 * ncw, STW), # t l curve
		d_arc41(ych - ch / 3 + 0.5 * STW, xc + 0.5 * ncw, vg + ch / 3 - 0.5 * STW, xc, STW), # t r curve
		d_arc3(vg + ch / 3 - 0.5 * STW + ssvo, xc, vg + ch / 3 - 0.5 * STW, xc - 0.5 * ncw, STW), # t lr curve
		d_circle(xc + 0.5 * ncw - rp, ych - ssvo, rp), # ball
		d_arc23(ych - ch / 3 + 0.5 * STW, xc, vg + ch / 3 - 0.5 * STW, xc - 0.5 * ncw, STW), # b l curve
		d_arc41(vg + ch / 3 + 0.5 * STW, xc + 0.5 * ncw, vg, xc, STW), # b r curve
		d_arc1(ych - ch / 3 + 0.5 * STW, xc + 0.5 * ncw, ych - ch / 3 + 0.5 * STW - ssvo, xc, STW), # b ur curve
		d_arc3(vg + ssvo, xc, vg, xc - 0.5 * ncw, STW)
	) # b lr curve
	write_svg(d_lss, "00a7")

	# 0074 latin small letter t
	# yslt <- 0.7 * h
	yslt <- yxh
	ds <- c(
		d_rect2(ych, xc + 0.5 * STW, vg + rt, xc - 0.5 * STW), # stem
		d_rect2(ych, xc - 0.5 * STW, ych - STW, xc - 0.5 * STW - STW), # t serif
		d_rect2(yslt, xc + 2.0 * STW, yslt - STW, xc - 2.0 * STW), # bar
		d_arc3(vg + rt, xc - 0.5 * STW + rt, vg, xc - 0.5 * STW, STW), # terminal..
		d_rect2(vg + STW, xc + 0.5 * STW + rt, vg, xc - 0.5 * STW + rt)
	) # terminal
	write_svg(ds, "0074")

	# 0026 ampersand is a stylized ligature of e t
	# https://en.wikipedia.org/wiki/Ampersand
	if (font == "square") {
		x_bar_r <- xcw - 2.0 * STW
	} else {
		x_bar_r <- xcw - 1.0 * STW
	}
	ds <- c(
		d_arc1(ych, x_bar_r, ych - cvo, xc, STW), # ur curve
		d_arc23(ych, xc, yc - 0.5 * STW, hg, STW), # ul curve
		d_arc23(yc + 0.5 * STW, xc, vg, hg, STW), # ll curve
		d_arc4(vg + 1.5 * STW, x_bar_r, vg, xc, STW), # lr curve
		d_rect2(yc - 0.5 * STW, x_bar_r, vg + 1.5 * STW, x_bar_r - STW), # stem
		d_rect2(yc + 0.5 * STW, xcw, yc - 0.5 * STW, xc), # bar
		d_circle(x_bar_r - rp, ych - cvo, rp) # ball
	)
	write_svg(ds, "0026")

	# 0075 latin small letter u
	xl_ds <- xcw - rt - srw2
	ds <- c(
		d_rect2(yxh - STW, hg + STW + srw2, vg + rt, hg + srw2), # l stem
		d_rect2(yxh, hg + STW + 2 * srw2, yxh - STW, hg), # l bar
		d_arc3(vg + rt, 0.5 * (hg + srw2 + xl_ds + STW), vg, hg + srw2, STW),
		d_arc4(vg + rt, xl_ds + STW, vg, 0.5 * (hg + srw2 + xl_ds + STW), STW),
		d_arc3(vg + rt, xl_ds + rt, vg, xl_ds, STW), # terminal..
		d_rect2(yxh, xl_ds + STW + srw2, yxh - STW, xl_ds - srw2), # r bar
		d_rect2(yxh - STW, xl_ds + STW, vg + rt, xl_ds), # r stem
		d_rect2(vg + STW, xl_ds + rt + srw2, vg, xl_ds + rt) # terminal
	)
	write_svg(ds, "0075")

	# 0076 latin small letter v
	ds <- c(
		d_bslash(yxh - STW, xc, vg, hg + srw2, STW, left = "horizontal", right = "vertical"), # l stroke
		d_fslash(yxh - STW, xcw - srw2, vg, xc, STW, left = "vertical", right = "horizontal"), # r stroke
		d_rect2(yxh, hg + 2 * srw2 + STW, yxh - STW, hg), # l serif
		d_rect2(yxh, xcw, yxh - STW, xcw - 2 * srw2 - STW)
	) # r serif
	write_svg(ds, "0076")

	# 0077 latin small letter w
	if (font == "narrow") {
		wstw <- 0.85 * STW
	} else {
		wstw <- STW
	}
	ds <- c(
		d_bslash(yxh - STW, 0.35 * w, vg, hg + srw3, wstw, left = "horizontal", right = "vertical"), # ll stroke
		d_fslash(yxh, xc, vg, 0.35 * w, wstw, left = "vertical", right = "vertical"), # lr stroke
		d_bslash(yxh, 0.65 * w, vg, xc, wstw, left = "vertical", right = "vertical"), # rl stroke
		d_fslash(
			yxh - STW,
			xcw - srw3,
			vg,
			0.65 * w,
			wstw,
			left = "vertical",
			right = "horizontal"
		), # rr stroke
		d_rect2(yxh, hg + 2 * srw3 + wstw, yxh - STW, hg), # l serif
		# d_rect2(yxh, xc + srw3, yxh - STW, xc - srw3), # m serif
		d_rect2(yxh, xcw, yxh - STW, xcw - 2 * srw3 - wstw)
	) # r serif
	write_svg(ds, "0077")

	# 0078 latin small letter x
	ds <- c(
		d_bslash(yxh - STW, xcw - srw2, vg + STW, hg + srw2, STW), # l stroke
		d_fslash(yxh - STW, xcw - srw2, vg + STW, hg + srw2, STW), # r stroke
		d_rect2(yxh, hg + 2 * srw2 + STW, yxh - STW, hg), # ul serif
		d_rect2(yxh, xcw, yxh - STW, xcw - 2 * srw2 - STW), # ur serif
		d_rect2(vg + STW, hg + 2 * srw2 + STW, vg, hg), # bl serif
		d_rect2(vg + STW, xcw, vg, xcw - 2 * srw2 - STW)
	) # br serif
	write_svg(ds, "0078")

	# 0079 latin small letter y
	xl_ds <- xcw - srw2
	ds <- c(
		d_rect2(yxh - STW, hg + STW + srw2, vg + rt, hg + srw2), # l stem
		d_rect2(yxh, hg + STW + 2 * srw2, yxh - STW, hg), # l bar
		d_arc3(vg + rt, 0.5 * (hg + srw2 + xl_ds), vg, hg + srw2, STW),
		d_arc4(vg + rt, xl_ds, vg, 0.5 * (hg + srw2 + xl_ds), STW),
		d_rect2(yxh, xl_ds + srw2, yxh - STW, xl_ds - STW - srw2), # r bar
		d_rect2(yxh - STW, xl_ds, STW + 0.4 * STW, xl_ds - STW), # r stem
		d_arc4(STW + 0.4 * STW, xl_ds, 0, xc, STW),
		d_arc3(STW + 0.4 * STW, xc, 0, hg + srw2, STW)
	)
	write_svg(ds, "0079")

	# 007a latin small letter z
	ds <- c(
		d_rect2(yxh, xc + 0.5 * ncw, yxh - STW, xc - 0.5 * ncw), # t bar
		d_rect2(vg + STW, xc + 0.5 * ncw, vg, xc - 0.5 * ncw), # b bar
		d_fslash(yxh - STW, xc + 0.5 * ncw, vg + STW, xc - 0.5 * ncw, STW), # stroke
		d_rect2(vg + 2 * STW, xc + 0.5 * ncw, vg, xc + 0.5 * ncw - STW), # b serif
		d_rect2(yxh, xc - 0.5 * ncw + STW, yxh - 2 * STW, xc - 0.5 * ncw)
	) # t serif
	write_svg(ds, "007a")

	# 005f low line (to go under baseline and reaches from side to side)
	d <- d_rect2(STW, w, 0, 0)
	write_svg(d, "005f")

	# 203e over line
	d <- d_rect2(h, w, h - STW, 0)
	write_svg(d, "203e")

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

	# 003c less-than sign
	ds <- c(
		d_fslash(yc + 0.50 * cw, xcw, yc, hg, STW, left = "horizontal", right = "vertical"), # t stroke
		d_bslash(yc, xcw, yc - 0.50 * cw, hg, STW, left = "horizontal", right = "vertical")
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
			STW,
			left = "horizontal",
			right = "vertical"
		), # t stroke
		d_bslash(
			yc,
			xc + 0.5 * waqm,
			yc - 0.50 * cw,
			xc - 0.5 * waqm,
			STW,
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
			STW,
			left = "vertical",
			right = "horizontal"
		), # t stroke
		d_fslash(
			yc,
			xc + 0.5 * waqm,
			yc - 0.50 * cw,
			xc - 0.5 * waqm,
			STW,
			left = "vertical",
			right = "horizontal"
		)
	) # b stroke
	write_svg(ds, "203a")

	# 00ab left-pointing double angle quotation mark
	ds <- c(
		d_fslash(yc + 0.50 * cw, hg + waqm, yc, hg, STW, left = "horizontal", right = "vertical"), # l t stroke
		d_bslash(yc, hg + waqm, yc - 0.50 * cw, hg, STW, left = "horizontal", right = "vertical"), # l b stroke
		d_fslash(yc + 0.50 * cw, xcw, yc, xcw - waqm, STW, left = "horizontal", right = "vertical"), # r t stroke
		d_bslash(yc, xcw, yc - 0.50 * cw, xcw - waqm, STW, left = "horizontal", right = "vertical")
	) # r b stroke
	write_svg(ds, "00ab")

	# 00bb right-pointing double angle quotation mark
	ds <- c(
		d_bslash(yc + 0.50 * cw, hg + waqm, yc, hg, STW, left = "vertical", right = "horizontal"), # l t stroke
		d_fslash(yc, hg + waqm, yc - 0.50 * cw, hg, STW, left = "vertical", right = "horizontal"), # l b stroke
		d_bslash(yc + 0.50 * cw, xcw, yc, xcw - waqm, STW, left = "vertical", right = "horizontal"), # r t stroke
		d_fslash(yc, xcw, yc - 0.50 * cw, xcw - waqm, STW, left = "vertical", right = "horizontal")
	) # r b stroke
	write_svg(ds, "00bb")

	# 003d equals sign
	d <- d_rect(xc, yc + c(-0.25, 0.25) * cw, cw, STW)
	write_svg(d, "003d")

	# 003e greater-than sign
	ds <- c(
		d_bslash(yc + 0.50 * cw, xcw, yc, hg, STW, left = "vertical", right = "horizontal"), # t stroke
		d_fslash(yc, xcw, yc - 0.50 * cw, hg, STW, left = "vertical", right = "horizontal")
	) # b stroke
	write_svg(ds, "003e")

	# 007b left curly bracket
	ds <- c(
		d_arc3(yc - 0.25 * ch, xc + 1.5 * STW, vg, xc - 0.5 * STW, STW), # b b curve
		d_arc2(ych, xc + 1.5 * STW, yc + 0.25 * ch, xc - 0.5 * STW, STW), # t t curve
		d_arc4(yc + 0.25 * ch, xc + 0.5 * STW, yc - 0.5 * STW, xc - 1.5 * STW, STW), # t b curve
		d_arc1(yc + 0.5 * STW, xc + 0.5 * STW, yc - 0.25 * ch, xc - 1.5 * STW, STW)
	) # b t curve
	write_svg(ds, "007b")
	# 007d right curly bracket
	ds <- c(
		d_arc4(yc - 0.25 * ch, xc + 0.5 * STW, vg, xc - 1.5 * STW, STW), # b b curve
		d_arc1(ych, xc + 0.5 * STW, yc + 0.25 * ch, xc - 1.5 * STW, STW), # t t curve
		d_arc3(yc + 0.25 * ch, xc + 1.5 * STW, yc - 0.5 * STW, xc - 0.5 * STW, STW), # t b curve
		d_arc2(yc + 0.5 * STW, xc + 1.5 * STW, yc - 0.25 * ch, xc - 0.5 * STW, STW)
	) # b t curve
	write_svg(ds, "007d")

	# 00d7 multiplication sign
	ms <- 0.8 * 0.5
	d <- c(
		d_fslash(yc + ms * cw, xc + ms * cw, yc - ms * cw, xc - ms * cw, STW, nib = "diagonal"),
		d_bslash(yc + ms * cw, xc + ms * cw, yc - ms * cw, xc - ms * cw, STW, nib = "diagonal")
	)
	write_svg(d, "00d7")

	# 00a8 diaeresis
	write_svg(d_circle(xc + c(-2, 2) * rp, ych - rp, rp), "00a8")

	# 00b7 middle dot
	write_svg(d_circle(xc, yc, rp), "00b7")

	# 2022 bullet
	write_svg(d_circle(xc, yc, 2 * rp), "2022")

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

	# 00a4 (generic) currency sign
	gcso <- 0.2 * ncw
	ds <- c(
		d_circle(xc, yc, 0.45 * ncw + c(0, -STW)),
		d_fslash(yc - gcso, xc - gcso, yc - 0.5 * ncw, hg, STW, nib = "diagonal"), # ll stroke
		d_bslash(yc - gcso, xcw, yc - 0.5 * ncw, xc + gcso, STW, nib = "diagonal"), # lr stroke
		d_bslash(yc + 0.5 * ncw, xc - gcso, yc + gcso, hg, STW, nib = "diagonal"), # ul stroke
		d_fslash(yc + 0.5 * ncw, xcw, yc + gcso, xc + gcso, STW, nib = "diagonal")
	) # ur stroke
	write_svg(ds, "00a4")

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
		))
	) |>
		as_hex()
}
