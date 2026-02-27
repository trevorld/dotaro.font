create_miscellaneous_symbols <- function(font = "square") {
	h <- dotaro_height(font) # glyph height
	w <- dotaro_width(font) # glyph width
	do.call(local_options, glyph_options(w, h, font))

	ch <- dotaro_cap_height(font)
	cw <- dotaro_cap_width(font)
	vg <- dotaro_vertical_gap(font)
	hg <- dotaro_horizontal_gap(font)
	ow <- dotaro_outline_stroke_width(font)
	srw <- dotaro_stroke_width(font)

	ch <- dotaro_cap_height(font)

	xc <- w / 2
	yc <- h / 2

	ych <- h - vg
	xcw <- w - hg

	# 2605 Black Star
	ro <- 0.5 * cw
	s <- isotoxal_2ngon_inner_radius(n = 5, d = 2)
	d_2605 <- d_isotoxal_2ngon(xc, yc, ro, s, 5)
	write_svg(d_2605, "2605")

	# 2606 White Star
	d_2606 <- d_isotoxal_2ngon(xc, yc, ro, s, 5, offset = -ow)
	write_svg(d_2605 + d_2606, "2606")

	# 272a circled white star
	write_svg(d_circle(xc, yc, ro) + d_2605, "272a")
	# 272b open center black star
	ri <- s * ro
	write_svg(d_2605 + d_circle(xc, yc, ri), "272b")
	# 272c black center white star
	write_svg(d_2605 + d_2606 + d_circle(xc, yc, ri - ow), "272c")
	# 272d outlined black star
	write_svg(d_2605 + d_2606 + d_isotoxal_2ngon(xc, yc, ro, s, 5, offset = -2 * ow), "272d")
	# Throws an error if I let `offset` get bigger than -2.4 * ow
	# # 272e heavy outlined black star
	# write_svg(d_2605 + d_2606 + d_polygon(x, y, offset = -2.4 * ow), "272e")

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
	d_2742 <- d_circle(xc, yc, c(ro, ri, ri - ow)) + d_isotoxal_2ngon(xc, yc, ro, s, 8)
	write_svg(d_2742, "2742")

	# 2739 twelve-pointed black star
	s <- isotoxal_2ngon_inner_radius(n = 12, d = 5)
	d_2739 <- d_isotoxal_2ngon(xc, yc, ro, s, 12)
	write_svg(d_2739, "2739")

	# 2663 Black Club Suit
	if (font == "square") {
		r <- 1.1 * srw
		rt <- r
		ycl <- yc
		base <- d_rect(x = xc, y = yc - 0.5 * r, w = srw, h = ch - r)
	} else {
		r <- 1.6 * srw
		rt <- 2.1 * srw
		ycl <- yc - 0.1 * ch
		base <- c(
			d_rect(x = xc, y = yc - 0.5 * r, w = srw, h = ch - r),
			d_polygon(
				x = c(xc - srw, xc, xc + srw),
				y = c(vg, vg + 0.5 * ch, vg)
			)
		)
	}
	l2 <- d_rect(x = xc, y = ycl, h = srw, w = cw - 2 * r)
	c1 <- d_circle(x = hg + r, y = ycl, r = r)
	c2 <- d_circle(x = xc, y = h - vg - rt, r = rt)
	c3 <- d_circle(x = w - hg - r, y = ycl, r = r)
	write_svg(c(base, l2, c1, c2, c3), "2663")
	# 2667 white club suit (derived via OUTLINE_FROM_TO)

	# 2660 Black Spade Suit
	hr <- cw / 4
	vr <- cw / 5
	if (font == "narrow") {
		base <- d_polygon(
			x = c(xc - 0.7 * srw, xc, xc + 0.7 * srw),
			y = c(vg, yc + 2 * srw, vg)
		)
	}
	bso <- srw
	d <- M(xc, vg + vr + bso) +
		A(hr, vr, 0, 0, 0, xc - 2 * hr, vg + vr + bso) +
		Q(hg, ych - 0.56 * ch, xc, h - vg) +
		Q(w - hg, ych - 0.56 * ch, xc + 2 * hr, vg + vr + bso) +
		AZ(hr, vr, 0, 0, 0, xc, vg + vr + bso)
	write_svg(c(base, d), "2660")
	# 2664 white spade suit (derived via OUTLINE_FROM_TO)

	# 2665 Black Heart Suit
	hr <- cw / 4
	vr <- cw / 4
	d <- M(xc, h - vg - vr) +
		A(hr, vr, 0, 0, 0, xc + 2 * hr, h - vg - vr) +
		Q(w - hg, vg + 0.56 * ch, xc, vg) +
		Q(hg, vg + 0.56 * ch, xc - 2 * hr, h - vg - vr) +
		AZ(hr, vr, 0, 0, 0, xc, h - vg - vr)
	write_svg(d, "2665")
	# 2661 white heart suit (derived via OUTLINE_FROM_TO)

	# 2666 Black Diamond Suit
	x <- c(xc, w - hg, xc, hg)
	y <- c(h - vg, yc, vg, yc)
	d_2666 <- MZ(x, y)
	write_svg(d_2666, "2666")

	# 2662 White Diamond Suit
	d_2662 <- d_polygon(x, y, offset = -ow)
	write_svg(d_2666 + d_2662, "2662")

	d_26aa <- d_circle(xc, yc, 0.5 * cw - ow)
	d_26ab <- d_circle(xc, yc, 0.5 * cw)
	# 26aa Medium White Circle
	write_svg(d_26ab + d_26aa, "26aa")
	# 26ab Medium Black Circle
	write_svg(d_26ab, "26ab")

	# PUA f5b8 Latin Coin Suit
	d_diam <- d_diamond(xc, yc, w = 0.5 * cw, h = 0.5 * cw)
	write_svg(d_26ab + d_diam, "f5b8")

	# 265a black chess king
	d <- M(hg + srw, vg) +
		A(srw, yc + 2 * srw - vg, x = hg, y = yc + 2 * srw) +
		A(xc - 0.5 * srw - hg, yc + 2 * srw - yc, x = xc - 0.5 * srw, y = yc) +
		L(
			x = xc + c(-0.5, -1.5, -1.5, -0.5, -0.5, 0.5, 0.5, 1.5, 1.5, 0.5, 0.5) * srw,
			y = c(ych - c(2, 2, 1, 1, 0, 0, 1, 1, 2, 2) * srw, yc)
		) +
		A(xc - 0.5 * srw - hg, yc + 2 * srw - yc, x = xcw, y = yc + 2 * srw) +
		AZ(srw, yc + 2 * srw - vg, x = xcw - srw, y = vg)
	write_svg(d, "265a")

	# 2654 white chess king (derived via OUTLINE_FROM_TO)
	# 1fa09 white chess king rotated 90 degrees (derived via RIGHT_FROM_TO)
	# 1fa1e white chess turned king (derived via TURNED_FROM_TO)
	# 1fa33 white chess king rotated 270 degrees (derived via LEFT_FROM_TO)
	# 1fa0f black chess king rotated 90 degrees (derived via RIGHT_FROM_TO)
	# 1fa24 black chess turned king (derived via TURNED_FROM_TO)
	# 1fa39 black chess king rotated 270 degrees (derived via LEFT_FROM_TO)

	# 265b black chess queen
	rcq <- 0
	# rcq <- 1.5 * srw
	x <- c(hg + srw, hg, hg + cw / 3, xc, xcw - cw / 3, xcw, xcw - srw)
	y <- c(vg, ych - 2 * rcq - srw, yc, ych - 2 * rcq, yc, ych - 2 * rcq - srw, vg)
	# ds <- c(d_polygon(x, y), d_circle(xc, ych - rq, rq))
	ds <- d_polygon(x, y)
	write_svg(ds, "265b")
	# 2655 white chess queen
	# ds <- c(d_polygon(x, y, offset = c(0, -ow)), d_circle(xc, ych - rq, rq + c(0, -ow)))
	ds <- d_polygon(x, y, offset = c(0, -ow))
	write_svg(ds, "2655")
	# 1fa1f white chess turned queen
	ds <- d_polygon(x, y, origin_at_bottom = FALSE, offset = c(0, -ow))
	write_svg(ds, "1fa1f")
	# 1fa25 black chess turned queen
	ds <- d_polygon(x, y, origin_at_bottom = FALSE)
	write_svg(ds, "1fa25")
	# 1fa0a white chess queen rotated 90 degrees (derived via RIGHT_FROM_TO)
	# 1fa34 white chess queen rotated 270 degrees (derived via LEFT_FROM_TO)
	# 1fa10 black chess queen rotated 90 degrees (derived via RIGHT_FROM_TO)
	# 1fa3a black chess queen rotated 270 degrees (derived via LEFT_FROM_TO)

	# 265c black chess rook
	if (font == "narrow") {
		wcr <- cw / 4.0
	} else {
		wcr <- cw / 4
	}
	x <- c(
		hg,
		hg,
		hg + wcr,
		hg + wcr,
		xc - wcr / 2,
		xc - wcr / 2,
		xc + wcr / 2,
		xc + wcr / 2,
		xcw - wcr,
		xcw - wcr,
		xcw,
		xcw
	)
	ybt <- vg + 0.20 * ch
	ytb <- ych - 0.20 * ch
	ytb2 <- ych - 0.35 * ch
	y <- c(
		ytb2 + 2,
		ych,
		ych,
		ytb,
		ytb,
		ych,
		ych,
		ytb,
		ytb,
		ych,
		ych,
		ytb2 + 2
	)
	ds <- c(
		d_rect2(ybt, xcw, vg, hg), # base of tower
		d_rect2(ytb2, xcw - srw, ybt + 2, hg + srw), # center of tower
		d_polygon(x, y) # roof of tower
	)
	write_svg(ds, "265c")
	# 2656 white chess rook (derived via OUTLINE_FROM_TO)
	# ds <- c(
	# 	d_rect2(ybt, xcw, vg, hg, offset = c(0, -ow)),
	# 	d_rect2(ytb2, xcw - srw, ybt + 2, hg + srw, offset = c(0, -ow)),
	# 	d_polygon(x, y, offset = c(0, -ow))
	# )
	# write_svg(ds, "2656")
	# 1fa0b white chess rook rotated 90 degrees (derived via RIGHT_FROM_TO)
	# 1fa20 white chess turned rook (derived via TURNED_FROM_TO)
	# 1fa35 white chess rook rotated 270 degrees (derived via LEFT_FROM_TO)
	# 1fa11 black chess rook rotated 90 degrees (derived via RIGHT_FROM_TO)
	# 1fa26 black chess turned rook (derived via TURNED_FROM_TO)
	# 1fa3b black chess rook rotated 270 degrees (derived via LEFT_FROM_TO)

	# 265d black chess bishop
	ybt <- 0.32 * (vg + ych)
	yc1 <- 0.5 * (vg + ybt)
	yc2 <- 0.6 * (vg + ych)
	xbb <- hg + srw
	xc2 <- hg
	ytb <- 0.6 * (vg + ych)

	# Code by Claude Code to break up right Bezier curve to introduce rectangular slit
	# Right-side bezier: from (xc, ych) via (w-xc2, yc2) to (xcw, ybt)
	bS <- c(xc, ych)
	bC <- c(w - xc2, yc2)
	bE <- c(xcw, ybt)

	# Slit parameters
	t1 <- 0.16 # upper edge of slit (0 = top of bishop, 1 = right-middle)
	t2 <- 0.30 # lower edge of slit
	slit_depth <- 2 * srw # how far inward the slit cuts

	# Quadratic bezier utilities
	qb_lerp <- function(a, b, t) (1 - t) * a + t * b
	qb_pt <- function(S, C, E, t) qb_lerp(qb_lerp(S, C, t), qb_lerp(C, E, t), t)
	qb_tang <- function(S, C, E, t) 2 * (1 - t) * (C - S) + 2 * t * (E - C)

	# Slit edge points on the bezier
	pt1 <- qb_pt(bS, bC, bE, t1)
	pt2 <- qb_pt(bS, bC, bE, t2)

	# Inward normal: CW rotation of tangent (c(ty, -tx)), then normalize and scale
	inward_offset <- function(S, C, E, t, depth) {
		tang <- qb_tang(S, C, E, t)
		n <- c(tang[2], -tang[1])
		(n / sqrt(sum(n^2))) * depth
	}
	if (font == "narrow") {
		t1o <- -pi / 8
		t2o <- -pi / 8
	} else {
		t1o <- 0
		t2o <- 0
	}
	in1 <- pt1 + inward_offset(bS, bC, bE, t1 + t1o, slit_depth)
	in2 <- pt2 + inward_offset(bS, bC, bE, t2 + t1o, slit_depth)

	# Sub-bezier control points
	c_pre <- qb_lerp(bS, bC, t1) # control for bS -> pt1
	t_p <- (t2 - t1) / (1 - t1) # reparametrize remaining cruve
	c_post <- qb_lerp(qb_lerp(bC, bE, t1), bE, t_p) # control for pt2 -> bE

	d_bshp <- M(xbb, vg) +
		Q(hg, yc1, hg, ybt) + # ll curve
		Q(xc2, yc2, bS[1], bS[2]) + # ul curve
		Q(c_pre[1], c_pre[2], pt1[1], pt1[2]) +
		L(in1[1], in1[2]) +
		L(in2[1], in2[2]) +
		L(pt2[1], pt2[2]) +
		Q(c_post[1], c_post[2], bE[1], bE[2]) +
		QZ(xcw, yc1, w - xbb, vg) # lr curve
	ds <- c(d_bshp)
	write_svg(ds, "265d")
	# 2657 white chess bishop (derived via OUTLINE_FROM_TO)
	# 1fa0c white chess bishop rotated 90 degrees (derived via RIGHT_FROM_TO)
	# 1fa21 white chess turned bishop (derived via TURNED_FROM_TO)
	# 1fa36 white chess bishop rotated 270 degrees (derived via LEFT_FROM_TO)
	# 1fa12 black chess bishop rotated 90 degrees (derived via RIGHT_FROM_TO)
	# 1fa27 black chess turned bishop (derived via TURNED_FROM_TO)
	# 1fa3c black chess bishop rotated 270 degrees (derived via LEFT_FROM_TO)

	# 265e black chess knight
	ysb <- vg + 0.08 * ch
	yst <- vg + 0.25 * ch
	ycb <- vg + 0.48 * ch
	yct <- vg + 0.7 * ch
	yeb <- vg + 0.80 * ch
	yem <- vg + 0.90 * ch
	yht <- vg + 0.85 * ch
	xb <- hg + 0.4 * cw
	xsb <- hg + 0.25 * cw
	xeb <- hg + 0.22 * cw
	xem <- hg + 0.18 * cw
	xet <- hg + 0.22 * cw
	xht <- hg + 0.4 * cw
	xhorse <- c(xb, xsb, xsb, hg, hg, xeb, xem, xet, xht)
	xhorse <- c(xhorse, w - rev(xhorse))
	yhorse <- c(vg, ysb, yst, ycb, yct, yeb, yem, ych, yht)
	yhorse <- c(yhorse, rev(yhorse))
	eye_dx <- srw
	eye_dy <- (11 / 18) * srw
	eye_rx <- (7 / 9) * srw
	eye_ry <- (4 / 9) * srw
	nostril_rx <- srw / 3
	nostril_ry <- srw / 6
	nostril_dx <- (4 / 9) * srw
	nostril_dy <- (2 / 9) * srw
	d_eye_left <- d_ellipse(hg + eye_dx, yct - eye_dy, eye_rx, eye_ry, a = -20)
	d_eye_right <- d_ellipse(xcw - eye_dx, yct - eye_dy, eye_rx, eye_ry, a = +20)
	d_nostril_left <- d_ellipse(xsb + nostril_dx, ysb + nostril_dy, nostril_rx, nostril_ry, a = -55)
	d_nostril_right <- d_ellipse(
		w - xsb - nostril_dx,
		ysb + nostril_dy,
		nostril_rx,
		nostril_ry,
		a = +55
	)
	d <- d_polygon(xhorse, yhorse) +
		d_eye_left +
		d_eye_right +
		d_nostril_left +
		d_nostril_right
	write_svg(d, "265e")

	# 2658 white chess knight
	d <- d_polygon(xhorse, yhorse, offset = c(0, -ow)) +
		d_eye_left +
		d_eye_right +
		d_nostril_left +
		d_nostril_right
	write_svg(d, "2658")

	# 1fa22 white chess turned knight (derived via TURNED_FROM_TO)
	# 1fa0d white chess knight rotated 90 degrees (derived via RIGHT_FROM_TO)
	# 1fa37 white chess knight rotated 270 degrees (derived via LEFT_FROM_TO)
	# 1fa13 black chess knight rotated 90 degrees (derived via RIGHT_FROM_TO)
	# 1fa28 black chess turned knight (derived via TURNED_FROM_TO)
	# 1fa3d black chess knight rotated 270 degrees (derived via LEFT_FROM_TO)

	# 265f black chess pawn
	ybt <- vg + 0.20 * ch
	ytr <- 0.18 * ch
	xy1 <- as_coord2d(degrees(225), radius = ytr + 2)$translate(x = xc, y = ych - ytr)
	xy2 <- as_coord2d(degrees(315), radius = ytr + 2)$translate(x = xc, y = ych - ytr)
	d_p <- M(hg + srw, ybt + 2) +
		L(xy1$x, xy1$y) +
		A(ytr + 2, x = xy2$x, y = xy2$y, sweep_flag = TRUE) +
		LZ(xcw - srw, ybt + 2)
	ds <- c(d_rect2(ybt, xcw, vg, hg), d_circle(xc, ych - ytr, ytr), d_p)
	write_svg(ds, "265f")

	# 2659 white chess pawn (derived via OUTLINE_FROM_TO)
	# 1fa0e white chess pawn rotated 90 degrees (derived via RIGHT_FROM_TO)
	# 1fa23 white chess turned pawn (derived via TURNED_FROM_TO)
	# 1fa38 white chess pawn rotated 270 degrees (derived via LEFT_FROM_TO)
	# 1fa14 black chess pawn rotated 90 degrees (derived via RIGHT_FROM_TO)
	# 1fa29 black chess turned pawn (derived via TURNED_FROM_TO)
	# 1fa3e black chess pawn rotated 270 degrees (derived via LEFT_FROM_TO)

	#### More chess glyphs in Unicode...
	# https://en.wikipedia.org/wiki/Chess_symbols_in_Unicode

	if (requireNamespace("piecepackr", quietly = TRUE)) {
		# fc431 black meeple
		xy_meeple <- piecepackr::pp_shape("meeple")$npc_coords |>
			as_coord2d()
		xy_meeple <- xy_meeple$translate(x = -0.5, y = -0.5)$scale(cw, ch)$translate(x = xc, y = yc)
		write_svg(d_polygon(xy_meeple), "fc431")

		# fc432 white meeple
		write_svg(d_polygon(xy_meeple, offset = c(0, -ow)), "fc432")
	}

	as_hex(c(
		as.hexmode(c("2605", "2606")),
		as.hexmode("2660"),
		as.hexmode("2662"):as.hexmode("2663"),
		as.hexmode("2655"):as.hexmode("2656"),
		as.hexmode("2658"),
		as.hexmode("265a"):as.hexmode("265f"),
		as.hexmode("2665"):as.hexmode("2666"),
		as.hexmode("26aa"):as.hexmode("26ab"),
		as.hexmode("272a"):as.hexmode("272d"),
		as.hexmode(c("2734", "2736", "2737", "2738", "2739", "2742")),
		as.hexmode(c("f5b8")),
		as.hexmode(c("1fa1f", "1fa20")),
		as.hexmode(c("1fa25", "1fa26")),
		if (requireNamespace("piecepackr")) as.hexmode(c("fc431", "fc432"))
	))
}
