# All but the three "shade" glyphs
create_block_elements <- function(font = "square") {
	h <- dotaro_height(font) # glyph height
	w <- dotaro_width(font) # glyph width
	do.call(local_options, glyph_options(w, h, font))

	xc <- w / 2
	yc <- h / 2

	# 2580 upper half block
	d <- d_rect2(xl = 0, xr = w, yb = yc, yt = h)
	write_svg(d, "2580")

	# 2581--2587 lower 1/8 -- lower 7/8 block
	for (i in 1:7) {
		hex <- as.character(2580 + i)
		d <- d_rect2(xl = 0, xr = w, yb = 0, yt = (i / 8) * h)
		write_svg(d, hex)
	}

	# 2588 full block
	d <- d_rect2(xl = 0, xr = w, yb = 0, yt = h)
	write_svg(d, "2588")

	# 2589 -- 258f left 7/8 -- left 1/8 block
	for (i in 1:7) {
		hex <- as.hexmode("2588") + i
		d <- d_rect2(xl = 0, xr = ((8 - i) / 8) * w, yb = 0, yt = h)
		write_svg(d, hex)
	}

	# 2590 right half block
	d <- d_rect2(xl = xc, xr = w, yb = 0, yt = h)
	write_svg(d, "2590")

	#### 2591--2593 LIGHT SHADE--HEAVY SHADE

	# 2594 upper 1/8
	d <- d_rect2(xl = 0, xr = w, yb = (7 / 8) * h, yt = h)
	write_svg(d, "2594")

	# 2595 right 1/8
	d <- d_rect2(xl = (7 / 8) * w, xr = w, yb = 0, yt = h)
	write_svg(d, "2595")

	# 2596 quadrant lower left
	dll <- d_rect2(xl = 0, xr = xc, yb = 0, yt = yc)
	write_svg(dll, "2596")

	# 2597 quadrant lower right
	dlr <- d_rect2(xl = xc, xr = w, yb = 0, yt = yc)
	write_svg(dlr, "2597")

	# 2598 quadrant upper left
	dul <- d_rect2(xl = 0, xr = xc, yb = yc, yt = h)
	write_svg(dul, "2598")

	# 259d quadrant upper right
	dur <- d_rect2(xl = xc, xr = w, yb = yc, yt = h)
	write_svg(dur, "259d")

	# 259a quadrant upper left and lower right
	write_svg(dul + dlr, "259a")

	# 259e quadrant upper right and lower left
	write_svg(dur + dll, "259e")

	# 2599 quadrant upper left, lower left, and lower right
	x <- c(0, 0, xc, xc, w, w)
	y <- c(0, h, h, yc, yc, 0)
	write_svg(MZ(x, y), "2599")

	# 259b quadrant upper left, upper right, lower left
	x <- c(0, 0, w, w, xc, xc)
	y <- c(0, h, h, yc, yc, 0)
	write_svg(MZ(x, y), "259b")

	# 259c quadrant upper left, upper right, lower right
	x <- c(0, 0, w, w, xc, xc)
	y <- c(yc, h, h, 0, 0, yc)
	write_svg(MZ(x, y), "259c")

	# 259f quadrant upper right, lower left, lower right
	x <- c(0, 0, xc, xc, w, w)
	y <- c(0, yc, yc, h, h, 0)
	write_svg(MZ(x, y), "259f")

	as_hex(c(as.hexmode("2580"):as.hexmode("2590"), as.hexmode("2594"):as.hexmode("259f")))
}

d_rect2 <- function(yt, xr, yb, xl) {
	x <- c(xl, xl, xr, xr)
	y <- c(yb, yt, yt, yb)
	MZ(x, y)
}
