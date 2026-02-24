#' Generate a FontForge sfd file
#'
#' `generate_sfd()` generates a FontForge Spline Font Database (sfd) file for
#' the indicated Dotaro font.
#' @param font The desired Dotaro font i.e. "square" or "narrow".
#' @param output A string of desired FontForge spline font database file name.
#' @return The `output` filename invisible.
#'         As a side effect generates a FontForge spline font database file.
#' @export
generate_sfd <- function(font = c("square", "narrow"), output = paste0("dotaro_", font, ".sfd")) {
	font <- match.arg(font)
	glyph_height <- dotaro_height(font)
	glyph_width <- dotaro_width(font)

	fontforge <- reticulate::import("fontforge")
	psMat <- reticulate::import("psMat")
	ff_font <- fontforge$font()

	cap_font <- paste0(toupper(substr(font, 1L, 1L)), substr(font, 2L, nchar(font)))
	ff_font$fontname <- paste0("Dotaro-", cap_font)
	ff_font$fullname <- paste0("Dotaro ", cap_font)
	# Name ID 16: Preferred/Typographic Family groups variants under "Dotaro"
	# in applications that support it, while ID 1 (familyname) is what
	# fontconfig uses for fc-match, so it must be the full "Dotaro Narrow" etc.
	ff_font$familyname <- paste0("Dotaro ", cap_font)
	ff_font$appendSFNTName("English (US)", "Preferred Family", "Dotaro")
	ff_font$appendSFNTName("English (US)", "Preferred Styles", cap_font)

	# Sum of `ascent` and `descent` is a power of two for truetype fonts (often 2048 or 4096)
	# Set `ascent` and `descent` **before** importing glyphs
	ff_font$ascent <- as.integer(dotaro_height(font) - dotaro_vertical_gap(font))
	ff_font$descent <- as.integer(dotaro_vertical_gap(font))
	ff_font$encoding <- "UnicodeFull"

	ff_font$version <- packageVersion("dotaro.font") |> as.character()

	copyright <- "Copyright (c) 2025 Trevor L. Davis"
	ff_font$appendSFNTName("English (US)", "Copyright", copyright)
	license <- "This Font Software is licensed under the SIL Open Font License, Version 1.1.\nThis license is available with a FAQ at:\nhttps://openfontlicense.org"
	ff_font$appendSFNTName("English (US)", "License", license)
	ofl_url <- "https://openfontlicense.org"
	ff_font$appendSFNTName("English (US)", "License URL", ofl_url)

	space <- ff_font$createChar(utf8ToInt(" "))
	space$width <- glyph_width

	hexes <- create_glyphs(font)
	for (hex in hexes) {
		# cat(hex, "\n")
		int <- hex |> as.hexmode() |> as.integer()
		if (hasName(GLYPH_NAMES, hex)) {
			glyph <- ff_font$createChar(int, glue('"{GLYPH_NAMES[[hex]]}"'))
		} else {
			glyph <- ff_font$createChar(int)
		}
		glyph$width <- glyph_width
		glyph$importOutlines(glyph_file(font, hex), scale = FALSE)
		# glyph$autoTrace()
		glyph$removeOverlap()
	}

	for (hex in names(OUTLINE_FROM_TO)) {
		from_int <- hex |> as.hexmode() |> as.integer()
		to_int <- OUTLINE_FROM_TO[[hex]] |> as.hexmode() |> as.integer()

		ff_font$selection$select(from_int)
		ff_font$copy()
		glyph <- ff_font$createChar(to_int)
		ff_font$selection$select(to_int)
		ff_font$paste()
		ow <- dotaro_outline_stroke_width(font)
		glyph$stroke("circular", as.integer(ow), join = "miter", joinlimit = 80)
	}

	for (hex in names(TURNED_FROM_TO)) {
		from_int <- hex |> as.hexmode() |> as.integer()
		to_int <- TURNED_FROM_TO[[hex]] |> as.hexmode() |> as.integer()

		rotate_glyph(ff_font, from_int, to_int, pi, psMat)
	}

	if (font == "square") {
		for (hex in names(LEFT_FROM_TO)) {
			from_int <- hex |> as.hexmode() |> as.integer()
			to_int <- LEFT_FROM_TO[[hex]] |> as.hexmode() |> as.integer()

			rotate_glyph(ff_font, from_int, to_int, pi / 2, psMat)
		}

		for (hex in names(RIGHT_FROM_TO)) {
			from_int <- hex |> as.hexmode() |> as.integer()
			to_int <- RIGHT_FROM_TO[[hex]] |> as.hexmode() |> as.integer()

			rotate_glyph(ff_font, from_int, to_int, -pi / 2, psMat)
		}
	}

	ff_font$save(output)
	ff_font$close()
	invisible(output)
}

rotate_glyph <- function(ff_font, from_int, to_int, radians, psMat) {
	ff_font$selection$select(from_int)
	ff_font$copy()
	glyph <- ff_font$createChar(to_int)
	ff_font$selection$select(to_int)
	ff_font$paste()
	bb <- glyph$boundingBox()
	cx <- (bb[[3]] + bb[[1]]) / 2
	cy <- (bb[[4]] + bb[[2]]) / 2
	trcen = psMat$translate(-cx, -cy)
	rotcen = psMat$compose(trcen, psMat$compose(psMat$rotate(radians), psMat$inverse(trcen)))
	glyph$transform(rotcen)
}

# http://designwithfontforge.com/en-US/The_Final_Output_Generating_Font_Files.html
# I observe this doesn't always update font file if not run in a clean R session

#' Generate a font
#'
#' `generate_font()` generates a font file using FontForge.
#'
#' I observe this doesn't always update the font file if not run in a clean R session.
#' @param input Input filename (e.g. a FontForge spline font database file).
#' @param output Font filename.
#' @return The `output` filename invisible.
#'         As a side effect creates a font file.
#' @export
generate_font <- function(input, output = gsub("sfd$", "ttf", input)) {
	fontforge <- reticulate::import("fontforge")
	font <- fontforge$open(input)
	unlink(output)
	font$generate(output)
	font$close()
	invisible(output)
}

get_unicode_code_points <- function(input) {
	reticulate::py_run_string(
		glue("import fontforge\nfont = fontforge.open('{input}')"),
		convert = FALSE
	)
	points <- reticulate::py_eval("[glyph.unicode for glyph in font.glyphs()]")
	reticulate::py_run_string("font.close()")
	points <- points[which(points > 0L)] |> sort()
	as.hexmode(points)
}
