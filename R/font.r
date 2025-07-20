#' Generate a FontForge sfd file
#'
#' `generate_sfd()` generates a FontForge Spline Font Database (sfd) file for
#' the indicated Dotaro font.
#' @param font The desired Dotaro font i.e. "square" or "narrow".
#' @param output A string of desired FontForge spline font database file name.
#' @return The `output` filename invisible.
#'         As a side effect generates a FontForge spline font database file.
#' @export
generate_sfd <- function(font = c("square", "narrow"),
                         output = paste0("dotaro_", font, ".sfd")) {
    font <- match.arg(font)
    glyph_height <- dotaro_height(font)
    glyph_width <- dotaro_width(font)

    fontforge <- reticulate::import("fontforge")
    ff_font <- fontforge$font()

    fname <- paste0("Dotaro-", toupper(substr(font, 1L, 1L)), substr(font, 2L, nchar(font)))
    ff_font$fontname <- fname
    ff_font$familyname <- fname
    ff_font$fullname <- fname
    # Sum of `ascent` and `descent` is a power of two for truetype fonts (often 2048 or 4096)
    # Set `ascent` and `descent` **before** importing glyphs
    ff_font$descent <- glyph_height %/% 2L
    ff_font$ascent <- glyph_height %/% 2L
    ff_font$encoding <- "UnicodeFull"

    ff_font$version <- packageVersion("dotaro.font") |> as.character()

    copyright <- "Copyright (c) 2025, Trevor L. Davis"
    ff_font$appendSFNTName("English (US)", "Copyright", copyright)
    license <- "This Font Software is licensed under the SIL Open Font License, Version 1.1."
    ff_font$appendSFNTName("English (US)", "License", license)
    ofl_url <- "https://openfontlicense.org"
    ff_font$appendSFNTName("English (US)", "License URL", ofl_url)

    space <- ff_font$createChar(utf8ToInt(" "))
    space$width <- glyph_width

    hexes <- create_glyphs(font)
    for (hex in hexes) {
        int <- hex |> as.hexmode() |> as.integer()
        glyph <- ff_font$createChar(int)
        glyph$width <- glyph_width
        glyph$importOutlines(glyph_file(font, hex), scale = FALSE)
        # glyph$autoTrace()
        glyph$removeOverlap()
    }

    ff_font$save(output)
    ff_font$close()
    invisible(output)
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
    reticulate::py_run_string(glue("import fontforge\nfont = fontforge.open('{input}')"),
                       convert = FALSE)
    points <- reticulate::py_eval("[glyph.unicode for glyph in font.glyphs()]")
    reticulate::py_run_string("font.close()")
    points <- points[which(points > 0L)] |> sort()
    as.hexmode(points)
}
