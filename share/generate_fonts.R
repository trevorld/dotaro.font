devtools::load_all()

reticulate::use_python(Sys.which("python3"))
generate_sfd("ranks", "share/dotaro-ranks.sfd")
generate_font("share/dotaro-ranks.sfd", "share/dotaro-ranks.ttf")

generate_sfd("suits", "share/dotaro-suits.sfd")
generate_font("share/dotaro-suits.sfd", "share/dotaro-suits.ttf")
