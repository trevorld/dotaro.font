devtools::load_all()

reticulate::use_python(Sys.which("python3"))
generate_sfd("narrow", "share/dotaro-narrow.sfd")
generate_font("share/dotaro-narrow.sfd", "share/dotaro-narrow.ttf")

generate_sfd("square", "share/dotaro-square.sfd")
generate_font("share/dotaro-square.sfd", "share/dotaro-square.ttf")
