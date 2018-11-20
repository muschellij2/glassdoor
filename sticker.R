library(hexSticker)
library(desc)
desc = desc::description$new()
fig_dir = file.path("man", "figures")
if (dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}
package = desc$get("Package")
# outline = "#0caa41"
outline = "black"
# background = "#0caa41"
background = "dodgerblue2"
p_color = "black"
sticker("icon.png",
        package = package,
        h_fill = background,
        h_color = outline,
        s_width = 0.2,
        s_height = 0.4,
        s_x = 1,
        filename = file.path(fig_dir, "sticker.jpg"))

usethis::use_build_ignore(
  c("icon.png", "sticker.R", "sticker.png", "sticker.jpg"))
