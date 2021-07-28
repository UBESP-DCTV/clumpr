library("png")
library("patchwork")


images <- here("inst/shiny_interface/data/") |>
  list.files(pattern = "0\\.png$", full.names = TRUE) |>
  {\(x) set_names(x, basename(x))}() |>
  map(readPNG, native = TRUE, info = TRUE)



ggplot() +
  inset_element(p = images[[2]],
                left = 0,
                bottom = 0.5,
                right = 0.5,
                top = 1,
                align_to = "full") +
  labs(tag = "A") +
  inset_element(p = images[[1]],
                left = 0.5,
                bottom = 0.5,
                right = 1,
                top = 1,
                align_to = "full") +
  labs(tag = "B") +
  inset_element(p = images[[4]],
                left = 0,
                bottom = 0,
                right = 0.5,
                top = 0.5,
                align_to = "full") +
  labs(tag = "C") +
  inset_element(p = images[[3]],
                left = 0.5,
                bottom = 0,
                right = 1,
                top = 0.5,
                align_to = "full") +
  labs(tag = "D") +
  inset_element(
    p = readPNG(
      here("inst/shiny_interface/data/legend.png"),
      native = TRUE, info = TRUE
    ),
    left = 0.24,
    bottom = 0.75,
    right = 0.44,
    top = 0.95,
    align_to = "full")
