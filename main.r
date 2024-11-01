library(ggplot2)
library(viridis)
library(yaml)

wave_amplitude <- function(x, a, l, fd) a * sin(x * pi / l + fd)
get_distance <- function(o, x, y) sqrt((o$x - x)^2 + (o$y - y)^2)

pushList <- function(list, element) {
  list[[length(list) + 1]] <- element
  return(list)
}

waves <- list()
wave_length <- 5
amplitude <- 1
slit_width <- 50

x_range <- c(-1600, 1600)
y_range <- c(0, 800)
medium_clearness <- 1

slit_positions <- list(
  list(x = -50, y = 0),
  list(x = 50, y = 0)
)

for (s in slit_positions) {
  start_pos <- s$x - (slit_width / 2)
  for (i in 1:slit_width) {
    waves <- pushList(waves, list(
      origin = data.frame(x = start_pos + i, y = 0),
      amplitude = amplitude,
      wave_length = wave_length,
      fd = 0
    ))
  }
}

x <- seq(x_range[1], x_range[2], length.out = 1601)
y <- seq(y_range[1], y_range[2], length.out = 801)
grid <- expand.grid(x = x, y = y)

grid$a <- rowSums(sapply(waves, function(w) {
  distance <- get_distance(w$origin, grid$x, grid$y)
  loss <- medium_clearness**distance
  return(wave_amplitude(distance, w$amplitude, w$wave_length, w$fd) * loss)
}))

plot <- ggplot(grid, aes(x = x, y = y, fill = a)) +
  geom_tile() +
  scale_fill_viridis(name = "Displacement", option = "H") +
  labs(title = "Wave Amplitude Visualization", x = "X", y = "Y") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) +
  coord_equal(ratio = 1)

ggsave("wave_amplitude.png", plot, dpi = 600)

intensity_top <- subset(grid, y == 400)

intensity_plot <- ggplot(intensity_top, aes(x = x, y = a*a)) +
  geom_line(color = "blue") +
  labs(title = "Light Intensity", x = "X", y = "Intensity") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90")
  )

ggsave("light_intensity_top_row.png", intensity_plot, dpi = 600)