library(ggplot2)
library(viridis)
library(yaml)
library(pbapply)  # for progress bars


wave_amplitude <- function(x, a, l, fd) a * sin(x * pi / l + fd)
get_distance <- function(o, x, y) sqrt((o$x - x)^2 + (o$y - y)^2)

pushList <- function(list, element) {
  list[[length(list) + 1]] <- element
  return(list)
}

config <- yaml.load_file("config.yml")
wave_length <- config$wave_length
amplitude <- config$amplitude
slit_width <- config$slit_width
medium_clearness <- config$medium_clearness
slit_positions <- config$slit_positions
x_range <- config$x_range
y_range <- config$y_range

waves <- list()
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


print("Rendering plot...")
x <- seq(x_range$from, x_range$to, length.out = 1601)
y <- seq(y_range$from, y_range$to, length.out = 801)
grid <- expand.grid(x = x, y = y)


# Add progress bar for calculating grid$a
grid$a <- rowSums(pbsapply(waves, function(w) {
  distance <- get_distance(w$origin, grid$x, grid$y)
  loss <- medium_clearness**distance
  return(wave_amplitude(distance, w$amplitude, w$wave_length, w$fd) * loss)
}, simplify = TRUE))

print("Plotting the wave amplitude visualization...")
plot <- ggplot(grid, aes(x = x, y = y, fill = a), dpi = config$amplitude_plot_dpi) +
  geom_raster() + 
  scale_fill_viridis(name = "Displacement", option = "H") +
  labs(title = "Wave Amplitude Visualization", x = "X", y = "Y") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) +
  coord_equal(ratio = 1)

plot$render <- FALSE

ggsave("wave_amplitude.png", plot, dpi = config$amplitude_plot_dpi)

print("Plotting the light intensity at the top row...")
intensity_top <- subset(grid, y == 400)

intensity_plot <- ggplot(intensity_top, aes(x = x, y = a * a)) +
  geom_line(color = "blue") +
  labs(title = "Light Intensity", x = "X", y = "Intensity") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90")
  )

intensity_plot$render <- FALSE
ggsave("light_intensity_top_row.png", intensity_plot, dpi = 300)
