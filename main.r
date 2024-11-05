library(ggplot2)
library(viridis)
library(yaml)
library(pbapply)
library(purrr)

# Functions
wave_amplitude <- function(x, a, l, fd) a * sin(x * pi / l + fd)
get_distance <- function(o, x, y) sqrt((o$x - x)^2 + (o$y - y)^2)

# Load Configuration
config <- yaml.load_file("config.yml")
wave_length <- config$wave_length
amplitude <- config$amplitude
slit_width <- config$slit_width
medium_clearness <- config$medium_clearness
slit_positions <- config$slit_positions
wave_supposition_resolution <- config$wave_supposition_resolution
x_range <- config$x_range
y_range <- config$y_range
transparent_plot_background <- config$transparent_plot_background

# Generate waves with optimized list handling
waves <- map(slit_positions, function(s) {
  start_pos <- s$x - (slit_width / 2)
  amplitude_correction <- slit_width / (1 / (wave_supposition_resolution / 1000))
  seq(0, slit_width, 1 / (wave_supposition_resolution / 1000)) %>%
    map(~ list(origin = data.frame(x = start_pos + .x, y = 0),
               amplitude = amplitude / amplitude_correction,
               wave_length = wave_length,
               fd = 0))
}) %>%
  flatten()

# Grid setup
x <- seq(x_range$from, x_range$to, 1)
y <- seq(y_range$from, y_range$to, 1)
grid <- expand.grid(x = x, y = y)

# Calculate wave amplitude over the grid with vectorized distance calculation
grid$a <- rowSums(pbsapply(waves, function(w) {
  dist_matrix <- sqrt((grid$x - w$origin$x)^2 + (grid$y - w$origin$y)^2)
  loss <- medium_clearness^dist_matrix
  wave_amplitude(dist_matrix, w$amplitude, w$wave_length, w$fd) * loss
}, simplify = TRUE))

# Plotting Functions
save_plot <- function(filename, plot, dpi) {
  ggsave(filename, plot, dpi = dpi)
  print(paste("Saved", filename))
}

handle_background_colour <- function(plot) {
  if (transparent_plot_background) {
    plot <- plot + theme(plot.background = element_rect(fill = "transparent"))
  }
  else {
    plot <- plot + theme(plot.background = element_rect(fill = "white"))
  }
  plot
}

# Wave Amplitude Visualization
plot_wave_amplitude <- function() {
  plot <- ggplot(grid, aes(x = x, y = y, fill = a)) +
    geom_raster() +
    scale_fill_viridis(name = "Displacement", option = "H") +
    labs(title = "Wave Amplitude Visualization", x = "X", y = "Y") +
    theme_minimal() +
    coord_equal()

  handle_background_colour(plot)
}

# Light Intensity Plot at Top Row
plot_light_intensity <- function() {
  amplitude_top <- subset(grid, y == y_range$to)
  plot <- ggplot(amplitude_top, aes(x = x, y = a^2)) +
    geom_line(color = "blue") +
    labs(title = "Light Intensity", x = "X", y = "Intensity") +
    theme_minimal()

  handle_background_colour(plot)
}

# Bright and Dark Fringes Plot
plot_fringes <- function() {
  amplitude_top <- subset(grid, y == y_range$to)
  plot <- ggplot(amplitude_top, aes(x = x, y = y, fill = a^2)) +
    geom_raster() +
    scale_fill_gradient2(low = "white", mid = "black", high = "white", name = "Intensity") +
    labs(title = "Bright and Dark Fringes", x = "X") +
    theme_minimal() +
    coord_equal(ratio = 200)

  handle_background_colour(plot)
}

# Save plots
print("Rendering and saving plots...")
save_plot("wave_amplitude.png", plot_wave_amplitude(), config$amplitude_plot_dpi) 
save_plot("light_intensity_top_row.png", plot_light_intensity(), 300)
save_plot("fringes_plot.png", plot_fringes(), 300)
