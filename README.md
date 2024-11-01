# Ripple in R

A project to generate visualisations of wave amplitude and light intensity using `R` and the `ggplot2` library

---

## Overview
This project simulates wave propagation through a medium with adjustable properties, such as wave length, amplitude, and medium clearness.

## Configuration
The project uses a YAML configuration file (config.yml) to set parameters for the simulation. The file contains settings for:

- Wave length
- Amplitude
- Slit width
- Medium clearness
- Plot ranges (x and y axes)
- Slit positions

## Output
The project generates two output files:

- `wave_amplitude.png`: a visualization of the wave amplitude
- `light_intensity_top_row.png`: a plot of the light intensity at the top row of the simulation (on the screen)

## Requirements
- R programming language (`Rscript`)
- `ggplot2` library
- `viridis` library
- `yaml` library
- `pbapply` library

## Usage

1. Update the `config.yml` file with desired simulation parameters.
2. Run the `main.r` script to generate the visualizations.

---

**Note**: The result of the simulation cannot be garanteed to be the same as the real world.

Last updated: 2024-11-01
