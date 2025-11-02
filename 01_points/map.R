## 30 days map challenge ##
## Jose A. Lastra ##
## 2025-11-02 ##
# Points data download from earthquake.usgs.gov
# Earthquakes > 6 from 1960 til 2024


## Libraries -------
pacman::p_load(tidyverse, sf, rnaturalearth, ggblend)

## Loading base data ----
### world ----
world <- ne_countries() %>%
  st_transform(crs = "ESRI:54035")

### quakes ----
quakes <- read_csv("01_points/eartquakes_data.csv")
quakes_sf <- quakes %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = "ESRI:54035") %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  )

## Basemaps -----
# Set colors
col_world <- "#9CB4BF"
col_back <- "#1D201F"

# Set theme
theme_custom <- theme_void() +
  theme(plot.background = element_rect(fill = col_back, color = NA))

ggplot() +
  # World basemap
  geom_sf(
    data = world,
    fill = alpha(col_world, .75), color = alpha("#9CB4BF", 0.75)
  ) +
  geom_point(
    data = quakes_sf,
    aes(x = x, y = y, size = mag, color = mag, alpha = mag)
  ) |> blend("lighten") +
  scale_size_continuous(
    name = "Magnitude (mww)", trans = "log",
    range = c(0.5, 4)
  ) +
  scale_alpha_continuous(
    name = "Magnitude (mww)", trans = "log",
    range = c(0.1, .7)
  ) +
  scale_color_viridis_c(
    option = "inferno", direction = 1, trans = "log", name = "Magnitude (mww)"
  ) +
  theme_custom
