## 30 days map challenge ##
## Jose A. Lastra ##
## 2025-11-02 ##
# Points data download from earthquake.usgs.gov
# Earthquakes > 6 from 1960 til 2024


## Libraries -------
pacman::p_load(tidyverse, sf, rnaturalearth, ggblend, ggspatial)

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
col_world <- "black"
col_back <- "black"

# Set theme
theme_custom <- theme_void() +
  theme(
    plot.background = element_rect(fill = col_back, color = NA),
    text = element_text(colour = "white", size = 12, family = "mono"),
    legend.title.position = "top",
    legend.position = "bottom"
  )

# base plot
base <- ggplot() +
  # World basemap
  geom_sf(
    data = world,
    fill = alpha(col_world, .75), linewidth = 0.1, color = alpha("white", 0.5)
  ) +
  geom_point(
    data = quakes_sf,
    aes(x = x, y = y, size = mag, color = mag, alpha = mag)
  ) +
  scale_size_continuous(
    name = "Magnitude (mww)", trans = "log",
    range = c(1, 8)
  ) +
  scale_alpha_continuous(
    name = "Magnitude (mww)", trans = "log",
    range = c(0.2, .8)
  ) +
  labs(
    title = "Earthquakes above 6.0 mww [1960 - 2024]",
    caption = "USGS data | @joselastram"
  ) +
  scale_color_gradient2(
    low = "#fee8c8", mid = "#fdbb84", high = "#e34a33",
    midpoint = 7.5, name = "Magnitude (mww)", transform = "log"
  ) +
  guides(color = guide_legend(), size = guide_legend()) +
  theme_custom

base
ggsave(base, filename = "earthquakes_1960_2024.png", width = 12, height = 10, dpi = 300)
