## 30 days map challenge Day 2 ##
## Jose A. Lastra ##
## 2025-11-02 ##
# AMV map
# OSM


## Libraries -------
pacman::p_load(tidyverse, sf, rnaturalearth, ggblend, ggspatial, osmdata)

## Loading base data ----
### BBOX ----
valpo <- getbb("Santiago, Chile")

### streets
calles <- opq(bbox = valpo) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = calles$osm_lines, color = "white", size = 0.3) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black", color = NA),
    plot.title.position = bottom
  ) +
  labs(
    title = "Santiago, Chile",
    caption = "OSM data | @joselastram"
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
ggsave(base, filename = "earthquakes_1960_2024.pdf", width = 12, height = 10, dpi = 300)
