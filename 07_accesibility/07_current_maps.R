# Paquetes necesarios
pacman::p_load(tidyverse, terra, sf, rnaturalearth, rnaturalearthdata, CopernicusMarine)

# Opcional: library(CopernicusMarine) para descarga automática (ver más abajo)

# --- 1. Definir parámetros
product_id  <- "GLOBAL_ANALYSISFORECAST_PHY_001_024"
dataset_id  <- "cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m"  # por ejemplo: corriente diaria media superficie  
vars <- c("uo","vo")
time_target <- "2024-05-01"  # por ejemplo  
lat_range   <- c(-5, 5)       # franja ecuatorial  
lon_range   <- c(-180, 180)   # todo el mundo en longitud  
depth_level <- c(0, -0.5)              # superficie  

# --- 2. Descargar / abrir datos  
# Opción A: descarga manual desde CMEMS web y asignar ruta al archivo  

currents <- cms_download_subset(destination   = "file.nc",
                   product       = product_id,
                   layer         = dataset_id,
                   variable      = vars,
                   region        = c(-80,-45, -69,-15),
                   timerange     = c(time_target, time_target),
                   verticalrange = depth_level)

curr_df <- currents %>% 
  st_as_sf() %>% 
  mutate(across(where(~ inherits(.x, "units")), as.numeric)) %>% 
  mutate(lon = st_coordinates(st_centroid(.))[,1],
         lat = st_coordinates(st_centroid(.))[,2],
         speed = sqrt(uo^2 + vo^2)) %>% 
  st_drop_geometry()
  
# ---- 4. Mapa base + vectores ----
# Reducimos número de flechas para evitar saturación
curr_sample <- curr_df |> sample_frac(0.05)

ggplot() +
  geom_raster(data = curr_df, aes(x = lon, y = lat, fill = speed)) +
  geom_segment(
    data = curr_sample,
    aes(x = lon, y = lat, xend = lon + uo , yend = lat + vo ),
    arrow = arrow(length = unit(0.15, "cm")), color = "black"
  ) +
  scale_fill_viridis_c(name = "Velocidad (m/s)", option = "plasma") +
  coord_quickmap(xlim = c(-80, -69), ylim = c(-45, -15), expand = FALSE) +
  labs(
    title = "Corrientes superficiales en el ecuador",
    subtitle = paste("Copernicus Marine Service -", time_target),
    x = "Longitud", y = "Latitud"
  ) +
  theme_minimal()

