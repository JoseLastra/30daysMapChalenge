# Paquetes necesarios
pacman::p_load(tidyverse, terra, sf, rnaturalearth, 
               rnaturalearthdata, CopernicusMarine,
               legendry)

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
times <- seq.Date(ymd('2023-09-01'), ymd('2023-12-01'), by = 'month')

current_list <- lapply(times, function(tiempo){
  currents <- cms_download_subset(destination   = "file.nc",
                                  product       = product_id,
                                  layer         = dataset_id,
                                  variable      = vars,
                                  region        = c(-85,20, -33,68),
                                  timerange     = c(tiempo, tiempo),
                                  verticalrange = depth_level)
  
  
  
  curr_df <- currents %>% 
    st_as_sf() %>% 
    mutate(across(where(~ inherits(.x, "units")), as.numeric)) %>% 
    mutate(lon = st_coordinates(st_centroid(.))[,1],
           lat = st_coordinates(st_centroid(.))[,2],
           speed = sqrt(uo^2 + vo^2),
           time_step = tiempo) %>% 
    st_drop_geometry()
  
  curr_df
})

curr_df <- current_list %>% bind_rows()
  
# ---- 4. Mapa base + vectores ----
# Reducimos número de flechas para evitar saturación
curr_sample <- curr_df |> sample_frac(0.005)

## coastline
world <- ne_countries(scale = "small", returnclass = "sf")

g <- ggplot() +
  geom_raster(data = curr_df, aes(x = lon, y = lat, fill = speed)) +
  geom_sf(data = world, fill = NA, color = "white", linewidth = 0.5) +
  # geom_segment(
  #   data = curr_sample,
  #   aes(x = lon, y = lat, xend = lon + uo , yend = lat + vo,),
  #   arrow = arrow(length = unit(0.15, "cm")),linewidth = 0.5, color = "black"
  # ) +
  scale_fill_viridis_c(name = "Speed (m/s)", option = "turbo") +
  #scale_color_viridis_c(name = "Speed (m/s)", option = "turbo") +
  coord_sf(xlim = c(-85, -33), ylim = c(20, 68), expand = FALSE) +
  facet_wrap(~time_step, nrow = 1) +
  labs(
    title = "Gulf Stream",
    caption = 'Data: Global Ocean Physics Analysis and Forecast, Copernicus Marine Service',
    x = "Longitude", y = "Latitude"
  ) +
  ggdark::dark_theme_gray(base_family = "Bahnschrift", base_size = 15) +
  theme(legend.position = 'bottom', legend.title.position = 'top')  
#g

ggsave(g, filename ='07_accesibility/Gulf_Stream.png', width = 15, height = 8, units = 'in', dpi = 300)


