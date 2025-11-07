kmeans.espacial <- function(data = data, metadata = metadata, k = k)
{
  # Lee Metadatos
  data.metadata <- read.csv(metadata, skip = 1, header = FALSE)
  colnames(data.metadata) <- c("prov", "nombre", "Estacion", "lat", "lon", "alt")
  
  # Transpone los datos para que las filas sean estaciones
  mat <- data[, -1]   # se elimina la columna 'Semana'
  mat_t <- t(mat)     # transponemos
  
  # Asignar nombres correctos
  colnames(mat_t) <- data$SE
  rownames(mat_t) <- colnames(mat)
  
  # Estandarizar
  mat_t_scaled <- scale(mat_t)
  
  # En caso de faltantes completa con valor medio de cada estación
  for (i in 1:nrow(mat_t_scaled)){
    mat_t_scaled[i, which(is.na(mat_t_scaled[i, ]))] <- mean(mat_t_scaled[i, ],
                                                             na.rm = TRUE)
  }
  
  set.seed(123)
  kmeans_res <- kmeans(mat_t_scaled, centers = k, nstart = 1)
  
  # Guardar resultados
  res_df <- data.frame(
    estacion = rownames(mat_t_scaled),
    cluster = factor(kmeans_res$cluster)
  )
  
  res_df$estacion <- as.integer(res_df$estacion)
  
  res_geo <- res_df %>%
    left_join(data.metadata, by = c("estacion" = "Estacion"))
  
  # Crear objeto espacial
  est_sf <- st_as_sf(res_geo, coords = c("lon", "lat"), crs = 4326)
  
  sa <- st_read("/ms-36/hidro/shp/paises/SA_shapefiles/SA.shp")
  
  fig <- ggplot() +
    geom_sf(data = sa, fill = "gray95", color = "gray60") +
    geom_sf(
      data = est_sf,
      aes(fill = cluster, shape = cluster),
      size = 4, color = "black", stroke = 1.2
    ) +
    coord_sf(xlim = c(-75, -52), ylim = c(-55, -23)) +
    theme_minimal() +
    labs(
      title = "Clusters espaciales de estaciones según ALM",
      fill = "Cluster",
      shape = "Cluster"
    ) +
    scale_fill_brewer(palette = "Set1") +
    scale_shape_manual(values = c(21, 22, 23, 24, 25, 3, 4))
  
  
  
}

