
load.meteo.data <- function(initial.date = initial.date,
                            final.date = final.date,
                            url.serv = url.serv,
                            user = user,
                            password = password,
                            nro.station = nro.station,
                            bhoa.table = bhoa.table)
{
  # Consulta datos meteorológicos
  meteo.request <- ConsumirDatosEstacion(
    url.consulta = url.serv, usuario = user, clave = password,
    fecha.inicial = initial.date,
    fecha.final = final.date,
    id.estacion = nro.station
  )
  
  # Cambio de formato a Date
  meteo.request$Fecha <- as.Date(meteo.request$Fecha)
  
  # Captura de datos BHOA para la estación y período
  bhoa.station <- bhoa.table %>%
    dplyr::filter(
      Nint == nro.station,
      Fecha >= initial.date,
      Fecha <= final.date
    )
  # Elimina columna Nint
  bhoa.station <- bhoa.station[,-1]

  # Datos meteorológicos y BHOA unificados en única variable
  data.station <- bhoa.station %>%
    dplyr::left_join(meteo.request, by = "Fecha") %>%
    dplyr::mutate(
      nro.estacion = dplyr::coalesce(nro.estacion, nro.station)
    ) %>%
    dplyr::select(
      1, nro.estacion, dplyr::everything()
    )

  # precipitación mensual (últimos 28 días)
  data.station <- data.station %>%
    mutate(Prcp.1m = rollapply(prcp, width = 28, FUN = sum,
                               align = "right", fill = NA))
  
  # 4 dias consecutivos con Tmin por debajo de un umbral
  data.station <- data.station %>%
    mutate(Tmin.count.4d = as.integer(rollapply(tmin, width = 4,
                                                FUN = function(x) all(x < 18),
                                                align = "right", fill = NA)))
  
  # 7 dias consecutivos con Tmin por debajo de un umbral
  data.station <- data.station %>%
    mutate(Tmin.count.7d = as.integer(rollapply(tmin, width = 7,
                                                FUN = function(x) all(x < 18),
                                                align = "right", fill = NA)))
  
  return(data.station)
  
}

