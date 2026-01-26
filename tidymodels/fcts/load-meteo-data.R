
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

  return(data.station)
  
}

