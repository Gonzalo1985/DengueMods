# Carga de paquetes necesarios para hacer los requests a la API y graficar resultados
library("dplyr")
library("glue")
library("httr")
library("jsonlite")

# ------------------------------------------------------------------------------
# FUNCIONES
# Función para acceder a un servicio web definido por una URL utilizando el método GET.
# Devuelve la respuesta como texto plano.
ConsumirServicioGET <- function(url, usuario, clave) {
  req  <- httr::GET(url = url, 
                    config = httr::authenticate(user = usuario, 
                                                password = clave))
  return (httr::content(req, as = "text"))
}

# Convierte una fecha a formato IS0 8601 (YYYY-MM-DDTHH:mm:ss) utilizando el huso horario GMT-0.
# Este es formato un estándar para representar fechas como una cadena de caracteres [7].
ConvertirFechaISO8601 <- function(fecha) {
  return (strftime(fecha, "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
}

# Función para acceder a un servicio web definido por una URL utilizando un usuario y clave.
# Asumiendo que la respuesta es un string JSON, hace la conversión del mismo a Data Frame.
ConsumirServicioJSON <- function(url, usuario, clave) {
  respuesta <- ConsumirServicioGET(url, usuario, clave)
  return (jsonlite::fromJSON(respuesta))
}
# ------------------------------------------------------------------------------


ConsumirDatosEstacion <- function(url.consulta = url.consulta,
                                  usuario = usuario, clave = clave,
                                  fecha.inicial = fecha.inicial,
                                  fecha.final = fecha.final,
                                  id.estacion = id.estacion){
  # El parámetro ssl_verifypeer=FALSE implica que no se va a verificar la validez del certificado 
  # utilizado en la conexión SSL establecida con la API del CRC-SAS. Esto es útil cuando la máquina 
  # cliente no puede validar el certificado emitido por la CA del CRC-SAS.
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  
  # Credenciales para la API
  base.url        <- url.consulta
  usuario.default <- usuario
  clave.default   <- clave
  
  fecha.desde           <- ConvertirFechaISO8601(as.Date(fecha.inicial, tz = UTC))
  fecha.hasta           <- ConvertirFechaISO8601(as.Date(fecha.final, tz = UTC))
  
  for (k in 1:length(id.estacion)){
    
    estacion <- id.estacion[k]
    print(estacion)
    url.registros.diarios <- glue::glue("{base.url}/registros_diarios/{estacion}/{fecha.desde}/{fecha.hasta}")
    registros.largo       <- ConsumirServicioJSON(url = url.registros.diarios,
                                                  usuario = usuario.default, clave = clave.default)
    
    registros.ancho       <- dplyr::select(registros.largo, -num_observaciones) %>%
      tidyr::spread(key = variable_id, value = valor)
    
    registros.ancho <- registros.ancho[which(registros.ancho$estado == "A" | registros.ancho$estado == "S" | is.na(registros.ancho$estado)),
                                       c("omm_id", "fecha", "tmin", "prcp", "hr")]
    
    tmin <- aggregate(tmin ~ fecha, data = registros.ancho, FUN = sum)
    prcp <- aggregate(prcp ~ fecha, data = registros.ancho, FUN = sum)
    hr <- aggregate(hr ~ fecha, data = registros.ancho, FUN = sum)
    
    aux.merge <- base::merge(tmin, prcp, all = TRUE)
    merge.final <- base::merge(aux.merge, hr, all = TRUE)
    
    colnames(merge.final)[1] <- "Fecha"
    
    if (k == 1) {salida <- merge.final} else {salida <- rbind(salida, merge.final)}
    
    # Tabla de datos de todas las variables
    print("Los ultimos registros de la consulta son:")
    print(tail(merge.final))
  }
 return(salida)
}

  

