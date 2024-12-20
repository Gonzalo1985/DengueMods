rm(list = ls())

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


# Uso de paquete Cairo para generar gráficos
options(bitmapType = "cairo")

# El parámetro ssl_verifypeer=FALSE implica que no se va a verificar la validez del certificado 
# utilizado en la conexión SSL establecida con la API del CRC-SAS. Esto es útil cuando la máquina 
# cliente no puede validar el certificado emitido por la CA del CRC-SAS.
httr::set_config(httr::config(ssl_verifypeer = FALSE))

# Credenciales para la API
base.url        <- 'https://api.crc-sas.org/ws-api'
usuario.default <- ''
clave.default   <- ''

fecha.desde           <- ConvertirFechaISO8601(as.Date("2024-08-01", tz = UTC))
fecha.hasta           <- ConvertirFechaISO8601(as.Date("2024-08-21", tz = UTC))

for (k in c("87121", "87155", "87344")){
  
  estacion <- k
  
  url.registros.diarios <- glue::glue("{base.url}/registros_diarios/{estacion}/{fecha.desde}/{fecha.hasta}")
  registros.largo       <- ConsumirServicioJSON(url = url.registros.diarios,
                                                usuario = usuario.default, clave = clave.default)
  
  registros.ancho       <- dplyr::select(registros.largo, -num_observaciones) %>%
    tidyr::spread(key = variable_id, value = valor)
  
  registros.ancho <- registros.ancho[which(registros.ancho$estado == "A" | registros.ancho$estado == "S"),
                                     c("omm_id", "fecha", "tmin", "prcp", "hr")]
  
  tmin <- aggregate(tmin ~ fecha, data = registros.ancho, FUN = sum)
  prcp <- aggregate(prcp ~ fecha, data = registros.ancho, FUN = sum)
  hr <- aggregate(hr ~ fecha, data = registros.ancho, FUN = sum)
  
  aux.merge <- merge(tmin, prcp)
  merge.final <- merge(aux.merge, hr)
  
  # Tabla de datos de todas las variables
  print(knitr::kable(merge.final))
}
  

