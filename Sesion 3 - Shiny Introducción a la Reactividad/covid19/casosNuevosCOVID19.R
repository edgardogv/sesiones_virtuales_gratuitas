# casosNuevosCOVID19 - Agrega una columna al conjunto de datos 
# con los casos nuevos por fecha y por país
#
# @param covid19_agrupado data.frame que contiene los datos COVID-19 
# en formato largo y totalizados por fecha y por país. Se asume que los nombres 
# de las columnas son: 
# "Value" para casos, "Date" para fechas y "Country.Region" para paises y regiones.
#
# @return data frame original con una columna adicional que 
# incluye los casos nuevos por fecha y por país
#
# @examples
# \donotrun{
#    covid19 <- read.csv("time_series_covid19_confirmed_global_narrow.csv", comment.char = "#")
#    casosNuevosCOVID19(covid19)
# }
#
# @author Edgardo Gutierrez contacto@edgardogv.com
casosNuevosCOVID19 <- function(covid19_agrupado){
  
  # Nombre unicos de paises (porque se repiten por fecha)
  paises <- unique(covid19_agrupado$Country.Region)
  
  # Calculando los datos nuevos por día
  almacenamiento_nuevos <- list()
  for(pais in paises){
    datos_filtrados <- covid19_agrupado[covid19_agrupado$Country.Region == pais, ]
    datos_filtrados$Nuevos <- c(datos_filtrados$Value[1],diff(datos_filtrados$Value))
    almacenamiento_nuevos[[pais]] <- datos_filtrados
  }
  
  # Retornando resultado
  return(almacenamiento_nuevos)
}