# ########################## #
# Clase 07 - Flights AR 2025 #
# ########################## #       

# Carga de librerías

library(tidyverse)

# Carga de archivos

## Aterrizajes y despeges 
# https://datos.transporte.gob.ar/dataset/aterrizajes-y-despegues-procesados-por-la-administracion-nacional-de-aviacion-civil-anac

anac_2025 <- read.csv2(file = "~/Repos/Cs de Datos para Negocios/Curso-e250-2026c1/202512-informe-ministerio-actualizado-dic-final.csv")
glimpse(anac_2025)
summary(anac_2025)

# retocamos la tabla anac_2025

anac_2025 <-
  anac_2025 |> # le aplico un proceso
  mutate(
    
    clase_vuelo = factor(`Clase.de.Vuelo..todos.los.vuelos.`)
    , clasif_vuelo = factor(`Clasificación.Vuelo`)
    , tipo_movimiento = factor(`Tipo.de.Movimiento`)
    , aeropuerto = factor(`Aeropuerto`)
    , origen_destino = factor(`Origen...Destino`)
    , aerolinea = factor(`Aerolinea.Nombre`)
    , aeronave = factor(`Aeronave`)
    , calidad_dato = factor(`Calidad.dato`)  # OJO!!! si uso la comilla simple " ' " toma el nombre de la columna como string. Para que tome los contenidos de la columna necesito usar el tilde para atrás " ` ".
  )

summary(anac_2025)
glimpse(anac_2025)

a <- 3
a <- 5
a



## Aeropuertos
# Códigos de aeropuertos
aeropuertos <- read_csv(file="~/Repos/Cs de Datos para Negocios/Curso-e250-2026c1/iata-icao.csv")
glimpse(aeropuertos)


## Clima
# Servicio metereológico naciona
clima <- read_csv(file="~/Repos/Cs de Datos para Negocios/Curso-e250-2026c1/registro_temperatura365d_smn.txt")

# glimse(clima) no funciona porque estoy abriendo un txt
View(clima)
clima[1:2,]

columnas_clima <- read_fwf(file = "~/Repos/Cs de Datos para Negocios/Curso-e250-2026c1/registro_temperatura365d_smn.txt"
                           , col_positions = fwf_widths(
                                                        c(8, 1, 5, 1,5,200) # agrego 2 columnas x e y que luego dropearé
                                                        , c('fecha', 'x', 'tmax', 'y', 'tmin','nombre')
                           ),
                           skip = 3 
                           ) |> select(-x, -y) # aplico el proceso select a read_fwf, elimino las columnas x e y
