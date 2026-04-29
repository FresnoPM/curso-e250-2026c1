
# cargar librerias --------------------------------------------------------

library(tidyverse)

# cargar datos ------------------------------------------------------------

install.packages("readr")
library(readr)

#vuelos AR 2025
anac_2025 <- read_csv2("tarea05/202512-informe-ministerio-actualizado-dic-final.csv")

# analisis de los datos ---------------------------------------------------

glimpse(anac_2025)
install.packages("tidyverse")
library(tidyverse)

anac_2025 

# aeropuertos

aeropuertos <- 
  read_csv("tarea05/iata-icao.csv")

#clima

clima <- read_fwf("tarea05/registro_temperatura365d_smn.txt", 
                  col_positions = fwf_widths(
                    c(8, 1, 5, 1, 5, 200), 
                    c('fecha', 'x', 'tmax', 'y', 'tmin', 'nombre')
                  ),
                  skip = 3) |> 
  select(-x,-y)

glimpse(anac_2025)

clima[1:2,]

library(dplyr)

anac_2025 <- anac_2025 |>
  mutate(
    tipo_vuelo = factor(`Clase de Vuelo (todos los vuelos)`),
    clasif_vuelos = factor(`Clasificación Vuelo`),
    tipo_movimiento = factor(`Tipo de Movimiento`),
    aeropuerto = factor(Aeropuerto),
    origen_destino = factor(`Origen / Destino`),
    aerolinea = factor(`Aerolinea Nombre`),
    calidad_dato = factor(`Calidad dato`),
    aeronave=factor(Aeronave)
  )

summary(anac_2025)
