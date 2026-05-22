# =============================================================
# Comparación: Viajes realizados utilizando el sistema de bicicletas públicas "Ecobici" vs Viajes estimados anuales del público en general
# Ciudad de Buenos Aires — 2012 a 2017
# =============================================================

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)

# --- 1. Carga y conteo de fuentes de datos ------------------
#
rec_2010_ecobici_csv <- read_csv("Hackaton-cs-de-datos-e250/csv-source/recorridos-realizados-2010.csv") rec_2011_ecobici_csv <- read_csv("Hackaton-cs-de-datos-e250/csv-source/recorridos-realizados-2011.csv")
rec_2012_ecobici_csv <- read_csv("Hackaton-cs-de-datos-e250/csv-source/recorridos-realizados-2012.csv")
rec_2013_ecobici_csv <- read_csv("Hackaton-cs-de-datos-e250/csv-source/recorridos-realizados-2013.csv")
rec_2014_ecobici_csv <- read_csv("Hackaton-cs-de-datos-e250/csv-source/recorridos-realizados-2014.csv")
rec_2015_ecobici_csv <- read_csv("Hackaton-cs-de-datos-e250/csv-source/recorridos-realizados-2015.csv")
rec_2016_ecobici_csv <- read_csv("Hackaton-cs-de-datos-e250/csv-source/recorridos-realizados-2016.csv")
rec_2017_ecobici_csv <- read_csv("Hackaton-cs-de-datos-e250/csv-source/recorridos-realizados-2017.csv")
uso_de_bicicletas_en_general_por_anio_csv <- read_csv("./Hackaton-cs-de-datos-e250/csv-source/Conteocicilistas-GCBA.csv")


# --- 2. Selecciono las columnas de interés ------------------

rec_2012_ecobici <- rec_2012_ecobici_csv |>
  summarise(
    anio = 2012,
    cant_recorridos_anuales_ecobici = n(),
    cant_estaciones_ecobici =
      length(unique(rec_2012_ecobici_csv$id_estacion_origen)) +
      length(unique(rec_2012_ecobici_csv$id_estacion_destino))
  )

rec_2013_ecobici <- rec_2013_ecobici_csv |>
  summarise(
    anio = 2013,
    cant_recorridos_anuales_ecobici = n(),
    cant_estaciones_ecobici =
      length(unique(rec_2013_ecobici_csv$id_estacion_origen)) +
      length(unique(rec_2013_ecobici_csv$id_estacion_destino))
  )

rec_2014_ecobici <- rec_2014_ecobici_csv |>
  summarise(
    anio = 2014,
    cant_recorridos_anuales_ecobici = n(),
    cant_estaciones_ecobici =
      length(unique(rec_2014_ecobici_csv$NOMBRE_ORIGEN)) +
      length(unique(rec_2014_ecobici_csv$DESTINO_ESTACION))
  )

rec_2015_ecobici <- rec_2015_ecobici_csv |>
  summarise(
    anio = 2015,
    cant_recorridos_anuales_ecobici = n(),
    cant_estaciones_ecobici =
      length(unique(
        rec_2015_ecobici_csv$id_estacion_origen + rec_2015_ecobici_csv$id_estacion_destino


        ))
#      length(unique(rec_2015_ecobici_csv$id_estacion_destino))
  )

rec_2016_ecobici <- rec_2016_ecobici_csv |>
  summarise(
    anio = 2016,
    cant_recorridos_anuales_ecobici = n(),
    cant_estaciones_ecobici =
      length(unique(rec_2016_ecobici_csv$id_estacion_origen)) +
      length(unique(rec_2016_ecobici_csv$id_estacion_destino))
  )

rec_2017_ecobici <- rec_2017_ecobici_csv |>
  summarise(
    anio = 2017,
    cant_recorridos_anuales_ecobici = n(),
    cant_estaciones_ecobici =
      length(unique(rec_2017_ecobici_csv$id_estacion_origen)) +
      length(unique(rec_2017_ecobici_csv$id_estacion_destino))
  )


recorridos_general_anual <- uso_de_bicicletas_en_general_por_anio_csv |>
  select(AÑO, `VIAJES ANUALES`) |>
  rename(
    anio = AÑO,
    cant_recorridos_anuales_general = `VIAJES ANUALES`
  ) |>
  mutate(
    cant_recorridos_anuales_general = as.numeric(gsub("\\.", "", cant_recorridos_anuales_general))
  )

# --- 3. Unimos los datos de cada año de recorridos de Ecobici registrados y los recorridos estimados en general generando una tabla donde estén todos los adtos relevantes por año ------------------

recorridos_ecobici_anual <-
  bind_rows(rec_2012_ecobici, rec_2013_ecobici, rec_2014_ecobici, rec_2015_ecobici, rec_2016_ecobici, rec_2017_ecobici)

recorridos_totales_anual <-
  merge(x = recorridos_ecobici_anual, y = recorridos_general_anual, by = "anio", all = TRUE)


# --- 4. Agrego datos comparativos ------------------------------
recorridos_totales_anual <- recorridos_totales_anual |>
  mutate(
      dif_gen_i_menos_ecobici_i  =
        cant_recorridos_anuales_general
        - cant_recorridos_anuales_ecobici

    , dif_gen_i_menos_ecobici_h =
        cant_recorridos_anuales_general
        - dplyr::lag(cant_recorridos_anuales_ecobici, n=1)

    , dif_gen_i_menos_ecobici_j =
        cant_recorridos_anuales_general
        - dplyr::lead(cant_recorridos_anuales_ecobici, n=1)

    , variacion_recorridos_generales_anual =
        cant_recorridos_anuales_general -
        dplyr::lag(cant_recorridos_anuales_general, n=1)

    , cobertura_pct =
        round((cant_recorridos_anuales_ecobici /  cant_recorridos_anuales_general) * 100, 1)
  )


# Exporto los datos que contienen varios NA a un CSV para tenerlo de respaldo

write_csv(recorridos_totales_anual, "./Hackaton-cs-de-datos-e250/resumen_comparativo.csv")

# Filtro las filas que tienen NAs molestos

recorridos_totales_anual <- recorridos_totales_anual |>
  filter(!is.na(cant_recorridos_anuales_general), !is.na(cant_recorridos_anuales_ecobici))


# Plots

ggplot(recorridos_totales_anual) +
  geom_line(aes(x=anio,y=dif_gen_i_menos_ecobici_i),color='red', size = 1) +
  geom_line(aes(x=anio,y=dif_gen_i_menos_ecobici_h),color='blue', size = 1) +
  geom_line(aes(x=anio,y=dif_gen_i_menos_ecobici_j),color='black', size = 1) +
  ylab('diferencias')+xlab('año')



ggplot( recorridos_totales_anual, aes( x = anio) ) +
  geom_line(aes(y = variacion_recorridos_generales_anual, color = 'green', size = 1)) +
  geom_line(aes(y = cant_recorridos_anuales_general, color = 'blue', size = 1))  +
  geom_line(aes(y = cant_recorridos_anuales_ecobici, color = 'red', size = 1)) +
  labs(title = 'variacion por año', x = 'Anio', y = 'Recorridos')
