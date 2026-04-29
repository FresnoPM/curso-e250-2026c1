


install.packages("nycflights13")
install.packages("tidyverse")
# Data importada desde https://github.com/tidyverse/nycflights13

library(nycflights13)
library(tidyverse)

# Resumen de https://r4ds.hadley.nz/data-transform.html

# Maneras de explorar el tibble 'Flights' (de gran tamaño):
flights
View(flights)
glimpse(flights)
print(flights, width = Inf)
print(flights, n = 50)


flights |>
  filter(dest == "IAH") |> # tomo el output de la función filter (sólo toma aquellas filas cuya cuya columa dest=IAH) y lo pasa como argumento a la siguiente función, group_by
    # la función filter() mantiene el orden de las filas pero filtra cuáles se mostrarán
  group_by(year, month, day) |> # tomo el output de la función group_by (que agrupa esas filas según el año, mes y día) y lo pasa como argumento a la siguiente función, summarize
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE) # uso la función summarize para agregar una columna que toma los datos de la columna arr_delay e imprime la media de todas sus filas que tengan como destino IAH, agrupadas por año, mes y día
  )

flights |>
    filter(dep_delay > 120)


flights |>
  filter(month == 1 & day == 2) # sólo muestra vuelos del 2 de enero


flights |>
  filter(month == 1 | month == 2) # sólo muestra vuelos que salieron en enero o en febrero

flights |>
  filter(month %in% c(1, 2)) # otra forma de escribir el mismo comando de arriba, para que sólo muestre vuelos del mes 1 o 2

jan1 <- flights |> filter(month==1 & day==1) # asigno el tibble vuelos que hayan salido el 1 de enero a la variable jan1

flights |>
  arrange(year, month, day, dep_time # la función arrange() cambia el orden de las columnas
          , desc(dep_delay)) # y permite ordenar según alguna columna en particular



flights |>
  distinct(origin, dest # muesta un tibble con 2 columnas: origen y destino, donde sólo se muestran 1 vez cada par de combinaciones de origen y destino
           , .keep_all = TRUE) # muestra el resto de las columnas también, en el orden indicado por estas 2 columnas con combinaciones únicas

flights |>
  count(origin, dest # muestra cada par combinado de origen y desino con la cantidad de filas que coinciden con ese par
        , sort = TRUE) # ordenado de forma descendente por cantidad

flights |> distinct(month)



# Columnas

print(
  flights |>
    mutate(
      gain = dep_delay - arr_delay,
      speed = distance / air_time * 60
    ), width = Inf)   # uso print(tibble con transformacion, width=Inf) para poder ver las 2 colmnas nuevas agregadas por mutate() al final de todo



flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = 3     # esto posiciona las nuevas columnas antes de la 3ra columna
  )

flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .after = day # esto posiciona las nuevas columnas después de la columna "day"
  )


flights |>
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used"  # descarta todo salvo las columnas involucradas: las nuevas recién creadas y aquellas en bases a las cuales se generaron sus datos
  )




flights |>
  select(year, month, day) # Select columns by name

flights |>
  select(year:dep_time) # Select all columns between 2 (inclusive), by name

flights |>
  select(!year:day) # Select all columns except those from year to day (inclusive), by name


flights |>
  select(where(is.character)) # Select all columns that are characters

# otros usos de select() See ?select for more details.
  select(starts_with("arr")) # matches names that begin with “arr”.
  select(ends_with("xyz")) # matches names that end with “xyz”.
  select(contains("ijk")) # matches names that contain “ijk”.
  select(num_range("x", 1:3)) # matches x1, x2 and x3.

flights |>
    select(tail_numbeeer = tailnum) # toma los valores de la columna de la derecha y se la asigna a una columna nueva con el nombre de la izq, muestra sólo esa columna


flights |>
  rename(tail_numbeeer2 = tailnum) # muestra la tabla completa pero únicamente cambia el nombre de esa columna

flights |>
  relocate(year:dep_time, .after = time_hour)

flights |>
  relocate(starts_with("arr"), .before = dep_time)


# Groups grupos

flights |>
  group_by(month) # no cambia la tabla pero crea la clase month (12 grupos, uno por cada mes)
                  # podemos ver que en el output, arriba de todo dice "A tibble: 336,776 × 19 # Groups:   month [12]"

flights |>
  group_by(month) |>
  summarize(                    # una vez agrupados en clase month, se puede sumarizar información de cada uno de los 12 grupos de esa clase
    avg_delay1 = mean(dep_delay),
    avg_delay2 = mean(dep_delay, na.rm = TRUE),  # para no lidiar con valores vacíos o sucios, eliminamos los NA
    n = n() # muestra la cantidad de elementos por cada grupo de la clase month
  )


# Slices


df |> slice_head(n = 1) # takes the first row from each group.
df |> slice_tail(n = 1) # takes the last row in each group.
df |> slice_min(x, n = 1) # takes the row with the smallest value of column x.
df |> slice_max(x, n = 1) # takes the row with the largest value of column x.
df |> slice_sample(n = 1) # takes one random row.


flights |>
  group_by(dest) |> # creo la clase dest que tiene 105 grupos
  slice_max(arr_delay, n = 2, # toma las 4 filas con mayor arr_delay de cada grupo
            with_ties = TRUE) |> # no entiendo que hace esto to-do
  relocate(dest) # ubica la columna dest al principio de todo

daily <- flights |>
  group_by(year, month, day) # agrupo por varias variables, en este caso son 365 grupos (1 grupo por día del año) y

dayly_flights <- dayly |>
  summarize(n = n()) # muestra la cantidad de filas (vuelos) por cada día del año agrupados en dayly

