library(nycflights13)
library(tidyverse)

# 3.2.5 Exercises
#
# 1) In a single pipeline for each condition, find all flights that meet the condition:
# - Had an arrival delay of two or more hours
flights |>
    filter(arr_delay >= 120)

# - Flew to Houston (IAH or HOU)
flights |>
    filter(dest == 'IAH' | dest == 'HOU')

# - Were operated by United, American, or Delta
flights |>
    filter(carrier == 'UA' | carrier =='DL')

# - Departed in summer (July, August, and September)
flights |>
    filter(month == 7 | month == 8 | month == 9)

# - Arrived more than two hours late but didn’t leave late
flights |>
    filter(arr_delay >= 120 & dep_time == sched_dep_time)

# - Were delayed by at least an hour, but made up over 30 minutes in flight
flights |>
    filter(dep_delay >= 60) |>
    filter(arr_delay <= dep_delay-30)


# 2) Sort flights to find the flights with the longest departure delays. Find the flights that left earliest in the morning.

flights |>
    filter(dep_delay >= mean(dep_delay, na.rm = TRUE)) |>  # muestro únicamente los vuelos cuya demora de partida sea mayor a la media
    filter(dep_time < 1200 & dep_time >= 600) |> # muestro sólo los que salieron durante la mañana (supongo que la mañana empieza a las 6 y antes de eso es la madrugada)
    arrange(dep_time)


# 3) Sort flights to find the fastest flights. (Hint: Try including a math calculation inside of your function.)

#to-do


# 4) Was there a flight on every day of 2013?

flights |>
    filter(year == 2013) |> # por las dudas me aseguro de que no se cuele ninguna línea que no haga refrencia al año 2013
    count(year, month, day) # cuento cuántas combinaciones posibles hay de día-mes-año

# una forma alternativa de hacer esto sería:

flights |>
    filter(year == 2013) |>
    distinct(year, month, day)

# Devuelve un tibble: 365 × 4, sabiendo que cada fila registra un vuelo que sucedió y no hay ninguna fila que indique la ausencia de vuelos, concluyo que hubo al menos 1 vuelo por cada día del año 2013

# 5) Which flights traveled the farthest distance? Which traveled the least distance?
flights |>
    filter()



# Does it matter what order you used filter() and arrange() if you’re using both? Why/why not? Think about the results and how much work the functions would have to do.
