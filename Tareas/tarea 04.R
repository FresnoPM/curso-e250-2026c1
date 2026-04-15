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
    filter(distance >= mean(distance, na.rm=TRUE)) # los vuelos que recorrieron una distancia mayor a la media

flights |>
    filter(distance <= mean(distance, na.rm=TRUE)) # los vuelos que recorrieron una distancia menor a la media


# Does it matter what order you used filter() and arrange() if you’re using both? Why / why not? Think about the results and how much work the functions would have to do.
#
# No tiene sentido usar arrange antes de filter porque le das más trabajo a arrange.
#




# 3.3.5 Exercises

# 1) Compare dep_time, sched_dep_time, and dep_delay. How would you expect those three numbers to be related?



# 2) Brainstorm as many ways as possible to select dep_time, dep_delay, arr_time, and arr_delay from flights.



# 3) What happens if you specify the name of the same variable multiple times in a select() call?
flights |>
    select(arr_delay, arr_delay, dep_time)
# muestra la columna repetida sólo una vez


# 4) What does the any_of() function do? Why might it be helpful in conjunction with this vector?

variables <- c("year", "month", "day", "dep_delay", "arr_delay")

# 5) Does the result of running the following code surprise you? How do the select helpers deal with upper and lower case by default? How can you change that default?

flights |> select(contains("TIME"))

# select helpers son case insensitive

# 6) Rename air_time to air_time_min to indicate units of measurement and move it to the beginning of the data frame.


flights |>
    rename(air_time_min = air_time) |>
    relocate(air_time_min, .before = 1)

# forma alternativa

flights |>
    relocate(air_time_min = air_time, .before = 1)


# 7) Why doesn’t the following work, and what does the error mean?

    flights |>
    select(tailnum) |>
    arrange(arr_delay)

#> Error in `arrange()`:
#> ℹ In argument: `..1 = arr_delay`.
#> Caused by error:
#> ! object 'arr_delay' not found

# este código falla al aplicar la función arrange() a una columna que ya no se encuentra en la tabla por haber quedado afuera de la selección de la primera fila select()
#
#
#
#  3.5.7 Exercises

# 1) Which carrier has the worst average delays?
# Challenge: can you disentangle the effects of bad airports vs. bad carriers? Why/why not? (Hint: think about flights |> group_by(carrier, dest) |> summarize(n()))
#

flights |>
    group_by(carrier) |> # 16 carriers
    summarise(
        avg_dep_delay = mean(dep_delay, na.rm=TRUE),
        avg_arr_delay = mean(arr_delay, na.rm=TRUE)
    ) |>
    # arrange(desc(avg_dep_delay))
    slice_max(avg_dep_delay, n = 6) |>
    slice_max(avg_arr_delay, n = 1)   # esto se puede mejorar to-do



# el carrier con peor delay, tanto de partida como de llegada es el F9

# 2) Find the flights that are most delayed upon departure to each destination.



# 3) How do delays vary over the course of the day? Illustrate your answer with a plot.

# 4) What happens if you supply a negative n to slice_min() and friends?

# 5) Explain what count() does in terms of the dplyr verbs you just learned. What does the sort argument to count() do?

# 6) Suppose we have the following tiny data frame:

        df <- tibble(
            x = 1:5,
            y = c("a", "b", "a", "a", "b"),
            z = c("K", "K", "L", "L", "K")
        )

# Write down what you think the output will look like, then check if you were correct, and describe what group_by() does.

    df |>
        group_by(y)

# 7) Write down what you think the output will look like, then check if you were correct, and describe what arrange() does. Also, comment on how it’s different from the group_by() in part (a).

    df |>
        arrange(y)

# Write down what you think the output will look like, then check if you were correct, and describe what the pipeline does.

    df |>
        group_by(y) |>
        summarize(mean_x = mean(x))

    Write down what you think the output will look like, then check if you were correct, and describe what the pipeline does. Then, comment on what the message says.

    df |>
        group_by(y, z) |>
        summarize(mean_x = mean(x))

    Write down what you think the output will look like, then check if you were correct, and describe what the pipeline does. How is the output different from the one in part (d)?

        df |>
        group_by(y, z) |>
        summarize(mean_x = mean(x), .groups = "drop")

    Write down what you think the outputs will look like, then check if you were correct, and describe what each pipeline does. How are the outputs of the two pipelines different?

        df |>
        group_by(y, z) |>
        summarize(mean_x = mean(x))

    df |>
        group_by(y, z) |>
        mutate(mean_x = mean(x))