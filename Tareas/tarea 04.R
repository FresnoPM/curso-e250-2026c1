library(nycflights13)
library(tidyverse)

# 3.2.5 Exercises
#
# 1) In a single pipeline for each condition, find all flights that meet the condition:
# - Had an arrival delay of two or more hours
# - Flew to Houston (IAH or HOU)
# - Were operated by United, American, or Delta
# - Departed in summer (July, August, and September)
# - Arrived more than two hours late but didn’t leave late
# - Were delayed by at least an hour, but made up over 30 minutes in flight

flights |>
    filter(arr_delay >= 120) |>
    filter(dest == 'IAH' | dest == 'HOU') |>
    filter(carrier == 'UA' | carrier =='DL') |>
    filter(month == 7 | month == 8 | month == 9) |>
    filter(arr_delay >= 120 & dep_time == sched_dep_time)
        # & (dep_delay >= 60 & arr_delay) to-do

        )



# 2) Sort flights to find the flights with the longest departure delays. Find the flights that left earliest in the morning.

flights |>
    arrange(desc(dep_delay)) |>
    filter(dep_time < 1200)
    arrange(dep_time)


# 3) Sort flights to find the fastest flights. (Hint: Try including a math calculation inside of your function.)

#to-do


# 4) Was there a flight on every day of 2013?

flights |>
    filter(year == 2013) |>
    count(year,month,day)

# Devuelve un tibble: 365 × 4, sabiendo que cada fila registra un vuelo que sucedió y no hay ninguna fila que indique la ausencia de vuelos, concluyo que hubo al menos 1 vuelo por cada día del año 2013

# 5) Which flights traveled the farthest distance? Which traveled the least distance?




# Does it matter what order you used filter() and arrange() if you’re using both? Why/why not? Think about the results and how much work the functions would have to do.
