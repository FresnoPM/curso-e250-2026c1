#
# ____________________________-
#
#
#     quiero una tabla que tenga
#
# faculty_id
# Group
# Group_type
# cant_con_out
# from_whom (list)
# cant_con_in
# to_whom (list)
# cant_con_total
#


rm(list =ls())

library(igraph)
library(igraphdata)
data(package = "igraphdata")$results[, "Item"]
data("UKfaculty")
g <- UKfaculty
# total de conexiones en el formato 57->52
g

original_df = as_data_frame(UKfaculty)
# total de conexiones from - to - weight



library(dplyr)
mi_df <- data.frame(faculty_id=1:81)
mi_df <- mi_df |> mutate(
    degree = degree(
    g, v = mi_df$faculty_id,
    mode = c("all", "out", "in", "total"),
    loops = TRUE,
    normalized = FALSE
    )
    , outward_count = outward_count_list
    , inward_count = inward_count_list
    , total_conections = outward_count + inward_count
)
mi_df
i=1
outward_count_list <- c()
inward_count_list <- c()
for (i in 1:81) {
    # Create new list element
    outward_count_list[length(outward_count_list) + 1] <- nrow(original_df[original_df$from == i, ])
    inward_count_list[length(inward_count_list) + 1] <- nrow(original_df[original_df$to == i, ])
    i = i+1
}

