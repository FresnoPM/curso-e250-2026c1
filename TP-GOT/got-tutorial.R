# https://shirinsplayground.netlify.app/2018/03/got_network/
rm(list=ls())
library(readr)     # fast reading of csv files
library(tidyverse) # tidy data analysis
library(tidygraph) # tidy graph analysis
library(ggraph)    # for plotting


path <- "asoiaf/data/"
files <- list.files(path = path, full.names = TRUE)

cooc_all_edges <- read_csv(files[1])  # El primer archivo es "asoiaf/data//asoiaf-all-edges.csv" donde están sólo las aristas, todas las conexiones son undirected
# Las columnas son: Source, Target, Type, id, weight

main_ch <- cooc_all_edges %>%   #
    select(-Type) %>% # elimina columna Type porque es redundante ya que sólo tiene un valor: Undirected
    gather(x, name, Source:Target) %>% # crea una única columna llamada "name" que se llena con todos los valores, ya sea que estén en de la columna source o de la columna target.
    # Detalle: usa "x" para referirse al df al que se le están aplicando estos filtros.
    group_by(name) %>% # agrupa por nombre
    summarise(sum_weight = sum(weight)) %>% # se suman los pesos de los vínculos en los que participa ese personaje (ya sea en rol de source o de target)
    ungroup()

main_ch_l <- main_ch %>%
    arrange(desc(sum_weight)) %>% #
    top_n(100, sum_weight) # Selecciona los nombres de los 100 personajes cuyos vínculos sumen, en total una ponderación más alta

cooc_all_f <- cooc_all_edges %>%
    filter(Source %in% main_ch_l$name & Target %in% main_ch_l$name) # filtra el listado original y deja únicamente aquellas líneas donde haya un nombre de los 100 principales en la col Source Y TAMBIÉN en la columna Target



# Muy útil funciones de tidygraph:
#
# ?edge_is_...
# edge_is_multiple()
# edge_is_loop()
# edge_is_mutual()
# edge_is_from(from)
# edge_is_to(to)
# edge_is_between(from, to, ignore_dir = !graph_is_directed())
# edge_is_incident(nodes)
# edge_is_bridge()
# edge_is_feedback_arc(weights = NULL, approximate = TRUE)


as_tbl_graph(cooc_all_f, directed = FALSE)
# # Node Data: 100 × 1 (active)  <---- OJO NODES ACTIVE
# # Edge Data: 798 × 5 - (columnas: from, to, Type, id, weight)

# directed TRUE o FALSE da lo mismo porque son todas undirected en la tabla original

as_tbl_graph(cooc_all_f, directed = FALSE) %>%
    activate(edges) %>% # activa las aristas
    filter(!edge_is_multiple()) # quita las aristas que no son múltiples. Una arista el múltiple cuando tiene hermanas paralelas ¿QUÉ CORCHO ES ESO?
# # Edge Data: 798 × 5 (active)  <---- OJO EDGES ACTIVE
# # Node Data: 100 × 1

# node_rank {tidygraph}	R Documentation
# Calculate node ranking: This set of functions tries to calculate a ranking of the nodes in a graph so that nodes sharing certain topological traits are in proximity in the resulting order. These functions are of great value when composing matrix layouts and arc diagrams but could concievably be used for other things as well.




install.packages("seriation")
as_tbl_graph(cooc_all_f, directed = FALSE) %>%
    activate(nodes) %>%
    mutate(n_rank_trv = node_rank_traveller()) %>% # Minimize hamiltonian path length using a travelling salesperson solver # whatever the f that means
    arrange(n_rank_trv)
