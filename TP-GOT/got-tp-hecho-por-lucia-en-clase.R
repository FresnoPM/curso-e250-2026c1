rm(list=ls())

library(tidyverse)
library(igraph)
library(statnet)

load("curso-e250-2026c1/TP-GOT/union_characters.RData")
summary(union_characters)
baratheon_nodes <- union_characters %>%
  filter(house == "House Baratheon")


load("curso-e250-2026c1/TP-GOT/union_edges.RData")
summary(union_edges)

baratheon_edges_total <- rbind(   # 31 elementos
   union_edges %>% filter(str_detect(source, "Baratheon")) ,
   union_edges %>% filter(str_detect(target, "Baratheon")) ,

 )
#  %>% hoist unique() eliminar duplicados


baratheon_graph_endo <- igraph::induced_subgraph(
  union_graph_undir,
  vids = igraph::V(union_graph_undir)$name %in% baratheon_nodes$name
) # 11 edges



