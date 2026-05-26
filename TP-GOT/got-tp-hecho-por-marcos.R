#TRABAJO EN CLASE 1

union_graph
union_graph_undir

#1.- caracteristicas de la red
g <- union_graph_undir

#cuantos nodos y aristas tiene
summary(g)

vcount(g)   # nodos
ecount(g)   # aristas

#Que atributos tienen nodos y aristas?

#nodos
vertex_attr_names(g)

#aristas
edge_attr_names(g)

#Es dirigida o no?
is_directed(g)

#Es ponderada o no ponderada?
is_weighted(g)

#2.-la red esta conectada?
igraph::is_connected(g)

#numero de componentes
comp <- igraph::components(g)

comp$no

#numero del componente mas grande?
max(comp$csize)

#3.- Como se conecta la red

#grado promedio
mean(igraph::degree(g))

median(igraph::degree(g))

#4.- Histograma
hist(
    igraph::degree(g),
    main = "Distribución de grados",
    xlab = "Grado",
    ylab = "Frecuencia"
)

#5.- Cuales son los nodos mas conectados(hubs)

sort(igraph::degree(g), decreasing = TRUE)[1:5]

#6.- Densidad de la red
density <- igraph::edge_density(g)

density

#7.- clustering global

clustering_global <- igraph::transitivity(
    g,
    type = "global"
)

clustering_global

#8.- distancia promedio entre nodos

avg_distance <- igraph::mean_distance(
    g,
    directed = FALSE
)

avg_distance

#9.-componentes de la red
comp <- igraph::components(g)

comp$no
#tamano del componente mas grande
max(comp$csize)

#10.- comunidades (louvain)
com <- cluster_louvain(g)

#cantidad de comunidades
length(com)

#tamano de comunidades
sizes(com)

#modularidad
modularity(com)
