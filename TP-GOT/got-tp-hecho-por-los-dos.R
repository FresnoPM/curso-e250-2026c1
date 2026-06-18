#TRABAJO EN CLASE 1

union_graph
union_graph_undir

#1.- caracteristicas de la red
g <- union_graph_undir

#cuantos nodos y aristas tiene
summary(g)

vcount(g)   # 208 nodos
ecount(g)   # 326 aristas no dirigidas

#Que atributos tienen nodos y aristas?

#nodos
vertex_attr_names(g)
# [1] "name"       "male"       "culture"    "house"      "popularity" "house2"     "color"      "shape"

#aristas
edge_attr_names(g)
# character(0)

#Es dirigida o no?
is_directed(g) # FALSE

#Es ponderada o no ponderada?
is_weighted(g) # FALSE

#2.-la red esta conectada?
igraph::is_connected(g) # FALSE

#numero de componentes
comp <- igraph::components(g)

comp$no # 2 componentes

#numero del componente mas grande?
max(comp$csize)
min(comp$csize)
# el más grande 189 y el más chico 19 (la mayoría son todos los greyjoys, incluido Theon y un par más)
# en el más grande tenemos la mayor cantidad de personajes importantes

#3.- Como se conecta la red

#grado promedio
mean(igraph::degree(g)) # 3.134615
median(igraph::degree(g)) # 2.5
# Homogeneidad: como la media es 1.25384 veces la mediana, podemos inferir que la red no es homogénea y se jsutifica buscar Hubs

#4.- Histograma
hist(
    igraph::degree(g),
    main = "Distribución de grados",
    xlab = "Grado (cantidad de enlaces)",
    ylab = "Frecuencia"
)

#5.- Cuales son los nodos mas conectados(hubs)

sort(igraph::degree(g), decreasing = TRUE)[1:5]

# Quellon Greyjoy       Walder Frey     Oberyn Martell      Eddard Stark    Catelyn Stark
# 12                    10              10                  9               8
# Cada una de estas personas se conectan con múltiples familias y tienen grnades líneas narrativas
#
# degree {igraph}	R Documentation
# Degree and degree distribution of the vertices
# The degree of a vertex is its most basic structural property, the number of its adjacent edges.



#6.- Densidad de la red
density <- igraph::edge_density(g)

density # 0.01514307 es MUY esparsa, esto se debe a que la mayoría de los personajes no se comunican entre si

#7.- clustering global

clustering_global <- igraph::transitivity(
    g,
    type = "global"
)


# transitivity {igraph}	R Documentation
# Transitivity of a graph
# Transitivity measures the probability that the adjacent vertices of a vertex are connected. This is sometimes also called the clustering coefficient.


clustering_global #  0.2850679
# si el clustering es bajo (como en este caso) implica que la red es más dispersa, coincidiendo con el valor de densidad ínfimo que obtuvimos antes
# Duda: CHATGPT dice que el clustering global de 0.28 es alto porque las casas forman grupos muy conectados internamente ¿es así? por qué?

#8.- distancia promedio entre nodos

avg_distance <- igraph::mean_distance(
    g,
    directed = FALSE
)

avg_distance # 8.288956
# cantidad promedio de pasos (saltos) necesarios para conectar 2 personajes cualesquiera

#9.-componentes de la red
comp <- igraph::components(g)

comp$no
#tamano del componente mas grande
max(comp$csize)

#10.- comunidades (louvain)
com <- cluster_louvain(g) # 13 comunidades
# sin explorar los miembros de cada comunidad podemos asumir que cada comunidad incluye personajes de más de una casa, ya que contamos con 59 casas

#cantidad de comunidades
length(com)

#tamano de comunidades
sizes(com)

# Community sizes
# 1  2  3  4  5  6  7  8  9  10 11 12 13
# 23 18 16 16 11 11 19 22 20 14 11 18  9

#modularidad
modularity(com) # 0.84
# es una modularidad alta, indicando que las comunidades están bien delimitadas
# en esta red las comunidades representan vínculos consanguíneos y/o alianzas políticas de matrimonio entre casas que generan cierto alto grado de endogamia


#Trabajo 2 - casa Baratheon

library(tidyverse)
#ver numero de la casa
unique(union_characters$house) # 59 casas

#1.- filtrar personajes de baratheon

baratheon_nodes <- union_characters %>%
    filter(house == "House Baratheon")
# Duda: ¿deberíamos incluir en esta lista a mujeres de otras casas que tengan un vínculo de tipo "spouse" con un miembro varón de la flia Baratheon o que tengan un vínculo de tipo "mother" con una mujer de la flia Baratheon?

#2.- crear. una subred
baratheon_graph <- igraph::induced_subgraph(
    union_graph_undir,
    vids = igraph::V(union_graph_undir)$name %in% baratheon_nodes$name
) # son vínculos entre miembros de la propia familia, no tiene en cuenta las conexiones de ninguno de ellos con personajes de otras familias

#3.- resumen de la red
summary(baratheon_graph) # 10 nodos y 12 conexion no dirigidas

#4.- cantidad de nodos
#4.- cantidad de aristas
igraph::vcount(baratheon_graph)

igraph::ecount(baratheon_graph)

#Los nodos representan personajes de la Casa Baratheon.
#Las aristas representan relaciones entre ellos.

#5.- esta conectada?
igraph::is_connected(baratheon_graph) # TRUE

#6.- componentes

comp_b <- igraph::components(baratheon_graph) # es un solo componente que contiene los 10 personajes de la casa

comp_b$no

comp_b$csize

#7.- tamano del componente mas grande

max(comp_b$csize)

#8.- grado promedio

# si el promedio es mayor que la mediana existen hubs
# Eso indica que algunos personajes concentran muchas conexiones

mean(igraph::degree(baratheon_graph)) # 2.4

#9.- mediana del grado

median(igraph::degree(baratheon_graph)) # 1.5

### ES MUY ESPARSA
### Algunos pocos pesonajes concentran muchos vínculos mientras que la mayoría no
### 5 pesonajes tienen 1 grado, 1 pesonaje tiene 2 y 1 personaje tiene 3, 1 personaje tiene 4, mientras que hay 2 personajes con grado 5 de conexión

#10.- histograma de grados
hist(
    igraph::degree(baratheon_graph),
    main = "Distribución de grados - Casa Baratheon",
    xlab = "Grado de conexión",
    ylab = "Frecuencia"
)

#Si hay muchos nodos con pocos enlaces y pocos nodos con muchos enlaces, la red tiene cola larga.
#Eso es típico de redes sociales.

#11.- Top 5 personajes mas conectados
sort(
    igraph::degree(baratheon_graph),
    decreasing = TRUE
)


# Robert Baratheon   Steffon Baratheon   Cassana Baratheon  Stannis Baratheon    Renly Baratheon
# 5                  5                   4                  3                    2
# Joffrey Baratheon  Myrcella Baratheon  Ormund Baratheon  Shireen Baratheon   Tommen Baratheon
# 1                  1                   1                 1                   1

#Los personajes con mayor grado actúan como centros de conexión de la familia.(hubs)

#12.- densidad de la red

igraph::edge_density(baratheon_graph) #  0.2666667

#Alta densidad → familia muy interconectada.
#Baja densidad → relaciones más dispersas.

#13.- Clustering global

igraph::transitivity(
    baratheon_graph,
    type = "global"
) # 0.3

# Mide si los personajes relacionados con un Baratheon también están conectados entre sí.
# Duda: si los personajes de la muestra son TODO baratheon, ¿cómo puede ser que tengan un grado tan bajo de clustering?

#14.- distancia promedio

igraph::mean_distance(
    baratheon_graph,
    directed = FALSE
) # 2.133333

#15.- deteccion de comunidades

com_b <- igraph::cluster_louvain(baratheon_graph)

#Las comunidades son subgrupos dentro de la casa.

# IGRAPH clustering multi level, groups: 3, mod: 0.28
# + groups:
#     $`1`
# [1] "Cassana Baratheon"(4) "Ormund Baratheon"(1)  "Renly Baratheon"(2)   "Steffon Baratheon"(5)
#
# $`2`
# [1] "Joffrey Baratheon"(1)  "Myrcella Baratheon"(1) "Robert Baratheon"(5)   "Tommen Baratheon"(1)
#
# $`3`
# [1] "Shireen Baratheon"(1) "Stannis Baratheon"(3)



# cantidad de comunidades
length(com_b)

# tamaño de comunidades
igraph::sizes(com_b)

# modularidad
igraph::modularity(com_b) # 0.2777778 (Comunidades moderadas)

# | Valor      | Interpretación            |
# | ---------- | ------------------------- |
# | Cerca de 0 | No hay comunidades claras |
# | 0.3 – 0.5  | Comunidades moderadas     |
# | > 0.5      | Comunidades fuertes       |

#  En tu red Baratheon
#  El valor de modularidad es medio-bajo pero podemos observar que ninguna de estas comunidades comparte personajes, por lo que las consideramos bien diferenciadas.
#  Duda: ¿no debería en ese caso una modularidad alta?

#16.- visualizacion de la red

plot(
    baratheon_graph,
    vertex.label = igraph::V(baratheon_graph)$name,
    vertex.color = "gold",
    vertex.size = 25,
    vertex.label.cex = 0.8,
    edge.color = "gray",
    main = "Red de la Casa Baratheon"
)

