rm(list =ls())


library(igraph)
library(igraphdata)
data(package = "igraphdata")$results[, "Item"]
data("UKfaculty")
g <- UKfaculty
df = as_data_frame(UKfaculty)

summary(g) # + attr: Type (g/c), Date (g/c), Citation (g/c), Author (g/c), Group (v/n), weight (e/n)


df # from to weight

kable(head(df),caption = "Uk Faculty")

# Table: Uk Faculty
#
# | from| to| weight|
#     |----:|--:|------:|
#     |   57| 52|      4|
#     |   76| 42|     14|
#     |   12| 69|      4|
#     |   43| 34|      4|
#     |   28| 47|     10|
#     |   58| 51|      2|

vcount(g) # vértices: 81
ecount(g) # edges: 817
V(g)[1:10]$Group # muestra los primeros 10 nodos (muestra únicamente el nombre)
E(g)[1:10] # muestra las primeros 10 conexiones
vertex_attr_names(g)
edge_attr_names(g)

edge_attr(g,"Reciptype",index = c(1:5))  # NULL

V(g)[[1:5]]
#
# + 5/81 vertices, from 6f42903:
#     Group
# 1     3
# 2     1
# 3     3
# 4     3
# 5     2

E(g)[[1:5]]
# + 5/817 edges from 6f42903:
#     tail head tid hid weight
# 1   57   52  57  52      4
# 2   76   42  76  42     14
# 3   12   69  12  69      4
# 4   43   34  43  34      4
# 5   28   47  28  47     10


# Cómo se  conecta

# Grado promedio y la mediana; Son parecidos o muy distintos? Qué dice eso sobre la red?

grados <- degree(g)
media <- mean(grados) # 20.17284
mediana <- median(grados) # 18
media/mediana
# son muy parecidos, la media es solamente 1.120713 veces la mediana lo cual indica una distribución no simétrica pero sí a 2 colas


sort(grados, decreasing = TRUE)
# 62 54 44 43 39 38 38 37 37 36 35 35 35 33 30 29 29 28 27 27 26 26 26 25 24 24 24 22 22 22 21 21 20 20
# 20 20 19 19 19 19 18 18 17 17 16 15 15 14 14 14 13 13 13 13 13 13 12 12 12 12 12 11 11 10 10 10 10 10
# 10 10  9  9  8  8  8  6  6  6  5  4  2

#     Histograma de grados. Tiene cola larga?

hist(degree(g), breaks = 50)  #  La mayoría de proteínas tiene 1-5 conexiones,
# tiene cola pero no es larga


#     Cuáles son  los 5 nodos más conectados (hubs?)  Tiene sentido que sean hubs?

is_directed(g)   # TRUE
is_weighted(g)   # TRUE
is_connected(g)   # TRUE
# es conexa, está integrada, concentrada?


comp <- components(g)
comp$no # 1
comp$csize # 81
# hay un solo componente de 81 nodos, es una sola clase comunicante

max(grados) # 62



### Por qué importa esto para el análisis?
### Algunas métricas no se pueden calcular sobre una red desconectada o dan resultados raros.
### Por ejemplo diameter.
diameter(g)        # da Inf o un valor extraño
mean_distance(g)   # solo considera pares conectados
### La solución estándar: trabajar con la componente gigante!!!!!!




#cuando más que una conexión entre 2 nodos, al hacer comunidades entonces podemos simplificar colapsandolas en una sola conexión
