rm(list =ls())

# -------------------
# EXPLORACIÓN INICIAL
# -------------------

library(igraph)
library(igraphdata)
data(package = "igraphdata")$results[, "Item"]
data("UKfaculty")
g <- UKfaculty
g
E[g]$Group

# Tengo 81 miembros que se conectan entre sí (nodos)
# hay un total de 817 conexiones dirigidas (edges) o "Groups"
# cada una de estas conexiones tiene un peso "weight" que representa la importancia de esa conexión
#
# IGRAPH 6f42903 D-W- 81 817 --
#     + attr:
#     Type (g/c), Date (g/c), Citation (g/c), Author (g/c), # estos son datos sobre el paquete y sus creadores, no tiene nada que ver con los datos contenidos dentro del paquete
#     Group (v/n),
#     weight (e/n)
# + edges from 6f42903: ...
is_directed(g)   # TRUE
is_weighted(g)   # TRUE
is_connected(g)   # TRUE
# es conexa, está integrada, concentrada?
comp <- components(g)
comp$no # 1
comp$csize # 81
# hay un solo componente de 81 nodos, es una sola clase comunicante

df = as_data_frame(UKfaculty)
summary(df)

# from            to              weight
# Min.   : 1.00   Min.   : 1.00   Min.   : 1.000
# 1st Qu.:20.00   1st Qu.:20.00   1st Qu.: 2.000
# Median :37.00   Median :38.00   Median : 2.000
# Mean   :38.24   Mean   :39.41   Mean   : 4.565
# 3rd Qu.:57.00,rd Qu.:58.00,rd Qu.: 6.000
# Max.   :81.00   Max.   :81.00   Max.   :16.000

library(dplyr)
as.data.frame(table(df$weight)) |> arrange(desc(Freq))
#   #    Weight Freq
  # 1   ,   ,07
  # 2     1   ,02
  # 3     4     107
  # 4     6     80
  # 5     8     51
  # 6    12  ,9
  # 7    10  ,7
  # 8    14  ,4
  # 9  ,  ,1
  # 10   16   ,2
  # 11    5     5
  # 12    7   ,

# La mayoría de las conexiones tienen peso 1 o 2 mientras que hay muy poquitas de peso 7 o 5
# ¿qué relevancia tiene esto?

# -------------------
# CONSIGNAS HACKATON
# -------------------



# Cómo se  conecta?
# Grado promedio y la mediana;
# Son parecidos o muy distintos?
# Qué dice eso sobre la red?


grados <- degree(g)
summary(grados)
# Min.  1st Qu. Median  Mean ,rd Qu. Max.
# 2.00  12.00   18.00 ,0.17 ,6.00   62.00
media <- mean(grados)         # grado promedio
### # 20.17284
### Cada grupo interactúa con ~20 otros en promedio. ¿Es mucho o poco?
mediana <- median(grados)       # mediana (más robusta si hay hubs)
# 18
max(grados)         # nodo más conectado (hub principal)
# 62
min(grados)         # nodo menos conectado
# 2
media/mediana
# son muy parecidos, la media es solamente 1.120713 veces la mediana lo cual indica una distribución no simétrica pero sí a 2 colas
mode <- function(x) {unique(x)[which.max(tabulate(match(x, unique(x))))]}
moda <- mode(grados)
# 10
desvio <- sd(grados)
# 11.76625


#   Histograma de grados.

histDen <- function (x, color_default, color_bajo, color_alto, cex = 0.9 , title="Histograma",xlab="Variable", ...) {
  color_desviado <- ifelse(my_hist$breaks<= median(x)-sd(x), color_bajo ,
                           ifelse(my_hist$breaks >= median(x)+sd(x), color_alto,
                                  color_default))
  hist(x, prob=TRUE, breaks=50, col=color_desviado,border=par('bg'), main=title, xlab=xlab ,...) # Histograma
  lines(density(x), col = "purple", lwd = 2) # Densidad
  x2 <- seq(min(x), max(x), length = 80)
  abline(v=median(x), col="purple", lwd=1, lty="dashed")

  legend("topright", c("Histograma", "Densidad", "", "< Mediana - 1 desvío", "Mediana", "> Mediana + 1 desvío"), box.lty = 0, cex = cex
         , col = c(color_default, "purple","white", color_bajo, "purple", color_alto), lty = c(1,1,1,1,2,1), lwd=c(3,2,0,3,1,3))

}


histDen(grados,color_default="lightgray", color_bajo="pink", color_alto="lightblue", cex = 0.8, title="Histograma de Grados", xlab="Grados de conexión entre miembros de la facultad")

#   Tiene cola larga?
#
#   Cuáles son  los 5 nodos más conectados (hubs?)

V(g)[order(betweenness(g), decreasing=TRUE)[1:5]]
# 5/81 vertices, from 6f42903:
# 37 62  5, 29

sort(degree(g), decreasing = TRUE)[1:10]
# 62 54 44 43 39 38 38 37 37 36


V(g)[order(betweenness(g)[37], decreasing=FALSE)[1:5]]
# los menos conectados
# 8 11 14 18 32
betweenness(g)[29]

#   Tiene sentido que sean hubs?
# En grafos dirigidos se pueden diferenciar dos tipos de centralidad de los vertices.
# Los hubs son vertices que tienen un grado de salida alto.
# Las autoridades son vertices que tienen un grado de entrada alto.

E(g)[[1:5]]
# + 5/817 edges from 6f42903:
#   tail head tid hid weight
# 1   57   52  57  52      4
# 2   76   42  76  42     14
# 3   12   69  12  69      4
# 4   43   34  43  34      4
# 5   28   47  28  47     10


# Group the club members from two Groups
c1 = grepl(4, V(g)$Group)
c2 = grepl(1, V(g)$Group)
#Edges between each 2 Groups (in or out, not directed)
E(g)[V(g)[c1]%--%V(g)[c2]]


# 33 en Grupo 1:
# 2,8,11,14,15,18,19,20,21,24,25,26,29,31,32,34,35,37,39,41,43,46,48,51,52,54,55,56,57,58,64,79,80
#
# 26 en Grupo 2:
# 5 ,6 ,7 ,10,12,13,16,22,23,27,28,30,33,40,42,47,49,63,65,66,67,68,69,71,72,76,77
#
# 19 en Grupo 3:
# 1, 3, 4, 9, 17,36,38, 44,45,53,59,60,61,62,73,74,75,78,81
#
# 2 en Grupo 4:
# 50, 70

summary(E[g])

# Grupo 1 con Grupo 1 : 317
# Grupo 1 con Grupo 2 : 65
# Grupo 1 con Grupo 3 : 34
# Grupo 1 con Grupo 4 : 25
# Grupo 1 : 441
#
# Grupo 2 con Grupo 1 : 65
# Grupo 2 con Grupo 2 : 250
# Grupo 2 con Grupo 3 : 19
# Grupo 2 con Grupo 4 : 5
# Grupo 2 : 339
#
# Grupo 3 con Grupo 1 : 34
# Grupo 3 con Grupo 2 : 19
# Grupo 3 con Grupo 3 : 96
# Grupo 3 con Grupo 4 : 4
# Grupo 3 : 153
#
# Grupo 4 con Grupo 1 : 25
# Grupo 4 con Grupo 2 : 5
# Grupo 4 con Grupo 3 : 4
# Grupo 4 con Grupo 4 : 2
# Grupo 4 : 36




# Vecinos de cada uno de los nodos más conectados
# 37 62  5  2 29)
length(neighbors(g, "37") ) # 36
neighbors(g, "62") # 34
neighbors(g,  "5") #  28
neighbors(g,  "2") #  17
neighbors(g, "29") # 41

sort(neighbors(V(g)))

hubs_orden[1:5] <- order(degree(g), decreasing = TRUE)
# c(29, 37, 77, 62, 52)
degree(
  g,
#  v= c(37, 62,  5,  2, 29),
  v = c(29, 37, 77,	62, 52),
  mode = c("all", "out", "in", "total"),
  loops = TRUE,
  normalized = FALSE
)

# [1] 43 26  6 33 20

#Compute betweenness centrality
BetC = betweenness(g,directed = TRUE)
#Compute Closeness centrality
CloC = closeness(g,mode = "all")
#Compute Degree centrality using both In & Out Edges
DegC = degree(g,mode = "all")
#Add attribute to the nodes
V(g)$BetC = BetC
V(g)$CloC = CloC
V(g)$DegC = DegC
#grouping the top nodes and other than top nodes
important = as.vector(ifelse(degree(g) >= 9, "Top" , "Simple"))

#making nodes and edges data frames
nodes <- data.frame(id = V(g)$Group, title = V(g)$Group, group = important)
nodes <- nodes[order(nodes$id, decreasing = F),]
edges <- data.frame(g[1:5], what="edges")

vis.nodes<- nodes
vis.links <- edges

#giving some styles to nodes and edges
vis.nodes$shape  <- as.vector(ifelse(important=="Top", "square" , "dot"))
vis.nodes$shadow <- TRUE # Nodes will drop shadow
vis.nodes$title  <- vis.nodes$id # Text on click
vis.nodes$label  <- vis.nodes$id # Node label
vis.nodes$size   <- degree(g)+10 # Node size
vis.nodes$borderWidth <- 2 # Node border width

vis.links$width <- E(g)$weight # line width
vis.links$color <- "gray"    # line color
vis.links$arrows <- "to" # arrows: 'from', 'to', or 'middle'
vis.links$smooth <- FALSE    # should the edges be curved?
vis.links$shadow <- FALSE    # edge shadow


vis.nodes$color.background <- as.vector(ifelse(important=="Top", "slategrey" , "tomato"))
vis.nodes$color.border <- "black"
vis.nodes$color.highlight.background <- "orange"
vis.nodes$color.highlight.border <- "darkred"

visnet3 = visNetwork(vis.nodes, vis.links)
visnet3 <- visGroups(visnet3, groupname = "Top", shape = "square",
                     color = list(background = "gray", border="black"))
visnet3 <- visGroups(visnet3, groupname = "Simple", shape = "dot",
                     color = list(background = "tomato", border="black"))
visLegend(visnet3, main="Legend", position="right", ncol=1)







library(RColorBrewer)
# select colors
colors = brewer.pal(4, "Dark2")
# assign colors to groups
V(UKfaculty)$color = sapply(V(UKfaculty)$Group, function(x) colors[x])



plot(UKfaculty_undirected, layout = layout_nicely(UKfaculty_undirected, dim = 2),
     vertex.color = V(UKfaculty_undirected)$color, vertex.frame.color = NA,
     vertex.label = V(UKfaculty_undirected)$Group, vertex.shape = 'square',
     vertex.size = 3.5, edge.arrow.size = 0.3, edge.curved = TRUE,
     edge.width = E(UKfaculty_undirected)$weight ^ 0.8,
     edge.color = rgb(0, 0, 0, alpha = 0.1))
title("UK Faculty Friendship Network (Directed)", cex.main = 1)
summary(V(UKfaculty))
UKfaculty_undirected <- as_undirected(
  UKfaculty,
  mode = "mutual", # keeping only mutual ties
  edge.attr.comb = list(weight="sum", "ignore") # sum edge weights, ignore other attributes
)




#   Densidad? Es una red densa o esparsa?
edge_density(g)        # 0.1260802
### La densidad es 0.13%.
### Comparación útil: una red completa con 81 nodos tendría 3280 aristas.
### UKfaculty tiene 817. Eso significa que se aprovecha el 25% de su potencial.


### Por qué importa esto para el análisis?
### Algunas métricas no se pueden calcular sobre una red desconectada o dan resultados raros.
### Por ejemplo diameter.
diameter(g)        # da Inf o un valor extraño
mean_distance(g)   # solo considera pares conectados

# cuando hay más que una conexión entre 2 nodos, al hacer comunidades entonces podemos simplificar colapsandolas en una sola conexión



# Coeficiente de clustering (transitividad)
transitivity(g, type = "global")   # global
mean(transitivity(g, type = "local"), na.rm = TRUE)  # local promedio


