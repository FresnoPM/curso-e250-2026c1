# PPT Clase 16
rm(list =ls())

library(igraph)
library(igraphdata)
data("karate")
summary(karate)
library(knitr)
karate
#convert the graph into data frame
df = as_data_frame(karate)
#displaying the first 6 records in table.
kable(head(df),caption = "Zachary Karate Club Data")

vcount(karate) # vértices: 34
ecount(karate) # edges: 78
V(karate)[1:10]$name # muestra los primeros 10 nodos (muestra únicamente el nombre)
E(karate)[1:10] # muestra las primeros 10 conexiones
vertex_attr_names(karate)
edge_attr_names(karate)
edge_attr(karate,"Reciptype",index = c(1:5))
V(karate)[[1:5]]
E(karate)[[1:5]]


c1 = grepl(1, V(karate)$community)
c2 = grepl(4, V(karate)$community)
#Edges between both communities
E(karate)[V(karate)[c1]%--%V(karate)[c2]]
neighbors(karate, "Mr Hi") #Interaction of club members with Mr Hi
neighbors(karate, "John A") #Interaction of club members with John A
plot(karate)

library(visNetwork)
fc = cluster_spinglass(karate)
V(karate)$community = fc$membership

nodes <- data.frame(id = V(karate)$name, title = V(karate)$name, group = V(karate)$community)
nodes <- nodes[order(nodes$id, decreasing = F),]
edges <- get.data.frame(karate, what="edges")[1:2]

#visNetwork(nodes, edges) %>%
#   visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
#   visLegend()
vis.nodes <- nodes
vis.links <- edges
vis.nodes$shape  <- "dot"
vis.nodes$shadow <- TRUE # Nodes will drop shadow
vis.nodes$title  <- V(karate)$name # Text on click
vis.nodes$label  <- V(karate)$name # Node label
vis.nodes$size   <- degree(karate)+25 # Node size
vis.nodes$borderWidth <- 2 # Node border width

vis.links$width <- E(karate)$weight # line width
vis.links$color <- "gray"    # line color
vis.links$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
vis.links$smooth <- FALSE    # should the edges be curved?
vis.links$shadow <- FALSE    # edge shadow


vis.nodes$color.background <- c("slategrey", "tomato", "gold")[V(karate)$community]
vis.nodes$color.border <- "black"
vis.nodes$color.highlight.background <- "orange"
vis.nodes$color.highlight.border <- "darkred"

visNetwork(vis.nodes, vis.links)



# ---------- Find important club members ---------- #
#    ----------  ----------  ----------   #


#Compute betweenness centrality
BetC = betweenness(karate,directed = TRUE)
#Compute Closeness centrality
CloC = closeness(karate,mode = "all")
#Compute Degree centrality using both In & Out Edges
DegC = degree(karate,mode = "all")
#Add attribute to the nodes
V(karate)$BetC = BetC
V(karate)$CloC = CloC
V(karate)$DegC = DegC


# ---------- DEGREE CENTRALITY ---------- #
#    ----------  ----------  ----------   #

#grouping the top nodes and other than top nodes
important = as.vector(ifelse(degree(karate) >= 9, "Top" , "Simple"))

#making nodes and edges data frames
nodes <- data.frame(id = V(karate)$name, title = V(karate)$name, group = important)
nodes <- nodes[order(nodes$id, decreasing = F),]
edges <- as_data_frame(karate, what="edges")[1:2]

vis.nodes <- nodes
vis.links <- edges



# necesito que sea más informativo entones uso métricas de centralidad para ponderar la relevancia de cada actor en la red
# señalizamos esa relevancia a través de diferentes cualidades de los nodos


#giving some styles to nodes and edges
vis.nodes$shape  <- as.vector(ifelse(important=="Top", "square" , "dot"))
vis.nodes$shadow <- TRUE # Nodes will drop shadow
vis.nodes$title  <- vis.nodes$id # Text on click
vis.nodes$label  <- vis.nodes$id # Node label
vis.nodes$size   <- degree(karate)+10 # Node size
vis.nodes$borderWidth <- 2 # Node border width

vis.links$width <- E(karate)$weight # line width
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

plot(vis.nodes)

sort(degree(karate),decreasing = TRUE)[1:5]

# Camino (copiamos desde el PPT)
# Una secuencia de nodos de modo que cada nodo esté conectado al siguiente nodo a lo largo del camino mediante un enlace. (Ej:  1 → 2 → 5 → 4 → 3; d=4)
# Camino corto
# El camino con la distancia más corta entre dos nodos (puede ser no único).
# Diámetro
# El camino más corto máximo, o la distancia entre los dos nodos más lejanos
# Ej: el diámetro entre nodos 1 y 4,  dmax=3.
# Camino corto promedio 〈d〉
# El promedio de caminos cortos entre todos pares de nodos. (Ej:  〈d〉=1.6)






# ------------------ Betweenness Centrality --------------- #
# ----------- 2. CENTRALIDAD DE INTERMEDIACIÓN ------------ #
# ----------  ----------  ---------- --------- -----------  #

# cuento cuántos caminos posibles conectan 2 nodos, directos o indirectos
# ESTO ES MUY IMPORTANTE:
# el "grado de centralidad de intermediación" de un nodo se determina por la cantidad de CAMINOS CORTOS que pasan por él como puente
# Puede pasar que un nodo tenga un alto grado de centralidad de intermediación pero baja conectividad
#
# Now, the showing the top 5 persons through betweenness centrlity.

# grouping the top nodes and other than top nodes
important = as.vector(ifelse(betweenness(karate,directed = FALSE) >= 38.13333, "Top" , "Simple"))

# making nodes and edges data frames
nodes <- data.frame(id = V(karate)$name, title = V(karate)$name, group = important)
nodes <- nodes[order(nodes$id, decreasing = F),]
edges <- get.data.frame(karate, what="edges")[1:2]

vis.nodes <- nodes
vis.links <- edges

# giving some styles to nodes and edges
vis.nodes$shape  <- as.vector(ifelse(important=="Top", "square" , "dot"))
vis.nodes$shadow <- TRUE # Nodes will drop shadow
vis.nodes$title  <- vis.nodes$id # Text on click
vis.nodes$label  <- vis.nodes$id # Node label
vis.nodes$size   <- betweenness(karate,directed = FALSE)*0.2 # Node size
vis.nodes$borderWidth <- 2 # Node border width

vis.links$width <- E(karate)$weight # line width
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
sort(betweenness(karate,directed = FALSE),decreasing = TRUE)[1:5]



# ------------------ Closeness Centrality --------------- #
# -------------- 3. CENTRALIDAD DE CENCANÍA ------------- #
# ----------  ----------  ---------- --------- ---------- #

# El nodo con mayor centralidad de cercanía es el que tiene menor cantidad de pasos hasta llegar a los otros nodos (caminos más cortos)
#


#grouping the top nodes and other than top nodes
important = as.vector(ifelse(closeness(karate,mode = "all") >= 0.006134969, "Top" , "Simple"))

#making nodes and edges data frames
nodes <- data.frame(id = V(karate)$name, title = V(karate)$name, group = important)
nodes <- nodes[order(nodes$id, decreasing = F),]
edges <- get.data.frame(karate, what="edges")[1:2]

vis.nodes <- nodes
vis.links <- edges

#giving some styles to nodes and edges
vis.nodes$shape  <- as.vector(ifelse(important=="Top", "diamond" , "dot"))
vis.nodes$shadow <- TRUE # Nodes will drop shadow
vis.nodes$title  <- vis.nodes$id # Text on click
vis.nodes$label  <- vis.nodes$id # Node label
vis.nodes$size   <- closeness(karate,mode = "all")*4000 # Node size
vis.nodes$borderWidth <- 2 # Node border width

vis.links$width <- 2 # line width
vis.links$color <- "gray"    # line color
#vis.links$arrows <- "from" # arrows: 'from', 'to', or 'middle'
vis.links$smooth <- FALSE    # should the edges be curved?
vis.links$shadow <- FALSE    # edge shadow


vis.nodes$color.background <- as.vector(ifelse(important=="Top", "slategrey" , "tomato"))
vis.nodes$color.border <- "black"
vis.nodes$color.highlight.background <- "orange"
vis.nodes$color.highlight.border <- "darkred"

visnet3 = visNetwork(vis.nodes, vis.links)
visnet3 <- visGroups(visnet3, groupname = "Top", shape = "diamond",
                     color = list(background = "gray", border="black"))
visnet3 <- visGroups(visnet3, groupname = "Simple", shape = "dot",
                     color = list(background = "tomato", border="black"))
visLegend(visnet3, main="Legend", position="right", ncol=1)


