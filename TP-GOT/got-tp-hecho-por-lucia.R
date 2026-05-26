# https://shiring.github.io/networks/2017/05/15/got_final
rm(list=ls())

install.packages("statnet")
library(tidyverse)
library(igraph)
library(statnet)

load("curso-e250-2026c1/Clases/union_characters.RData")
summary(union_characters)
# son 208 nodos, peronajes
# cols: name, male, culture, house, popularity, house2, color, shape
# Vendrían a ser los nodos
# Todos los personajes tienen nombre, male, house y popularity pero culture, house2 y color puede que sean NA
# Ejemplo:
#     name        male     culture   house        popularity  house2  color   shape
# 1   Alys Arryn  0        <NA>      House Arryn  0.08026756  <NA>    <NA>    circle
# el atributo de estilo "shape" representa el género male=1 --> shape="square" y male=0 --> shape="circle"
# el atributo de estilo "color" representa la casa, existen casas que no tienen color, eso significa que no son importantes


load("curso-e250-2026c1/Clases/union_edges.RData")
summary(union_edges)
# cols: source, target, type, color, lty
# Serían las aristas o vínculos entre los personajes, hay un total de 404 relaciones entre esos 208 personajes

# estas son relaciones dirigidas entre source y target definida por alguno de sus 4 tipos:
#   "mother" "father"  "father/mother" "spouse"
# luego 2 datos más de estilo para graficar la relación (color y tipo de línea)
# El estilo describe el tipo de relación

# Ejemplo:
#       source      target          type    color    lty
# 1     Lysa Arryn  Robert Arryn    mother  #7570B3  solid

union_graph <- graph_from_data_frame(union_edges, directed = TRUE, vertices = union_characters)
head(union_graph)
# IGRAPH 216d11b DN-- 208 404 --
#     + attr: name (v/c), male (v/n), culture (v/x), house (v/c), popularity (v/n), house2 (v/c),
# | color (v/c), shape (v/c), type (e/c), color (e/c), lty (e/c)
# son los 404 vínculos dirigidos

#####################################
#####################################
### GRÁFICO DE RELACIONES ENTRE MIEMBROS AGRUPADOS POR CASAS
#####################################
#####################################
# For plotting the legend, I am summarizing the edge and node colors.

color_vertices <- union_characters %>%
    group_by(house, color) %>% # agrupa personajes en grupos según sus casas y colores
    summarise(n = n()) %>% # arma un nuevo df con una sola línea por casa (59 casas) y su color, agregando una columna aparte "n" que indica la cantidad de elementos de la lista union_characters pertenecen a ese grupo
    filter(!is.na(color)) # elimina aquellas casas que no tienen color, quedando un total de 9 casas (de 59 originales) que tienen color asignado

colors_edges <- union_edges %>%
    group_by(type, color) %>% # agrupa relaciones entre personajes según su tipo de vínculo (ej:padre, pareja)
    summarise(n = n()) # %>%
    #filter(!is.na(color)) #quita las relaciones que no tienen color

# Now, we can plot the graph object (here with Fruchterman-Reingold layout):

    layout <- layout_with_fr(union_graph)

    plot(union_graph, # red dirigida
         layout = layout,
         # CARACTERÍSTICAS DE LOS NODOS DEL GRÁFICO (PERSONAJES)
         vertex.label = gsub(" ", "\n", V(union_graph)$name), #cada nodo representa un nombre del dataset "union_graph" (no dirigido, están todos los vínculos sin colapsar)
         vertex.shape = V(union_graph)$shape,
         vertex.color = V(union_graph)$color,
         vertex.size = (V(union_graph)$popularity + 0.5) * 5,
         vertex.frame.color = "gray",
         vertex.label.color = "black",
         vertex.label.cex = 0.8,
         # CARACTERÍSTICAS DE LOS VÍNCULOS DEL GRÁFICO (RELACIONES)
         edge.arrow.size = 0.5,
         edge.color = E(union_graph)$color,
         edge.lty = E(union_graph)$lty)

legend("topleft", legend = c(NA, "Node color:", as.character(color_vertices$house), NA, "Edge color:", as.character(colors_edges$type)), pch = 19,
       col = c(NA, NA, color_vertices$color, NA, NA, colors_edges$color), pt.cex = 5, cex = 2, bty = "n", ncol = 1, title = "")
legend("topleft", legend = "", cex = 4, bty = "n", ncol = 1,
       title = "Game of Thrones Family Ties")

#####################################
#####################################
#####################################



# How do we find out who the most important characters are in this network?
#
#     We consider a character “important” if he has connections to many other characters. There are a few network properties, that tell us more about this. For this, I am considering the network as undirected to account for parent/child relationships as being mutual.


union_graph_undir <- as_undirected(union_graph, mode = "collapse")
summary(union_graph_undir)
# IGRAPH def2487 UN-- 208 326 --
#     + attr: name (v/c), male (v/n), culture (v/x), house (v/c), popularity (v/n), house2 (v/c),
# | color (v/c), shape (v/c)
# al colapsar los vínculos recíprocos como no dirigidos quedan 326 de los iniciales 404 dirigidos


# CENTRALITY
# https://en.wikipedia.org/wiki/Centrality
# For the whole network, we can calculate centrality by degree (centr_degree()), closeness (centr_clo()) or eigenvector centrality (centr_eigen()) of vertices.

centr_degree(union_graph_undir, mode = "total")$centralization
## [1] 0.04282795
## the number of links incident upon a node (i.e., the number of ties that a node has). The degree can be interpreted in terms of the immediate risk of a node for catching whatever is flowing through the network (such as a virus, or some information). In the case of a directed network (where ties have direction), we usually define two separate measures of degree centrality, namely indegree and outdegree. Accordingly, indegree is a count of the number of ties directed to the node and outdegree is the number of ties that the node directs to others. When ties are associated to some positive aspects such as friendship or collaboration, indegree is often interpreted as a form of popularity, and outdegree as gregariousness.

centr_clo(union_graph_undir, mode = "total")$centralization
## [1] 1.138423
##  is the average length of the shortest path between the node and all other nodes in the graph. Thus the more central a node is, the closer it is to all other nodes.

centr_eigen(union_graph_undir, directed = FALSE)$centralization
## [1] 0.8787532
## Eigenvector centrality (also called eigencentrality) is a measure of the influence of a node in a network. It assigns relative scores to all nodes in the network based on the concept that connections to high-scoring nodes contribute more to the score of the node in question than equal connections to low-scoring nodes.[29][7] Google's PageRank and the Katz centrality are variants of the eigenvector centrality.




# NODE DEGREE

# Node degree or degree centrality describes how central a node is in the network (i.e. how many in- AND outgoing edges it has or to how many other nodes it is directly connected via one edge).
# “The degree of a vertex is its most basic structural property, the number of its adjacent edges.” From the help pages of degree()
#
# We can calculate the number of out- OR ingoing edges of each node, or - as I am doing here - THE SUM OF BOTH.
#
# In this case, the node degree reflects how many offspring and spouses a character had. With 3 wifes and several children, Quellon Greyjoy, the grandfather of Theon and Asha/Yara comes out on top (of course, had I included all offspring and wifes of Walder Frey’s, he would easily be on top but the network would have gotten infinitely more confusing).

union_graph_undir_degree <- igraph::degree(union_graph_undir, mode = "all") # mode puede ser all, out, in, total. "all" y "total" son sinónimos. Este argumento es ignorado en gráficos no dirigidos.

#standardized by number of nodes
union_graph_undir_degree_std <- union_graph_undir_degree / (vcount(union_graph_undir) - 1)

node_degree <- data.frame(degree = union_graph_undir_degree,
                          degree_std = union_graph_undir_degree_std) %>%
    tibble::rownames_to_column() # ordena estos datos recién calculados en un df

union_characters <- left_join(union_characters, node_degree, by = c("name" = "rowname")) #junta en una sola columna llamada "rowname" el nombre y apellido de cada personaje

node_degree %>%
    arrange(-degree) %>% # ordena por grado, de forma descendente notar el signo "-" al principio de "-degree"
    .[1:10, ] #selecciona los 10 personajes con mayor grado de centralidad nominal (no estandarizada)




#
#
#
# Closeness de nodos
#
# The closeness of a node describes its distance to all other nodes. A node with highest closeness is more central and can spread information to many other nodes.
# The characters with highest closeness all surround central characters that connect various storylines and houses in Game of Thrones.

closeness <- igraph::closeness(union_graph_undir, mode = "total")

#standardized by number of nodes
closeness_std <- closeness / (vcount(union_graph_undir) - 1)

node_closeness <- data.frame(closeness = closeness,
                             closeness_std = closeness_std) %>%
    tibble::rownames_to_column()

union_characters <- left_join(union_characters, node_closeness, by = c("name" = "rowname"))

node_closeness %>%
    arrange(-closeness) %>%
    .[1:10, ]



#
# Betweenness centrality  de edges
#
# Betweenness describes the number of shortest paths between nodes. Nodes with high betweenness centrality are on the path between many other nodes, i.e. they are people who are key connections or bridges between different groups of nodes. In a social network, these nodes would be very important because they are likely to pass on information to a wide reach of people.
betweenness <- igraph::betweenness(union_graph_undir, directed = FALSE)

# standardize by number of node pairs
betweenness_std <- betweenness / ((vcount(union_graph_undir) - 1) * (vcount(union_graph_undir) - 2) / 2)

node_betweenness <- data.frame(betweenness = betweenness,
                               betweenness_std = betweenness_std) %>%
    tibble::rownames_to_column()

union_characters <- left_join(union_characters, node_betweenness, by = c("name" = "rowname"))

node_betweenness %>%
    arrange(-betweenness) %>%
    .[1:10, ]


edge_betweenness <- igraph::edge_betweenness(union_graph_undir, directed = FALSE) # creo un objeto igraph

data.frame(edge = attr(E(union_graph_undir), "vnames"), # creo un df con 2 cols: edge (vnames es el nombre del vértice de los edges) y betweenness de cada uno de esos edges ???
           betweenness = edge_betweenness) %>% # cuán "en el medio" están del resto, son los nucleares
    tibble::rownames_to_column() %>% #crea una columna que se llena con los ids de cada vínculo
    arrange(-betweenness) %>% # se ordena por betweenness
    .[1:10, ]   # se seleccionan los 10 con mayor betweenness

plot(union_graph_undir,
     layout = layout,
     vertex.label = gsub(" ", "\n", V(union_graph_undir)$name),
     vertex.shape = V(union_graph_undir)$shape,
     vertex.color = V(union_graph_undir)$color,
     vertex.size = edge_betweenness * 0.001 ,
     vertex.frame.color = "gray",
     vertex.label.color = "black",
     vertex.label.cex = 0.5,
     edge.width = edge_betweenness * 0.01,
     edge.arrow.size = 0.5,
     edge.color = E(union_graph_undir)$color,
     edge.lty = E(union_graph_undir)$lty)
legend("topleft", legend = c("Node color:", as.character(color_vertices$house), NA, "Edge color:", as.character(colors_edges$type)), pch = 19,
       col = c(NA, color_vertices$color, NA, NA, colors_edges$color), pt.cex = 5, cex = 2, bty = "n", ncol = 1)


# Ned Stark is the character with highest betweenness. This makes sense, as he and his children (specifically Sansa and her arranged marriage to Tyrion) connect to other houses and are the central points from which the story unfolds. However, we have to keep in mind here, that my choice of who is important enough to include in the network (e.g. the Stark ancestors) and who not (e.g. the whole complicated mess that is the Targaryen and Frey family tree) makes this result somewhat biased.







# Diameter de la red
#
# In contrast to the shortest path between two nodes, we can also calculate the longest path, or diameter:

    diameter(union_graph_undir, directed = FALSE)

## [1] 21

# In our network, the longest path connects 21 nodes.
#
# “get_diameter returns a path with the actual diameter. If there are many shortest paths of the length of the diameter, then it returns the first one found.” diameter() help
#
# This, we can also plot:

    union_graph_undir_diameter <- union_graph_undir
node_diameter <- get_diameter(union_graph_undir_diameter,  directed = FALSE)

V(union_graph_undir_diameter)$color <- scales::alpha(V(union_graph_undir_diameter)$color, alpha = 0.5)
V(union_graph_undir_diameter)$size <- 2

V(union_graph_undir_diameter)[node_diameter]$color <- "red"
V(union_graph_undir_diameter)[node_diameter]$size <- 5

E(union_graph_undir_diameter)$color <- "grey"
E(union_graph_undir_diameter)$width <- 1

E(union_graph_undir_diameter, path = node_diameter)$color <- "red"
E(union_graph_undir_diameter, path = node_diameter)$width <- 5

plot(union_graph_undir_diameter,
     layout = layout,
     vertex.label = gsub(" ", "\n", V(union_graph_undir_diameter)$name),
     vertex.shape = V(union_graph_undir_diameter)$shape,
     vertex.frame.color = "gray",
     vertex.label.color = "black",
     vertex.label.cex = 0.8,
     edge.arrow.size = 0.5,
     edge.lty = E(union_graph_undir_diameter)$lty)
legend("topleft", legend = c("Node color:", as.character(color_vertices$house), NA, "Edge color:", as.character(colors_edges$type)), pch = 19,
       col = c(NA, color_vertices$color, NA, NA, colors_edges$color), pt.cex = 5, cex = 2, bty = "n", ncol = 1)


# Transitivity
#
# “Transitivity measures the probability that the adjacent vertices of a vertex are connected. This is sometimes also called the clustering coefficient.” transitivity() help
#
# We can calculate the transitivity or ratio of triangles to connected triples for the whole network:

    transitivity(union_graph_undir, type = "global")

## [1] 0.2850679

# Or for each node:

    transitivity <- data.frame(name = V(union_graph_undir)$name,
                               transitivity = transitivity(union_graph_undir, type = "local")) %>% # la transitividad es 1 o 0 para cara personaje
    mutate(name = as.character(name))

union_characters <- left_join(union_characters, transitivity, by = "name")

transitivity %>%
    arrange(-transitivity) %>%
    .[1:10, ]

##                 name transitivity
## 1       Robert Arryn            1
## 2   Ormund Baratheon            1
## 3     Selyse Florent            1
## 4  Shireen Baratheon            1
## 5   Amarei Crakehall            1
## 6       Marissa Frey            1
## 7        Olyvar Frey            1
## 8        Perra Royce            1
## 9        Perwyn Frey            1
## 10         Tion Frey            1

# Because ours is a family network, characters with a transitivity of one form triangles with their parents or offspring.
#
# PageRank (originally used by Google to rank the importance of search results) is similar to eigenvector centrality. Eigenvector centrality scores nodes in a network according to the number of connections to high-degree nodes they have. It is therefore a measure of node importance. PageRank similarly considers nodes as more important if they have many incoming edges (or links).

page_rank <- page_rank(union_graph_undir, directed = FALSE)

page_rank_centrality <- data.frame(name = names(page_rank$vector),
                                   page_rank = page_rank$vector) %>%
    mutate(name = as.character(name))

union_characters <- left_join(union_characters, page_rank_centrality, by = "name")

page_rank_centrality %>%
    arrange(-page_rank) %>%
    .[1:10, ]

##                 name   page_rank
## 1     Oberyn Martell 0.018402407
## 2    Quellon Greyjoy 0.016128129
## 3        Walder Frey 0.012956029
## 4       Eddard Stark 0.011725019
## 5       Cregan Stark 0.010983561
## 6      Catelyn Stark 0.010555473
## 7       Lyarra Stark 0.009876629
## 8  Aegon V Targaryen 0.009688458
## 9      Balon Greyjoy 0.009647049
## 10         Jon Arryn 0.009623742

# Oberyn Martell, Quellon Greyjoy and Walder Frey all have the highest number of spouses, children and grandchildren are are therefore scored highest for PageRank.
# Matrix representation of a network
#
# Connections between nodes can also be represented as an adjacency matrix. We can convert our graph object to an adjacency matrix with igraph’s as_adjacency_matrix() function. Whenever there is an edge between two nodes, this field in the matrix will get assigned a 1, otherwise it is 0.

adjacency <- as.matrix(as_adjacency_matrix(union_graph_undir))

# Eigenvector centrality

# We can now calculate the eigenvalues and eigenvectors of the adjacency matrix.

#degree diagonal matrix
degree_diag <- diag(1 / igraph::degree(union_graph_undir))

# PageRank matrix
pagerank <- adjacency %*% degree_diag

eigenvalues <- eigen(pagerank)

# The eigenvector with the highest eigenvalue scores those vertices highly, that have many eges or that are connected to vertices with many edges.

eigenvector <- data.frame(name = rownames(pagerank),
                          eigenvector = as.numeric(eigenvalues$vectors[, which.max(eigenvalues$values)]))

union_characters <- left_join(union_characters, eigenvector, by = "name")

eigenvector %>%
    arrange(eigenvector) %>%
    .[1:10, ]

##                       name eigenvector
## 1          Quellon Greyjoy  -0.6625628
## 2            Balon Greyjoy  -0.3864950
## 3   Lady of House Sunderly  -0.3312814
## 4           Alannys Harlaw  -0.2760678
## 5  Lady of House Stonetree  -0.2208543
## 6      Asha (Yara) Greyjoy  -0.1656407
## 7            Robin Greyjoy  -0.1104271
## 8            Euron Greyjoy  -0.1104271
## 9          Urrigon Greyjoy  -0.1104271
## 10       Victarion Greyjoy  -0.1104271

# Because of their highly connected family ties (i.e. there are only a handful of connections but they are almost all triangles), the Greyjoys have been scored with the highest eigenvalues.
#
# We can find the eigenvector centrality scores with:

    eigen_centrality <- igraph::eigen_centrality(union_graph_undir, directed = FALSE)

eigen_centrality <- data.frame(name = names(eigen_centrality$vector),
                               eigen_centrality = eigen_centrality$vector) %>%
    mutate(name = as.character(name))

union_characters <- left_join(union_characters, eigen_centrality, eigenvector, by = "name")

eigen_centrality %>%
    arrange(-eigen_centrality) %>%
    .[1:10, ]

##                name eigen_centrality
## 1   Tywin Lannister        1.0000000
## 2  Cersei Lannister        0.9168980
## 3  Joanna Lannister        0.8358122
## 4    Jeyne Marbrand        0.8190076
## 5   Tytos Lannister        0.8190076
## 6   Genna Lannister        0.7788376
## 7   Jaime Lannister        0.7642870
## 8  Robert Baratheon        0.7087042
## 9        Emmon Frey        0.6538709
## 10      Walder Frey        0.6516021

# When we consider eigenvector centrality, Tywin and the core Lannister family score highest.
# Who are the most important characters?
#
#     We can now compare all the node-level information to decide which characters are the most important in Game of Thrones. Such node level characteristics could also be used as input for machine learning algorithms.

# Let’s look at all characters from the major houses:

union_characters %>%
    filter(!is.na(house2)) %>%
    dplyr::select(-contains("_std")) %>%
    gather(x, y, degree.x:eigen_centrality) %>%
    ggplot(aes(x = name, y = y, color = house2)) +
    geom_point(size = 3) +
    facet_grid(x ~ house2, scales = "free") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


# Taken together, we could say that House Stark (specifically Ned and Sansa) and House Lannister (especially Tyrion) are the most important family connections in Game of Thrones.
# Groups of nodes
#
# We can also analyze dyads (pairs of two nodes), triads (groups of three nodes) and bigger cliques in our network. For dyads, we can use the function dyad_census() from igraph or dyad.census() from sna. Both are identical and calculate a Holland and Leinhardt dyad census with
#
# mut: The number of pairs with mutual connections (in our case, spouses).
# asym: The number of pairs with non-mutual connections (in the original network: mother-child and father-child relationships; but in the undirected network, there are none).
# null: The number of pairs with no connection between them.

#igraph::dyad_census(union_graph_undir)
sna::dyad.census(adjacency)

##      Mut Asym  Null
## [1,] 326    0 21202
#
# The same can be calculated for triads (see ?triad_census for details on what each output means).

#igraph::triad_census(union_graph_undir)
sna::triad.census(adjacency)

##          003 012   102 021D 021U 021C 111D 111U 030T 030C 201 120D 120U 120C 210 300
## [1,] 1412100   0 65261    0    0    0    0    0    0    0 790    0    0    0   0 105

triad.classify(adjacency, mode = "graph")

## [1] 2

# We can also calculate the number of paths and cycles of any length we specify, here e.g. of length <= 5. For edges, we obtain the sum of counts for all paths or cycles up to the given maximum length. For vertices/nodes, we obtain the number of paths or cycles to which each node belongs.

node_kpath <- kpath.census(adjacency, maxlen = 5, mode = "graph", tabulate.by.vertex = TRUE, dyadic.tabulation = "sum")
edge_kpath <- kpath.census(adjacency, maxlen = 5, mode = "graph", tabulate.by.vertex = FALSE)
edge_kpath

## $path.count
##     1     2     3     4     5
##   326  1105  2973  7183 17026

# This, we could plot with (but here, it does not give much additional information):

    gplot(node_kpath$paths.bydyad,
          label.cex = 0.5,
          vertex.cex = 0.75,
          displaylabels = TRUE,
          edge.col = "grey")

node_kcycle <- kcycle.census(adjacency, maxlen = 8, mode = "graph", tabulate.by.vertex = TRUE, cycle.comembership = "sum")
edge_kcycle <- kcycle.census(adjacency, maxlen = 8, mode = "graph", tabulate.by.vertex = FALSE)
edge_kcycle

## $cycle.count
##   2   3   4   5   6   7   8
##   0 105 136  27  57  58  86

node_kcycle_reduced <- node_kcycle$cycle.comemb
node_kcycle_reduced <- node_kcycle_reduced[which(rowSums(node_kcycle_reduced) > 0), which(colSums(node_kcycle_reduced) > 0)]

gplot(node_kcycle_reduced,
      label.cex = 0.5,
      vertex.cex = 0.75,
      displaylabels = TRUE,
      edge.col = "grey")



# “A (maximal) clique is a maximal set of mutually adjacency vertices.” clique.census() help

node_clique <- clique.census(adjacency, mode = "graph", tabulate.by.vertex = TRUE, clique.comembership = "sum")
edge_clique <- clique.census(adjacency, mode = "graph", tabulate.by.vertex = FALSE, clique.comembership = "sum")
edge_clique$clique.count

##   1   2   3
##   0  74 105

node_clique_reduced <- node_clique$clique.comemb
node_clique_reduced <- node_clique_reduced[which(rowSums(node_clique_reduced) > 0), which(colSums(node_clique_reduced) > 0)]

gplot(node_clique_reduced,
      label.cex = 0.5,
      vertex.cex = 0.75,
      displaylabels = TRUE,
      edge.col = "grey")



# The largest group of nodes ín this network is three, i.e. all parent/child relationships. Therefore, it does not really make sense to plot them all, but we could plot and color them with:

    vcol <- rep("grey80", vcount(union_graph_undir))

# highlight first of largest cliques
vcol[unlist(largest_cliques(union_graph_undir)[[1]])] <- "red"

plot(union_graph_undir,
     layout = layout,
     vertex.label = gsub(" ", "\n", V(union_graph_undir)$name),
     vertex.shape = V(union_graph_undir)$shape,
     vertex.color = vcol,
     vertex.size = 5,
     vertex.frame.color = "gray",
     vertex.label.color = "black",
     vertex.label.cex = 0.8,
     edge.width = 2,
     edge.arrow.size = 0.5,
     edge.color = E(union_graph_undir)$color,
     edge.lty = E(union_graph_undir)$lty)

# Clustering
#
# We can also look for groups within our network by clustering node groups according to their edge betweenness:

    ceb <- cluster_edge_betweenness(union_graph_undir)
modularity(ceb)

## [1] 0.8359884

plot(ceb,
     union_graph_undir,
     layout = layout,
     vertex.label = gsub(" ", "\n", V(union_graph_undir)$name),
     vertex.shape = V(union_graph_undir)$shape,
     vertex.size = (V(union_graph_undir)$popularity + 0.5) * 5,
     vertex.frame.color = "gray",
     vertex.label.color = "black",
     vertex.label.cex = 0.8)



# Or based on propagating labels:

    clp <- cluster_label_prop(union_graph_undir)

plot(clp,
     union_graph_undir,
     layout = layout,
     vertex.label = gsub(" ", "\n", V(union_graph_undir)$name),
     vertex.shape = V(union_graph_undir)$shape,
     vertex.size = (V(union_graph_undir)$popularity + 0.5) * 5,
     vertex.frame.color = "gray",
     vertex.label.color = "black",
     vertex.label.cex = 0.8)

#
# Network properties
#
# We can also feed our adjacency matrix to other functions, like GenInd() from the NetIndices packages. This function calculates a number of network properties, like number of compartments (N), total system throughput (T..), total system throughflow (TST), number of internal links (Lint), total number of links (Ltot), like density (LD), connectance (C), average link weight (Tijbar), average compartment throughflow (TSTbar) and compartmentalization or degree of connectedness of subsystems in the network (Cbar).

library(NetIndices)
graph.properties <- GenInd(adjacency)
graph.properties

## $N
## [1] 208
##
## $T..
## [1] 652
##
## $TST
## [1] 652
##
## $Lint
## [1] 652
##
## $Ltot
## [1] 652
##
## $LD
## [1] 3.134615
##
## $C
## [1] 0.01514307
##
## $Tijbar
## [1] 1
##
## $TSTbar
## [1] 3.134615
##
## $Cbar
## [1] 0.01086163

# Alternatively, the network package provides additional functions to obtain network properties. Here, we can again feed in the adjacency matrix of our network and convert it to a network object.

library(network)
adj_network <- network(adjacency, directed = TRUE)
adj_network

##  Network attributes:
##   vertices = 208
##   directed = TRUE
##   hyper = FALSE
##   loops = FALSE
##   multiple = FALSE
##   bipartite = FALSE
##   total edges= 652
##     missing edges= 0
##     non-missing edges= 652
##
##  Vertex attribute names:
##     vertex.names
##
## No edge attributes

# From this network object, we can e.g. get the number of dyads and edges within a network and the network size.

network.dyadcount(adj_network)

## [1] 43056

network.edgecount(adj_network)

## [1] 652

network.size(adj_network)

## [1] 208

# “equiv.clust uses a definition of approximate equivalence (equiv.fun) to form a hierarchical clustering of network positions. Where dat consists of multiple relations, all specified relations are considered jointly in forming the equivalence clustering.” equiv.clust() help

ec <- equiv.clust(adj_network, mode = "graph", cluster.method = "average", plabels = network.vertex.names(adj_network))
ec

## Position Clustering:
##
##  Equivalence function: sedist
##  Equivalence metric: hamming
##  Cluster method: average
##  Graph order: 208

ec$cluster$labels <- ec$plabels
plot(ec)



# From the sna package, we can e.g. use functions that tell us the graph density and the dyadic reciprocity of the vertices or edges

gden(adjacency)

## [1] 0.01514307

grecip(adjacency)

## Mut
##   1

grecip(adjacency, measure = "edgewise")

## Mut
##   1

sessionInfo()

