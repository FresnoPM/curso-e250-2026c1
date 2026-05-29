# https://shiring.github.io/networks/2017/05/15/got_final
rm(list=ls())

library(tidyverse)
library(igraph)
library(statnet)
library(dplyr)

load("curso-e250-2026c1/TP-GOT/union_characters.RData") # crea el objeto de tipo list "union_characters"
load("curso-e250-2026c1/TP-GOT/union_edges.RData") # crea el objeto de tipo list "union_edges"


# creo 2 objetos de tipo igraph en base a los 208 nodos de union_characters y las 404 conexiones dirigidas de union_edges: union_graph (v:208, e:404) y union_graph_undir (v:208, e:326)
union_graph <- graph_from_data_frame(union_edges, directed = TRUE, vertices = union_characters)
union_graph_undir <- as_undirected(union_graph, mode = "collapse")



# atributos de los edges:
#
#   type          color
# 1 father	      #1B9E77 VERDE
# 2	father/mother	#D95F02 NARANJA
# 3	mother	      #7570B3 LILA
# 4	spouse	      #E7298A FUCSIA
#
#
# lty: solid-dotted


# creo objetos de tipo list filtrando union_characters y union_edges para que muestre únicamente aquellas filas donde haya al menos 1 Baratheon involucrado.
# El df "baratheon_characters" es un listado de los 10 personajes de la casa Baratheon

baratheon_characters <- union_characters %>%
  filter(house == "House Baratheon")

# El df "baratheon_edges_total" incluye vínculos entre 2 Baratheons y entre un Baratheon y alguien de otra casa. Los vínculos son dirigidos. Este 2do lo utilizaremos en el futuro.

baratheon_edges_total <- rbind(
  union_edges %>% filter(str_detect(source, "Baratheon")) ,
  union_edges %>% filter(str_detect(target, "Baratheon"))
) |> distinct(source, target, .keep_all = TRUE)


# creo un subgrafo en base a union_graph y union_graph_undir con los vínculos entre 2 baratheons: "baratheon_graph_endo_undir" (v:10, e:12) y "baratheon_graph_endo" (v:10, e:13)

baratheon_graph_endo_undir <- igraph::induced_subgraph(
  union_graph_undir,
  vids = igraph::V(union_graph_undir)$name %in% baratheon_characters$name
)

baratheon_graph_endo <- igraph::induced_subgraph(
  union_graph,
  vids = igraph::V(union_graph_undir)$name %in% baratheon_characters$name
)


# How do we find out who the most important characters are in this network?
# CENTRALITY
# https://en.wikipedia.org/wiki/Centrality
# For the whole network, we can calculate centrality by degree (centr_degree()), closeness (centr_clo()) or eigenvector centrality (centr_eigen()) of vertices.
# The degree can be interpreted in terms of the immediate risk of a node for catching whatever is flowing through the network (such as a virus, or some information).

centr_degree(baratheon_graph_endo, mode = "in")$centralization
# 0.07777778
# Indegree is a count of the number of ties directed to the node (popularity)

centr_degree(baratheon_graph_endo, mode = "out")$centralization
# 0.3
# outdegree is the number of ties that the node directs to others (gregariousness)

centr_degree(baratheon_graph_endo, mode = "total")$centralization
# 0.2098765

centr_degree(baratheon_graph_endo_undir, mode = "total")$centralization
# 0.2888889

centr_clo(baratheon_graph_endo_undir, mode = "total")$centralization
# 0.4705829
# It's the average length of the shortest path between the node and all other nodes in the graph. Thus the more central a node is, the closer it is to all other nodes.

centr_eigen(baratheon_graph_endo_undir, directed = FALSE)$centralization
# 0.5969745
# Eigenvector centrality (also called eigencentrality) is a measure of the influence of a node in a network.
# It assigns relative scores to all nodes in the network based on the concept that connections to high-scoring nodes contribute more to the score of the node in question than equal connections to low-scoring nodes.[29][7] Google's PageRank and the Katz centrality are variants of the eigenvector centrality.



# Robert es el patriarca, todos los personajes son sus
# - hijos: Tommen, Myrcella, Joffrey,
# - madre: Cassana,
# - padre: Steffon,
# - hermanos directos:Stannis, Renly, también hijos de Cassana y Steffon
# - hermano indirecto: Ormund, hijo de Steffon con otra mujer
# - sobrino: Shireeen, hijo de Stannis


# NODE DEGREE

# Node degree or degree centrality describes how central a node is in the network (i.e. how many in- AND outgoing edges it has or to how many other nodes it is directly connected via one edge).
#
# “The degree of a vertex is its most basic structural property, the number of its adjacent edges.” From the help pages of degree()
#
# We can calculate the number of out- OR ingoing edges of each node, or - as I am doing here - THE SUM OF BOTH.
#
# In this case, the node degree reflects how many offspring and spouses a character had. With 3 wifes and several children, Quellon Greyjoy, the grandfather of Theon and Asha/Yara comes out on top (of course, had I included all offspring and wifes of Walder Frey’s, he would easily be on top but the network would have gotten infinitely more confusing).

baratheon_graph_endo_undir_degree <- igraph::degree(baratheon_graph_endo_undir, mode = "all") # mode puede ser all, out, in, total. "all" y "total" son sinónimos. Este argumento es ignorado en gráficos no dirigidos.

# Cassana Baratheon  Joffrey Baratheon Myrcella Baratheon   Ormund Baratheon    Renly Baratheon
# 4                  1                 1                    1                   2
# Robert Baratheon  Shireen Baratheon  Stannis Baratheon  Steffon Baratheon   Tommen Baratheon
# 5                 1                  3                  5                   1



# standardized by number of nodes
baratheon_graph_endo_undir_degree_std <- baratheon_graph_endo_undir_degree / (vcount(baratheon_graph_endo_undir) - 1)
# Cassana Baratheon  Joffrey Baratheon Myrcella Baratheon   Ormund Baratheon    Renly Baratheon
# 0.4444444          0.1111111          0.1111111          0.1111111          0.2222222
# Robert Baratheon  Shireen Baratheon  Stannis Baratheon  Steffon Baratheon   Tommen Baratheon
# 0.5555556          0.1111111          0.3333333          0.5555556          0.1111111


node_degree <- data.frame(degree = baratheon_graph_endo_undir_degree,
                          degree_std = baratheon_graph_endo_undir_degree_std) %>%
    tibble::rownames_to_column() # ordena estos datos recién calculados en un df

node_degree %>%
  arrange(-degree)

#               rowname degree  degree_std
# 1    Robert Baratheon      5  0.5555556
# 2   Steffon Baratheon      5  0.5555556
# 3   Cassana Baratheon      4  0.4444444
# 4   Stannis Baratheon      3  0.3333333
# 5     Renly Baratheon      2  0.2222222
# 6   Joffrey Baratheon      1  0.1111111
# 7  Myrcella Baratheon      1  0.1111111
# 8    Ormund Baratheon      1  0.1111111
# 9   Shireen Baratheon      1  0.1111111
# 10   Tommen Baratheon      1  0.1111111


baratheon_characters <- left_join(baratheon_characters, node_degree, by = c("name" = "rowname")) # agrega a la tabla ya existente "baratheon_characters" 2 columnas más: "degree" y "degree_std"

# Closeness de nodos
#
# The closeness of a node describes its distance to all other nodes. A node with highest closeness is more central and can spread information to many other nodes.
# The characters with highest closeness all surround central characters that connect various storylines and houses in Game of Thrones.

closeness <- igraph::closeness(baratheon_graph_endo_undir, mode = "total")
# Cassana Baratheon  Joffrey Baratheon Myrcella Baratheon   Ormund Baratheon    Renly Baratheon
# 0.07142857         0.04545455         0.04545455         0.04761905         0.05000000
# Robert Baratheon  Shireen Baratheon  Stannis Baratheon  Steffon Baratheon   Tommen Baratheon
# 0.07142857         0.03846154         0.05555556         0.07692308         0.04545455

#standardized by number of nodes
closeness_std <- closeness / (vcount(baratheon_graph_endo_undir) - 1)
# Cassana Baratheon  Joffrey Baratheon Myrcella Baratheon   Ormund Baratheon    Renly Baratheon
# 0.007936508        0.005050505        0.005050505        0.005291005        0.005555556
# Robert Baratheon  Shireen Baratheon  Stannis Baratheon  Steffon Baratheon   Tommen Baratheon
# 0.007936508        0.004273504        0.006172840        0.008547009        0.005050505


node_closeness <- data.frame(closeness = closeness,
                             closeness_std = closeness_std) %>%
    tibble::rownames_to_column()

node_closeness %>%
  arrange(-closeness)

#               rowname  closeness closeness_std
# 1   Steffon Baratheon 0.07692308   0.008547009
# 2   Cassana Baratheon 0.07142857   0.007936508
# 3    Robert Baratheon 0.07142857   0.007936508
# 4   Stannis Baratheon 0.05555556   0.006172840
# 5     Renly Baratheon 0.05000000   0.005555556
# 6    Ormund Baratheon 0.04761905   0.005291005
# 7   Joffrey Baratheon 0.04545455   0.005050505
# 8  Myrcella Baratheon 0.04545455   0.005050505
# 9    Tommen Baratheon 0.04545455   0.005050505
# 10  Shireen Baratheon 0.03846154   0.004273504


baratheon_characters <- left_join(baratheon_characters, node_closeness, by = c("name" = "rowname")) # agrego a la tabla existente baratheon_characters las 2 columnas "closeness" y "closeness_std"



# Betweenness centrality  de edges
#
# Betweenness describes the number of shortest paths between nodes. Nodes with high betweenness centrality are on the path between many other nodes, i.e. they are people who are key connections or bridges between different groups of nodes. In a social network, these nodes would be very important because they are likely to pass on information to a wide reach of people.
betweenness <- igraph::betweenness(baratheon_graph_endo_undir, directed = FALSE)

# standardize by number of node pairs
betweenness_std <- betweenness / ((vcount(baratheon_graph_endo_undir) - 1) * (vcount(baratheon_graph_endo_undir) - 2) / 2)

node_betweenness <- data.frame(betweenness = betweenness,
                               betweenness_std = betweenness_std) %>%
    tibble::rownames_to_column()
node_betweenness %>%
  arrange(-betweenness)

#               rowname betweenness betweenness_std
# 1   Cassana Baratheon           7       0.1944444
# 2   Joffrey Baratheon           0       0.0000000
# 3  Myrcella Baratheon           0       0.0000000
# 4    Ormund Baratheon           0       0.0000000
# 5     Renly Baratheon           0       0.0000000
# 6    Robert Baratheon          21       0.5833333
# 7   Shireen Baratheon           0       0.0000000
# 8   Stannis Baratheon           8       0.2222222
# 9   Steffon Baratheon          15       0.4166667
# 10   Tommen Baratheon           0       0.0000000


baratheon_characters <- left_join(baratheon_characters, node_betweenness, by = c("name" = "rowname"))

edge_betweenness <- igraph::edge_betweenness(baratheon_graph_endo_undir, directed = FALSE) # creo un objeto igraph

data.frame(edge = attr(E(baratheon_graph_endo_undir), "vnames"), # creo un df con 2 cols: edge (vnames es el nombre del vértice de los edges) y betweenness de cada uno de esos edges ???
           betweenness = edge_betweenness) %>% # cuán "en el medio" están del resto, son los nucleares
    tibble::rownames_to_column() %>% #crea una columna que se llena con los ids de cada vínculo
    arrange(-betweenness)

#    rowname                                edge betweenness
# 1       10  Robert Baratheon|Steffon Baratheon          14
# 2        2  Cassana Baratheon|Robert Baratheon          10
# 3        3  Joffrey Baratheon|Robert Baratheon           9
# 4        4 Myrcella Baratheon|Robert Baratheon           9
# 5        6 Shireen Baratheon|Stannis Baratheon           9
# 6        8  Ormund Baratheon|Steffon Baratheon           9
# 7       11 Stannis Baratheon|Steffon Baratheon           9
# 8       12   Robert Baratheon|Tommen Baratheon           9
# 9        5 Cassana Baratheon|Stannis Baratheon           7
# 10       9   Renly Baratheon|Steffon Baratheon           5
# 11       1   Cassana Baratheon|Renly Baratheon           4
# 12       7 Cassana Baratheon|Steffon Baratheon           2

plot(baratheon_graph_endo, # imagen: edge-betweenness.png
     #layout = layout,
     vertex.label = gsub(" ", "\n", V(baratheon_graph_endo)$name),
     # vertex.shape = V(baratheon_graph_endo_undir)$shape,
     # vertex.color = V(baratheon_graph_endo_undir)$color,
     vertex.size = edge_betweenness * 0.1 ,
     vertex.frame.color = "gray",
     vertex.label.color = "black",
     vertex.label.cex = 1,
     edge.width = edge_betweenness * 0.2,
     edge.arrow.size = 0.8,
    # edge.color = E(baratheon_graph_endo_undir)$color,
     edge.lty = E(baratheon_graph_endo)$lty)




# legend("topleft", legend = c("Node color:", as.character(color_vertices$house), NA, "Edge color:", as.character(colors_edges$type)), pch = 19,
#        col = c(NA, color_vertices$color, NA, NA, colors_edges$color), pt.cex = 5, cex = 2, bty = "n", ncol = 1)


# Ned Stark is the character with highest betweenness.
# This makes sense, as he and his children (specifically Sansa and her arranged marriage to Tyrion) connect to other houses and are the central points from which the story unfolds.
# However, we have to keep in mind here, that my choice of who is important enough to include in the network (e.g. the Stark ancestors) and who not (e.g. the whole complicated mess that is the Targaryen and Frey family tree) makes this result somewhat biased.







# Diameter de la red
#
# In contrast to the shortest path between two nodes, we can also calculate the longest path, or diameter:

    diameter(baratheon_graph_endo_undir, directed = FALSE)

## [1] 4

# In our network, the longest path connects 4 nodes.
# “get_diameter returns a path with the actual diameter.
# If there are many shortest paths of the length of the diameter, then it returns the first one found.” diameter() help
#
# This, we can also plot:

    baratheon_graph_endo_undir_diameter <- baratheon_graph_endo_undir
node_diameter <- get_diameter(baratheon_graph_endo_undir_diameter,  directed = FALSE)

V(baratheon_graph_endo_undir_diameter)$color <- scales::alpha(V(baratheon_graph_endo_undir_diameter)$color, alpha = 0.5)
V(baratheon_graph_endo_undir_diameter)$size <- 2

V(baratheon_graph_endo_undir_diameter)[node_diameter]$color <- "red"
V(baratheon_graph_endo_undir_diameter)[node_diameter]$size <- 5

E(baratheon_graph_endo_undir_diameter)$color <- "grey"
E(baratheon_graph_endo_undir_diameter)$width <- 1

E(baratheon_graph_endo_undir_diameter, path = node_diameter)$color <- "red"
E(baratheon_graph_endo_undir_diameter, path = node_diameter)$width <- 5

plot(baratheon_graph_endo_undir_diameter, # imagen: diameter-undirected.png
     #layout = layout,
     vertex.label = gsub(" ", "\n", V(baratheon_graph_endo_undir_diameter)$name),
     vertex.shape = V(baratheon_graph_endo_undir_diameter)$shape,
     vertex.frame.color = "gray",
     vertex.label.color = "black",
     vertex.label.cex = 0.8,
     edge.arrow.size = 0.5,
     edge.lty = E(baratheon_graph_endo_undir_diameter)$lty)

# el camino más largo va de
# Joffrey -(1)-> Robert -(2)-> Cassana -(3)-> Stannis -(4)-> Shireen

# legend("topleft", legend = c("Node color:", as.character(color_vertices$house), NA, "Edge color:", as.character(colors_edges$type)), pch = 19,
#        col = c(NA, color_vertices$color, NA, NA, colors_edges$color), pt.cex = 5, cex = 2, bty = "n", ncol = 1)


# Transitivity
#
# “Transitivity measures the probability that the adjacent vertices of a vertex are connected. This is sometimes also called the clustering coefficient.” transitivity() help
#
# We can calculate the transitivity or ratio of triangles to connected triples for the whole network:
transitivity(baratheon_graph_endo_undir, type = "global")
#  0.3

# Or for each node:

transitivity <- data.frame(name = V(baratheon_graph_endo_undir)$name,
                               transitivity = transitivity(baratheon_graph_endo_undir, type = "local")) %>% # la transitividad es 1 o 0 para cara personaje
    mutate(name = as.character(name))

transitivity %>%
  arrange(-transitivity)

#
#                                  name transitivity
# Renly Baratheon       Renly Baratheon    1.0000000
# Cassana Baratheon   Cassana Baratheon    0.5000000
# Stannis Baratheon   Stannis Baratheon    0.3333333
# Steffon Baratheon   Steffon Baratheon    0.3000000
# Robert Baratheon     Robert Baratheon    0.1000000
# Joffrey Baratheon   Joffrey Baratheon          NaN
# Myrcella Baratheon Myrcella Baratheon          NaN
# Ormund Baratheon     Ormund Baratheon          NaN
# Shireen Baratheon   Shireen Baratheon          NaN
# Tommen Baratheon     Tommen Baratheon          NaN

baratheon_characters <- left_join(baratheon_characters, transitivity, by = "name")


# Because ours is a family network, characters with a transitivity of one form triangles with their parents or offspring.
#
# PageRank (originally used by Google to rank the importance of search results) is similar to eigenvector centrality. Eigenvector centrality scores nodes in a network according to the number of connections to high-degree nodes they have. It is therefore a measure of node importance. PageRank similarly considers nodes as more important if they have many incoming edges (or links).

page_rank <- page_rank(baratheon_graph_endo_undir, directed = FALSE)

page_rank_centrality <- data.frame(name = names(page_rank$vector),
                                   page_rank = page_rank$vector) %>%
    mutate(name = as.character(name))

page_rank_centrality %>%
  arrange(-page_rank)

#                                  name  page_rank
# Robert Baratheon     Robert Baratheon 0.20810323
# Steffon Baratheon   Steffon Baratheon 0.19122073
# Cassana Baratheon   Cassana Baratheon 0.15121390
# Stannis Baratheon   Stannis Baratheon 0.12169986
# Renly Baratheon       Renly Baratheon 0.07964048
# Joffrey Baratheon   Joffrey Baratheon 0.05037755
# Myrcella Baratheon Myrcella Baratheon 0.05037755
# Tommen Baratheon     Tommen Baratheon 0.05037755
# Shireen Baratheon   Shireen Baratheon 0.04948163
# Ormund Baratheon     Ormund Baratheon 0.04750752


baratheon_characters <- left_join(baratheon_characters, page_rank_centrality, by = "name")



##
# Matrix representation of a network
#
# Connections between nodes can also be represented as an adjacency matrix. We can convert our graph object to an adjacency matrix with igraph’s as_adjacency_matrix() function. Whenever there is an edge between two nodes, this field in the matrix will get assigned a 1, otherwise it is 0.

adjacency <- as.matrix(as_adjacency_matrix(baratheon_graph_endo_undir))

# Eigenvector centrality

# We can now calculate the eigenvalues and eigenvectors of the adjacency matrix.

#degree diagonal matrix
degree_diag <- diag(1 / igraph::degree(baratheon_graph_endo_undir))

# PageRank matrix
pagerank <- adjacency %*% degree_diag
eigenvalues <- eigen(pagerank)

# The eigenvector with the highest eigenvalue scores those vertices highly, that have many eges or that are connected to vertices with many edges.
# métrica espectral , cuán conectada estoy con nodos importantes de forma directa o indirecta

eigenvector <- data.frame(name = rownames(pagerank),
                          eigenvector = as.numeric(eigenvalues$vectors[, which.max(eigenvalues$values)]))

baratheon_characters <- left_join(baratheon_characters, eigenvector, by = "name")

eigenvector %>%
    arrange(eigenvector)
#                  name eigenvector
# 1    Robert Baratheon  -0.5455447
# 2   Steffon Baratheon  -0.5455447
# 3   Cassana Baratheon  -0.4364358
# 4   Stannis Baratheon  -0.3273268
# 5     Renly Baratheon  -0.2182179
# 6   Shireen Baratheon  -0.1091089
# 7    Ormund Baratheon  -0.1091089
# 8   Joffrey Baratheon  -0.1091089
# 9    Tommen Baratheon  -0.1091089
# 10 Myrcella Baratheon  -0.1091089

# We can find the eigenvector centrality scores with:

    eigen_centrality <- igraph::eigen_centrality(baratheon_graph_endo_undir, directed = FALSE)

eigen_centrality <- data.frame(name = names(eigen_centrality$vector),
                               eigen_centrality = eigen_centrality$vector) %>%
    mutate(name = as.character(name))

eigen_centrality %>%
  arrange(-eigen_centrality)
#                                  name eigen_centrality
# Steffon Baratheon   Steffon Baratheon        1.0000000
# Cassana Baratheon   Cassana Baratheon        0.9288774
# Robert Baratheon     Robert Baratheon        0.8141961
# Stannis Baratheon   Stannis Baratheon        0.6476487
# Renly Baratheon       Renly Baratheon        0.5875552
# Ormund Baratheon     Ormund Baratheon        0.3046099
# Myrcella Baratheon Myrcella Baratheon        0.2480122
# Tommen Baratheon     Tommen Baratheon        0.2480122
# Joffrey Baratheon   Joffrey Baratheon        0.2480122
# Shireen Baratheon   Shireen Baratheon        0.1972802

baratheon_characters <- left_join(baratheon_characters, eigen_centrality, eigenvector, by = "name")




# Who are the most important characters?
#
#     We can now compare all the node-level information to decide which characters are the most important in Game of Thrones. Such node level characteristics could also be used as input for machine learning algorithms.

# Let’s look at all characters from the major houses:

baratheon_characters %>%
    filter(!is.na(house2)) %>%
    dplyr::select(-contains("_std")) %>%
    gather(x, y, degree:eigen_centrality) %>%
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

igraph::dyad_census(baratheon_graph_endo)
# $mut: 1
# $asym: 11
# $null: 33

sna::dyad.census(adjacency)
# Mut Asym Null
# 12    0   33

# The same can be calculated for triads (see ?triad_census for details on what each output means).

igraph::triad_census(baratheon_graph_endo)
# 51 41  4  9  0 11  1  0  0  0  0  0  3  0  0  0

sna::triad.census(adjacency)
# 003 012 102 021D 021U 021C 111D 111U 030T 030C 201 120D 120U 120C 210 300
# 51   0  45    0    0    0    0    0    0    0  21    0    0    0   0   3

triad.classify(adjacency, mode = "graph")
# 0

# We can also calculate the number of paths and cycles of any length we specify, here e.g. of length <= 5. For edges, we obtain the sum of counts for all paths or cycles up to the given maximum length. For vertices/nodes, we obtain the number of paths or cycles to which each node belongs.

node_kpath <- kpath.census(adjacency, maxlen = 5, mode = "graph", tabulate.by.vertex = TRUE, dyadic.tabulation = "sum")
edge_kpath <- kpath.census(adjacency, maxlen = 5, mode = "graph", tabulate.by.vertex = FALSE)
edge_kpath


# This, we could plot with (but here, it does not give much additional information):

    gplot(node_kpath$paths.bydyad, # imagen: node-kpath-bydyad
          label.cex = 0.5,
          vertex.cex = 0.75,
          displaylabels = TRUE,
          edge.col = "grey")

node_kcycle <- kcycle.census(adjacency, maxlen = 8, mode = "graph", tabulate.by.vertex = TRUE, cycle.comembership = "sum")
edge_kcycle <- kcycle.census(adjacency, maxlen = 8, mode = "graph", tabulate.by.vertex = FALSE)
node_kcycle_reduced <- node_kcycle$cycle.comemb
node_kcycle_reduced <- node_kcycle_reduced[which(rowSums(node_kcycle_reduced) > 0), which(colSums(node_kcycle_reduced) > 0)]

gplot(node_kcycle_reduced, # imagen: node-kcycle-reduced
      label.cex = 0.5,
      vertex.cex = 0.75,
      displaylabels = TRUE,
      edge.col = "grey")



# “A (maximal) clique is a maximal set of mutually adjacency vertices.” clique.census() help

node_clique <- clique.census(adjacency, mode = "graph", tabulate.by.vertex = TRUE, clique.comembership = "sum")
edge_clique <- clique.census(adjacency, mode = "graph", tabulate.by.vertex = FALSE, clique.comembership = "sum")
edge_clique$clique.count
# 1 2 3
# 0 5 3

node_clique_reduced <- node_clique$clique.comemb
node_clique_reduced <- node_clique_reduced[which(rowSums(node_clique_reduced) > 0), which(colSums(node_clique_reduced) > 0)]

gplot(node_clique_reduced, # imagen: node-clique-reduced
      label.cex = 0.5,
      vertex.cex = 0.75,
      displaylabels = TRUE,
      edge.col = "grey")



# The largest group of nodes ín this network is 5, i.e. all parent/child relationships. Therefore, it does not really make sense to plot them all, but we could plot and color them with:

    vcol <- rep("grey80", vcount(baratheon_graph_endo_undir))

# highlight first of largest cliques
vcol[unlist(largest_cliques(baratheon_graph_endo_undir)[[1]])] <- "red"

plot(baratheon_graph_endo_undir, # imagen: largest-cliques.png
     #layout = layout,
     vertex.label = gsub(" ", "\n", V(baratheon_graph_endo_undir)$name),
     vertex.shape = V(baratheon_graph_endo_undir)$shape,
     vertex.color = vcol,
     vertex.size = 5,
     vertex.frame.color = "gray",
     vertex.label.color = "black",
     vertex.label.cex = 0.8,
     edge.width = 2,
     edge.arrow.size = 0.5,
     edge.color = E(baratheon_graph_endo_undir)$color,
     edge.lty = E(baratheon_graph_endo_undir)$lty)




# Clustering
#
# We can also look for groups within our network by clustering node groups according to their edge betweenness:

    ceb <- cluster_edge_betweenness(baratheon_graph_endo_undir)
modularity(ceb)

#  0.2777778 <-- irrelevante para grupos chicos

plot(ceb,  # imagen: cluster-edge-betweenness
     baratheon_graph_endo_undir,
     #layout = layout,
     vertex.label = gsub(" ", "\n", V(baratheon_graph_endo_undir)$name),
     vertex.shape = V(baratheon_graph_endo_undir)$shape,
     vertex.size = (V(baratheon_graph_endo_undir)$popularity + 0.5) * 5,
     vertex.frame.color = "gray",
     vertex.label.color = "black",
     vertex.label.cex = 0.8)


# Hay 2 clusters separados por edge betweennes que indica los hijos de Cassana y Steffon y los hijos de Robert

# Or based on propagating labels:

    clp <- cluster_label_prop(baratheon_graph_endo_undir)

plot(clp, # imagen: cluster-label-prop
     baratheon_graph_endo_undir,
     #layout = layout,
     vertex.label = gsub(" ", "\n", V(baratheon_graph_endo_undir)$name),
     vertex.shape = V(baratheon_graph_endo_undir)$shape,
     vertex.size = (V(baratheon_graph_endo_undir)$popularity + 0.5) * 5,
     vertex.frame.color = "gray",
     vertex.label.color = "black",
     vertex.label.cex = 0.8)

#
# Network properties
#
# We can also feed our adjacency matrix to other functions, like GenInd() from the NetIndices packages.
# This function calculates a number of network properties, like number of compartments (N), total system throughput (T..), total system throughflow (TST), number of internal links (Lint), total number of links (Ltot), like density (LD), connectance (C), average link weight (Tijbar), average compartment throughflow (TSTbar) and compartmentalization or degree of connectedness of subsystems in the network (Cbar).

library(NetIndices)
graph.properties <- GenInd(adjacency)
graph.properties

# $N: 10 number of compartments
# $T..: 24 total system throughput
# $TST: 24 total system throughflow
# $Lint: 24 number of internal links
# $Ltot: 24 total number of links
# $LD: 2.4 like density
# $C: 0.2666667 connectance
# $Tijbar: 1 average link weight
# $TSTbar: 2.4 verage compartment throughflow
# $Cbar: 0.1976808 compartmentalization or degree of connectedness of subsystems in the network

# Alternatively, the network package provides additional functions to obtain network properties. Here, we can again feed in the adjacency matrix of our network and convert it to a network object.

library(network)
adj_network <- network(adjacency, directed = TRUE)
adj_network
#
# Network attributes:
#   vertices = 10
# directed = TRUE
# hyper = FALSE
# loops = FALSE
# multiple = FALSE
# bipartite = FALSE
# total edges= 24
# missing edges= 0
# non-missing edges= 24
#
# Vertex attribute names:
#   vertex.names
## No edge attributes

# From this network object, we can e.g. get the number of dyads and edges within a network and the network size.

network.dyadcount(adj_network)

## 90

network.edgecount(adj_network)

## 24

network.size(adj_network)

## 10

# “equiv.clust uses a definition of approximate equivalence (equiv.fun) to form a hierarchical clustering of network positions. Where dat consists of multiple relations, all specified relations are considered jointly in forming the equivalence clustering.” equiv.clust() help

ec <- equiv.clust(adj_network, mode = "graph", cluster.method = "average", plabels = network.vertex.names(adj_network))
ec
# Position Clustering:
#
#   Equivalence function: sedist
# Equivalence metric: hamming
# Cluster method: average
# Graph order: 10

ec$cluster$labels <- ec$plabels
plot(ec) # imagen: cluster-dendogram



# From the sna package, we can e.g. use functions that tell us the graph density and the dyadic reciprocity of the vertices or edges

gden(adjacency)

## 0.2666667

grecip(adjacency)

## Mut
##   1

grecip(adjacency, measure = "edgewise")

## Mut
##   1

sessionInfo()


