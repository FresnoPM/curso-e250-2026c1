# https://kateto.net/network-visualization

install.packages("igraph")
install.packages("network")
install.packages("sna") # social network analysis
install.packages("ggraph")
install.packages("visNetwork")
install.packages("threejs")
install.packages("networkD3")
install.packages("ndtv") # no lo puedo instalar porque no tengo dependencias que hay que isntalar por separado y no está disponible para ubuntu 24....

library(igraph)
library(network)
library(sna)
library(ggraph)
library(visNetwork)
library(threejs)
library(networkD3)
library(ndtv)
rm(list =ls())

plot(x=1:10, y=rep(5,10), pch=19, cex=3, col="dark red")
points(x=1:10, y=rep(6, 10), pch=19, cex=3, col="557799")
points(x=1:10, y=rep(4, 10), pch=19, cex=3, col=rgb(.25, .5, .3))



# podemos hacer una matriz de adyacencia (nodo a nodo, de a dos)
#   - binaria (existe vínculo o no) o
#   - ponderada (cantidad de conexiones por cada vínculo entre nodos)
#
#   si la matriz es demasiado grande, para no alocar tanto espacio podemos usar un formato "lista de adyacencia" cuya estructura sólo expresa los vínculos que hay y no gasta recursos en señalar aquellos nodos que no se vinculan. Ejemplo:
#   nodo_i = {nodo_j, nodo_k, nodo_m}
#   nodo_l = {nodo_j, nodo_i, nodo_m}
#   nodo_m = {nodo_k}
#
# cuando tengo nodos muy diversos y complejos conviene usar un gráfico "Forth direct" que me sorve para identificar clusters de existencia o intensidad y comparar matrices de transición- Sirve para encontrar aptrones globales o modularidad es estructuras muy atomizadas.
#
# Heatmaps nos sirve para identificar umbrales, límites de fricción


nodes <- read.csv("https://kateto.net/workshops/data/Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("https://kateto.net/workshops/data/Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

head(nodes)
head(links)

summary(nodes)


# creamos nuestro objeto


net <- graph_from_data_frame(d = links, vertices = nodes, directed = T)
class(net) # igraph

net
# IGRAPH    <-- clase
# 836f873   <-- identificador de posición en memoria
# DNW       <-- D: Directed, N: named (s01, s02, etc), W: weight (ponderación)
# 17        <-- cantidad de nodos s1, s2, s3, ...
# 49        <-- cantidad de conexiones nodo a nodo ej: s01->s02, s01->s03, s01->s04, s01->s15, ..

E(net)
# 49/49 edges from 836f873 (vertex names):
E(net)$type
# atributo "type" de la tabla "links" que está dentro del objeto "net"
V(net)$media
# atributo "media" de la tabla "nodes"
summary(net)
# vemos todos los atributos


plot (net)
# gráfico direccional

plot(net, edge.arrow.size =.4, vertex.label=NA)
# cada vez que ejecuto este comando me da una diferente configuración, esto gasta recursos innecesarios
# fijar el seed me sirve también para que al compatir el código y que todo el mundo vea lo mismo

set.seed(87654)
# si no fijo esta semilla ese valor se toma del entorno, variable

#agrego información relevante para que sea interpretable
plot(net, edge.arrow.size=.2, edge.color="orange",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=V(net)$media, vertex.label.color="black")


# to-do: terminar el tutorial

