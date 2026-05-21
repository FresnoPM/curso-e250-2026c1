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
E(g)[1:10]$weight
print(g, n=12)

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
# 3rd Qu.:57.00   3rd Qu.:58.00   3rd Qu.: 6.000
# Max.   :81.00   Max.   :81.00   Max.   :16.000

library(dplyr)
as.data.frame(table(df$weight)) |> arrange(desc(Freq))
#   #    Weight Freq
  # 1     2     207
  # 2     1     202
  # 3     4     107
  # 4     6     80
  # 5     8     51
  # 6    12     39
  # 7    10     37
  # 8    14     34
  # 9     3     31
  # 10   16     22
  # 11    5     5
  # 12    7     2

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
# Min.  1st Qu. Median  Mean    3rd Qu. Max.
# 2.00  12.00   18.00   20.17   26.00   62.00
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
# 37 62  5  2 29

#   Tiene sentido que sean hubs?



#   Densidad? Es una red densa o esparsa?
edge_density(g)        # 0.1260802

### La densidad es 0.13%.
### Si fuera densa (densidad alta), todos los
### grupos interactuarían con todos y no habría especialización funcional.
### La rareza es una propiedad, no un defecto.

### Comparación útil: una red completa con 2.617 nodos tendría ~3.4 millones de aristas.
### Yeast tiene ~12.000. O sea, usa menos del 1% de las conexiones posibles, pero
### alcanza para mantener viva a la levadura.


### Por qué importa esto para el análisis?
### Algunas métricas no se pueden calcular sobre una red desconectada o dan resultados raros.
### Por ejemplo diameter.
diameter(g)        # da Inf o un valor extraño
mean_distance(g)   # solo considera pares conectados
### La solución estándar: trabajar con la componente gigante!!!!!!

# cuando más que una conexión entre 2 nodos, al hacer comunidades entonces podemos simplificar colapsandolas en una sola conexión



# Coeficiente de clustering (transitividad)
transitivity(g, type = "global")   # global
mean(transitivity(g, type = "local"), na.rm = TRUE)  # local promedio




library(RColorBrewer)
# select colors
colors = brewer.pal(4, "Dark2")
# assign colors to groups
V(UKfaculty)$color = sapply(V(UKfaculty)$Group, function(x) colors[x])


plot(UKfaculty, layout = layout_nicely(UKfaculty, dim = 2),
     vertex.color = V(UKfaculty)$color, vertex.frame.color = NA,
     vertex.label = NA, vertex.shape = 'square',
     vertex.size = 3.5, edge.arrow.size = 0.3, edge.curved = TRUE,
     edge.width = E(UKfaculty)$weight ^ 0.8,
     edge.color = rgb(0, 0, 0, alpha = 0.1))
title("UK Faculty Friendship Network (Directed)", cex.main = 1)

UKfaculty_undirected <- as_undirected(
  UKfaculty,
  mode = "mutual", # keeping only mutual ties
  edge.attr.comb = list(weight="sum", "ignore") # sum edge weights, ignore other attributes
)




