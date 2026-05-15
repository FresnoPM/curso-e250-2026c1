# SAND with R, chapter4.tex PPI network as a reference

# Clear your workspace by removing all objects returned by ls():
rm(list = ls())

library(igraph)
library(igraphdata)

# ============================================================
# 1. EXPLORACIÓN INICIAL: Esto es un grafo. ¿Qué nos dice este encabezado?
# ============================================================
data(yeast)
yeast           # ver el encabezado

# IGRAPH 65c41bb
# UN-- significa
#   U undirected;
#   N: named;
#   -: no ponderado
#   -: unipartita  (cuántos tipos de nodos hay)
# 2617 # cantidad de nodos
# 11855 # cantidad de links (también llamados vínculos, edges, aristas, arcos, según el campo de aplicación son términos equivalentes)
# --
# Yeast protein interactions, von Mering et al.
# + attr: name (g/c), Citation (g/c), Author (g/c), URL (g/c), Classes | (g/x), name (v/c), Class (v/c), Description (v/c), Confidence (e/c)
#     lo que está entre paréntesis después de cada atributo
#     (g/c) atributo del grafo tipo character
#     (g/x) atributo del grafo tipo x
#     (v/c) atributo vertices tipo character
#     ...
#
# + edges from 65c41bb (vertex names):

summary(yeast)
vcount(yeast)   # cantidad de nodos
# ---
## [1] 2617
# ---
ecount(yeast)   # cantidad de aristas
# ---
## [1] 11855
# ---

##### De qué tamaño les parece esta red comparada con por ej, Facebook?
##### Con un cerebro humano? (Facebook ~3.000 millones, cerebro ~86.000 millones de neuronas.
##### Yeast es chiquita, pero muy informativa).

### información y atributos de esta red:
### ~2.617 nodos, ~11.855 aristas, no dirigida, no ponderada.
### Atributos de nodos: name (ORF), Class (clase funcional), Description.
is_directed(yeast)
is_weighted(yeast)
is_connected(yeast)   # ¿es conexa?

## podemos investigar cuantos componentes hay
### ~80–100 componentes en total
### Una gigante con ~2.300 – 2.400 nodos (más del 90% de la red)
### Decenas de componentes pequeños con 2, 3 o 4 nodos cada uno
comp <- components(yeast)

# ¿Cuántos componentes hay?
comp$no

# Tamaño de cada componente (cantidad de nodos)
comp$csize

# Tamaño del componente más grande (la "componente gigante")
max(comp$csize)

# Proporción de nodos en la componente gigante
max(comp$csize) / vcount(yeast)

### Por qué importa esto para el análisis? Algunas métricas no se pueden
### calcular sobre una red desconectada o dan resultados raros.
### Por ejemplo diameter.
diameter(yeast)        # da Inf o un valor extraño
mean_distance(yeast)   # solo considera pares conectados
### La solución estándar: trabajar con la componente gigante!!!!!!

# Identificar la componente gigante
comp <- components(yeast)
id_gigante <- which.max(comp$csize)
nodos_gigante <- which(comp$membership == id_gigante)

# Extraer la subred
gigante <- induced_subgraph(yeast, nodos_gigante)

# Verificar
is_connected(gigante)   # TRUE
vcount(gigante)         # ~2400+
diameter(gigante)
mean_distance(gigante)
transitivity(gigante)

vertex_attr_names(yeast) # atributos de los nodos
edge_attr_names(yeast)   # atributos de las aristas

# Atributos comunes en este dataset:
V(yeast)$name            # identificador ORF de cada proteína (ej. "YLR098C")
V(yeast)$Class           # clase funcional (ej. metabolismo, transcripción, etc.)
V(yeast)$Description     # descripción de la proteína
E(yeast)$Confidence      # nivel de confianza de la interacción


# ============================================================
# 2. GRADO PROMEDIO: Cuán conectada está?
# El grado de un nodo es la cantidad de conexiones que tiene
# El grado promedio resume cuán conectada está la red en promedio
# ============================================================
grados <- degree(yeast)

mean(grados)        # grado promedio
### # ~9
### Cada proteína interactúa con ~9 otras en promedio. ¿Es mucho o poco?

median(grados)      # mediana (más robusta si hay hubs)
max(grados)         # nodo más conectado (hub principal)
min(grados)

summary(grados)

# Fórmula equivalente (útil de recordar):
# Para grafo no dirigido: grado_promedio = 2 * E / N
2 * ecount(yeast) / vcount(yeast)

# ============================================================
# 3.  DENSIDAD
# ============================================================
edge_density(yeast)        # ~0.003

### La densidad es 0.3%. Si fuera densa (densidad alta), todas las
### proteínas interactuarían con todas y no habría especialización funcional.
### La rareza es una propiedad, no un defecto.

### Comparación útil: una red completa con 2.617 nodos tendría ~3.4 millones de aristas.
### Yeast tiene ~12.000. O sea, usa menos del 1% de las conexiones posibles, pero
### alcanza para mantener viva a la levadura.


# Fórmula equivalente para grafo no dirigido:
# densidad = 2 * E / (N * (N - 1))
2 * ecount(yeast) / (vcount(yeast) * (vcount(yeast) - 1))

# ============================================================
# 4. DISTRIBUCIÓN DE GRADOS Y POWER LAW : Todas las proteínas son iguales?
# ============================================================

# --- 4.1 Histograma básico ---
hist(degree(yeast), breaks = 50)  #  La mayoría de proteínas tiene 1-5 conexiones,
                                  #   pero hay algunas con cientos.

# o se puede hacer con

hist(grados, breaks = 50, col = "steelblue",
     main = "Distribución de grados (lineal)",
     xlab = "Grado", ylab = "Frecuencia")

sort(grados, decreasing = TRUE)[1:10]   # los 10 hubs principales
                                        # quiénes son esos hubs (en yeast suelen ser proteínas como HSP82, CDC28 — chaperonas, kinasas, factores de transcripción).

# --- 4.2 Distribución acumulada complementaria en log-log ---
# Si es power law, en log-log se ve aproximadamente como una recta.
dist_cruda <- degree_distribution(yeast, cumulative = FALSE)
plot(0:(length(dist_cruda)-1), dist_cruda, log = "xy", pch = 19)

### Cada clase es un módulo: proteínas que cumplen funciones relacionadas
### (metabolismo energético, transcripción, etc.).
### La red no es homogénea: está organizada en módulos.

table(V(yeast)$Class)        # distribución de clases funcionales
### Hay 256 proteínas con clase P
### Hay 558 proteínas con clase U
### Hay 60 con clase A
### Etc.

dist_grados <- degree_distribution(yeast, cumulative = TRUE)
plot(0:(length(dist_grados)-1), dist_grados,
     log = "xy", type = "p", pch = 19, col = "tomato",
     xlab = "Grado (k)", ylab = "P(K >= k)",
     main = "Distribución acumulada en log-log")

# --- 4.3 Ajustar una ley de potencias ---
fit_pl <- fit_power_law(grados)
fit_pl

########  Visualización log-log "a mano"
# Frecuencia de cada grado
tab <- table(grados)
k <- as.numeric(names(tab))
freq <- as.numeric(tab)

plot(k, freq, log = "xy", pch = 19, col = "tomato",
     xlab = "Grado k (log)", ylab = "Frecuencia (log)",
     main = "Distribución de grados (log-log)")

# Superponer la recta del ajuste
abline(a = log10(freq[1]), b = -fit_pl$alpha, col = "blue", lwd = 2, untf = TRUE)
legend("topright", legend = paste("α =", round(fit_pl$alpha, 2)),
       col = "blue", lwd = 2, bty = "n")


################### Qué consecuencias tiene? robustez vs vulnerabilidad
## Si sacamos proteínas al azar, la red apenas se inmuta (porque la mayoría son periféricas)
## Si sacamos específicamente los hubs, la red colapsa
## Biológicamente: los hubs son letales si se mutan, las proteínas periféricas son redundantes


# ============================================================
####      SIMULACION DE UNOS ESCENARIOS DE VULNERABILIDAD

#### Por qué con la misma cantidad de nodos eliminados el resultado es tan distinto?
####
#### Aleatorio: ~0.92 (queda 92% de la red conectada)
#### Dirigido: ~0.50 (queda 50% — colapsó)
# ============================================================
# Robustez: sacar nodos al azar vs sacar hubs
##  sacar 200 nodos al azar casi no afecta,
##  pero sacar los 200 hubs principales fragmenta la red.
##  Esto es el corazón de por qué los antibióticos y antitumorales modernos
##  buscan hubs como blancos.
# ============================================================
set.seed(1)

# Componente gigante original
g <- yeast
tam_inicial <- max(components(g)$csize)

# Ataque aleatorio
g_random <- delete_vertices(g, sample(V(g), 200))
max(components(g_random)$csize) / tam_inicial

# Ataque dirigido a hubs
hubs <- order(degree(g), decreasing = TRUE)[1:200]
g_attack <- delete_vertices(g, hubs)
max(components(g_attack)$csize) / tam_inicial

### VISUALIZAMOS EL RESULTADOS CON UN GRAFICO DE BARRAS
resultados <- c(
  Original = tam_inicial,
  Aleatorio = max(components(g_random)$csize),
  Dirigido = max(components(g_attack)$csize)
)

barplot(resultados / tam_inicial,
        col = c("gray70", "steelblue", "tomato"),
        ylim = c(0, 1),
        ylab = "Componente gigante (proporción)",
        main = "Efecto de remover 200 proteínas",
        names.arg = c("Red original", "Falla aleatoria", "Ataque a hubs"))

abline(h = 1, lty = 2, col = "gray")

### Sacamos exactamente la misma cantidad de nodos en los dos escenarios.
###  Pero el daño no es igual ni cerca. ¿Qué pasa?

###  Cuántos nodos se vuelven inalcanzables? No basta con mirar la componente gigante:
###  también podemos mirar cuántos nodos quedaron "huérfanos" (sin conexión a la red principal).

# Función auxiliar
analizar_red <- function(g) {
  comp <- components(g)
  gigante <- max(comp$csize)
  aislados <- sum(comp$csize == 1)
  n_componentes <- comp$no
  list(gigante = gigante, aislados = aislados, componentes = n_componentes)
}

cat("Red original:        ", unlist(analizar_red(yeast)), "\n")
cat("Tras falla aleatoria:", unlist(analizar_red(g_random)), "\n")
cat("Tras ataque dirigido:", unlist(analizar_red(g_attack)), "\n")

#### -> tras el ataque dirigido: Hay muchos más componentes
#### la red se fragmentó en pedazos: Muchos más nodos aislados
#### -> El ataque dirigido no solo achica la componente gigante: la fragmenta.
#### La red deja de ser un todo y se vuelve un montón de islitas

##### En vez de comparar un solo punto (200 nodos), comparamos toda la trayectoria:
#### sacando 1%, 2%, 5%, 10%... hasta 50% de los nodos.
set.seed(123)
N <- vcount(yeast)
tam_inicial <- max(components(yeast)$csize)

# Fracciones a evaluar (0% a 50%)
fracciones <- seq(0, 0.5, by = 0.02)

# Curva 1: falla aleatoria
curva_random <- sapply(fracciones, function(f) {
  if (f == 0) return(1)
  nodos <- sample(V(yeast), round(f * N))
  max(components(delete_vertices(yeast, nodos))$csize) / tam_inicial
})

# Curva 2: ataque dirigido (recalculando hubs en cada paso sería más realista,
# pero para empezar usamos los hubs iniciales)
hubs_orden <- order(degree(yeast), decreasing = TRUE)

curva_attack <- sapply(fracciones, function(f) {
  if (f == 0) return(1)
  nodos <- hubs_orden[1:round(f * N)]
  max(components(delete_vertices(yeast, nodos))$csize) / tam_inicial
})

# Gráfico
plot(fracciones, curva_random, type = "o", pch = 19, col = "steelblue", lwd = 2,
     ylim = c(0, 1),
     xlab = "Fracción de nodos removidos (f)",
     ylab = "S(f) = Componente gigante / N inicial",
     main = "Robustez de la red Yeast: aleatorio vs dirigido")

lines(fracciones, curva_attack, type = "o", pch = 19, col = "tomato", lwd = 2)

abline(h = 0.5, lty = 2, col = "gray")
legend("topright",
       legend = c("Falla aleatoria", "Ataque dirigido (hubs)"),
       col = c("steelblue", "tomato"), pch = 19, lwd = 2, bty = "n")

##### MENSAJE: Esta es la firma de las redes scale-free. La curva azul cae linealmente: la red soporta fallas aleatorias muy bien. La curva roja cae como un acantilado: con sacar 10-15% de los nodos correctos,
##### la red ya está fragmentada. Robusto pero frágil

### Existe un punto crítico?
#### hay una fracción crítica f_c a partir de la cual la red colapsa.
#### Es una transición de fase (igual que cuando el agua hierve).

# Encontrar f_c aproximado: dónde la componente gigante cae por debajo del 50%
f_critico_random <- fracciones[which(curva_random < 0.5)[1]]
f_critico_attack <- fracciones[which(curva_attack < 0.5)[1]]

cat("f_crítico aleatorio:", f_critico_random, "\n")
cat("f_crítico dirigido:", f_critico_attack, "\n")

#### MENSAJE: Aleatorio: necesitás sacar ~50% para fragmentar (o ni siquiera lo lográs con 50%)
#### Dirigido: alcanza con ~10-15% para fragmentar

# ============================================================
# 6. OTRAS MÉTRICAS ÚTILES: Tiene estructura?

# ============================================================

# Coeficiente de clustering (transitividad)
transitivity(yeast, type = "global")   # global
mean(transitivity(yeast, type = "local"), na.rm = TRUE)  # local promedio

# Diámetro y distancia promedio (en la componente gigante)
componentes <- components(yeast)
gigante <- induced_subgraph(yeast, which(componentes$membership == which.max(componentes$csize)))

diameter(gigante)
mean_distance(gigante)

#
d.yeast <- degree(yeast)
hist(d.yeast,col="blue",
     xlab="Degree", ylab="Frequency",
     main="Degree Distribution")

plot(d[ind], dd.yeast[ind], log="xy", col="blue",
     xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
     main="Log-Log Degree Distribution")

# CHUNK 7
a.nn.deg.yeast <- knn(yeast,V(yeast))$knn
plot(d.yeast, a.nn.deg.yeast, log="xy",
     col="goldenrod", xlab=c("Log Vertex Degree"),
     ylab=c("Log Average Neighbor Degree"))


#### plot network

plot(yeast,
     vertex.label = NA,        # sin etiquetas (imposibles de leer)
     vertex.size = 2,          # nodos chiquitos
     edge.arrow.size = 0.1)

comp <- components(yeast)
gigante <- induced_subgraph(yeast, which(comp$membership == which.max(comp$csize)))

plot(gigante,
     vertex.label = NA,
     vertex.size = 2,
     edge.color = adjustcolor("gray70", alpha.f = 0.3),
     vertex.color = "tomato",
     layout = layout_with_fr)   # Fruchterman-Reingold

# Quedarse con los nodos de grado >= 10
sub <- induced_subgraph(yeast, V(yeast)[degree(yeast) >= 10])

plot(sub,
     vertex.label = NA,
     vertex.size = degree(sub) / 5,   # tamaño según grado
     vertex.color = "steelblue",
     edge.color = adjustcolor("gray", alpha.f = 0.3),
     layout = layout_with_fr)

table(V(yeast)$Class)   # ver las clases

# Asignar un color a cada clase
clases <- as.factor(V(yeast)$Class)
paleta <- rainbow(length(levels(clases)))
V(yeast)$color <- paleta[clases]

plot(yeast,
     vertex.label = NA,
     vertex.size = 2,
     edge.color = adjustcolor("gray80", alpha.f = 0.2),
     layout = layout_with_fr)

legend("topleft", legend = levels(clases),
       col = paleta, pch = 19, cex = 0.7, bty = "n")

# Layouts útiles para redes grandes:
l1 <- layout_with_fr(yeast)         # Fruchterman-Reingold (clásico, lento)
l2 <- layout_with_kk(yeast)         # Kamada-Kawai
l3 <- layout_with_drl(yeast)        # DrL (rápido, bueno para redes grandes)
l4 <- layout_with_lgl(yeast)        # Large Graph Layout (pensado para redes enormes)

plot(yeast, layout = l3,
     vertex.label = NA, vertex.size = 2,
     edge.color = adjustcolor("gray", alpha.f = 0.2))



#### combinando todo
#library(igraph)
#library(igraphdata)
#data(yeast)

# Componente gigante
comp <- components(yeast)
g <- induced_subgraph(yeast, which(comp$membership == which.max(comp$csize)))

# Colores por clase
clases <- as.factor(V(g)$Class)
paleta <- rainbow(length(levels(clases)), alpha = 0.8)
V(g)$color <- paleta[clases]

# Tamaño por grado
V(g)$size <- sqrt(degree(g)) * 0.6

# Layout
set.seed(123)  # para reproducibilidad
l <- layout_with_drl(g)

# Plot
plot(g,
     layout = l,
     vertex.label = NA,
     vertex.frame.color = NA,
     edge.color = adjustcolor("gray70", alpha.f = 0.15),
     edge.width = 0.3,
     main = "Red PPI de Saccharomyces cerevisiae")

legend("bottomleft", legend = levels(clases),
       col = paleta, pch = 19, cex = 0.6, bty = "n", ncol = 2)

##### version intercativa
# Opción A: visNetwork (interactivo, podés hacer zoom y arrastrar)
# install.packages("visNetwork")
library(visNetwork)

# Tomamos una muestra para que sea ágil
sub <- induced_subgraph(yeast, V(yeast)[degree(yeast) >= 20])
data_vis <- toVisNetworkData(sub)
visNetwork(nodes = data_vis$nodes, edges = data_vis$edges) %>%
  visIgraphLayout(layout = "layout_with_fr") %>%
  visNodes(size = 10)

# Opción B: ggraph (sintaxis tipo ggplot, muy flexible)
# install.packages("ggraph")
library(ggraph)
library(ggplot2)

ggraph(g, layout = "drl") +
  geom_edge_link(alpha = 0.1, color = "gray") +
  geom_node_point(aes(color = Class, size = degree(g))) +
  theme_void() +
  theme(legend.position = "bottom")