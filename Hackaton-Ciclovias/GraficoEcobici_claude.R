# =============================================================
# Gráfico combinado: barras (recorridos reales) +
#                   línea (estimación anual)
# Muestra la brecha entre EcoBici y el total de ciclistas
# Ciudad de Buenos Aires — 2013 a 2017
# =============================================================

library(dplyr)
library(readr)
library(ggplot2)
library(scales)

# --- 1. Carga y conteo de recorridos reales ------------------

reales <- bind_rows(
  read_csv("CienciaDatos/curso-E520-2026C1/Clase/recorridos-realizados-2013.csv") %>% summarise(anio = 2013, recorridos_reales = n()),
  read_csv("CienciaDatos/curso-E520-2026C1/Clase/recorridos-realizados-2014.csv") %>% summarise(anio = 2014, recorridos_reales = n()),
  read_csv("CienciaDatos/curso-E520-2026C1/Clase/recorridos-realizados-2015.csv") %>% summarise(anio = 2015, recorridos_reales = n()),
  read_csv("CienciaDatos/curso-E520-2026C1/Clase/recorridos-realizados-2016.csv") %>% summarise(anio = 2016, recorridos_reales = n()),
  read_csv("CienciaDatos/curso-E520-2026C1/Clase/recorridos-realizados-2017.csv") %>% summarise(anio = 2017, recorridos_reales = n())
)

# --- 2. Carga de estimaciones --------------------------------

conteo <- read_csv("CienciaDatos/curso-E520-2026C1/Clase/Conteocicilistas-GCBA (1).csv")
colnames(conteo) <- c("anio", "viajes_diarios_est", "viajes_anuales_est")

conteo <- conteo %>%
  mutate(
    viajes_diarios_est = as.numeric(gsub("\\.", "", viajes_diarios_est)),
    viajes_anuales_est = as.numeric(gsub("\\.", "", viajes_anuales_est))
  )

# --- 3. Unir datos -------------------------------------------

datos <- inner_join(conteo, reales, by = "anio") %>%
  mutate(
    brecha      = viajes_anuales_est - recorridos_reales,
    cobertura   = round(recorridos_reales / viajes_anuales_est * 100, 1),
    label_real  = paste0(round(recorridos_reales / 1e6, 2), "M"),
    label_est   = paste0(round(viajes_anuales_est  / 1e6, 1), "M")
  )

# --- 4. Factor de escala para doble eje ----------------------
# Eje izquierdo: recorridos reales (hasta ~1.2M)
# Eje derecho:   estimación anual  (hasta ~65M)

factor_eje <- max(datos$viajes_anuales_est) / max(datos$recorridos_reales)

# --- 5. Gráfico ----------------------------------------------

grafico <- ggplot(datos, aes(x = factor(anio))) +
  
  # Área de brecha (relleno entre curvas) — va primero para que quede detrás
  geom_ribbon(
    aes(
      ymin  = recorridos_reales,
      ymax  = viajes_anuales_est / factor_eje,
      group = 1
    ),
    fill  = "#FF6B6B",
    alpha = 0.12
  ) +
  
  # Barras: recorridos reales
  geom_col(
    aes(y = recorridos_reales),
    fill  = "#4CAF50",
    alpha = 0.85,
    width = 0.55
  ) +
  
  # Línea + puntos: estimación anual (escalada al eje izquierdo)
  geom_line(
    aes(y = viajes_anuales_est / factor_eje, group = 1),
    color     = "#2196F3",
    linewidth = 1.2
  ) +
  geom_point(
    aes(y = viajes_anuales_est / factor_eje),
    color = "#2196F3",
    size  = 3.5
  ) +
  
  # Etiquetas sobre barras (recorridos reales)
  geom_text(
    aes(y = recorridos_reales, label = label_real),
    vjust    = -0.5,
    size     = 3.5,
    color    = "#2E7D32",
    fontface = "bold"
  ) +
  
  # Etiquetas sobre línea (estimación)
  geom_text(
    aes(y = viajes_anuales_est / factor_eje, label = label_est),
    vjust    = -0.8,
    size     = 3.5,
    color    = "#1565C0",
    fontface = "bold"
  ) +
  
  # Etiqueta de cobertura % dentro de cada barra
  geom_text(
    aes(y = recorridos_reales / 2, label = paste0(cobertura, "%\nde cobertura")),
    size   = 3,
    color  = "white",
    fontface = "bold",
    lineheight = 0.9
  ) +
  
  # Doble eje Y
  scale_y_continuous(
    name   = "Recorridos EcoBici (barras verdes)",
    labels = label_number(scale = 1e-3, suffix = "K", big.mark = "."),
    sec.axis = sec_axis(
      transform = ~ . * factor_eje,
      name      = "Viajes anuales estimados — total ciclistas (línea azul)",
      labels    = label_number(scale = 1e-6, suffix = "M")
    )
  ) +
  
  scale_x_discrete(name = "Año") +
  
  labs(
    title    = "EcoBici vs total de ciclistas estimados · Buenos Aires",
    subtitle = "Barras = recorridos reales EcoBici · Línea = estimación total de viajes en bicicleta\nEl área roja muestra la brecha entre ambas fuentes",
    caption  = "Fuente: datos abiertos GCBA · La cobertura indica qué % del estimado representan los recorridos EcoBici"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(color = "gray40", size = 10, lineheight = 1.3),
    plot.caption     = element_text(color = "gray50", size = 9),
    axis.title.y.left  = element_text(color = "#2E7D32", face = "bold"),
    axis.title.y.right = element_text(color = "#1565C0", face = "bold"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  )

# --- 6. Guardar ----------------------------------------------

ggsave("grafico_brecha_ecobici.png", grafico, width = 11, height = 7, dpi = 150)
cat("✔ Exportado: grafico_brecha_ecobici.png\n")

print(grafico)