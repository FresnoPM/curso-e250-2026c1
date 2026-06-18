# =============================================================
# Comparación: Viajes reales vs Viajes estimados anuales
# Ciudad de Buenos Aires — 2012 a 2019
# =============================================================

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)

# --- 1. Carga y conteo de recorridos reales ------------------

# 2012: sin columna id_usuario
rec_2012 <- read_csv("Hackaton-cs-de-datos-e250/csv-source/recorridos-realizados-2012.csv") %>%
  summarise(anio = 2012, registros_reales = n(), tipo_dato = "Recorridos")

# 2013: columna 'periodo'
rec_2013 <- read_csv("Hackaton-cs-de-datos-e250/csv-source/recorridos-realizados-2013.csv") %>%
  summarise(anio = 2013, registros_reales = n(), tipo_dato = "Recorridos")

# 2014: columnas distintas (ID, NOMBRE_ORIGEN, etc.) — igual se cuentan filas
rec_2014 <- read_csv("Hackaton-cs-de-datos-e250/csv-source/recorridos-realizados-2014.csv") %>%
  summarise(anio = 2014, registros_reales = n(), tipo_dato = "Recorridos")

usr_2015 <- read_csv("Hackaton-cs-de-datos-e250/csv-source/recorridos-realizados-2015.csv") %>%
  summarise(anio = 2015, registros_reales = n(), tipo_dato = "Recorridos")

usr_2016 <- read_csv("Hackaton-cs-de-datos-e250/csv-source/recorridos-realizados-2016.csv") %>%
  summarise(anio = 2016, registros_reales = n(), tipo_dato = "Recorridos")

usr_2017 <- read_csv("Hackaton-cs-de-datos-e250/csv-source/recorridos-realizados-2013.csv") %>%
  summarise(anio = 2017, registros_reales = n(), tipo_dato = "Recorridos")

# Unir todos los años
reales <- bind_rows(rec_2012, rec_2013, rec_2014, usr_2015, usr_2016, usr_2017)

cat("=== Registros reales por año ===\n")
print(reales)

# --- 2. Carga de estimaciones (archivo de conteo ciclistas) --

conteo <- read_csv("CienciaDatos/curso-E520-2026C1/Clase/Conteocicilistas-GCBA (1).csv")
colnames(conteo) <- c("anio", "viajes_diarios_est", "viajes_anuales_est")

# Limpiar números con puntos como separador de miles
conteo <- conteo %>%
  mutate(
    viajes_diarios_est = as.numeric(gsub("\\.", "", viajes_diarios_est)),
    viajes_anuales_est = as.numeric(gsub("\\.", "", viajes_anuales_est))
  )

cat("\n=== Estimaciones anuales ===\n")
print(conteo)

# --- 3. Tabla comparativa final ------------------------------

comparacion <- full_join(conteo, reales, by = "anio") %>%
  arrange(anio) %>%
  mutate(
    diferencia    = registros_reales - viajes_anuales_est,
    cobertura_pct = round((registros_reales / viajes_anuales_est) * 100, 1)
  )

cat("\n=== TABLA COMPARATIVA FINAL ===\n")
print(comparacion)

write_csv(comparacion, "tabla_comparativa_final.csv")
cat("\n✔ Exportada: tabla_comparativa_final.csv\n")

# --- 4. Resumen comparativo (solo años con ambos datos) ------

resumen <- comparacion %>%
  filter(!is.na(viajes_anuales_est) & !is.na(registros_reales)) %>%
  select(anio, viajes_anuales_est, registros_reales, tipo_dato, cobertura_pct)

cat("\n=== RESUMEN COMPARATIVO ===\n")
for (i in 1:nrow(resumen)) {
  f <- resumen[i, ]
  cat(sprintf(
    "Año %d | Estimado: %s | Registrado: %s (%s) | Cobertura: %s%%\n",
    f$anio,
    format(f$viajes_anuales_est, big.mark = ".", scientific = FALSE),
    format(f$registros_reales,   big.mark = ".", scientific = FALSE),
    f$tipo_dato,
    ifelse(is.na(f$cobertura_pct), "N/A", f$cobertura_pct)
  ))
}

write_csv(resumen, "resumen_comparativo.csv")
cat("\n✔ Exportada: resumen_comparativo.csv\n")

# --- 5. Gráfico (solo años con recorridos reales) ------------

datos_grafico <- comparacion %>%
  filter(tipo_dato == "Recorridos") %>%
  select(anio, viajes_anuales_est, registros_reales) %>%
  pivot_longer(
    cols      = c(viajes_anuales_est, registros_reales),
    names_to  = "tipo",
    values_to = "cantidad"
  ) %>%
  mutate(tipo = recode(tipo,
                       "viajes_anuales_est" = "Estimación anual",
                       "registros_reales"   = "Recorridos EcoBici"
  ))

grafico <- ggplot(datos_grafico, aes(x = factor(anio), y = cantidad, fill = tipo)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  scale_fill_manual(values = c(
    "Estimación anual"   = "#2196F3",
    "Recorridos EcoBici" = "#4CAF50"
  )) +
  labs(
    title    = "Viajes estimados vs recorridos EcoBici",
    subtitle = "Ciudad de Buenos Aires",
    x = "Año", y = "Cantidad de viajes", fill = NULL,
    caption  = "Fuente: datos abiertos GCBA"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", plot.title = element_text(face = "bold"))

ggsave("grafico_comparacion_viajes.png", grafico, width = 10, height = 6, dpi = 150)
cat("✔ Exportado: grafico_comparacion_viajes.png\n")