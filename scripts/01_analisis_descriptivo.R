# 01_analisis_descriptivo.R
# Análisis descriptivo de los datos.
#
# Uso: desde la raíz del repo (directorio de trabajo = raíz),
#   source("scripts/01_analisis_descriptivo.R")
# Deja en el entorno: `datos` (1000 dispositivos) y `escenarios`
# (10 fabricantes), ambos con las categóricas ya convertidas.

library(readxl)
library(moments)
library(ggplot2)

ruta_xlsx <- "datos/DatosProyectoRL_Android_202620.xlsx"


# 1. Definición de categorías (una sola vez, se usa en Datos y Escenarios)
#    El PRIMER nivel de cada factor es la categoría de referencia en lm():
#    los coeficientes se leen como diferencia frente a ese nivel.
#    Son factores NO ordenados (ordered = FALSE): así lm() usa variables
#    indicadoras (contraste de tratamiento) y no contrastes polinomiales.
niveles <- list(
  color_dispositivo = c("Negro", "Blanco", "Azul"),
  tipo_conectividad = c("Wi-fi", "4G", "5G"),
  dia_semana        = c("lunes", "martes", "miercoles", "jueves",
                        "viernes", "sabado", "domingo"))

# Variables 0/1: se conservan numéricas (para el modelo) y se crea una
# copia como factor con sufijo _f (solo para gráficas y tablas).
binarias <- c("uso_gps", "ahorro_energia", "bluetooth", "modo_oscuro")

# 2. Función de conversión (la misma para `datos` y `escenarios`,
#    así predict() siempre encuentra los mismos niveles)
preparar_categoricas <- function(df) {
  for (v in names(niveles)) {
    x <- trimws(as.character(df[[v]]))
    inesperados <- setdiff(unique(x), niveles[[v]])
    if (length(inesperados) > 0) {
      stop("Valores inesperados en '", v, "': ",
           paste(inesperados, collapse = ", "))
    }
    df[[v]] <- factor(x, levels = niveles[[v]], ordered = FALSE)
  }
  for (v in binarias) {
    if (!all(df[[v]] %in% c(0, 1))) {
      stop("'", v, "' tiene valores distintos de 0/1")
    }
    df[[paste0(v, "_f")]] <- factor(df[[v]], levels = c(0, 1),
                                    labels = c("No", "Si"))
  }
  df
}

# 3. Lectura y conversión
datos      <- as.data.frame(read_excel(ruta_xlsx, sheet = "Datos"))
escenarios <- as.data.frame(read_excel(ruta_xlsx, sheet = "Escenarios"))

datos      <- preparar_categoricas(datos)
escenarios <- preparar_categoricas(escenarios)

# 4. Verificaciones
stopifnot(
  nrow(datos) == 1000,
  !anyNA(datos),
  all(setdiff(names(datos), "duracion_bateria") %in% names(escenarios))
)

cat("\n== Dimensiones ==\n")
cat("datos:", nrow(datos), "filas x", ncol(datos), "columnas\n")
cat("escenarios:", nrow(escenarios), "filas\n")

cat("\n== Categóricas de texto: niveles y frecuencias ==\n")
for (v in names(niveles)) {
  cat("\n", v, "\n", sep = "")
  print(table(datos[[v]]))
}

cat("\n== Variables 0/1: frecuencias ==\n")
for (v in binarias) {
  cat("\n", v, "\n", sep = "")
  print(table(datos[[paste0(v, "_f")]]))
}

# 5. Variables indicadoras del modelo
cat("\n== Variables indicadoras que usará lm() ==\n")
for (v in names(niveles)) {
  cat("\n", v, " (referencia: ", niveles[[v]][1], ")\n", sep = "")
  print(contrasts(datos[[v]]))
}
cat("\nVariables 0/1 (entran como 0/1): ", paste(binarias, collapse = ", "), "\n", sep = "")

# 6. Carpeta de salida de tablas
dir_tablas <- "resultados/tablas"

# 7. Resumen estadístico de variables numéricas
numericas <- c("duracion_bateria", "capacidad_bateria", "antiguedad_bateria",
               "resolucion_pantalla", "apps_segundo_plano", "brillo_pantalla",
               "tiempo_pantalla_encendida", "contactos_guardados",
               "volumen_timbre", "notificaciones", "temperatura_C")


resumen_num <- do.call(rbind, lapply(numericas, function(v) {
  x <- datos[[v]]
  data.frame(variable = v, n = length(x), media = mean(x), mediana = median(x),
             desv_est = sd(x), minimo = min(x),
             q1 = unname(quantile(x, 0.25)), q3 = unname(quantile(x, 0.75)),
             maximo = max(x), asimetria = skewness(x))
}))
resumen_num[-1] <- round(resumen_num[-1], 2)

cat("\n== Resumen estadístico: variables numéricas ==\n")
print(resumen_num, row.names = FALSE)
write.csv(resumen_num, file.path(dir_tablas, "resumen_numericas.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# 8. Frecuencias de variables categóricas
categoricas <- c("color_dispositivo", "tipo_conectividad", "dia_semana",
                  "uso_gps_f", "ahorro_energia_f", "bluetooth_f", "modo_oscuro_f")

frecuencias <- do.call(rbind, lapply(categoricas, function(v) {
  t <- table(datos[[v]])
  data.frame(variable = sub("_f$", "", v), nivel = names(t), n = as.integer(t),
             porcentaje = round(100 * as.numeric(t) / sum(t), 1))
}))

cat("\n== Frecuencias: variables categóricas ==\n")
print(frecuencias, row.names = FALSE)
write.csv(frecuencias, file.path(dir_tablas, "frecuencias_categoricas.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# 9. Datos en formato largo y ruta de gráficas
dir_graficas <- "resultados/graficas/descriptivo"

datos_largo <- data.frame(
  variable = factor(rep(numericas, each = nrow(datos)), levels = numericas),
  valor = as.vector(as.matrix(datos[numericas]))
)

# 10. Histogramas de variables numéricas
g_hist <- ggplot(datos_largo, aes(x = valor)) +
  geom_histogram(bins = 30, fill = "#4C72B0", color = "white") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(title = "Distribución de las variables numéricas", x = NULL, y = "Frecuencia") +
  theme_minimal()
ggsave(file.path(dir_graficas, "histogramas_numericas.png"), g_hist,
       width = 11, height = 9, dpi = 300)

# 11. Boxplots de variables numéricas
g_box <- ggplot(datos_largo, aes(x = "", y = valor)) +
  geom_boxplot(fill = "#4C72B0", alpha = 0.6, outlier.color = "#C44E52") +
  coord_flip() +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(title = "Boxplots de las variables numéricas", x = NULL, y = NULL) +
  theme_minimal()
ggsave(file.path(dir_graficas, "boxplots_numericas.png"), g_box,
       width = 11, height = 9, dpi = 300)

# 12. Barras de variables categóricas
frec_graf <- frecuencias
frec_graf$variable <- factor(frec_graf$variable, levels = unique(frec_graf$variable))
frec_graf$nivel <- factor(frec_graf$nivel, levels = unique(frec_graf$nivel))

g_barras <- ggplot(frec_graf, aes(x = nivel, y = porcentaje)) +
  geom_col(fill = "#4C72B0") +
  geom_text(aes(label = n), vjust = -0.4, size = 3) +
  facet_wrap(~ variable, scales = "free_x", ncol = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Distribución de las variables categóricas (n sobre cada barra)",
       x = NULL, y = "Porcentaje (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(dir_graficas, "barras_categoricas.png"), g_barras,
       width = 11, height = 8, dpi = 300)

# 13. Matriz de correlaciones
correlaciones <- cor(datos[numericas])
write.csv(round(correlaciones, 3), file.path(dir_tablas, "correlaciones_numericas.csv"),
          fileEncoding = "UTF-8")

cor_largo <- as.data.frame(as.table(correlaciones))
names(cor_largo) <- c("var1", "var2", "r")
cor_largo$var1 <- factor(cor_largo$var1, levels = numericas)
cor_largo$var2 <- factor(cor_largo$var2, levels = rev(numericas))

g_cor <- ggplot(cor_largo, aes(x = var1, y = var2, fill = r)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", r)), size = 3) +
  scale_fill_gradient2(low = "#C44E52", mid = "white", high = "#4C72B0", limits = c(-1, 1)) +
  labs(title = "Correlación de Pearson entre variables numéricas",
       x = NULL, y = NULL, fill = "r") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(dir_graficas, "correlaciones_numericas.png"), g_cor,
       width = 10, height = 8, dpi = 300)

cat("\n== Correlación de cada variable con duracion_bateria ==\n")
print(round(sort(correlaciones[, "duracion_bateria"][-1]), 3))

cat("\n== 10 pares con mayor correlación absoluta ==\n")
pares <- cor_largo[as.integer(cor_largo$var1) < match(cor_largo$var2, numericas), ]
pares <- pares[order(-abs(pares$r)), ]
print(head(pares, 10), row.names = FALSE)