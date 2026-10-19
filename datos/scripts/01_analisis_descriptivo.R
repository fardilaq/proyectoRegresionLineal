# =====================================================================
# 00_preparar_datos.R
# Lectura de los datos y conversión de variables categóricas.
#
# Uso: desde la raíz del repo (directorio de trabajo = raíz),
#   source("scripts/00_preparar_datos.R")
# Deja en el entorno: `datos` (1000 dispositivos) y `escenarios`
# (10 fabricantes), ambos con las categóricas ya convertidas.
# =====================================================================

library(readxl)

ruta_xlsx <- "datos/DatosProyectoRL_Android_202620.xlsx"

# ---------------------------------------------------------------------
# 1. Definición de categorías (una sola vez, se usa en Datos y Escenarios)
#    El PRIMER nivel de cada factor es la categoría de referencia en lm():
#    los coeficientes se leen como diferencia frente a ese nivel.
#    Son factores NO ordenados (ordered = FALSE): así lm() usa variables
#    indicadoras (contraste de tratamiento) y no contrastes polinomiales.
# ---------------------------------------------------------------------
niveles <- list(
  color_dispositivo = c("Negro", "Blanco", "Azul"),  # ref.: Negro (el más frecuente)
  tipo_conectividad = c("Wi-fi", "4G", "5G"),        # ref.: Wi-fi (sin red celular)
  dia_semana        = c("lunes", "martes", "miercoles", "jueves",
                        "viernes", "sabado", "domingo")  # ref.: lunes; orden de calendario
)

# Variables 0/1: se conservan numéricas (para el modelo) y se crea una
# copia como factor con sufijo _f (solo para gráficas y tablas).
binarias <- c("uso_gps", "ahorro_energia", "bluetooth", "modo_oscuro")

# ---------------------------------------------------------------------
# 2. Función de conversión (la misma para `datos` y `escenarios`,
#    así predict() siempre encuentra los mismos niveles)
# ---------------------------------------------------------------------
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

# ---------------------------------------------------------------------
# 3. Lectura y conversión
# ---------------------------------------------------------------------
datos      <- as.data.frame(read_excel(ruta_xlsx, sheet = "Datos"))
escenarios <- as.data.frame(read_excel(ruta_xlsx, sheet = "Escenarios"))

datos      <- preparar_categoricas(datos)
escenarios <- preparar_categoricas(escenarios)

# ---------------------------------------------------------------------
# 4. Verificaciones
# ---------------------------------------------------------------------
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

cat("\n== Variables indicadoras que usará lm() (ejemplo: tipo_conectividad) ==\n")