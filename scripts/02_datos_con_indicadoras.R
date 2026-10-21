# 02_datos_con_indicadoras.R
# Exporta un CSV con los datos listos para regresión: las variables
# categóricas de texto quedan convertidas en indicadoras 0/1 usando el
# contraste de tratamiento (el primer nivel es la referencia y se omite),
# igual que las que ya usa lm() en 01_analisis_descriptivo.R.
#
# Uso: desde la raíz del repo,
#   source("scripts/02_datos_con_indicadoras.R")

library(readxl)

ruta_xlsx <- "datos/DatosProyectoRL_Android_202620.xlsx"

# Mismos niveles y referencias que en 01_analisis_descriptivo.R
niveles <- list(
  color_dispositivo = c("Negro", "Blanco", "Azul"),
  tipo_conectividad = c("Wi-fi", "4G", "5G"),
  dia_semana        = c("lunes", "martes", "miercoles", "jueves",
                        "viernes", "sabado", "domingo"))

binarias <- c("uso_gps", "ahorro_energia", "bluetooth", "modo_oscuro")

# 1. Validación y conversión a factor (misma lógica que el script 01)
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
  }
  df
}

# 2. Construcción de las indicadoras. Se omite el primer nivel (referencia).
#    Los nombres quedan como <variable>_<nivel>, p. ej. color_dispositivo_Blanco.
construir_indicadoras <- function(df) {
  bloques <- lapply(names(niveles), function(v) {
    niv_no_ref <- niveles[[v]][-1]
    ind <- lapply(niv_no_ref, function(nivel) as.integer(df[[v]] == nivel))
    names(ind) <- paste0(v, "_", niv_no_ref)
    as.data.frame(ind, check.names = FALSE)
  })
  do.call(cbind, bloques)
}

# 3. Lectura y preparación
datos      <- as.data.frame(read_excel(ruta_xlsx, sheet = "Datos"))
escenarios <- as.data.frame(read_excel(ruta_xlsx, sheet = "Escenarios"))

datos      <- preparar_categoricas(datos)
escenarios <- preparar_categoricas(escenarios)

# 4. Reemplazo de las categóricas de texto por sus indicadoras.
#    El resto de columnas (numéricas y binarias 0/1) se conservan tal cual.
ensamblar <- function(df) {
  resto <- df[, setdiff(names(df), names(niveles)), drop = FALSE]
  cbind(resto, construir_indicadoras(df))
}

datos_final      <- ensamblar(datos)
escenarios_final <- ensamblar(escenarios)

# 5. Escritura
ruta_datos      <- "datos/datos_con_indicadoras.csv"
ruta_escenarios <- "datos/escenarios_con_indicadoras.csv"

write.csv(datos_final,      ruta_datos,      row.names = FALSE, fileEncoding = "UTF-8")
write.csv(escenarios_final, ruta_escenarios, row.names = FALSE, fileEncoding = "UTF-8")

cat("\n== Archivos generados ==\n")
cat(ruta_datos,      ": ", nrow(datos_final),      " filas x ", ncol(datos_final),      " columnas\n", sep = "")
cat(ruta_escenarios, ": ", nrow(escenarios_final), " filas x ", ncol(escenarios_final), " columnas\n", sep = "")

cat("\n== Columnas finales ==\n")
print(names(datos_final))
