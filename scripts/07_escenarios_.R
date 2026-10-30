# 07_escenarios.R
# Punto 7: intervalos del 95 % por fabricante con el modelo C y variables que explican las diferencias.
# Uso: desde la raíz del repo, source("scripts/07_escenarios.R")
# Entrada: resultados/tablas/seleccion/modelo_C_punto5.rds (generado por 05_seleccion_modelos.R)
# Salidas: resultados/tablas/escenarios/*.csv y variables_influyentes.txt, resultados/graficas/escenarios/*.png

library(readxl)
library(ggplot2)

ruta_xlsx    <- "datos/DatosProyectoRL_Android_202620.xlsx"
ruta_modelo  <- "resultados/tablas/seleccion/modelo_C_punto5.rds"
dir_tablas   <- "resultados/tablas/escenarios"
dir_graficas <- "resultados/graficas/escenarios"

# 1. Modelo y datos
modelo <- readRDS(ruta_modelo)
niveles_conectividad <- c("Wi-fi", "4G", "5G")
leer_hoja <- function(hoja) {
  df <- as.data.frame(read_excel(ruta_xlsx, sheet = hoja))
  df$tipo_conectividad <- factor(trimws(as.character(df$tipo_conectividad)), levels = niveles_conectividad)
  df
}
datos      <- leer_hoja("Datos")
escenarios <- leer_hoja("Escenarios")
stopifnot(!anyNA(datos$tipo_conectividad), !anyNA(escenarios$tipo_conectividad))

# 2. Conversión al formato del modelo (centrado y componentes con los objetos guardados)
puntaje <- function(obj, df, k) {
  as.numeric(predict(obj$pca, newdata = df[, obj$vars, drop = FALSE])[, k]) * obj$signos[k]
}
preparar <- function(df) {
  df$brillo_c      <- df$brillo_pantalla    - modelo$medias_centrado[["brillo_pantalla"]]
  df$apps_c        <- df$apps_segundo_plano - modelo$medias_centrado[["apps_segundo_plano"]]
  df$temp_c        <- df$temperatura_C      - modelo$medias_centrado[["temperatura_C"]]
  df$pc_hardware   <- puntaje(modelo$pc_hardware, df, 1)
  df$pc_hardware2  <- puntaje(modelo$pc_hardware, df, 2)
  df$pc_uso        <- puntaje(modelo$pc_uso, df, 1)
  df
}
datos      <- preparar(datos)
escenarios <- preparar(escenarios)

formula_x   <- delete.response(terms(modelo$formula))
X_datos     <- model.matrix(formula_x, datos)
X_esc       <- model.matrix(formula_x, escenarios)
coef_modelo <- modelo$coeficientes
stopifnot(identical(colnames(X_esc), names(coef_modelo)))

# 3. Intervalos del 95 %: media (confianza) e individual (predicción)
ajuste        <- drop(X_esc %*% coef_modelo)
se_media      <- sqrt(rowSums((X_esc %*% modelo$vcov) * X_esc))
sd_individual <- modelo$sigma * sqrt(exp(modelo$coef_aux[[1]] + modelo$coef_aux[[2]] * escenarios$pc_uso))
se_individual <- sqrt(sd_individual^2 + se_media^2)
t_crit        <- qt(0.975, modelo$gl)

# 4. Revisión de extrapolación (valores fuera del rango observado en los 1000 datos)
variables_revisar <- c("capacidad_bateria", "antiguedad_bateria", "resolucion_pantalla", "apps_segundo_plano",
                       "brillo_pantalla", "tiempo_pantalla_encendida", "notificaciones", "temperatura_C",
                       "pc_hardware", "pc_hardware2", "pc_uso")
detalle_rango <- function(i) {
  f <- variables_revisar[sapply(variables_revisar, function(v) {
    escenarios[[v]][i] < min(datos[[v]]) || escenarios[[v]][i] > max(datos[[v]])
  })]
  if (length(f) == 0) return("")
  paste(sapply(f, function(v) {
    sprintf("%s=%.2f (rango observado %.2f a %.2f)", v, escenarios[[v]][i], min(datos[[v]]), max(datos[[v]]))
  }), collapse = "; ")
}
fuera_de_rango <- sapply(seq_len(nrow(escenarios)), detalle_rango)

intervalos <- data.frame(
  fabricante = escenarios$Fabricante,
  pc_uso = escenarios$pc_uso,
  duracion_predicha = ajuste,
  ic_media_inf = ajuste - t_crit * se_media,
  ic_media_sup = ajuste + t_crit * se_media,
  ancho_ic_media = 2 * t_crit * se_media,
  ic_individual_inf = ajuste - t_crit * se_individual,
  ic_individual_sup = ajuste + t_crit * se_individual,
  ancho_ic_individual = 2 * t_crit * se_individual,
  desv_error_individual = sd_individual,
  fuera_de_rango = fuera_de_rango)
intervalos[sapply(intervalos, is.numeric)] <- round(intervalos[sapply(intervalos, is.numeric)], 3)
write.csv(intervalos, file.path(dir_tablas, "escenarios_intervalos.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("\n== Intervalos del 95 % por fabricante ==\n"); print(intervalos, row.names = FALSE)

# 5. Aporte de cada variable frente al dispositivo promedio de los 1000 datos
grupo_termino <- c(pc_hardware = "Hardware", pc_hardware2 = "Hardware",
                   "log(antiguedad_bateria)" = "Antigüedad",
                   apps_c = "Apps y GPS", uso_gps = "Apps y GPS", "apps_c:uso_gps" = "Apps y GPS",
                   brillo_c = "Brillo y ahorro", ahorro_energia = "Brillo y ahorro",
                   "brillo_c:ahorro_energia" = "Brillo y ahorro",
                   pc_uso = "Uso de pantalla",
                   temp_c = "Temperatura", "I(temp_c^2)" = "Temperatura",
                   tipo_conectividad4G = "Conectividad", tipo_conectividad5G = "Conectividad",
                   bluetooth = "Otros", modo_oscuro = "Otros")
stopifnot(setequal(names(grupo_termino), setdiff(colnames(X_esc), "(Intercept)")))
grupos <- c("Hardware", "Antigüedad", "Apps y GPS", "Brillo y ahorro", "Uso de pantalla",
            "Temperatura", "Conectividad", "Otros")

media_x    <- colMeans(X_datos)
referencia <- sum(media_x * coef_modelo)
aporte_termino <- sweep(sweep(X_esc, 2, media_x), 2, coef_modelo, "*")[, names(grupo_termino)]
aportes <- sapply(grupos, function(g) rowSums(aporte_termino[, grupo_termino == g, drop = FALSE]))
rownames(aportes) <- escenarios$Fabricante
stopifnot(max(abs(rowSums(aportes) + referencia - ajuste)) < 1e-8)

tabla_aportes <- data.frame(fabricante = escenarios$Fabricante,
                            diferencia_vs_promedio = rowSums(aportes), aportes, check.names = FALSE)
tabla_aportes[-1] <- round(tabla_aportes[-1], 3)
write.csv(tabla_aportes, file.path(dir_tablas, "escenarios_aportes.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("\n== Duración del dispositivo promedio de los datos:", round(referencia, 2), "h ==\n")
cat("\n== Aporte de cada grupo de variables (h frente al promedio) ==\n"); print(tabla_aportes, row.names = FALSE)

# 6. Valor de cada fabricante frente al promedio de los datos, junto al aporte
variables_grupo <- list(Hardware = c("capacidad_bateria", "resolucion_pantalla"),
                        "Antigüedad" = "antiguedad_bateria",
                        "Apps y GPS" = c("apps_segundo_plano", "uso_gps"),
                        "Brillo y ahorro" = c("brillo_pantalla", "ahorro_energia"),
                        "Uso de pantalla" = c("tiempo_pantalla_encendida", "notificaciones"),
                        Temperatura = "temperatura_C",
                        Conectividad = "tipo_conectividad",
                        Otros = c("bluetooth", "modo_oscuro"))
describir <- function(v, i) {
  if (v == "tipo_conectividad") {
    return(sprintf("%s=%s (%.0f%% de los datos)", v, as.character(escenarios[[v]][i]),
                   100 * mean(datos[[v]] == escenarios[[v]][i])))
  }
  sprintf("%s=%s (promedio datos %s)", v, format(escenarios[[v]][i], trim = TRUE),
          format(round(mean(datos[[v]]), 2), trim = TRUE))
}
detalle_grupo <- function(g, i) paste(sapply(variables_grupo[[g]], describir, i = i), collapse = "; ")

detalle <- do.call(rbind, lapply(seq_len(nrow(escenarios)), function(i) {
  data.frame(fabricante = escenarios$Fabricante[i], grupo = grupos,
             valores_vs_promedio = sapply(grupos, detalle_grupo, i = i),
             aporte_h = round(aportes[i, grupos], 3), row.names = NULL)
}))
write.csv(detalle, file.path(dir_tablas, "escenarios_aportes_detalle.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# 7. Texto con las variables que más influyen
signo_h <- function(x) sprintf("%+.1f h", x)
lineas <- c(
  "VARIABLES QUE MÁS INFLUYEN EN LAS DIFERENCIAS ENTRE FABRICANTES (modelo C)",
  "",
  sprintf("Referencia: dispositivo promedio de los 1000 datos, duración %.1f h.", referencia),
  "Aporte de un grupo de variables = suma de coeficiente x (valor del fabricante - promedio de los datos), en horas.",
  "Positivo = suma horas de batería frente al promedio; negativo = resta horas.",
  "Apps, GPS y su interacción van juntas, igual que brillo, ahorro de energía y su interacción; temperatura incluye el término cuadrático.",
  "Lectura asociativa según el modelo, no una demostración de causa.",
  "")
for (i in seq_len(nrow(escenarios))) {
  orden <- order(-abs(aportes[i, ]))[1:3]
  lineas <- c(lineas,
    sprintf("%s: duración predicha %.1f h (%s frente al promedio). Intervalo individual 95 %%: [%.1f, %.1f] h.",
            escenarios$Fabricante[i], ajuste[i], signo_h(sum(aportes[i, ])),
            intervalos$ic_individual_inf[i], intervalos$ic_individual_sup[i]),
    "  Variables que más pesan:")
  for (k in seq_along(orden)) {
    g <- grupos[orden[k]]
    lineas <- c(lineas, sprintf("    %d. %s: %s  [%s]", k, g, signo_h(aportes[i, g]), detalle_grupo(g, i)))
  }
  if (nzchar(fuera_de_rango[i])) {
    lineas <- c(lineas, sprintf(
      "  ADVERTENCIA: fuera del rango observado en los datos: %s. La predicción es una extrapolación; mientras más lejos del rango, menos confiable, y los aportes asociados deben leerse con cautela.",
      fuera_de_rango[i]))
  }
  lineas <- c(lineas, "")
}

promedio_abs <- sort(colMeans(abs(aportes)), decreasing = TRUE)
lineas <- c(lineas, "RESUMEN: grupos de variables ordenados por aporte absoluto promedio entre los fabricantes (h)")
lineas <- c(lineas, sprintf("  %d. %s: %.1f h", seq_along(promedio_abs), names(promedio_abs), promedio_abs))
sin_extrapolar <- !nzchar(fuera_de_rango)
if (any(!sin_extrapolar) && any(sin_extrapolar)) {
  promedio_abs2 <- sort(colMeans(abs(aportes[sin_extrapolar, , drop = FALSE])), decreasing = TRUE)
  lineas <- c(lineas, "",
              sprintf("Mismo resumen sin los fabricantes marcados como extrapolación (%s):",
                      paste(escenarios$Fabricante[!sin_extrapolar], collapse = ", ")))
  lineas <- c(lineas, sprintf("  %d. %s: %.1f h", seq_along(promedio_abs2), names(promedio_abs2), promedio_abs2))
}
lineas <- c(lineas, "",
  sprintf("ANCHO DEL INTERVALO INDIVIDUAL: depende de pc_uso (varianza relativa = exp(%.3f + %.3f x pc_uso)); correlación entre pc_uso y el ancho: %.2f.",
          modelo$coef_aux[[1]], modelo$coef_aux[[2]], cor(intervalos$pc_uso, intervalos$ancho_ic_individual)),
  sprintf("Ancho entre %.1f h (%s) y %.1f h (%s).",
          min(intervalos$ancho_ic_individual), intervalos$fabricante[which.min(intervalos$ancho_ic_individual)],
          max(intervalos$ancho_ic_individual), intervalos$fabricante[which.max(intervalos$ancho_ic_individual)]))
con <- file(file.path(dir_tablas, "variables_influyentes.txt"), open = "w", encoding = "UTF-8")
writeLines(lineas, con)
close(con)
cat("\n", paste(lineas, collapse = "\n"), "\n", sep = "")

# 8. Gráficas
orden_fab <- escenarios$Fabricante[order(ajuste)]
etiqueta_fab <- ifelse(nzchar(fuera_de_rango), paste0(escenarios$Fabricante, "*"), escenarios$Fabricante)
names(etiqueta_fab) <- escenarios$Fabricante
nota_extrapolacion <- if (any(nzchar(fuera_de_rango))) "* Algún valor del escenario queda fuera del rango de los datos (ver variables_influyentes.txt)" else NULL

g_int <- data.frame(
  fabricante = factor(rep(intervalos$fabricante, 2), levels = orden_fab),
  inf = c(intervalos$ic_individual_inf, intervalos$ic_media_inf),
  sup = c(intervalos$ic_individual_sup, intervalos$ic_media_sup),
  intervalo = factor(rep(c("Individual (predicción)", "Media (confianza)"), each = nrow(intervalos)),
                     levels = c("Individual (predicción)", "Media (confianza)")),
  ajuste = rep(ajuste, 2))
grafica_intervalos <- ggplot(g_int, aes(y = fabricante)) +
  geom_linerange(aes(xmin = inf, xmax = sup, color = intervalo, linewidth = intervalo)) +
  geom_point(data = g_int[g_int$intervalo == "Media (confianza)", ], aes(x = ajuste), size = 2.2, color = "#1a1a1a") +
  scale_color_manual(values = c("Individual (predicción)" = "#56B4E9", "Media (confianza)" = "#0072B2"), name = NULL) +
  scale_linewidth_manual(values = c("Individual (predicción)" = 1.6, "Media (confianza)" = 3.6), name = NULL) +
  scale_y_discrete(labels = etiqueta_fab) +
  labs(title = "Duración de la batería por fabricante: intervalos del 95 %",
       subtitle = "Punto: duración predicha. Barra ancha: media; barra fina: dispositivo individual",
       x = "Horas", y = NULL, caption = nota_extrapolacion) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), legend.position = "bottom")
ggsave(file.path(dir_graficas, "07_intervalos_fabricantes.png"), grafica_intervalos, width = 8, height = 5.5, dpi = 150)

colores_grupo <- c(Hardware = "#0072B2", "Antigüedad" = "#E69F00", "Apps y GPS" = "#009E73",
                   "Brillo y ahorro" = "#D55E00", "Uso de pantalla" = "#CC79A7",
                   Temperatura = "#56B4E9", Conectividad = "#7B3294", Otros = "#8A8A8A")
g_apo <- data.frame(fabricante = factor(rep(escenarios$Fabricante, times = length(grupos)), levels = orden_fab),
                    grupo = factor(rep(grupos, each = nrow(escenarios)), levels = grupos),
                    aporte = as.vector(aportes))
g_tot <- data.frame(fabricante = factor(escenarios$Fabricante, levels = orden_fab), total = rowSums(aportes))
grafica_aportes <- ggplot(g_apo, aes(x = fabricante, y = aporte)) +
  geom_col(aes(fill = grupo), color = "white", linewidth = 0.4, width = 0.7) +
  geom_hline(yintercept = 0, color = "#555555", linewidth = 0.4) +
  geom_point(data = g_tot, aes(y = total), shape = 18, size = 3.2, color = "#1a1a1a") +
  scale_fill_manual(values = colores_grupo, name = NULL) +
  scale_x_discrete(labels = etiqueta_fab) +
  labs(title = "Aporte de cada grupo de variables frente al dispositivo promedio",
       subtitle = sprintf("Horas sobre o bajo el promedio de los datos (%.1f h). Rombo: diferencia total", referencia),
       x = NULL, y = "Aporte a la duración (h)", caption = nota_extrapolacion) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        legend.position = "bottom", axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(file.path(dir_graficas, "07_aportes_variables.png"), grafica_aportes, width = 9, height = 5.8, dpi = 150)

cat("\n== Archivos guardados en", dir_tablas, "y", dir_graficas, "==\n")