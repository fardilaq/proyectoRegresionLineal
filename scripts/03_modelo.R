# 1. Librerías, datos y categóricas
library(readxl)

ruta_xlsx  <- "datos/DatosProyectoRL_Android_202620.xlsx"
dir_tablas <- "resultados/tablas/modelo"

niveles <- list(
    color_dispositivo = c("Negro", "Blanco", "Azul"),
    tipo_conectividad = c("Wi-fi", "4G", "5G"),
    dia_semana        = c("lunes", "martes", "miercoles", "jueves",
                            "viernes", "sabado", "domingo")
    )
binarias <- c("uso_gps", "ahorro_energia", "bluetooth", "modo_oscuro")

preparar_categoricas <- function(df) {
  for (v in names(niveles)) {
    x <- trimws(as.character(df[[v]]))
    inesperados <- setdiff(unique(x), niveles[[v]])
    if (length(inesperados) > 0) {
      stop("Valores inesperados en '", v, "': ", paste(inesperados, collapse = ", "))
    }
    df[[v]] <- factor(x, levels = niveles[[v]], ordered = FALSE)
  }
  for (v in binarias) {
    if (!all(df[[v]] %in% c(0, 1))) stop("'", v, "' tiene valores distintos de 0/1")
  }
  df
}

datos      <- preparar_categoricas(as.data.frame(read_excel(ruta_xlsx, sheet = "Datos")))
escenarios <- preparar_categoricas(as.data.frame(read_excel(ruta_xlsx, sheet = "Escenarios")))

# 2. Variables centradas
medias_centrado <- c(brillo_pantalla = mean(datos$brillo_pantalla),
                     apps_segundo_plano = mean(datos$apps_segundo_plano),
                     temperatura_C = mean(datos$temperatura_C))

centrar <- function(df) {
  df$brillo_c <- df$brillo_pantalla - medias_centrado[["brillo_pantalla"]]
  df$apps_c   <- df$apps_segundo_plano - medias_centrado[["apps_segundo_plano"]]
  df$temp_c   <- df$temperatura_C - medias_centrado[["temperatura_C"]]
  df
}

datos      <- centrar(datos)
escenarios <- centrar(escenarios)

cat("\n== Medias usadas para centrar (calculadas con los 1000 datos) ==\n")
print(round(medias_centrado, 4))
write.csv(data.frame(variable = names(medias_centrado), media = unname(medias_centrado)),
          file.path(dir_tablas, "modelo_medias_centrado.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# 3. Modelo propuesto
formula_modelo <- duracion_bateria ~ capacidad_bateria + log(antiguedad_bateria) +
  resolucion_pantalla + apps_c * uso_gps + brillo_c * ahorro_energia +
  brillo_c * bluetooth + modo_oscuro + tiempo_pantalla_encendida +
  notificaciones + temp_c + I(temp_c^2) + tipo_conectividad

modelo <- lm(formula_modelo, data = datos)

cat("\n== Modelo propuesto ==\n")
print(summary(modelo))

# 4. Tabla de coeficientes y medidas de ajuste
resumen   <- summary(modelo)
intervalo <- confint(modelo)

tabla_coef <- data.frame(
    termino = names(coef(modelo)),
    estimado = coef(modelo),
    error_est = resumen$coefficients[, 2],
    t = resumen$coefficients[, 3],
    p_valor = resumen$coefficients[, 4],
    ic95_inf = intervalo[, 1],
    ic95_sup = intervalo[, 2],
    row.names = NULL)
tabla_coef[-1] <- signif(tabla_coef[-1], 4)
write.csv(tabla_coef, file.path(dir_tablas, "modelo_coeficientes.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

estad_f <- resumen$fstatistic
tabla_ajuste <- data.frame(
    n = nobs(modelo), parametros = length(coef(modelo)), gl_residuales = df.residual(modelo),
    R2 = resumen$r.squared, R2_ajustado = resumen$adj.r.squared, sigma = resumen$sigma,
    F_ep = estad_f[["value"]],
    F_critico = qf(0.95, estad_f[["numdf"]], estad_f[["dendf"]]),
    p_valor = pf(estad_f[["value"]], estad_f[["numdf"]], estad_f[["dendf"]], lower.tail = FALSE),
    AIC = AIC(modelo), BIC = BIC(modelo))
tabla_ajuste <- signif(tabla_ajuste, 5)
cat("\n== Medidas de ajuste ==\n")
print(tabla_ajuste, row.names = FALSE)
write.csv(tabla_ajuste, file.path(dir_tablas, "modelo_sigGlobal.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# 5. Efectos condicionales para interpretar interacciones
combinar <- function(nombre, pesos) {
  b <- coef(modelo)
  w <- setNames(numeric(length(b)), names(b))
  w[names(pesos)] <- pesos
  est <- sum(w * b)
  ee  <- sqrt(drop(t(w) %*% vcov(modelo) %*% w))
  gl  <- df.residual(modelo)
  data.frame(efecto = nombre, estimado = est, error_est = ee,
             ic95_inf = est - qt(0.975, gl) * ee, ic95_sup = est + qt(0.975, gl) * ee,
             p_valor = 2 * pt(-abs(est / ee), gl))
}

tabla_efectos <- rbind(
  combinar("brillo (+1 punto) con ahorro = 0 y bluetooth = 0", c(brillo_c = 1)),
  combinar("brillo (+1 punto) con ahorro = 1 y bluetooth = 0",
           c(brillo_c = 1, "brillo_c:ahorro_energia" = 1)),
  combinar("brillo (+1 punto) con ahorro = 0 y bluetooth = 1",
           c(brillo_c = 1, "brillo_c:bluetooth" = 1)),
  combinar("apps (+1 app) con GPS = 0", c(apps_c = 1)),
  combinar("apps (+1 app) con GPS = 1", c(apps_c = 1, "apps_c:uso_gps" = 1)),
  combinar("GPS = 1 frente a 0 (apps promedio)", c(uso_gps = 1)),
  combinar("ahorro = 1 frente a 0 (brillo promedio)", c(ahorro_energia = 1)),
  combinar("bluetooth = 1 frente a 0 (brillo promedio)", c(bluetooth = 1)),
  combinar("5G frente a 4G", c(tipo_conectividad5G = 1, tipo_conectividad4G = -1)))
tabla_efectos[-1] <- signif(tabla_efectos[-1], 4)
cat("\n== Efectos condicionales ==\n")
print(tabla_efectos, row.names = FALSE)
write.csv(tabla_efectos, file.path(dir_tablas, "modelo_efectos_condicionales.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

temp_optima <- medias_centrado[["temperatura_C"]] -
  coef(modelo)[["temp_c"]] / (2 * coef(modelo)[["I(temp_c^2)"]])
cat("\nTemperatura con duración máxima estimada (°C):", round(temp_optima, 2), "\n")

# 6. Variables excluidas y codificación de la conectividad
adicionales <- list(
  contactos_guardados = "contactos_guardados",
  volumen_timbre      = "volumen_timbre",
  color_dispositivo   = "color_dispositivo",
  dia_semana          = "dia_semana",
  las_cuatro          = c("contactos_guardados", "volumen_timbre",
                          "color_dispositivo", "dia_semana"))

pruebas_f <- do.call(rbind, lapply(names(adicionales), function(n) {
  ampliado <- update(modelo, as.formula(paste(". ~ . +", paste(adicionales[[n]], collapse = " + "))))
  a <- anova(modelo, ampliado)
  data.frame(prueba = paste("Agregar", n), gl = a$Df[2], F = a$F[2], p_valor = a$`Pr(>F)`[2])
}))

datos_prueba <- datos
datos_prueba$conectividad_num <- as.integer(datos_prueba$tipo_conectividad) - 1
modelo_num <- update(modelo, . ~ . - tipo_conectividad + conectividad_num, data = datos_prueba)
a_num <- anova(modelo_num, modelo)
pruebas_f <- rbind(pruebas_f,
                   data.frame(prueba = "Conectividad 0/1/2 frente a 2 indicadoras",
                              gl = a_num$Df[2], F = a_num$F[2], p_valor = a_num$`Pr(>F)`[2]))
pruebas_f[-1] <- signif(pruebas_f[-1], 4)
cat("\n== Pruebas F parciales ==\n")
print(pruebas_f, row.names = FALSE)
write.csv(pruebas_f, file.path(dir_tablas, "modelo_pruebas_f.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

modelo_completo <- update(modelo, as.formula(paste(". ~ . +", paste(adicionales$las_cuatro, collapse = " + "))))
comparacion <- data.frame(
  modelo = c("Propuesto", "Completo (con las cuatro variables excluidas)"),
  parametros = c(length(coef(modelo)), length(coef(modelo_completo))),
  R2_ajustado = c(summary(modelo)$adj.r.squared, summary(modelo_completo)$adj.r.squared),
  AIC = c(AIC(modelo), AIC(modelo_completo)),
  BIC = c(BIC(modelo), BIC(modelo_completo)))
comparacion[-1] <- signif(comparacion[-1], 5)
cat("\n== Modelo propuesto frente a modelo completo ==\n")
print(comparacion, row.names = FALSE)
write.csv(comparacion, file.path(dir_tablas, "modelo_comparacion_completo.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# 7. Matriz de datos del modelo
matriz_x <- model.matrix(modelo)
nombres_matriz <- c("(Intercept)" = "intercepto",
                    "log(antiguedad_bateria)" = "log_antiguedad",
                    "I(temp_c^2)" = "temp_c_cuadrado",
                    "tipo_conectividad4G" = "conectividad_4G",
                    "tipo_conectividad5G" = "conectividad_5G",
                    "apps_c:uso_gps" = "apps_c_x_gps",
                    "brillo_c:ahorro_energia" = "brillo_c_x_ahorro",
                    "brillo_c:bluetooth" = "brillo_c_x_bluetooth")
nombres_cols <- colnames(matriz_x)
renombrar <- nombres_cols %in% names(nombres_matriz)
nombres_cols[renombrar] <- unname(nombres_matriz[nombres_cols[renombrar]])
colnames(matriz_x) <- nombres_cols

matriz_modelo <- data.frame(duracion_bateria = datos$duracion_bateria, matriz_x,
                            row.names = NULL, check.names = FALSE)
cat("\n== Matriz de datos del modelo ==\n")
cat("Filas:", nrow(matriz_modelo), " Columnas:", ncol(matriz_modelo), "\n")
print(head(matriz_modelo, 5), row.names = FALSE)
write.csv(matriz_modelo, file.path(dir_tablas, "modelo_matriz_datos.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# 8. Interpretación de los coeficientes
b <- coef(modelo)
sube <- function(x) ifelse(x >= 0, "aumenta", "reduce")

texto <- c(
  "(Intercept)" = sprintf("Duración esperada (%.2f h) de un dispositivo de referencia: Wi-fi, sin GPS, ahorro, bluetooth ni modo oscuro, con apps, brillo y temperatura en su promedio, antigüedad de 1 mes y capacidad, resolución, tiempo de pantalla y notificaciones iguales a 0. Es una extrapolación sin sentido práctico; solo ancla la ecuación.",
                          b[["(Intercept)"]]),
  "capacidad_bateria" = sprintf("Con lo demás constante, cada 100 mAh adicionales de capacidad %s la duración esperada en %.2f h (%.4f h por mAh).",
                                sube(b[["capacidad_bateria"]]), abs(b[["capacidad_bateria"]]) * 100, abs(b[["capacidad_bateria"]])),
  "log(antiguedad_bateria)" = sprintf("Con lo demás constante, duplicar la antigüedad de la batería (por ejemplo, de 6 a 12 meses o de 18 a 36) %s la duración esperada en %.2f h; el efecto por mes es mayor al inicio y se estabiliza después.",
                                      sube(b[["log(antiguedad_bateria)"]]), abs(b[["log(antiguedad_bateria)"]]) * log(2)),
  "resolucion_pantalla" = sprintf("Con la capacidad y lo demás constante, cada 100 PPI adicionales %s la duración esperada en %.2f h (%.4f h por PPI). Su estimación está afectada por la colinealidad con la capacidad (numeral 4).",
                                  sube(b[["resolucion_pantalla"]]), abs(b[["resolucion_pantalla"]]) * 100, abs(b[["resolucion_pantalla"]])),
  "apps_c" = sprintf("Sin GPS, cada aplicación adicional en segundo plano %s la duración esperada en %.3f h.",
                     sube(b[["apps_c"]]), abs(b[["apps_c"]])),
  "uso_gps" = sprintf("Activar el GPS %s la duración esperada en %.2f h, con el número de apps en su promedio (%.1f).",
                      sube(b[["uso_gps"]]), abs(b[["uso_gps"]]), medias_centrado[["apps_segundo_plano"]]),
  "brillo_c" = sprintf("Sin ahorro de energía ni bluetooth, cada punto porcentual de brillo %s la duración esperada en %.4f h (%.2f h por cada 10 puntos).",
                       sube(b[["brillo_c"]]), abs(b[["brillo_c"]]), abs(b[["brillo_c"]]) * 10),
  "ahorro_energia" = sprintf("Activar el ahorro de energía %s la duración esperada en %.2f h, con el brillo en su promedio (%.1f %%).",
                             sube(b[["ahorro_energia"]]), abs(b[["ahorro_energia"]]), medias_centrado[["brillo_pantalla"]]),
  "bluetooth" = sprintf("Activar el bluetooth %s la duración esperada en %.2f h, con el brillo en su promedio.",
                        sube(b[["bluetooth"]]), abs(b[["bluetooth"]])),
  "modo_oscuro" = sprintf("Activar el modo oscuro %s la duración esperada en %.2f h.",
                          sube(b[["modo_oscuro"]]), abs(b[["modo_oscuro"]])),
  "tiempo_pantalla_encendida" = sprintf("Cada hora adicional con la pantalla encendida %s la duración esperada en %.3f h. Su estimación está afectada por la colinealidad con las notificaciones (numeral 4).",
                                        sube(b[["tiempo_pantalla_encendida"]]), abs(b[["tiempo_pantalla_encendida"]])),
  "notificaciones" = sprintf("Cada notificación adicional por hora %s la duración esperada en %.3f h (%.2f h por cada 10). Su estimación está afectada por la colinealidad con el tiempo de pantalla (numeral 4).",
                             sube(b[["notificaciones"]]), abs(b[["notificaciones"]]), abs(b[["notificaciones"]]) * 10),
  "temp_c" = sprintf("Con la temperatura promedio (%.1f °C), cada grado adicional %s la duración esperada en %.3f h. Como la temperatura tiene término cuadrático, este efecto cambia con la temperatura y se lee junto con el coeficiente cuadrático.",
                     medias_centrado[["temperatura_C"]], sube(b[["temp_c"]]), abs(b[["temp_c"]])),
  "I(temp_c^2)" = sprintf("Término de curvatura: parábola %s. La duración máxima se estima a %.2f °C y, a 10 °C de distancia de ese punto, la duración esperada queda %.2f h por debajo del máximo (%.4f h por °C²).",
                          ifelse(b[["I(temp_c^2)"]] < 0, "cóncava (hay un máximo)", "convexa (hay un mínimo)"),
                          temp_optima, abs(b[["I(temp_c^2)"]]) * 100, abs(b[["I(temp_c^2)"]])),
  "tipo_conectividad4G" = sprintf("Frente a Wi-fi, usar 4G %s la duración esperada en %.2f h.",
                                  sube(b[["tipo_conectividad4G"]]), abs(b[["tipo_conectividad4G"]])),
  "tipo_conectividad5G" = sprintf("Frente a Wi-fi, usar 5G %s la duración esperada en %.2f h (%.2f h %s que 4G).",
                                  sube(b[["tipo_conectividad5G"]]), abs(b[["tipo_conectividad5G"]]),
                                  abs(b[["tipo_conectividad5G"]] - b[["tipo_conectividad4G"]]),
                                  ifelse(b[["tipo_conectividad5G"]] - b[["tipo_conectividad4G"]] < 0, "menos", "más")),
  "apps_c:uso_gps" = sprintf("Interacción: con GPS activo, el efecto de cada aplicación adicional en segundo plano pasa de %+.3f h (sin GPS) a %+.3f h (con GPS); el coeficiente es esa diferencia.",
                             b[["apps_c"]], b[["apps_c"]] + b[["apps_c:uso_gps"]]),
  "brillo_c:ahorro_energia" = sprintf("Interacción: con ahorro de energía activo, el efecto de cada punto de brillo pasa de %+.4f h a %+.4f h; el coeficiente es esa diferencia.",
                                      b[["brillo_c"]], b[["brillo_c"]] + b[["brillo_c:ahorro_energia"]]),
  "brillo_c:bluetooth" = sprintf("Interacción: con bluetooth activo, el efecto de cada punto de brillo pasa de %+.4f h a %+.4f h; el coeficiente es esa diferencia.",
                                 b[["brillo_c"]], b[["brillo_c"]] + b[["brillo_c:bluetooth"]]))

unidades <- c("(Intercept)" = "h", "capacidad_bateria" = "h por mAh",
              "log(antiguedad_bateria)" = "h por unidad de ln(meses)",
              "resolucion_pantalla" = "h por PPI", "apps_c" = "h por app",
              "uso_gps" = "h", "brillo_c" = "h por punto de brillo", "ahorro_energia" = "h",
              "bluetooth" = "h", "modo_oscuro" = "h",
              "tiempo_pantalla_encendida" = "h por hora de pantalla",
              "notificaciones" = "h por notificación por hora",
              "temp_c" = "h por °C", "I(temp_c^2)" = "h por °C²",
              "tipo_conectividad4G" = "h", "tipo_conectividad5G" = "h",
              "apps_c:uso_gps" = "h por app (diferencia con GPS)",
              "brillo_c:ahorro_energia" = "h por punto de brillo (diferencia con ahorro)",
              "brillo_c:bluetooth" = "h por punto de brillo (diferencia con bluetooth)")

stopifnot(all(names(b) %in% names(texto)), all(names(b) %in% names(unidades)))

interpretacion <- data.frame(
  termino = names(b),
  estimado = signif(unname(b), 4),
  unidad = unname(unidades[names(b)]),
  p_valor = signif(unname(resumen$coefficients[, 4]), 3),
  significativo = ifelse(resumen$coefficients[, 4] < 0.05, "Sí", "No"),
  interpretacion = unname(texto[names(b)]),
  row.names = NULL)
cat("\n== Interpretación de los coeficientes ==\n")
print(interpretacion[, 1:5], row.names = FALSE)
write.csv(interpretacion, file.path(dir_tablas, "modelo_interpretacion_coeficientes.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# 9. Variables incluidas y excluidas
prueba_f <- function(reducido, completo) {
  a <- anova(reducido, completo)
  c(gl = a$Df[2], F = a$F[2], p_valor = a$`Pr(>F)`[2])
}
quitar <- function(terminos) {
  prueba_f(update(modelo, as.formula(paste(". ~ . -", paste(terminos, collapse = " - ")))), modelo)
}
agregar <- function(variable) {
  prueba_f(modelo, update(modelo, as.formula(paste(". ~ . +", variable))))
}

incluidas <- data.frame(
  variable = c("capacidad_bateria", "antiguedad_bateria", "resolucion_pantalla",
               "apps_segundo_plano", "uso_gps", "brillo_pantalla", "ahorro_energia",
               "bluetooth", "modo_oscuro", "tiempo_pantalla_encendida", "notificaciones",
               "temperatura_C", "tipo_conectividad"),
  forma_en_modelo = c("Lineal", "Logarítmica", "Lineal", "Lineal, centrada",
                      "0/1", "Lineal, centrada", "0/1", "0/1", "0/1", "Lineal", "Lineal",
                      "Cuadrática, centrada", "2 indicadoras (4G, 5G; referencia Wi-fi)"),
  terminos = c("capacidad_bateria", "log(antiguedad_bateria)", "resolucion_pantalla",
               "apps_c; apps_c:uso_gps", "uso_gps; apps_c:uso_gps",
               "brillo_c; brillo_c:ahorro_energia; brillo_c:bluetooth",
               "ahorro_energia; brillo_c:ahorro_energia",
               "bluetooth; brillo_c:bluetooth", "modo_oscuro",
               "tiempo_pantalla_encendida", "notificaciones",
               "temp_c; I(temp_c^2)", "tipo_conectividad"),
  argumento = c(
    "Determinante físico de la duración: más capacidad, más horas disponibles.",
    "Variable de hardware. El enunciado indica degradación acelerada al inicio y estable después; la forma logarítmica lo recoge y ajusta mejor que la lineal en el bivariado.",
    "Variable de hardware que Google identifica como relevante. Se conserva sin eliminarla aunque tiene colinealidad con la capacidad; el tratamiento va en el numeral 4.",
    "Factor de uso. Participa en la interacción con GPS que menciona Google.",
    "Factor de uso. Participa en la interacción con las apps en segundo plano que menciona Google.",
    "Factor de uso. Participa en la interacción con el ahorro de energía que menciona Google y en la regla del numeral 6 con bluetooth.",
    "Configuración del sistema operativo con el mayor efecto estimado; modera el efecto del brillo.",
    "Se conserva para contrastar la regla de Google del numeral 6 (efecto del brillo distinto con bluetooth); además tiene efecto principal significativo.",
    "Configuración de pantalla con efecto principal significativo.",
    "Factor de uso con una de las mayores asociaciones con la duración; colineal con las notificaciones (numeral 4).",
    "Factor de uso con efecto significativo; colineal con el tiempo de pantalla (numeral 4).",
    "El enunciado indica un rango intermedio óptimo de temperatura; se modela con término cuadrático.",
    "Efecto significativo y la forma con indicadoras no impone pasos iguales entre Wi-fi, 4G y 5G."),
  tipo_prueba = "Quitar sus términos",
  stringsAsFactors = FALSE)

terminos_quitar <- list(
  capacidad_bateria = "capacidad_bateria",
  antiguedad_bateria = "log(antiguedad_bateria)",
  resolucion_pantalla = "resolucion_pantalla",
  apps_segundo_plano = c("apps_c", "apps_c:uso_gps"),
  uso_gps = c("uso_gps", "apps_c:uso_gps"),
  brillo_pantalla = c("brillo_c", "brillo_c:ahorro_energia", "brillo_c:bluetooth"),
  ahorro_energia = c("ahorro_energia", "brillo_c:ahorro_energia"),
  bluetooth = c("bluetooth", "brillo_c:bluetooth"),
  modo_oscuro = "modo_oscuro",
  tiempo_pantalla_encendida = "tiempo_pantalla_encendida",
  notificaciones = "notificaciones",
  temperatura_C = c("temp_c", "I(temp_c^2)"),
  tipo_conectividad = "tipo_conectividad")

pruebas_incluidas <- t(sapply(incluidas$variable, function(v) quitar(terminos_quitar[[v]])))

excluidas <- data.frame(
  variable = c("contactos_guardados", "volumen_timbre", "color_dispositivo", "dia_semana"),
  forma_en_modelo = "No entra",
  terminos = "-",
  argumento = c(
    "No hay un mecanismo claro por el cual el número de contactos gaste batería, y en el bivariado no mostró relación con la duración. Agregarla no mejora el modelo.",
    "El volumen configurado para las llamadas no tiene un mecanismo claro de consumo relevante y no mostró relación en el bivariado. Agregarla no mejora el modelo.",
    "Es un atributo estético de la carcasa. Sin diferencias en la duración en el bivariado ni en el modelo.",
    "No hay razón física para que el día de la medición afecte la duración y no hubo diferencias en el bivariado. Con 6 indicadoras costaría 6 parámetros sin aporte."),
  tipo_prueba = "Agregarla al modelo",
  stringsAsFactors = FALSE)

pruebas_excluidas <- t(sapply(excluidas$variable, agregar))

tabla_inclusion <- rbind(
  data.frame(variable = incluidas$variable, decision = "Incluida",
             incluidas[c("forma_en_modelo", "terminos")], pruebas_incluidas,
             incluidas[c("tipo_prueba", "argumento")], row.names = NULL),
  data.frame(variable = excluidas$variable, decision = "Excluida",
             excluidas[c("forma_en_modelo", "terminos")], pruebas_excluidas,
             excluidas[c("tipo_prueba", "argumento")], row.names = NULL))
tabla_inclusion[c("F", "p_valor")] <- signif(tabla_inclusion[c("F", "p_valor")], 4)

cat("\n== Inclusión y exclusión de variables ==\n")
print(tabla_inclusion[, c("variable", "decision", "gl", "F", "p_valor", "tipo_prueba")], row.names = FALSE)
write.csv(tabla_inclusion, file.path(dir_tablas, "modelo_inclusion_exclusion.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")