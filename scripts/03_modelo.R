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