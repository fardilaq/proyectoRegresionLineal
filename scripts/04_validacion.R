# 04_validacion.R
# Punto 4: supuestos del término de error en el modelo de regresión lineal por MCO.
# Solo se usan las pruebas y métodos vistos en el curso:
#
#   Supuesto            Gráfica                Prueba                       Corrección
#   ------------------  ---------------------  ---------------------------  -------------------------------
#   Multicolinealidad   (correlación de pares) VIF                          PCA (componentes que aportan, prueba F)
#   Normalidad          Histograma, Q-Q, P-P   Shapiro, Chi cuadrado de     (si no se cumple, se revisa
#                                              Pearson, Kolmogorov-Smirnov   tras corregir lo demás)
#   Insesgados          e vs x                 RESET de Ramsey              (agregar términos si rechaza)
#   Homocedasticidad    e^2 vs x               Breusch-Pagan                MCP/WLS con pesos 1/sigma2_hat (sigma2 en función de pc_uso)
#   Independencia       e vs tiempo            Durbin-Watson                Cochrane-Orcutt (solo si rechaza)
#
# Orden del script:
#   0. Datos y modelo inicial (el del punto 3).
#   1. Diagnóstico del modelo inicial.
#   2. Corrección de la multicolinealidad (PCA) y nueva revisión de supuestos.
#   3. Corrección de la heterocedasticidad (WLS) y revisión de supuestos del modelo final.
#   4. Comparación entre modelo inicial y modelo corregido.
#
# Uso: desde la raíz del repo (directorio de trabajo = raíz),
#   source("scripts/04_validacion.R")
# Salidas: resultados/tablas/validacion/*.csv, resultados/graficas/validacion/*.png
#          y salida_consola_punto4.txt (copia de toda la consola).
# Si el script se interrumpe por un error, ejecute sink() para cerrar el archivo de consola.

# ---------------------------------------------------------------------------
# 0. Paquetes, parámetros, datos y modelo inicial
# ---------------------------------------------------------------------------
paquetes <- c("readxl", "ggplot2", "lmtest", "nortest")
faltan <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]
if (length(faltan) > 0) install.packages(faltan)
invisible(lapply(paquetes, library, character.only = TRUE))

alpha     <- 0.05   # nivel de significancia de todas las pruebas
umbral_vif <- 10    # VIF >= umbral indica multicolinealidad (regla usual; cámbielo a 5 si su profesor la usa)

ruta_xlsx    <- "datos/DatosProyectoRL_Android_202620.xlsx"
dir_tablas   <- "resultados/tablas/validacion"
dir_graficas <- "resultados/graficas/validacion"
dir.create(dir_tablas,   recursive = TRUE, showWarnings = FALSE)
dir.create(dir_graficas, recursive = TRUE, showWarnings = FALSE)
sink(file.path(dir_tablas, "salida_consola_punto4.txt"), split = TRUE)

AZUL <- "#4C72B0"; ROJO <- "#C44E52"

# Mismas categorías y conversión que en los scripts 01-03
niveles <- list(
  color_dispositivo = c("Negro", "Blanco", "Azul"),
  tipo_conectividad = c("Wi-fi", "4G", "5G"),
  dia_semana        = c("lunes", "martes", "miercoles", "jueves",
                        "viernes", "sabado", "domingo"))
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

datos <- preparar_categoricas(as.data.frame(read_excel(ruta_xlsx, sheet = "Datos")))
n <- nrow(datos)

# Variables centradas (idéntico al script 03)
medias_centrado <- c(brillo_pantalla = mean(datos$brillo_pantalla),
                     apps_segundo_plano = mean(datos$apps_segundo_plano),
                     temperatura_C = mean(datos$temperatura_C))
datos$brillo_c <- datos$brillo_pantalla    - medias_centrado[["brillo_pantalla"]]
datos$apps_c   <- datos$apps_segundo_plano - medias_centrado[["apps_segundo_plano"]]
datos$temp_c   <- datos$temperatura_C      - medias_centrado[["temperatura_C"]]

# MODELO INICIAL (el del punto 3, sin cambios)
formula_modelo <- duracion_bateria ~ capacidad_bateria + log(antiguedad_bateria) +
  resolucion_pantalla + apps_c * uso_gps + brillo_c * ahorro_energia +
  brillo_c * bluetooth + modo_oscuro + tiempo_pantalla_encendida +
  notificaciones + temp_c + I(temp_c^2) + tipo_conectividad

modelo <- lm(formula_modelo, data = datos)
cat("\n== MODELO INICIAL (punto 3) ==\n")
print(summary(modelo))

# Matriz de regresoras (sin intercepto) con nombres cortos, para cualquier modelo lm
nombres_cortos <- c("log(antiguedad_bateria)" = "log_antiguedad",
                    "I(temp_c^2)" = "temp_c_cuadrado",
                    "tipo_conectividad4G" = "conectividad_4G",
                    "tipo_conectividad5G" = "conectividad_5G",
                    "apps_c:uso_gps" = "apps_c_x_gps",
                    "brillo_c:ahorro_energia" = "brillo_c_x_ahorro",
                    "brillo_c:bluetooth" = "brillo_c_x_bluetooth")
matriz_corta <- function(m) {
  Xm <- model.matrix(m)[, -1, drop = FALSE]
  nm <- colnames(Xm)
  nm[nm %in% names(nombres_cortos)] <- unname(nombres_cortos[nm[nm %in% names(nombres_cortos)]])
  colnames(Xm) <- nm
  Xm
}
X_ini <- matriz_corta(modelo)

# Variables sobre las que se grafican los residuos (las numéricas del modelo)
vars_graf <- c("capacidad_bateria", "antiguedad_bateria", "resolucion_pantalla",
               "apps_segundo_plano", "brillo_pantalla", "tiempo_pantalla_encendida",
               "notificaciones", "temperatura_C")

# ---------------------------------------------------------------------------
# Funciones de apoyo
# ---------------------------------------------------------------------------
veredicto <- function(p) {
  ifelse(p < alpha, "se rechaza H0 -> HAY problema", "no se rechaza H0 -> no hay evidencia de problema")
}

registro <- data.frame()
registrar <- function(modelo_nombre, supuesto, prueba, est, p) {
  registro <<- rbind(registro, data.frame(
    modelo = modelo_nombre, supuesto = supuesto, prueba = prueba,
    estadistico = round(unname(est), 4), p_valor = signif(p, 3),
    decision = ifelse(p < alpha, "Se rechaza H0 (problema)", "No se rechaza H0"),
    stringsAsFactors = FALSE))
}

# VIF a partir de la matriz de correlación de las regresoras (sirve con interacciones y
# términos cuadráticos). Con pesos w usa la correlación ponderada.
vif_manual <- function(Xm, w = NULL) {
  R <- if (is.null(w)) cor(Xm) else cov.wt(Xm, wt = w / sum(w), cor = TRUE)$cor
  setNames(diag(solve(R)), colnames(Xm))
}

# Todas las pruebas de supuestos para un conjunto de residuos `res`.
#   Xm        : matriz de regresoras (para Breusch-Pagan)
#   modelo_lm : modelo lm para el RESET (NULL si no aplica)
evaluar_supuestos <- function(nombre, res, Xm, modelo_lm = NULL) {
  cat("\n---------- Supuestos:", nombre, "----------\n")
  res <- as.numeric(res)

  cat("\n[Normalidad] H0: los errores son normales\n")
  sw <- shapiro.test(res); print(sw)
  pe <- pearson.test(res); print(pe)
  ks <- ks.test(as.numeric(scale(res)), "pnorm"); print(ks)
  registrar(nombre, "Normalidad", "Shapiro-Wilk", sw$statistic, sw$p.value)
  registrar(nombre, "Normalidad", "Chi cuadrado de Pearson", pe$statistic, pe$p.value)
  registrar(nombre, "Normalidad", "Kolmogorov-Smirnov", ks$statistic, ks$p.value)
  normal_ok <- all(c(sw$p.value, pe$p.value, ks$p.value) >= alpha)
  cat("  >> Normalidad:", ifelse(normal_ok,
      "ninguna de las tres pruebas rechaza -> se cumple.",
      "al menos una prueba rechaza -> NO se cumple (regla del curso)."), "\n")

  if (!is.null(modelo_lm)) {
    cat("\n[Insesgados / especificación] H0: la especificación es correcta\n")
    rs <- resettest(modelo_lm, power = 2:3, type = "fitted"); print(rs)
    registrar(nombre, "Especificación", "RESET de Ramsey", rs$statistic, rs$p.value)
    cat("  >> RESET:", veredicto(rs$p.value), "\n")
  }

  dd  <- data.frame(res = res, Xm)
  aux <- lm(res ~ 1, data = dd)

  cat("\n[Homocedasticidad] H0: la varianza del error es constante\n")
  bp <- bptest(aux, varformula = reformulate(colnames(Xm)), data = dd); print(bp)
  registrar(nombre, "Homocedasticidad", "Breusch-Pagan", bp$statistic, bp$p.value)
  cat("  >> Breusch-Pagan:", veredicto(bp$p.value), "\n")

  cat("\n[Independencia] H0: no hay autocorrelación de primer orden\n")
  dw <- dwtest(aux, alternative = "two.sided"); print(dw)
  registrar(nombre, "Independencia", "Durbin-Watson", dw$statistic, dw$p.value)
  cat("  >> Durbin-Watson:", veredicto(dw$p.value), "\n")
  if (dw$p.value < alpha) {
    cat("     Habría autocorrelación: según el curso se corregiría con Cochrane-Orcutt.\n")
  }
  invisible(NULL)
}

# Gráficas de normalidad: histograma, Q-Q plot y P-P plot
graf_normalidad <- function(res, titulo, archivo) {
  res <- as.numeric(res)
  png(file.path(dir_graficas, archivo), width = 3000, height = 1100, res = 220)
  par(mfrow = c(1, 3), mar = c(4.5, 4.5, 3, 1))
  hist(res, breaks = 30, freq = FALSE, col = AZUL, border = "white",
       main = paste("Histograma -", titulo), xlab = "Residuo")
  curve(dnorm(x, mean(res), sd(res)), add = TRUE, col = ROJO, lwd = 2)
  qqnorm(res, pch = 16, cex = 0.5, col = rgb(0.3, 0.45, 0.7, 0.5),
         main = paste("Q-Q plot -", titulo)); qqline(res, col = ROJO, lwd = 2)
  teorica <- pnorm(sort(res), mean(res), sd(res))
  empirica <- (seq_along(res) - 0.5) / length(res)
  plot(teorica, empirica, pch = 16, cex = 0.5, col = rgb(0.3, 0.45, 0.7, 0.5),
       xlab = "Probabilidad teórica", ylab = "Probabilidad empírica",
       main = paste("P-P plot -", titulo)); abline(0, 1, col = ROJO, lwd = 2)
  dev.off()
}

# e vs x (potencia = 1) o e^2 vs x (potencia = 2) para las variables numéricas
graf_vs_x <- function(res, potencia, titulo, archivo) {
  res <- as.numeric(res)
  largo <- do.call(rbind, lapply(vars_graf, function(v)
    data.frame(variable = v, x = datos[[v]], r = res^potencia)))
  largo$variable <- factor(largo$variable, levels = vars_graf)
  g <- ggplot(largo, aes(x, r)) +
    geom_point(alpha = 0.25, color = AZUL, size = 0.7) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = ROJO) +
    facet_wrap(~ variable, scales = "free_x", ncol = 4) +
    labs(title = titulo, x = NULL, y = ifelse(potencia == 1, "e", "e^2")) +
    theme_minimal()
  if (potencia == 1) g <- g + geom_hline(yintercept = 0, linetype = "dashed")
  ggsave(file.path(dir_graficas, archivo), g, width = 13, height = 6.5, dpi = 300)
}

# e vs tiempo (los datos no traen fecha: el "tiempo" es el orden de las filas)
graf_vs_tiempo <- function(res, titulo, archivo) {
  res <- as.numeric(res)
  png(file.path(dir_graficas, archivo), width = 2400, height = 1000, res = 220)
  par(mar = c(4.5, 4.5, 3, 1))
  plot(seq_along(res), res, pch = 16, cex = 0.5, col = rgb(0.3, 0.45, 0.7, 0.6),
       xlab = "Orden de la observación (tiempo)", ylab = "e", main = titulo)
  abline(h = 0, col = ROJO, lty = 2); lines(lowess(seq_along(res), res), col = ROJO, lwd = 2)
  dev.off()
}

# ---------------------------------------------------------------------------
# 1. DIAGNÓSTICO DEL MODELO INICIAL
# ---------------------------------------------------------------------------
cat("\n\n========== 1. DIAGNÓSTICO DEL MODELO INICIAL ==========\n")

# 1.1 Multicolinealidad: VIF -------------------------------------------------
cat("\n-- 1.1 Multicolinealidad: VIF --\n")
vif_inicial <- vif_manual(X_ini)
tabla_vif_ini <- data.frame(variable = names(vif_inicial), VIF = round(vif_inicial, 2),
                            problema = vif_inicial >= umbral_vif, row.names = NULL)
print(tabla_vif_ini, row.names = FALSE)
write.csv(tabla_vif_ini, file.path(dir_tablas, "vif_inicial.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("\n  >> Variables con VIF >=", umbral_vif, ":",
    ifelse(any(tabla_vif_ini$problema),
           paste(tabla_vif_ini$variable[tabla_vif_ini$problema], collapse = ", "), "ninguna"), "\n")

colineales <- c("capacidad_bateria", "resolucion_pantalla",
                "tiempo_pantalla_encendida", "notificaciones")
r_cap_res   <- cor(datos$capacidad_bateria, datos$resolucion_pantalla)
r_tiem_noti <- cor(datos$tiempo_pantalla_encendida, datos$notificaciones)
cat("Correlación capacidad_bateria vs resolucion_pantalla:", round(r_cap_res, 3), "\n")
cat("Correlación tiempo_pantalla_encendida vs notificaciones:", round(r_tiem_noti, 3), "\n")

pares <- rbind(
  data.frame(par = "Capacidad vs resolución",
             x = datos$capacidad_bateria, y = datos$resolucion_pantalla),
  data.frame(par = "Tiempo de pantalla vs notificaciones",
             x = datos$tiempo_pantalla_encendida, y = datos$notificaciones))
g_pares <- ggplot(pares, aes(x, y)) +
  geom_point(alpha = 0.3, color = AZUL, size = 0.9) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = ROJO) +
  facet_wrap(~ par, scales = "free") +
  labs(title = "Pares de variables colineales",
       subtitle = sprintf("r = %.2f (capacidad-resolución) y r = %.2f (tiempo de pantalla-notificaciones)",
                          r_cap_res, r_tiem_noti), x = NULL, y = NULL) +
  theme_minimal()
ggsave(file.path(dir_graficas, "04_pares_colineales.png"), g_pares, width = 11, height = 4.8, dpi = 300)

# 1.2 a 1.5 Supuestos del término de error ----------------------------------
res_ini <- residuals(modelo)
evaluar_supuestos("Inicial (MCO)", res_ini, X_ini, modelo)
graf_normalidad(res_ini, "inicial", "04_inicial_normalidad.png")
graf_vs_x(res_ini, 1, "Modelo inicial: e vs x (especificación)", "04_inicial_e_vs_x.png")
graf_vs_x(res_ini, 2, "Modelo inicial: e^2 vs x (homocedasticidad)", "04_inicial_e2_vs_x.png")
graf_vs_tiempo(res_ini, "Modelo inicial: e vs tiempo (independencia)", "04_inicial_e_vs_tiempo.png")

# ---------------------------------------------------------------------------
# 2. CORRECCIÓN DE LA MULTICOLINEALIDAD: PCA
#    Cada par colineal se reescribe con sus dos componentes principales (variables
#    estandarizadas): PC1 = (z1 + z2)/sqrt(2) y PC2 = (z1 - z2)/sqrt(2). Las dos
#    componentes juntas equivalen al par original (no se pierde información) y son
#    ortogonales, así que no hay multicolinealidad. Se descarta una componente solo
#    si una prueba F muestra que no aporta (restricción: los dos efectos
#    estandarizados son iguales).
# ---------------------------------------------------------------------------
cat("\n\n========== 2. CORRECCIÓN DE LA MULTICOLINEALIDAD (PCA) ==========\n")

hacer_pc <- function(vars) {
  pca    <- prcomp(datos[, vars], center = TRUE, scale. = TRUE)
  signos <- ifelse(pca$rotation[1, ] < 0, -1, 1)   # carga de la 1ª variable positiva en cada componente
  list(pca = pca, signos = signos, vars = vars)
}
puntaje <- function(obj, df, k) {
  as.numeric(predict(obj$pca, newdata = df[, obj$vars, drop = FALSE])[, k]) * obj$signos[k]
}

pc_hardware <- hacer_pc(c("capacidad_bateria", "resolucion_pantalla"))
pc_uso      <- hacer_pc(c("tiempo_pantalla_encendida", "notificaciones"))
datos$pc_hardware  <- puntaje(pc_hardware, datos, 1)
datos$pc_hardware2 <- puntaje(pc_hardware, datos, 2)
datos$pc_uso       <- puntaje(pc_uso, datos, 1)
datos$pc_uso2      <- puntaje(pc_uso, datos, 2)

resumen_pc <- do.call(rbind, lapply(list(hardware = pc_hardware, uso = pc_uso), function(o) {
  data.frame(variable = rep(o$vars, 2), componente = rep(c("PC1", "PC2"), each = 2),
             carga = round(as.vector(sweep(o$pca$rotation, 2, o$signos, "*")), 4),
             varianza_explicada = round(rep(summary(o$pca)$importance[2, ], each = 2), 4),
             row.names = NULL)
}))
resumen_pc$par <- rep(c("hardware", "uso"), each = 4)
cat("\n-- Componentes: cargas y proporción de varianza explicada --\n")
print(resumen_pc[, c("par", "componente", "variable", "carga", "varianza_explicada")], row.names = FALSE)
write.csv(resumen_pc[, c("par", "componente", "variable", "carga", "varianza_explicada")],
          file.path(dir_tablas, "pca_componentes.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# Modelo con las cuatro componentes (equivale al modelo inicial) y prueba F de cada PC2
formula_pca_completo <- duracion_bateria ~ pc_hardware + pc_hardware2 + log(antiguedad_bateria) +
  apps_c * uso_gps + brillo_c * ahorro_energia + brillo_c * bluetooth +
  modo_oscuro + pc_uso + pc_uso2 + temp_c + I(temp_c^2) + tipo_conectividad
modelo_pca_completo <- lm(formula_pca_completo, data = datos)

pruebas_comp <- do.call(rbind, lapply(c("pc_hardware2", "pc_uso2"), function(v) {
  a <- anova(update(modelo_pca_completo, as.formula(paste(". ~ . -", v))), modelo_pca_completo)
  data.frame(componente = v, F = round(a$F[2], 3), p_valor = signif(a$`Pr(>F)`[2], 3),
             decision = ifelse(a$`Pr(>F)`[2] < alpha, "Aporta: se conserva", "No aporta: se descarta"))
}))
cat("\n-- Prueba F de cada segunda componente (H0: no aporta) --\n")
print(pruebas_comp, row.names = FALSE)
write.csv(pruebas_comp, file.path(dir_tablas, "pca_pruebas_componentes.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# Modelo con PCA: se conservan las componentes que aportan según la prueba anterior
formula_pca <- duracion_bateria ~ pc_hardware + pc_hardware2 + log(antiguedad_bateria) +
  apps_c * uso_gps + brillo_c * ahorro_energia + brillo_c * bluetooth +
  modo_oscuro + pc_uso + temp_c + I(temp_c^2) + tipo_conectividad

modelo_pca <- lm(formula_pca, data = datos)
X_pca <- matriz_corta(modelo_pca)
vif_pca <- vif_manual(X_pca)
cat("\n-- VIF del modelo con componentes --\n")
print(data.frame(variable = names(vif_pca), VIF = round(vif_pca, 2), row.names = NULL), row.names = FALSE)
cat("\n  >> VIF máximo:", round(max(vif_pca), 2), "(inicial:", round(max(vif_inicial), 2), ") ->",
    ifelse(max(vif_pca) < umbral_vif, "multicolinealidad corregida.", "persiste multicolinealidad."), "\n")

# Se vuelven a revisar los supuestos: la corrección cambia los residuos
evaluar_supuestos("Con PCA (MCO)", residuals(modelo_pca), X_pca, modelo_pca)

# ---------------------------------------------------------------------------
# 3. CORRECCIÓN DE LA HETEROCEDASTICIDAD: MÍNIMOS CUADRADOS PONDERADOS
#    Modelo auxiliar del curso:
#       ln(e_i^2) = a0 + a1*x1i + ... + ak*xki + w_i
#       sigma2_hat_i = exp(a0_hat + a1_hat*x1i + ... + ak_hat*xki)
#       peso_i = 1 / sigma2_hat_i      (se le pasa a lm(weights = ...))
#    Las x son las variables de las que depende la varianza. Se mira primero la
#    regresión auxiliar con todas las regresoras: solo pc_uso resulta significativa,
#    así que el modelo de varianza final usa únicamente pc_uso.
# ---------------------------------------------------------------------------
cat("\n\n========== 3. CORRECCIÓN DE LA HETEROCEDASTICIDAD (WLS) ==========\n")

ln_e2 <- log(residuals(modelo_pca)^2)
stopifnot(all(is.finite(ln_e2)))   # un residuo exactamente 0 daría ln(0) = -Inf
cat("\n-- Modelo auxiliar con todas las regresoras (para elegir de qué depende la varianza) --\n")
print(summary(lm(ln_e2 ~ ., data = data.frame(ln_e2 = ln_e2, X_pca))))

vars_aux <- "pc_uso"            # x del modelo auxiliar final
dd_aux <- data.frame(ln_e2 = ln_e2, X_pca[, vars_aux, drop = FALSE])
modelo_aux <- lm(ln_e2 ~ ., data = dd_aux)
cat("\n-- Modelo auxiliar final ln(e^2) ~ pc_uso --\n")
print(summary(modelo_aux))

sigma2_hat <- exp(fitted(modelo_aux))
datos$pesos <- 1 / sigma2_hat
stopifnot(all(is.finite(datos$pesos)), all(datos$pesos > 0))
cat("\nRazón entre el mayor y el menor peso:", round(max(datos$pesos) / min(datos$pesos), 1), "\n")

modelo_final <- lm(formula_pca, data = datos, weights = pesos)
cat("\n== MODELO CORREGIDO (PCA + WLS) ==\n")
print(summary(modelo_final))

# Residuos ponderados (sqrt(peso) * e): son los que deben cumplir los supuestos
res_final <- weighted.residuals(modelo_final)
cat("\nRESET no se repite para el modelo ponderado: los pesos no cambian la especificación,",
    "que ya se evaluó en el modelo con PCA.\n")
evaluar_supuestos("Final (PCA + WLS)", res_final, X_pca)

vif_final <- vif_manual(X_pca, w = datos$pesos)
cat("\nVIF máximo del modelo final (correlación ponderada):", round(max(vif_final), 2), "\n")

graf_normalidad(res_final, "final", "04_final_normalidad.png")
graf_vs_x(res_final, 1, "Modelo final: e ponderado vs x (especificación)", "04_final_e_vs_x.png")
graf_vs_x(res_final, 2, "Modelo final: e^2 ponderado vs x (homocedasticidad)", "04_final_e2_vs_x.png")
graf_vs_tiempo(res_final, "Modelo final: e ponderado vs tiempo (independencia)", "04_final_e_vs_tiempo.png")

# ---------------------------------------------------------------------------
# 4. COMPARACIÓN: MODELO INICIAL vs MODELO CORREGIDO
# ---------------------------------------------------------------------------
cat("\n\n========== 4. COMPARACIÓN INICIAL vs CORREGIDO ==========\n")

# 4.1 Resultados de todas las pruebas, modelo por modelo
cat("\n-- Pruebas de supuestos en cada etapa --\n")
print(registro[, c("modelo", "supuesto", "prueba", "estadistico", "p_valor", "decision")], row.names = FALSE)
write.csv(registro, file.path(dir_tablas, "pruebas_supuestos.csv"), row.names = FALSE, fileEncoding = "UTF-8")

modelos_nombres <- c("Inicial (MCO)", "Con PCA (MCO)", "Final (PCA + WLS)")
pruebas_nombres <- c("Shapiro-Wilk", "Chi cuadrado de Pearson", "Kolmogorov-Smirnov",
                     "RESET de Ramsey", "Breusch-Pagan", "Durbin-Watson")
tabla_p <- data.frame(prueba = pruebas_nombres, do.call(cbind, lapply(modelos_nombres, function(m) {
  sapply(pruebas_nombres, function(pr) {
    v <- registro$p_valor[registro$modelo == m & registro$prueba == pr]
    if (length(v) == 0) NA_real_ else v
  })
})), row.names = NULL)
names(tabla_p)[-1] <- modelos_nombres
cat("\n-- p-valores por prueba y modelo --\n")
print(tabla_p, row.names = FALSE)
write.csv(tabla_p, file.path(dir_tablas, "comparacion_p_valores.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# 4.2 Medidas globales
metricas <- data.frame(
  medida = c("Número de parámetros", "R2 ajustado", "Sigma residual (h)", "VIF máximo"),
  inicial = c(length(coef(modelo)), summary(modelo)$adj.r.squared, summary(modelo)$sigma, max(vif_inicial)),
  con_PCA = c(length(coef(modelo_pca)), summary(modelo_pca)$adj.r.squared, summary(modelo_pca)$sigma, max(vif_pca)),
  final = c(length(coef(modelo_final)), summary(modelo_final)$adj.r.squared, summary(modelo_final)$sigma, max(vif_final)))
metricas[-1] <- lapply(metricas[-1], signif, digits = 4)
cat("\n-- Medidas globales (en el modelo final R2 y sigma son ponderados: no son comparables 1 a 1) --\n")
print(metricas, row.names = FALSE)
write.csv(metricas, file.path(dir_tablas, "comparacion_medidas_globales.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# 4.3 Coeficientes (los 4 términos colineales se reemplazan por 2 componentes)
tab_coef <- function(m) {
  s <- summary(m)$coefficients
  data.frame(termino = rownames(s), estimado = s[, 1], error_est = s[, 2], p_valor = s[, 4], row.names = NULL)
}
ci <- tab_coef(modelo); cf <- tab_coef(modelo_final)
terminos_todos <- union(ci$termino, cf$termino)
comparacion_coef <- data.frame(
  termino = terminos_todos,
  ini_estimado = ci$estimado[match(terminos_todos, ci$termino)],
  ini_error_est = ci$error_est[match(terminos_todos, ci$termino)],
  ini_p = ci$p_valor[match(terminos_todos, ci$termino)],
  fin_estimado = cf$estimado[match(terminos_todos, cf$termino)],
  fin_error_est = cf$error_est[match(terminos_todos, cf$termino)],
  fin_p = cf$p_valor[match(terminos_todos, cf$termino)])
comparacion_coef[-1] <- lapply(comparacion_coef[-1], signif, digits = 4)
cat("\n-- Coeficientes: modelo inicial (ini) vs modelo corregido (fin) --\n")
print(comparacion_coef, row.names = FALSE)
write.csv(comparacion_coef, file.path(dir_tablas, "comparacion_coeficientes.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# 4.4 Gráfica de VIF antes y después
df_vif <- rbind(
  data.frame(modelo = "Inicial", variable = names(vif_inicial), VIF = as.numeric(vif_inicial)),
  data.frame(modelo = "Corregido (PCA)", variable = names(vif_final), VIF = as.numeric(vif_final)))
g_vif <- ggplot(df_vif, aes(x = VIF, y = reorder(variable, VIF))) +
  geom_col(fill = AZUL) +
  geom_vline(xintercept = umbral_vif, linetype = "dashed", color = ROJO) +
  facet_wrap(~ modelo, scales = "free_y") +
  labs(title = "VIF antes y después de la corrección", subtitle = paste("Línea roja: VIF =", umbral_vif),
       x = "VIF", y = NULL) +
  theme_minimal()
ggsave(file.path(dir_graficas, "04_vif_antes_despues.png"), g_vif, width = 11, height = 6, dpi = 300)
write.csv(df_vif, file.path(dir_tablas, "vif_antes_despues.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# 4.5 Objetos para los puntos siguientes (predicción de escenarios, etc.)
saveRDS(list(formula = formula_pca, coeficientes = coef(modelo_final), vcov = vcov(modelo_final),
             sigma_ponderado = summary(modelo_final)$sigma, gl = df.residual(modelo_final),
             pc_hardware = pc_hardware, pc_uso = pc_uso, medias_centrado = medias_centrado,
             modelo_auxiliar = coef(modelo_aux)),
        file.path(dir_tablas, "modelo_corregido_punto4.rds"))

cat("\n== Tablas en", dir_tablas, "y gráficas en", dir_graficas, "==\n")
sink()
