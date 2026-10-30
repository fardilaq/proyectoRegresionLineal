# 05_seleccion_modelos.R
# Punto 5: Modelo C = parsimonioso + PCA (ambas componentes de hardware) + WLS (varianza ~ pc_uso),
# frente al modelo A. El modelo B (solo PC1 de hardware) se conserva como candidato intermedio.
# Autocontenido: no depende de los scripts 03 ni 04.
# Uso: desde la raíz del repo, source("scripts/05_seleccion_modelos.R")
# Salidas: resultados/tablas/seleccion/*.csv, modelo_C_punto5.rds (numeral 7) y modelo_Cx_punto6.rds (numeral 6)

library(readxl)
library(lmtest)

ruta_xlsx  <- "datos/DatosProyectoRL_Android_202620.xlsx"
dir_tablas <- "resultados/tablas/seleccion"
dir.create(dir_tablas, recursive = TRUE, showWarnings = FALSE)

# 1. Datos
niveles <- list(
  color_dispositivo = c("Negro", "Blanco", "Azul"),
  tipo_conectividad = c("Wi-fi", "4G", "5G"),
  dia_semana        = c("lunes", "martes", "miercoles", "jueves",
                        "viernes", "sabado", "domingo"))
preparar_categoricas <- function(df) {
  for (v in names(niveles)) {
    df[[v]] <- factor(trimws(as.character(df[[v]])), levels = niveles[[v]], ordered = FALSE)
  }
  df
}
datos <- preparar_categoricas(as.data.frame(read_excel(ruta_xlsx, sheet = "Datos")))

# 2. Centrado (idéntico al script 03)
medias_centrado <- c(brillo_pantalla = mean(datos$brillo_pantalla),
                     apps_segundo_plano = mean(datos$apps_segundo_plano),
                     temperatura_C = mean(datos$temperatura_C))
datos$brillo_c <- datos$brillo_pantalla    - medias_centrado[["brillo_pantalla"]]
datos$apps_c   <- datos$apps_segundo_plano - medias_centrado[["apps_segundo_plano"]]
datos$temp_c   <- datos$temperatura_C      - medias_centrado[["temperatura_C"]]

# 3. PCA (misma lógica del script 04)
hacer_pc <- function(vars) {
  pca    <- prcomp(datos[, vars], center = TRUE, scale. = TRUE)
  signos <- ifelse(pca$rotation[1, ] < 0, -1, 1)
  list(pca = pca, signos = signos, vars = vars)
}
puntaje <- function(obj, df, k) {
  as.numeric(predict(obj$pca, newdata = df[, obj$vars, drop = FALSE])[, k]) * obj$signos[k]
}
obj_pc_hw  <- hacer_pc(c("capacidad_bateria", "resolucion_pantalla"))
obj_pc_uso <- hacer_pc(c("tiempo_pantalla_encendida", "notificaciones"))
datos$pc_hardware  <- puntaje(obj_pc_hw, datos, 1)
datos$pc_hardware2 <- puntaje(obj_pc_hw, datos, 2)
datos$pc_uso       <- puntaje(obj_pc_uso, datos, 1)

# 4. Modelos
formula_A <- duracion_bateria ~ capacidad_bateria + log(antiguedad_bateria) +
  resolucion_pantalla + apps_c * uso_gps + brillo_c * ahorro_energia +
  brillo_c * bluetooth + modo_oscuro + tiempo_pantalla_encendida +
  notificaciones + temp_c + I(temp_c^2) + tipo_conectividad

formula_B <- duracion_bateria ~ pc_hardware + log(antiguedad_bateria) +
  apps_c * uso_gps + brillo_c * ahorro_energia + bluetooth + modo_oscuro +
  pc_uso + temp_c + I(temp_c^2) + tipo_conectividad

formula_C <- duracion_bateria ~ pc_hardware + pc_hardware2 + log(antiguedad_bateria) +
  apps_c * uso_gps + brillo_c * ahorro_energia + bluetooth + modo_oscuro +
  pc_uso + temp_c + I(temp_c^2) + tipo_conectividad

formula_Cx <- update(formula_C, . ~ . + brillo_c:bluetooth)

formulas <- list(A = formula_A, B = formula_B, C = formula_C, Cx = formula_Cx)

modeloA     <- lm(formula_A, data = datos)
modelo_Bols <- lm(formula_B, data = datos)
modelo_Cols <- lm(formula_C, data = datos)
modelo_Cxols <- lm(formula_Cx, data = datos)

# Modelo de varianza: ln(e^2) = a0 + a1*pc_uso  ->  peso = 1/exp(valor ajustado)
aux_B <- lm(log(residuals(modelo_Bols)^2) ~ pc_uso, data = datos)
aux_C <- lm(log(residuals(modelo_Cols)^2) ~ pc_uso, data = datos)
aux_Cx <- lm(log(residuals(modelo_Cxols)^2) ~ pc_uso, data = datos)
datos$pesos_B <- 1 / exp(fitted(aux_B))
datos$pesos_C <- 1 / exp(fitted(aux_C))
modeloB <- lm(formula_B, data = datos, weights = pesos_B)
modeloC <- lm(formula_C, data = datos, weights = pesos_C)
datos$pesos_Cx <- 1 / exp(fitted(aux_Cx))
modeloCx <- lm(formula_Cx, data = datos, weights = pesos_Cx)

cat("\n== Modelo de varianza (C) ==\n"); print(summary(aux_C))
cat("\n== Modelo C (parsimonioso + PCA + WLS) ==\n"); print(summary(modeloC))
cat("\n== Modelo C ampliado (C + brillo_c:bluetooth, para el numeral 6) ==\n"); print(summary(modeloCx))

# Justificación de quitar brillo:bluetooth (F parcial dentro de A)
cat("\n== F parcial: quitar brillo_c:bluetooth de A ==\n")
print(anova(update(modeloA, . ~ . - brillo_c:bluetooth), modeloA))

# Justificación de conservar pc_hardware2 (F parcial dentro de C)
cat("\n== F parcial: quitar pc_hardware2 de C (equivale a pasar de C a B) ==\n")
print(anova(update(modeloC, . ~ . - pc_hardware2), modeloC))

# 5. Diagnóstico de supuestos
vif_max <- function(X, w = NULL) {
  R <- if (is.null(w)) cor(X) else cov.wt(X, wt = w / sum(w), cor = TRUE)$cor
  max(diag(solve(R)))
}
bp_p <- function(r, X) {                       # Breusch-Pagan (Koenker): n * R2 de e^2 ~ X
  stat <- length(r) * summary(lm(r^2 ~ X))$r.squared
  pchisq(stat, ncol(X), lower.tail = FALSE)
}
diagnostico <- function(m, nombre, con_reset = TRUE) {
  pesos_m <- m$weights
  r <- if (is.null(pesos_m)) residuals(m) else weighted.residuals(m)
  r <- as.numeric(r)
  X <- model.matrix(m)[, -1, drop = FALSE]
  data.frame(modelo = nombre, parametros = length(coef(m)),
             VIF_max = vif_max(X, pesos_m),
             p_Shapiro = shapiro.test(r)$p.value,
             p_RESET = if (con_reset) resettest(m, power = 2:3, type = "fitted")$p.value else NA,
             p_BP = bp_p(r, X),
             p_DW = dwtest(lm(r ~ 1), alternative = "two.sided")$p.value)
}
diag_tabla <- rbind(diagnostico(modeloA, "A (numeral 3)"),
                    diagnostico(modelo_Bols, "B sin pesos (OLS)"),
                    diagnostico(modeloB, "B con pesos (WLS)", con_reset = FALSE),
                    diagnostico(modelo_Cols, "C sin pesos (OLS)"),
                    diagnostico(modeloC, "C final (WLS)", con_reset = FALSE),
                    diagnostico(modelo_Cxols, "C ampliado sin pesos (OLS)"),
                    diagnostico(modeloCx, "C ampliado (WLS)", con_reset = FALSE))
diag_tabla[-1] <- signif(diag_tabla[-1], 4)
cat("\n== Supuestos (WLS: residuos ponderados; RESET se evalúa en el modelo OLS) ==\n")
print(diag_tabla, row.names = FALSE)
write.csv(diag_tabla, file.path(dir_tablas, "seleccion_supuestos.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# 6. Validación cruzada 10-fold: RMSE y cobertura de intervalos de predicción al 95 %
cv_modelo <- function(tipo, k = 10, semilla = 1) {
  set.seed(semilla)
  folds <- sample(rep(1:k, length.out = nrow(datos)))
  f <- formulas[[tipo]]
  out <- lapply(1:k, function(i) {
    tr <- datos[folds != i, ]; te <- datos[folds == i, ]
    if (tipo == "A") {
      m <- lm(f, data = tr)
      p <- predict(m, newdata = te, interval = "prediction", level = 0.95)
    } else {
      m0    <- lm(f, data = tr)
      aux_i <- lm(log(residuals(m0)^2) ~ pc_uso, data = tr)
      tr$w  <- 1 / exp(fitted(aux_i))
      m     <- lm(f, data = tr, weights = w)
      w_te  <- 1 / exp(predict(aux_i, newdata = te))
      p <- predict(m, newdata = te, interval = "prediction", level = 0.95, weights = w_te)
    }
    data.frame(y = te$duracion_bateria, p, pc_uso = te$pc_uso)
  })
  do.call(rbind, out)
}
resumen_cv <- function(r) {
  cubre <- r$y >= r$lwr & r$y <= r$upr
  alto  <- r$pc_uso > quantile(r$pc_uso, 0.8)
  data.frame(RMSE_CV = sqrt(mean((r$y - r$fit)^2)),
             cobertura_95 = mean(cubre),
             cobertura_95_uso_alto = mean(cubre[alto]),
             ancho_medio = mean(r$upr - r$lwr),
             ancho_uso_alto = mean((r$upr - r$lwr)[alto]),
             ancho_uso_bajo = mean((r$upr - r$lwr)[r$pc_uso < quantile(r$pc_uso, 0.2)]))
}
cv_A <- resumen_cv(cv_modelo("A")); cv_B <- resumen_cv(cv_modelo("B")); cv_C <- resumen_cv(cv_modelo("C")); cv_Cx <- resumen_cv(cv_modelo("Cx"))

# 7. Tabla comparativa (R2 y sigma de B y C son ponderados: no comparables 1 a 1)
resumen_global <- function(m, nombre) {
  s <- summary(m); f <- s$fstatistic
  data.frame(modelo = nombre, parametros = length(coef(m)), R2_ajustado = s$adj.r.squared,
             F = f[["value"]], F_critico = qf(0.95, f[["numdf"]], f[["dendf"]]),
             p_F = pf(f[["value"]], f[["numdf"]], f[["dendf"]], lower.tail = FALSE),
             AIC = AIC(m), BIC = BIC(m))
}
comparacion <- cbind(rbind(resumen_global(modeloA, "A (numeral 3)"),
                           resumen_global(modeloB, "B (PC1 hardware + WLS)"),
                           resumen_global(modeloC, "C (parsimonioso + PCA + WLS)"),
                           resumen_global(modeloCx, "C ampliado (C + brillo x bluetooth)")),
                     rbind(cv_A, cv_B, cv_C, cv_Cx))
comparacion[-1] <- signif(comparacion[-1], 4)
cat("\n== Comparación A vs B vs C ==\n"); print(comparacion, row.names = FALSE)
write.csv(comparacion, file.path(dir_tablas, "seleccion_comparacion.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# 8. Coeficientes de C
s <- summary(modeloC); ci <- confint(modeloC)
coef_C <- data.frame(termino = names(coef(modeloC)), estimado = coef(modeloC),
                     error_est = s$coefficients[, 2], t = s$coefficients[, 3],
                     p_valor = s$coefficients[, 4], ic95_inf = ci[, 1], ic95_sup = ci[, 2],
                     row.names = NULL)
coef_C[-1] <- signif(coef_C[-1], 4)
write.csv(coef_C, file.path(dir_tablas, "modeloC_coeficientes.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# 9. Matriz de datos de C
matriz_x <- model.matrix(modeloC)
renombres <- c("(Intercept)" = "intercepto", "log(antiguedad_bateria)" = "log_antiguedad",
               "I(temp_c^2)" = "temp_c_cuadrado", "tipo_conectividad4G" = "conectividad_4G",
               "tipo_conectividad5G" = "conectividad_5G", "apps_c:uso_gps" = "apps_c_x_gps",
               "brillo_c:ahorro_energia" = "brillo_c_x_ahorro")
nm <- colnames(matriz_x)
nm[nm %in% names(renombres)] <- unname(renombres[nm[nm %in% names(renombres)]])
colnames(matriz_x) <- nm
matriz_C <- data.frame(duracion_bateria = datos$duracion_bateria, matriz_x,
                       pesos = datos$pesos_C, row.names = NULL, check.names = FALSE)
write.csv(matriz_C, file.path(dir_tablas, "modeloC_matriz_datos.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# 10. Objeto para los puntos 6 y 7
# Varianza de un dispositivo nuevo: sigma^2 * exp(coef_aux[1] + coef_aux[2] * pc_uso_nuevo)
# pc_hardware y pc_hardware2 se calculan con puntaje(obj, df, 1) y puntaje(obj, df, 2)
saveRDS(list(formula = formula_C, coeficientes = coef(modeloC), vcov = vcov(modeloC),
             sigma = summary(modeloC)$sigma, gl = df.residual(modeloC),
             coef_aux = coef(aux_C), medias_centrado = medias_centrado,
             pc_hardware = obj_pc_hw, pc_uso = obj_pc_uso),
        file.path(dir_tablas, "modelo_C_punto5.rds"))

# 11. Coeficientes y objeto del C ampliado (numeral 6)
s <- summary(modeloCx); ci <- confint(modeloCx)
coef_Cx <- data.frame(termino = names(coef(modeloCx)), estimado = coef(modeloCx),
                      error_est = s$coefficients[, 2], t = s$coefficients[, 3],
                      p_valor = s$coefficients[, 4], ic95_inf = ci[, 1], ic95_sup = ci[, 2],
                      row.names = NULL)
coef_Cx[-1] <- signif(coef_Cx[-1], 4)
write.csv(coef_Cx, file.path(dir_tablas, "modeloCx_coeficientes.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

saveRDS(list(formula = formula_Cx, coeficientes = coef(modeloCx), vcov = vcov(modeloCx),
             sigma = summary(modeloCx)$sigma, gl = df.residual(modeloCx),
             coef_aux = coef(aux_Cx), medias_centrado = medias_centrado,
             pc_hardware = obj_pc_hw, pc_uso = obj_pc_uso),
        file.path(dir_tablas, "modelo_Cx_punto6.rds"))

cat("\n== Archivos guardados en", dir_tablas, "==\n")
