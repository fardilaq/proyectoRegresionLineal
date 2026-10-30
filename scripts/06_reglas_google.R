# 06_reglas_google.R
# Punto 6: reglas de Google.
#  (1) ¿El efecto del brillo es distinto con bluetooth activo?
#  (2) ¿El efecto del brillo es superior a la suma de los efectos de apps y GPS?
# Usa los .rds del script 05 (no necesita datos). Uso: source("scripts/06_reglas_google.R")

alpha      <- 0.05
dir_tablas <- "resultados/tablas/reglas_google"
dir.create(dir_tablas, recursive = TRUE, showWarnings = FALSE)

cx <- readRDS("resultados/tablas/seleccion/modelo_Cx_punto6.rds")   # C + brillo:bluetooth
cc <- readRDS("resultados/tablas/seleccion/modelo_C_punto5.rds")    # C (robustez para la pregunta 2)

# Combinación lineal de coeficientes: estimado, error, prueba t e IC 95 %
combinar <- function(mod, nombre, pesos) {
  b <- mod$coeficientes; V <- mod$vcov; gl <- mod$gl
  stopifnot(all(names(pesos) %in% names(b)))
  w <- setNames(numeric(length(b)), names(b)); w[names(pesos)] <- pesos
  est <- sum(w * b); ee <- sqrt(drop(t(w) %*% V %*% w)); t_est <- est / ee
  data.frame(contraste = nombre, estimado = est, error_est = ee, t = t_est, gl = gl,
             p_dos_colas = 2 * pt(-abs(t_est), gl),
             p_cola_izq  = pt(t_est, gl),                      # H1: combinación < 0
             ic95_inf = est - qt(0.975, gl) * ee, ic95_sup = est + qt(0.975, gl) * ee)
}

# ---------------------------------------------------------------------------
# Pregunta 1: brillo x bluetooth
# H0: coef(brillo_c:bluetooth) = 0   vs   H1: distinto de 0
# ---------------------------------------------------------------------------
p1 <- rbind(
  combinar(cx, "Interacción brillo x bluetooth (diferencia de efectos)", c("brillo_c:bluetooth" = 1)),
  combinar(cx, "Efecto de +1 punto de brillo, bluetooth apagado, ahorro apagado", c(brillo_c = 1)),
  combinar(cx, "Efecto de +1 punto de brillo, bluetooth activo, ahorro apagado",
           c(brillo_c = 1, "brillo_c:bluetooth" = 1)),
  combinar(cx, "Efecto de +1 punto de brillo, bluetooth apagado, ahorro activo",
           c(brillo_c = 1, "brillo_c:ahorro_energia" = 1)),
  combinar(cx, "Efecto de +1 punto de brillo, bluetooth activo, ahorro activo",
           c(brillo_c = 1, "brillo_c:ahorro_energia" = 1, "brillo_c:bluetooth" = 1)))
p1[-1] <- signif(p1[-1], 4)
cat("\n== Pregunta 1: brillo x bluetooth ==\n"); print(p1, row.names = FALSE)
write.csv(p1, file.path(dir_tablas, "regla1_brillo_bluetooth.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("Decisión:", ifelse(p1$p_dos_colas[1] < alpha,
    "se rechaza H0: el efecto del brillo SÍ cambia con bluetooth.",
    "no se rechaza H0: no hay evidencia de que el efecto del brillo cambie con bluetooth."), "\n")

# ---------------------------------------------------------------------------
# Pregunta 2: brillo vs apps + GPS
# L = b_brillo - b_apps - b_gps. Los tres efectos son negativos (restan horas), así que
# "brillo superior" = mayor pérdida = b_brillo < b_apps + b_gps  <=>  L < 0.
# H0: L >= 0   vs   H1: L < 0 (cola izquierda). También se reporta la prueba de dos colas.
# ---------------------------------------------------------------------------
contrastes_p2 <- function(mod, etiqueta) {
  r <- rbind(
    combinar(mod, "L base: b_brillo - b_apps - b_gps", c(brillo_c = 1, apps_c = -1, uso_gps = -1)),
    combinar(mod, "L con ahorro activo en el brillo", c(brillo_c = 1, "brillo_c:ahorro_energia" = 1, apps_c = -1, uso_gps = -1)),
    combinar(mod, "L con GPS activo en el efecto de apps", c(brillo_c = 1, apps_c = -1, "apps_c:uso_gps" = -1, uso_gps = -1)),
    combinar(mod, "+10 puntos de brillo vs +1 app y GPS", c(brillo_c = 10, apps_c = -1, uso_gps = -1)),
    combinar(mod, "+20 puntos de brillo vs +1 app y GPS", c(brillo_c = 20, apps_c = -1, uso_gps = -1)),
    combinar(mod, "+50 puntos de brillo vs +1 app y GPS", c(brillo_c = 50, apps_c = -1, uso_gps = -1)))
  cbind(modelo = etiqueta, r)
}
p2 <- rbind(contrastes_p2(cx, "C ampliado"), contrastes_p2(cc, "C"))
p2[-(1:2)] <- signif(p2[-(1:2)], 4)
cat("\n== Pregunta 2: brillo vs apps + GPS (estimado > 0: brillo pierde MENOS horas) ==\n")
print(p2, row.names = FALSE)
write.csv(p2, file.path(dir_tablas, "regla2_brillo_apps_gps.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("Decisión (C ampliado, L base):", ifelse(p2$p_cola_izq[1] < alpha,
    "se rechaza H0: el efecto del brillo es SUPERIOR a apps + GPS.",
    "no se rechaza H0: no hay evidencia de que el efecto del brillo sea superior a apps + GPS."), "\n")

# Punto de equivalencia: puntos de brillo cuyo efecto iguala el de 1 app + GPS (método delta)
b <- cx$coeficientes; V <- cx$vcov
k_est <- (b[["apps_c"]] + b[["uso_gps"]]) / b[["brillo_c"]]
grad <- setNames(numeric(length(b)), names(b))
grad["apps_c"]  <- 1 / b[["brillo_c"]]
grad["uso_gps"] <- 1 / b[["brillo_c"]]
grad["brillo_c"] <- -(b[["apps_c"]] + b[["uso_gps"]]) / b[["brillo_c"]]^2
ee_k <- sqrt(drop(t(grad) %*% V %*% grad))
equiv <- data.frame(puntos_brillo_equivalentes = k_est, error_est = ee_k,
                    ic95_inf = k_est - qt(0.975, cx$gl) * ee_k, ic95_sup = k_est + qt(0.975, cx$gl) * ee_k)
equiv <- signif(equiv, 4)
cat("\n== Puntos de brillo equivalentes a +1 app y GPS activo ==\n"); print(equiv, row.names = FALSE)
write.csv(equiv, file.path(dir_tablas, "regla2_punto_equivalencia.csv"), row.names = FALSE, fileEncoding = "UTF-8")