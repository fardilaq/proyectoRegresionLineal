# 02_analisis_bivariado.R
# Analisis bivariado: duracion_bateria (respuesta) frente a cada variable
# explicativa. El criterio central es VISUAL: sobre cada nube de puntos se
# superponen las formas candidatas (lineal, cuadratica, log, raiz) para ver
# cual sigue mejor la forma de los datos. Como unico apoyo numerico se reporta
# el R2 ajustado de cada forma (mayor = mejor). Ademas se grafican las dos
# interacciones sugeridas por Google.
#
# Uso: desde la raiz del repo (directorio de trabajo = raiz),
#   source("scripts/02_analisis_bivariado.R")
# Reutiliza la misma definicion de niveles y conversion que el script 01.

library(readxl)
library(ggplot2)

ruta_xlsx     <- "datos/DatosProyectoRL_Android_202620.xlsx"
dir_graficas  <- "resultados/graficas/bivariado"
dir_tablas    <- "resultados/tablas"
dir.create(dir_graficas, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tablas,   recursive = TRUE, showWarnings = FALSE)

AZUL <- "#4C72B0"; ROJO <- "#C44E52"

# ---------------------------------------------------------------------------
# 1. Lectura y preparacion (igual criterio que 01_analisis_descriptivo.R)
# ---------------------------------------------------------------------------
niveles <- list(
  color_dispositivo = c("Negro", "Blanco", "Azul"),
  tipo_conectividad = c("Wi-fi", "4G", "5G"),
  dia_semana        = c("lunes", "martes", "miercoles", "jueves",
                        "viernes", "sabado", "domingo"))
binarias <- c("uso_gps", "ahorro_energia", "bluetooth", "modo_oscuro")

preparar_categoricas <- function(df) {
  for (v in names(niveles)) {
    x <- trimws(as.character(df[[v]]))
    df[[v]] <- factor(x, levels = niveles[[v]], ordered = FALSE)
  }
  for (v in binarias) {
    df[[paste0(v, "_f")]] <- factor(df[[v]], levels = c(0, 1),
                                    labels = c("No", "Si"))
  }
  df
}

datos <- as.data.frame(read_excel(ruta_xlsx, sheet = "Datos"))
datos <- preparar_categoricas(datos)

resp        <- "duracion_bateria"
numericas   <- c("capacidad_bateria", "antiguedad_bateria", "resolucion_pantalla",
                 "apps_segundo_plano", "brillo_pantalla", "tiempo_pantalla_encendida",
                 "contactos_guardados", "volumen_timbre", "notificaciones", "temperatura_C")
categoricas <- c("color_dispositivo", "tipo_conectividad", "dia_semana")

y <- datos[[resp]]

# ---------------------------------------------------------------------------
# 2. Formas funcionales candidatas por variable numerica
#    Criterio central: comparacion VISUAL (seccion 3). Aqui se calcula el
#    R2 ajustado de cada forma como apoyo (mayor = mejor ajuste). Las formas
#    log y raiz solo aplican cuando la variable es estrictamente positiva.
# ---------------------------------------------------------------------------
r2adj <- function(formula_txt) summary(lm(as.formula(formula_txt), data = datos))$adj.r.squared

tabla_ff <- do.call(rbind, lapply(numericas, function(v) {
  pos  <- all(datos[[v]] > 0)
  r_li <- r2adj(sprintf("%s ~ %s", resp, v))
  r_cu <- r2adj(sprintf("%s ~ %s + I(%s^2)", resp, v, v))
  r_lo <- if (pos) r2adj(sprintf("%s ~ log(%s)", resp, v))  else NA
  r_ra <- if (pos) r2adj(sprintf("%s ~ sqrt(%s)", resp, v)) else NA
  r2s  <- c(lineal = r_li, cuadratica = r_cu, log = r_lo, raiz = r_ra)
  data.frame(
    variable = v,
    R2adj_lineal     = round(r_li, 4),
    R2adj_cuadratica = round(r_cu, 4),
    R2adj_log        = round(r_lo, 4),
    R2adj_raiz       = round(r_ra, 4),
    mejor_R2adj      = names(which.max(r2s)),
    row.names = NULL)
}))

cat("\n== R2 ajustado por forma (mayor = mejor ajuste) ==\n")
print(tabla_ff, row.names = FALSE)
write.csv(tabla_ff, file.path(dir_tablas, "bivariado_formas_funcionales.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ---------------------------------------------------------------------------
# 3. Panel: cada numerica con las formas candidatas superpuestas sobre los
#    puntos. Se dibuja la curva ajustada de cada forma para comparar a ojo
#    cual sigue la nube. (lineal punteada, cuadratica, log, raiz)
# ---------------------------------------------------------------------------
# Curvas ajustadas de cada forma, evaluadas en una malla de x, en formato largo
curvas_forma <- function(v) {
  x   <- datos[[v]]
  pos <- all(x > 0)
  xs  <- seq(min(x), max(x), length.out = 200)
  nd  <- setNames(data.frame(xs), v)
  pred <- function(ftxt) predict(lm(as.formula(ftxt), data = datos), newdata = nd)
  formas <- list(lineal     = pred(sprintf("%s ~ %s", resp, v)),
                 cuadratica = pred(sprintf("%s ~ %s + I(%s^2)", resp, v, v)))
  if (pos) {
    formas$log  <- pred(sprintf("%s ~ log(%s)", resp, v))
    formas$raiz <- pred(sprintf("%s ~ sqrt(%s)", resp, v))
  }
  do.call(rbind, lapply(names(formas), function(f)
    data.frame(variable = v, forma = f, x = xs, yhat = formas[[f]])))
}

puntos <- data.frame(
  variable = factor(rep(numericas, each = nrow(datos)), levels = numericas),
  x = as.vector(as.matrix(datos[numericas])),
  y = rep(y, times = length(numericas)))
curvas <- do.call(rbind, lapply(numericas, curvas_forma))
curvas$variable <- factor(curvas$variable, levels = numericas)
curvas$forma    <- factor(curvas$forma, levels = c("lineal", "cuadratica", "log", "raiz"))

colores_forma <- c(lineal = "black", cuadratica = ROJO, log = "#55A868", raiz = "#DD8452")
tipos_forma   <- c(lineal = "dashed", cuadratica = "solid", log = "solid", raiz = "solid")

g_formas <- ggplot() +
  geom_point(data = puntos, aes(x = x, y = y), alpha = 0.20, color = AZUL, size = 0.8) +
  geom_line(data = curvas, aes(x = x, y = yhat, color = forma, linetype = forma),
            linewidth = 1) +
  facet_wrap(~ variable, scales = "free_x", ncol = 3) +
  scale_color_manual(values = colores_forma) +
  scale_linetype_manual(values = tipos_forma) +
  labs(title = "Formas funcionales candidatas sobre cada variable numerica",
       subtitle = "Se busca cual curva sigue la nube de puntos",
       x = NULL, y = "duracion_bateria (h)", color = "forma", linetype = "forma") +
  theme_minimal()
ggsave(file.path(dir_graficas, "formas_candidatas_numericas.png"), g_formas,
       width = 13, height = 14, dpi = 300)

# ---------------------------------------------------------------------------
# 4. Zoom a las dos formas no lineales clave: temperatura y antiguedad
# ---------------------------------------------------------------------------
zoom_formas <- function(v, titulo, xlab) {
  pts <- data.frame(x = datos[[v]], y = datos[[resp]])
  cur <- curvas_forma(v)
  cur$forma <- factor(cur$forma, levels = c("lineal", "cuadratica", "log", "raiz"))
  ggplot() +
    geom_point(data = pts, aes(x, y), alpha = 0.22, color = AZUL, size = 1) +
    geom_line(data = cur, aes(x, yhat, color = forma, linetype = forma), linewidth = 1.1) +
    scale_color_manual(values = colores_forma) +
    scale_linetype_manual(values = tipos_forma) +
    labs(title = titulo, x = xlab, y = "duracion_bateria (h)",
         color = "forma", linetype = "forma") +
    theme_minimal()
}
ggsave(file.path(dir_graficas, "forma_temperatura.png"),
       zoom_formas("temperatura_C", "temperatura_C: la cuadratica captura el optimo",
                   "temperatura_C (C)"), width = 7, height = 5, dpi = 300)
ggsave(file.path(dir_graficas, "forma_antiguedad.png"),
       zoom_formas("antiguedad_bateria", "antiguedad_bateria: decaimiento tipo log",
                   "antiguedad_bateria (meses)"), width = 7, height = 5, dpi = 300)

# ---------------------------------------------------------------------------
# 5. Boxplots de la respuesta segun variables binarias y categoricas
# ---------------------------------------------------------------------------
bin_f <- paste0(binarias, "_f")
largo_bin <- do.call(rbind, lapply(bin_f, function(v)
  data.frame(variable = sub("_f$", "", v), nivel = datos[[v]], y = y)))
largo_bin$variable <- factor(largo_bin$variable, levels = binarias)

g_bin <- ggplot(largo_bin, aes(x = nivel, y = y)) +
  geom_boxplot(fill = AZUL, alpha = 0.6, outlier.color = ROJO, outlier.size = 0.8) +
  facet_wrap(~ variable, ncol = 4) +
  labs(title = "Duracion de bateria segun variables binarias",
       x = NULL, y = "duracion_bateria (h)") +
  theme_minimal()
ggsave(file.path(dir_graficas, "boxplots_binarias.png"), g_bin,
       width = 11, height = 4.5, dpi = 300)

largo_cat <- do.call(rbind, lapply(categoricas, function(v)
  data.frame(variable = v, nivel = datos[[v]], y = y)))
largo_cat$variable <- factor(largo_cat$variable, levels = categoricas)

g_cat <- ggplot(largo_cat, aes(x = nivel, y = y)) +
  geom_boxplot(fill = AZUL, alpha = 0.6, outlier.color = ROJO, outlier.size = 0.8) +
  facet_wrap(~ variable, scales = "free_x", ncol = 3) +
  labs(title = "Duracion de bateria segun variables categoricas",
       x = NULL, y = "duracion_bateria (h)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(file.path(dir_graficas, "boxplots_categoricas.png"), g_cat,
       width = 13, height = 4.8, dpi = 300)

# Pruebas de diferencia de medias (binarias: t de Welch; categoricas: ANOVA)
cat("\n== Diferencias de medias ==\n")
for (v in binarias) {
  tt <- t.test(y ~ datos[[v]])
  cat(sprintf("%-16s dif(1-0) = %+6.2f h   p = %.4g\n",
              v, -diff(tt$estimate), tt$p.value))
}
for (v in categoricas) {
  av <- anova(lm(y ~ datos[[v]]))
  cat(sprintf("%-18s ANOVA F = %6.2f   p = %.4g\n", v, av$`F value`[1], av$`Pr(>F)`[1]))
}

# ---------------------------------------------------------------------------
# 6. Interacciones sugeridas por Google
#    (a) brillo_pantalla x ahorro_energia
#    (b) uso_gps x apps_segundo_plano
# ---------------------------------------------------------------------------
g_i1 <- ggplot(datos, aes(x = brillo_pantalla, y = duracion_bateria,
                          color = ahorro_energia_f)) +
  geom_point(alpha = 0.25, size = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.1) +
  scale_color_manual(values = c(No = AZUL, Si = ROJO), name = "ahorro_energia") +
  labs(title = "Interaccion brillo_pantalla x ahorro_energia",
       x = "brillo_pantalla (%)", y = "duracion_bateria (h)") +
  theme_minimal()
ggsave(file.path(dir_graficas, "interaccion_brillo_ahorro.png"), g_i1,
       width = 7.5, height = 5, dpi = 300)

g_i2 <- ggplot(datos, aes(x = apps_segundo_plano, y = duracion_bateria,
                          color = uso_gps_f)) +
  geom_point(alpha = 0.25, size = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.1) +
  scale_color_manual(values = c(No = AZUL, Si = ROJO), name = "uso_gps") +
  labs(title = "Interaccion uso_gps x apps_segundo_plano",
       x = "apps_segundo_plano", y = "duracion_bateria (h)") +
  theme_minimal()
ggsave(file.path(dir_graficas, "interaccion_gps_apps.png"), g_i2,
       width = 7.5, height = 5, dpi = 300)

cat("\n== Terminos de interaccion (lm con interaccion) ==\n")
m1 <- lm(duracion_bateria ~ brillo_pantalla * ahorro_energia, data = datos)
m2 <- lm(duracion_bateria ~ apps_segundo_plano * uso_gps, data = datos)
cat("brillo:ahorro_energia  coef =", round(coef(m1)[4], 4),
    " p =", signif(summary(m1)$coefficients[4, 4], 4), "\n")
cat("apps:uso_gps           coef =", round(coef(m2)[4], 4),
    " p =", signif(summary(m2)$coefficients[4, 4], 4), "\n")

cat("\n== Figuras en", dir_graficas, "y tablas en", dir_tablas, "==\n")