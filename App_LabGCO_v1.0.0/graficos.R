# ─────────────────────────────────────────────────────────────────────────────
# Funciones para graficar los parámetros de "Resumen por muestra" y de
# "Datos punto a punto", comparando grupos con barras + error (media ± SEM).
# La significancia se calcula en estadistica.R y se pasa la lista.
# ─────────────────────────────────────────────────────────────────────────────

library(ggplot2)

# ── Diccionarios de variables disponibles ────────────────────────────────────
variables_graficos <- list(
  Cantidad = list(
    resumen = "Cantidad",
    niveles = FALSE),
  
  Densidad = list(
    resumen = "Densidad",
    niveles = FALSE),
  
  "Área" = list(
    resumen = "Area_mm2_prom",
    puntos  = "area.mm2",
    niveles = TRUE),
  
  "Intensidad media" = list(
    resumen = "Intensidad_prom",
    puntos = "Mean",
    niveles = TRUE),
  
  "IntDen" = list(
    resumen = "DefTotal_prom",
    puntos = "IntDen",
    niveles = TRUE),
  
  "Circularidad" = list(
    resumen = "Circ_prom",
    puntos = "Circ.",
    niveles = TRUE),
  
  "Redondez" = list(
    resumen = "Round_prom",
    puntos = "Round",
    niveles = TRUE),
  
  "Solidez" = list(
    resumen = "Solidity_prom",
    puntos = "Solidity",
    niveles = TRUE),
  
  CV = list(
    puntos = "CV",
    niveles = FALSE),
  
  DefTotal = list(
    resumen = "DefTotal",
    niveles = FALSE),
  
  "DefTotal Rel" = list(
    resumen = "DefTotal_Rel",
    niveles = FALSE),
  
  "IntDen calibrado" = list(
    puntos = "IntDen_calibrado",
    niveles = FALSE),
  
  "IntDen calibrado (ratio al control)" = list(
    puntos = "IntDen_calibrado_ratio",
    niveles = FALSE),
  
  "IntDen calibrado (centrado al control)" = list(
    puntos = "IntDen_calibrado_centrado",
    niveles = FALSE),
  
  "IntDen calibrado (z-score vs. control)" = list(
    puntos = "IntDen_calibrado_zscore",
    niveles = FALSE)
)

# Parámetro para seleccionar que variables descargar
vars_promedios <- unique(
  unlist(lapply(variables_graficos, function(x) x$resumen)))
vars_puntos <- unique(
  unlist(lapply(variables_graficos, function(x) x$puntos)))


# ── Preparación de datos ──────────────────────────────────────────────────────

# Arma un data.frame largo (patron, replica, valor) a partir de "resultados"
# (salida de procesar_CSVs), para una variable del resumen por muestra.
# Cada fila de grupos[[patron]] ya es una réplica (un archivo/muestra).
preparar_datos_resumen <- function(resultados, variable) {

  grupos   <- resultados$grupos
  patrones <- resultados$patrones

  datos <- do.call(rbind, lapply(patrones, function(p) {
    df <- grupos[[p]]
    if (is.null(df) || nrow(df) == 0 || !(variable %in% colnames(df))) return(NULL)
    data.frame(
      patron  = p,
      replica = seq_len(nrow(df)),
      valor   = df[[variable]],
      stringsAsFactors = FALSE
    )
  }))

  if (is.null(datos) || nrow(datos) == 0) return(NULL)

  datos$patron <- factor(datos$patron, levels = patrones)
  return(datos)
}

# Arma un data.frame largo (patron, replica, valor) a partir de "wb_puntos"
# (salida de procesar_puntos), para una variable de los datos punto a punto.
#
# nivel = "replica" -> promedia los puntos/partículas de cada réplica primero
#                      (RECOMENDADO: cada réplica pasa a ser una observación,
#                      evitando pseudorreplicación al comparar grupos).
# nivel = "punto"   -> usa cada punto/partícula individual como observación
#                      (útil para ver la dispersión total, pero ojo con las
#                      comparaciones estadísticas entre grupos).
preparar_datos_puntos <- function(wb_puntos, variable, nivel = "replica") {

  grupos   <- wb_puntos$grupos
  patrones <- names(grupos)

  datos <- do.call(rbind, lapply(patrones, function(p) {
    df <- grupos[[p]]
    if (is.null(df) || nrow(df) == 0 || !(variable %in% colnames(df))) return(NULL)

    if (nivel == "replica") {
      agregado <- aggregate(df[[variable]], by = list(replica = df$replica),
                            FUN = mean, na.rm = TRUE)
      data.frame(
        patron  = p,
        replica = agregado$replica,
        valor   = agregado$x,
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        patron  = p,
        replica = df$replica,
        valor   = df[[variable]],
        stringsAsFactors = FALSE
      )
    }
  }))

  if (is.null(datos) || nrow(datos) == 0) return(NULL)

  datos$patron <- factor(datos$patron, levels = patrones)
  return(datos)
}

# ── Estadística resumen (media, SEM, n, mediana, RIC) por grupo ────────────────────────────

resumen_estadistico <- function(datos) {

  patrones <- levels(datos$patron)

  stats <- do.call(rbind, lapply(patrones, function(p) {
    vals <- datos$valor[datos$patron == p]
    vals <- vals[!is.na(vals)]
    n     <- length(vals)
    media <- if (n > 0) mean(vals) else NA
    sd_v  <- if (n > 1) sd(vals) else NA
    sem   <- if (n > 1) sd_v / sqrt(n) else NA
    mediana <- if (n > 0) median(vals) else NA
    ric     <- if (n > 1) IQR(vals) else NA
    data.frame(patron = p, n = n, media = media, sd = sd_v, sem = sem, mediana = mediana, ric = ric,
               stringsAsFactors = FALSE)
  }))

  stats$patron <- factor(stats$patron, levels = patrones)
  return(stats)
}

# ── Función general de gráficos ──────────────────────────────────────────────

grafico_grupos <- function(datos, stats = NULL,
                           titulo_y = "Valor",
                           tipo = c("barras", "boxplot", "violin", "scatter"),
                           mostrar_puntos = TRUE,
                           color_barras = "#4C72B0",
                           color_puntos = "#333333",
                           pares = NULL) {
  
  tipo <- match.arg(tipo)
  
  # ── Base del gráfico ───────────────────────────────────────────────────────
  p <- ggplot(datos, aes(x = patron, y = valor))
  
  # ── Tipo de gráfico ────────────────────────────────────────────────────────
  
  if (tipo == "barras") {
    
    if (is.null(stats)) {
      stop("Para el gráfico de barras se necesita 'stats'.")
    }
    
    p <- ggplot() +
      geom_col(data = stats,
               aes(x = patron, y = media),
               fill = color_barras,
               color = "black",
               width = 0.65,
               na.rm = TRUE) +
      geom_errorbar(data = stats,
                    aes(x = patron,
                        ymin = media - sem,
                        ymax = media + sem),
                    width = 0.2,
                    linewidth = 0.6,
                    na.rm = TRUE)
    
    if (mostrar_puntos) {
      p <- p +
        geom_jitter(data = datos,
                    aes(x = patron, y = valor),
                    width = 0.12,
                    size = 1.8,
                    alpha = 0.6,
                    color = color_puntos)
    }
    
  } else if (tipo == "boxplot") {
    
    p <- p +
      geom_boxplot(fill = color_barras,
                   alpha = 0.75,
                   width = 0.6,
                   outlier.shape = NA)
    
    if (mostrar_puntos) {
      p <- p +
        geom_jitter(width = 0.12,
                    size = 1.8,
                    alpha = 0.6,
                    color = color_puntos)
    }
    
  } else if (tipo == "violin") {
    
    p <- p +
      geom_violin(fill = color_barras,
                  alpha = 0.5,
                  trim = FALSE,
                  width = 0.9) +
      geom_boxplot(width = 0.12,
                   fill = "white",
                   outlier.shape = NA,
                   linewidth = 0.5)
    
    if (mostrar_puntos) {
      p <- p +
        geom_jitter(width = 0.10,
                    size = 1.6,
                    alpha = 0.5,
                    color = color_puntos)
    }
    
  } else if (tipo == "scatter") {
    
    p <- p +
      geom_jitter(width = 0.12,
                  size = 2,
                  alpha = 0.75,
                  color = color_puntos)
  }
  
  # ── Significancia (sirve para todos los tipos) ────────────────────────────
  
  if (!is.null(pares) && nrow(pares) > 0) {
    
    niveles <- levels(datos$patron)
    posiciones_x <- setNames(seq_along(niveles), niveles)
    
    pares <- pares[pares$grupo1 %in% niveles &
                     pares$grupo2 %in% niveles, ]
    
    if (nrow(pares) > 0) {
      
      # Altura máxima observada
      techo_max <- max(datos$valor, na.rm = TRUE)
      paso      <- diff(range(datos$valor, na.rm = TRUE)) * 0.10
      
      if (!is.finite(paso) || paso == 0) paso <- 1
      
      y_base <- techo_max + paso
      
      pares$dist_x <- abs(posiciones_x[pares$grupo1] -
                            posiciones_x[pares$grupo2])
      pares <- pares[order(pares$dist_x), ]
      
      for (i in seq_len(nrow(pares))) {
        
        x1 <- posiciones_x[[pares$grupo1[i]]]
        x2 <- posiciones_x[[pares$grupo2[i]]]
        y  <- y_base + (i - 1) * paso * 1.5
        
        p <- p +
          annotate("segment", x = x1, xend = x2, y = y, yend = y) +
          annotate("segment", x = x1, xend = x1,
                   y = y, yend = y - paso * 0.25) +
          annotate("segment", x = x2, xend = x2,
                   y = y, yend = y - paso * 0.25) +
          annotate("text",
                   x = (x1 + x2) / 2,
                   y = y + paso * 0.20,
                   label = pares$etiqueta[i],
                   size = 4.2,
                   vjust = 0)
      }
      
      p <- p + expand_limits(y = y_base + nrow(pares) * paso * 1.5 + paso)
    }
  }
  
  # ── Tema ───────────────────────────────────────────────────────────────────
  
  p <- p +
    labs(x = NULL, y = titulo_y) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
  
  return(p)
}
