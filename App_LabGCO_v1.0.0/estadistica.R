# ─────────────────────────────────────────────────────────────────────────────
# Funciones para evaluar supuestos (normalidad, homogeneidad de varianzas),
# recomendar un test adecuado y ejecutar el test elegido (con post-hoc cuando
# corresponde). 
#
# Los post-hoc para >2 grupos siguen el criterio de GraphPad Prism:
#  
#  - ANOVA de una vía (varianzas iguales)  -> Tukey (todos vs todos)
#                                          -> Dunnet (todos vs referencia)
#
#  - Welch ANOVA (varianzas distintas)     -> Games-Howell
#
#  - Kruskal-Wallis (no paramétrico)       -> Dunn, corrección de Bonferroni
# ─────────────────────────────────────────────────────────────────────────────

# ── Normalidad, por grupo (Shapiro-Wilk) ─────────────────────────────────────
# Si un grupo tiene menos de 3 réplicas, Shapiro-Wilk no se puede calcular.

test_normalidad <- function(datos) {
  
  patrones <- levels(datos$patron)
  
  resultado <- do.call(rbind, lapply(patrones, function(p) {
    vals <- datos$valor[datos$patron == p]
    vals <- vals[!is.na(vals)]
    n <- length(vals)
    
    if (n < 3) {
      return(data.frame(
        patron = p, n = n, estadistico = NA, p_valor = NA, normal = NA,
        mensaje = "No se puede calcular: se necesitan al menos 3 réplicas",
        stringsAsFactors = FALSE
      ))
    }
    if (n > 5000) {
      return(data.frame(
        patron = p, n = n, estadistico = NA, p_valor = NA, normal = NA,
        mensaje = "Shapiro-Wilk no admite más de 5000 datos",
        stringsAsFactors = FALSE
      ))
    }
    
    prueba <- tryCatch(shapiro.test(vals), error = function(e) NULL)
    if (is.null(prueba)) {
      return(data.frame(
        patron = p, n = n, estadistico = NA, p_valor = NA, normal = NA,
        mensaje = "No se pudo calcular", stringsAsFactors = FALSE
      ))
    }
    
    data.frame(
      patron = p, n = n, estadistico = unname(prueba$statistic),
      p_valor = prueba$p.value, normal = prueba$p.value > 0.05,
      mensaje = "", stringsAsFactors = FALSE
    )
  }))
  
  resultado$patron <- factor(resultado$patron, levels = patrones)
  return(resultado)
}

# ── Homogeneidad de varianzas (Levene, versión de la mediana / Brown-Forsythe) ─

test_homogeneidad <- function(datos) {
  
  datos <- datos[!is.na(datos$valor), ]
  datos$patron <- droplevels(datos$patron)
  
  if (nlevels(datos$patron) < 2) {
    return(list(estadistico = NA, p_valor = NA, homogenea = NA))
  }
  
  medianas <- tapply(datos$valor, datos$patron, median, na.rm = TRUE)
  desvios  <- abs(datos$valor - medianas[as.character(datos$patron)])
  
  ajuste <- tryCatch(aov(desvios ~ datos$patron), error = function(e) NULL)
  if (is.null(ajuste)) return(list(estadistico = NA, p_valor = NA, homogenea = NA))
  
  s <- summary(ajuste)[[1]]
  f <- s[["F value"]][1]
  p <- s[["Pr(>F)"]][1]
  
  list(estadistico = f, p_valor = p, homogenea = if (is.na(p)) NA else p > 0.05)
}

# ── Recomendación de test según normalidad, homogeneidad y n° de grupos ──────

recomendar_test <- function(normalidad, homogeneidad, n_grupos) {
  
  todos_normales <- !any(is.na(normalidad$normal)) && all(normalidad$normal)
  homogenea <- isTRUE(homogeneidad$homogenea)
  
  if (n_grupos <= 1) return("No hay suficientes grupos para comparar")
  
  if (n_grupos == 2) {
    if (!todos_normales) return("Mann-Whitney (Wilcoxon)")
    if (!homogenea)      return("t-test de Welch")
    return("t-test (Student)")
  }
  
  # n_grupos > 2
  if (!todos_normales) return("Kruskal-Wallis")
  if (!homogenea)      return("Welch ANOVA")
  return("ANOVA de una vía")
}

# ── Etiqueta de asteriscos según p-valor ──────────────────────────────────────

etiqueta_p <- function(p) {
  if (is.na(p))       "s/d"
  else if (p < 0.0001) "****"
  else if (p < 0.001) "***"
  else if (p < 0.01)  "**"
  else if (p < 0.05)  "*"
  else                "ns"
}

# ── Convierte una matriz de p-valores (formato pairwise, con nombres de fila
# y columna = grupos) en un data.frame largo grupo1/grupo2/p_ajustado. Se usa
# para el resultado de PMCMRplus (Games-Howell), que devuelve una matriz
# triangular con esa estructura. No se parsea texto, así que no importa si
# los nombres de grupo tienen guiones u otros símbolos ────────────────────────

matriz_a_pares <- function(mat) {
  filas    <- rownames(mat)
  columnas <- colnames(mat)
  
  resultado <- do.call(rbind, lapply(filas, function(f) {
    do.call(rbind, lapply(columnas, function(cl) {
      p <- mat[f, cl]
      if (is.na(p)) return(NULL)
      data.frame(grupo1 = cl, grupo2 = f, p_ajustado = p, stringsAsFactors = FALSE)
    }))
  }))
  
  if (is.null(resultado)) {
    return(data.frame(grupo1 = character(0), grupo2 = character(0), p_ajustado = numeric(0)))
  }
  rownames(resultado) <- NULL
  resultado
}

# ── Asignación de cada patrón a un nivel de Factor 1 y de Factor 2, buscando
# qué nivel aparece como texto dentro del nombre del patrón (para el ANOVA de
# dos factores) ───────────────────────────────────────────────────────────────

asignar_factores <- function(patrones, niveles_f1, niveles_f2) {
  
  buscar_nivel <- function(patron, niveles) {
    niveles <- trimws(niveles)
    
    # Busca primero los nombres más largos
    niveles_ordenados <- niveles[order(nchar(niveles), decreasing = TRUE)]
    
    for(nivel in niveles_ordenados){
      if(grepl("^No\\s+", nivel, ignore.case = TRUE)) # Ignoramos caso binario
        next
      if(grepl(nivel, patron, ignore.case = TRUE, fixed = TRUE)){
        return(nivel)
      }
    }
    
    # Caso binario, niveles tipo "X" / "NO X"
    niveles_no <- niveles[grepl("^No\\s+", niveles, ignore.case = TRUE)]
    
    for(nivel_no in niveles_no){
      nivel_si <- sub("^No\\s+", "", nivel_no, ignore.case = TRUE)
      
      if(nivel_si %in% niveles){
        if(!grepl(nivel_si, patron, ignore.case = TRUE, fixed = TRUE)){
          return(nivel_no)
        }
      }
    }
    
    # En caso que no haya podido asignarse ningún nivel
    return(NA_character_)
  }
  
  asignacion <- do.call(rbind, lapply(patrones, function(p) {
    data.frame(
      patron  = p,
      factor1 = buscar_nivel(p, niveles_f1),
      factor2 = buscar_nivel(p, niveles_f2),
      stringsAsFactors = FALSE
    )
  }))
  
  sin_factor1 <- asignacion$patron[is.na(asignacion$factor1)]
  sin_factor2 <- asignacion$patron[is.na(asignacion$factor2)]
  
  errores <- c()
  if (length(sin_factor1) > 0) {
    errores <- c(errores, paste0(
      "No se pudo asignar el Factor 1 (ningún nivel encontrado, o más de uno) para: ",
      paste(sin_factor1, collapse = ", ")))
  }
  if (length(sin_factor2) > 0) {
    errores <- c(errores, paste0(
      "No se pudo asignar el Factor 2 (ningún nivel encontrado, o más de uno) para: ",
      paste(sin_factor2, collapse = ", ")))
  }
  
  if (length(errores) > 0) attr(asignacion, "error") <- paste(errores, collapse = " | ")
  
  asignacion
}

# ── Función para chequear que la asignación de factores es la deseada
resumir_asignacion <- function(asignacion) {
  resumen_f1 <- aggregate(patron ~ factor1, data = asignacion,
                          FUN = function(x) paste(x, collapse = ", "))
  names(resumen_f1) <- c("Nivel", "Patrones asignados")
  resumen_f1$Factor <- "Factor 1"
  
  resumen_f2 <- aggregate(patron ~ factor2, data = asignacion,
                          FUN = function(x) paste(x, collapse = ", "))
  names(resumen_f2) <- c("Nivel", "Patrones asignados")
  resumen_f2$Factor <- "Factor 2"
  
  rbind(resumen_f1[, c("Factor", "Nivel", "Patrones asignados")],
        resumen_f2[, c("Factor", "Nivel", "Patrones asignados")])
}

# ── Ejecución del test elegido ───────────────────────────────────────────────
# Devuelve una lista con: nombre, estadistico, p_valor, gl (si aplica), y
# pares (data.frame grupo1/grupo2/p_ajustado/etiqueta con las comparaciones
# de a pares, para dibujar las marcas en el gráfico). Para el ANOVA de dos
# factores devuelve además tabla_factores (F y p-valor de cada efecto).
#
# modo = "todos"   -> Comparaciones de a pares entre todos los grupos
# modo = "control" -> Cada grupo comparado solo contra "referencia"
#   (aplica para ANOVA de una vía, Welch ANOVA y Kruskal-Wallis,
#    se ignora en los tests de 2 grupos y en el ANOVA de dos factores)

ejecutar_test <- function(datos, tipo, factores = NULL, modo = "todos", referencia = NULL) {
  
  datos <- datos[!is.na(datos$valor), ]
  datos$patron <- droplevels(datos$patron)
  patrones <- levels(datos$patron)
  
  resultado <- list(nombre = tipo, estadistico = NA, p_valor = NA, gl = NA, pares = NULL)
  
  # ── Tests de a pares (requieren exactamente 2 grupos) ──────────────────────
  if (tipo %in% c("t-test (Student)", "t-test de Welch", "Mann-Whitney (Wilcoxon)")) {
    
    if (length(patrones) != 2) {
      resultado$error <- "Este test necesita exactamente 2 grupos seleccionados."
      return(resultado)
    }
    
    a <- datos$valor[datos$patron == patrones[1]]
    b <- datos$valor[datos$patron == patrones[2]]
    
    prueba <- suppressWarnings(switch(tipo,
                                      "t-test (Student)"                = t.test(a, b, var.equal = TRUE),
                                      "t-test de Welch"                 = t.test(a, b, var.equal = FALSE),
                                      "Mann-Whitney (Wilcoxon)"         = wilcox.test(a, b),
    ))
    
    resultado$estadistico <- unname(prueba$statistic)
    resultado$p_valor     <- prueba$p.value
    resultado$gl <- if (!is.null(prueba$parameter)) paste(round(prueba$parameter, 2), collapse = ", ") else NA
    resultado$pares <- data.frame(
      grupo1 = patrones[1], grupo2 = patrones[2],
      p_ajustado = prueba$p.value, etiqueta = etiqueta_p(prueba$p.value),
      stringsAsFactors = FALSE
    )
    return(resultado)
  }
  
  # ── ANOVA de una vía ─────────────────────────────────────────────────────────
  if (tipo == "ANOVA de una vía") {
    
    ajuste <- aov(valor ~ patron, data = datos)
    s <- summary(ajuste)[[1]]
    resultado$estadistico <- s[["F value"]][1]
    resultado$p_valor     <- s[["Pr(>F)"]][1]
    resultado$gl          <- paste(s[["Df"]][1], s[["Df"]][2], sep = ", ")
    
    if (modo == "control") {
      
      if (is.null(referencia) || !(referencia %in% patrones)) {
        resultado$error <- "Elegí un grupo de referencia válido."
        return(resultado)
      }
      if (!requireNamespace("PMCMRplus", quietly = TRUE)) {
        resultado$error <- "El test de Dunnett necesita el paquete 'PMCMRplus'. Instalalo con install.packages('PMCMRplus')."
        return(resultado)
      }
      
      datos_rel <- datos
      datos_rel$patron <- relevel(datos_rel$patron, ref = referencia)
      
      prueba_dun <- tryCatch(PMCMRplus::dunnettTest(valor ~ patron, data = datos_rel),
                             error = function(e) NULL)
      if (is.null(prueba_dun)) {
        resultado$error <- "No se pudo calcular el test de Dunnett con estos datos."
        return(resultado)
      }
      
      pv <- prueba_dun$p.value
      if (is.matrix(pv)) {
        grupos_comp <- rownames(pv)
        valores_p   <- pv[, 1]
      } else {
        grupos_comp <- names(pv)
        valores_p   <- as.numeric(pv)
      }
      
      pares <- data.frame(
        grupo1 = referencia, grupo2 = grupos_comp,
        p_ajustado = valores_p, stringsAsFactors = FALSE
      )
      pares$etiqueta <- vapply(pares$p_ajustado, etiqueta_p, character(1))
      resultado$pares <- pares
      return(resultado)
    }
    
    # modo == "todos": Tukey
    tukey <- TukeyHSD(ajuste)$patron
    combinaciones <- combn(patrones, 2)
    pares <- data.frame(
      grupo1 = combinaciones[1, ], grupo2 = combinaciones[2, ],
      p_ajustado = tukey[, "p adj"], stringsAsFactors = FALSE
    )
    pares$etiqueta <- vapply(pares$p_ajustado, etiqueta_p, character(1))
    resultado$pares <- pares
    return(resultado)
  }
  
  # ── Welch ANOVA ───────────────────────────────────────────────────────────────
  if (tipo == "Welch ANOVA") {
    
    prueba <- oneway.test(valor ~ patron, data = datos, var.equal = FALSE)
    resultado$estadistico <- unname(prueba$statistic)
    resultado$p_valor     <- prueba$p.value
    resultado$gl <- paste(round(prueba$parameter[1], 1), round(prueba$parameter[2], 1), sep = ", ")
    
    if (!requireNamespace("PMCMRplus", quietly = TRUE)) {
      resultado$error <- "El post-hoc Games-Howell necesita el paquete 'PMCMRplus'. Instalalo con install.packages('PMCMRplus')."
      return(resultado)
    }
    
    gh <- tryCatch(PMCMRplus::gamesHowellTest(valor ~ patron, data = datos), error = function(e) NULL)
    if (is.null(gh)) {
      resultado$error <- "No se pudo calcular el post-hoc Games-Howell con estos datos."
      return(resultado)
    }
    pares <- matriz_a_pares(gh$p.value)
    
    if (modo == "control") {
      if (is.null(referencia) || !(referencia %in% patrones)) {
        resultado$error <- "Elegí un grupo de referencia válido."
        return(resultado)
      }
      pares <- pares[pares$grupo1 == referencia | pares$grupo2 == referencia, ]
      resultado$aviso <- paste0(
        "Nota: GraphPad no tiene una versión 'todos contra uno' específica para Welch ANOVA. ",
        "Esto es un subconjunto de las comparaciones de Games-Howell (todas contra todas), ",
        "filtrado a las que involucran a '", referencia, "'. Si usás otro criterio en tu ",
        "flujo habitual, avisame para ajustarlo."
      )
    }
    
    pares$etiqueta <- vapply(pares$p_ajustado, etiqueta_p, character(1))
    resultado$pares <- pares
    return(resultado)
  }
  
  # ── Kruskal-Wallis ────────────────────────────────────────────────────────────
  if (tipo == "Kruskal-Wallis") {
    
    prueba <- kruskal.test(valor ~ patron, data = datos)
    resultado$estadistico <- unname(prueba$statistic)
    resultado$p_valor     <- prueba$p.value
    resultado$gl          <- unname(prueba$parameter)
    
    if (!requireNamespace("dunn.test", quietly = TRUE)) {
      resultado$error <- "El post-hoc de Dunn necesita el paquete 'dunn.test'. Instalalo con install.packages('dunn.test')."
      return(resultado)
    }
    
    # Se piden los p-valores SIN ajustar (method = "none") para poder aplicar
    # nosotros la corrección de Bonferroni con el número correcto de
    # comparaciones: todas las posibles (modo "todos") o solo las que
    # involucran al grupo de referencia (modo "control", k-1 comparaciones).
    
    dunn <- dunn.test::dunn.test(datos$valor, datos$patron, method = "none",
                                 kw = FALSE, table = FALSE, list = FALSE)
    partes <- strsplit(dunn$comparisons, " - ", fixed = TRUE)
    pares_todos <- data.frame(
      grupo1 = vapply(partes, `[`, character(1), 1),
      grupo2 = vapply(partes, `[`, character(1), 2),
      p_crudo = dunn$P,
      stringsAsFactors = FALSE
    )
    
    if (modo == "control") {
      if (is.null(referencia) || !(referencia %in% patrones)) {
        resultado$error <- "Elegí un grupo de referencia válido."
        return(resultado)
      }
      pares <- pares_todos[pares_todos$grupo1 == referencia | pares_todos$grupo2 == referencia, ]
    } else {
      pares <- pares_todos
    }
    
    # dunn.test devuelve p-valores unilaterales (Pr(Z >= |z|)).
    # GraphPad reporta p-valores bilaterales, por lo que se multiplican por 2.
    pares$p_crudo <- pmin(2 * pares$p_crudo, 1)
    pares$p_ajustado <- p.adjust(pares$p_crudo, method = "bonferroni")
    pares$etiqueta <- vapply(pares$p_ajustado, etiqueta_p, character(1))
    resultado$pares <- pares[, c("grupo1", "grupo2", "p_ajustado", "etiqueta")]
    return(resultado)
  }
  
  # ── ANOVA de dos factores ─────────────────────────────────────────────────────
  if (tipo == "ANOVA de dos factores") {
    
    if (is.null(factores)) {
      resultado$error <- "Faltan definir los niveles del Factor 1 y del Factor 2."
      return(resultado)
    }
    
    asignacion <- asignar_factores(patrones, factores$niveles_f1, factores$niveles_f2)
    resultado$verificacion_factores <- asignacion
    
    if (!is.null(attr(asignacion, "error"))) {
      resultado$error <- attr(asignacion, "error")
      return(resultado)
    }
    
    datos_char <- datos
    datos_char$patron <- as.character(datos_char$patron)
    datos_ampliado <- merge(datos_char, asignacion, by = "patron")
    datos_ampliado$factor1 <- factor(datos_ampliado$factor1)
    datos_ampliado$factor2 <- factor(datos_ampliado$factor2)
    
    # Pruebas de otros metodos anova
    contrasts(datos_ampliado$factor1) <- contr.sum
    contrasts(datos_ampliado$factor2) <- contr.sum
    ajuste <- tryCatch(lm(valor ~ factor1 * factor2, data = datos_ampliado), error = function(e) NULL)

    if (is.null(ajuste)) {
      resultado$error <- "No se pudo ajustar el ANOVA de dos factores con esta asignación de niveles."
      return(resultado)
    }
    # El tipo III es el que usa GraphPad, difiere con tipo I en caso que las muestras no sean balanceadas
    s <- car::Anova(ajuste, type = 3)
    
    resultado$tabla_factores <- data.frame(
      Efecto = c(factores$nombre_f1, factores$nombre_f2, "Interacción"),
      F = round(s[["F value"]][2:4], 3),
      p_valor = signif(s[["Pr(>F)"]][2:4], 4),
      stringsAsFactors = FALSE
    )
    resultado$tabla_factores$Significativo <- ifelse(
      is.na(resultado$tabla_factores$p_valor), "s/d",
      ifelse(resultado$tabla_factores$p_valor < 0.05, "Sí", "No")
    )
    
    # Para las marcas del gráfico, comparamos entre sí los grupos originales
    # (patron), igual que en el ANOVA de una vía
    ajuste_patron <- aov(valor ~ patron, data = datos)
    tukey <- TukeyHSD(ajuste_patron)$patron
    combinaciones <- combn(patrones, 2)
    pares <- data.frame(
      grupo1 = combinaciones[1, ], grupo2 = combinaciones[2, ],
      p_ajustado = tukey[, "p adj"], stringsAsFactors = FALSE
    )
    pares$etiqueta <- vapply(pares$p_ajustado, etiqueta_p, character(1))
    resultado$pares <- pares
    return(resultado)
  }
  
  resultado$error <- "Test no reconocido."
  return(resultado)
}

# ── Selecciona qué columnas de resumen_estadistico() mostrar, según el test ──
# Paramétricos (t-test, Welch, ANOVA): media ± DE, n
# No paramétricos (Wilcoxon, Kruskal-Wallis): mediana, RIC [Q1-Q3], n

tabla_resumen_para_test <- function(stats, tipo_test) {
  
  no_parametrico <- tipo_test %in% c("Mann-Whitney (Wilcoxon)", "Kruskal-Wallis")
  
  if (no_parametrico) {
    data.frame(
      Grupo         = as.character(stats$patron),
      n             = stats$n,
      Mediana       = round(stats$mediana, 3),
      `IQR (Q1-Q3)` = round(stats$ric, 3),
      stringsAsFactors = FALSE,
      check.names      = FALSE
    )
  } else {
    data.frame(
      Grupo             = as.character(stats$patron),
      n                 = stats$n,
      Media             = round(stats$media, 3),
      `Desvío estándar` = round(stats$sd, 3),
      stringsAsFactors  = FALSE,
      check.names       = FALSE
    )
  }
}
