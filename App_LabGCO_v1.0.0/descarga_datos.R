# Resumen del codigo ----

# Con este codigo vamos a procesar los datos analizados por ImageJ/Fiji que quedaron guardados en CSVs.
# Elegimos N grupos. El PRIMER grupo siempre es el control (patron de referencia),
# que se usa para relativizar DefTotal en todos los grupos.
# El programa procesa todos los CSVs de cada grupo y crea las tablas comparativas para graphpad.


# Importante, trabajar en la carpeta donde estan los CSVs:
# En RStudio, en la pestaña Session > Set Working Directory > Choose Directory... ,
# o usar Ctrl + Shift + H



# Librerías ----

library(openxlsx)
library(pzfx)      # Exportación a formato GraphPad Prism (.pzfx)

# Funciones ----

# Leemos los nombres de los patrones que están disponibles en la carpeta seleccionada
# además de evitar caracteres especiales que puedan aparecer en estos nombres.
# Los archivos esperados tienen el formato "Nombre.tif-result-azul.csv".

escapar_regex <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)

nombres <- function() {
  extraer_nombre <- function(x) {
    sub("^(.*)\\s[^\\s]+\\.tif-results-azul\\.csv$", "\\1", x)
  }
  files   <- list.files(pattern = "tif-results-azul")
  nombres <- unique(sapply(files, extraer_nombre, USE.NAMES = FALSE))
  return(nombres)
}

# Convierte los valores numéricos de una tabla a texto con coma decimal,
# para que al pegar en Excel/GraphPad (configuración regional AR/ES) se
# interpreten como el mismo número.
formatear_numeros_es <- function(df){
  df[] <- lapply(df, function(col) {
    if (is.numeric(col)) {
      texto <- format(col, scientific = FALSE, trim = TRUE, digits = 15)
      gsub("\\.", ",", texto, fixed = FALSE)
    } else {
      col
    }
  })
  df
}

# ---- Extracción y procesamiento de las metricas de cada grupo ----

# Procesa los CSVs de todos los grupos y arma, para cada uno, una tabla con
# una fila por imagen y las métricas resumen de esa imagen (Cantidad,
# Densidad, DefTotal, etc.).
#
# patrones:      vector de nombres de grupo. El PRIMER elemento es siempre el
#                control, usado como referencia para relativizar DefTotal
#                (columna DefTotal_Rel) en todos los grupos, incluido él mismo.
# archivo_areas: path a un .txt con el área (en cm2) de cada imagen individual
#                (columnas "Archivo" y "Area_cm2").
# area_ROI:      área global en px2, usada para TODAS las imágenes por igual.
#                Se ignora si se pasa archivo_areas.
# filtrar:       si es TRUE, antes de calcular las métricas se descartan las
#                partículas cuya Area esté fuera del rango (0.0001, 1) — un
#                filtro de ruido/artefactos típico de las mediciones de Fiji.

procesar_CSVs <- function(patrones, archivo_areas = NULL, area_ROI = NULL, filtrar = TRUE) {
  
  if (is.null(archivo_areas) && is.null(area_ROI))
    stop("Debes proveer 'archivo_areas' (txt por imagen) o 'area_ROI' (area global en px2).")
  
  # Factor de conversión de píxeles cuadrados a mm2, fijo para el escáner
  # usado en el laboratorio.
  factor_a_mm2 <- 0.000448
  
  # Calibración del canal de intensidad (Mean): antes de usar el valor crudo
  # de Fiji se lo transforma con (Mean - intercepto) / pendiente.
  intercepto_calibracion <- 4.94
  pendiente_calibracion  <- 6.06
  
  # Leer tabla de areas por imagen (modo nuevo)
  if (!is.null(archivo_areas)) {
    tabla_areas <- read.table(archivo_areas, header = TRUE, sep = "\t",
                              stringsAsFactors = FALSE)
    stopifnot("Archivo" %in% names(tabla_areas), "Area_cm2" %in% names(tabla_areas))
    area_ROI_cm2 <- NULL   # se determina por imagen
  } else {
    tabla_areas  <- NULL
    area_ROI_cm2 <- area_ROI * factor_a_mm2 / 100   # area global (modo viejo)
  }
  
  # Procesa todos los CSVs de UN grupo y devuelve una fila por archivo/imagen
  # con las métricas resumen de esa imagen.
  procesar_grupo <- function(archivos, prom_DefTotal_ref = NULL, filtrar) {
    
    tabla <- data.frame(
      Archivo = character(),
      Cantidad = numeric(),
      Densidad = numeric(),
      Area_mm2_prom = numeric(),
      DefTotal = numeric(),
      DefTotal_Rel = numeric(),
      DefTotal_prom = numeric(),
      Intensidad_prom = numeric(),
      Circ_prom = numeric(),
      Round_prom = numeric(),
      Solidity_prom = numeric(),
      stringsAsFactors = FALSE
    )
    
    if (length(archivos) == 0) return(tabla)
    
    for (archivo in archivos) {
      
      datos_completos <- read.csv(archivo, sep = ",")
      
      if (filtrar) {
        datos <- datos_completos[
          datos_completos$Area >  0.0001 &
            datos_completos$Area < 1,
        ]
      } else {
        datos <- datos_completos
      }
      
      if (nrow(datos) == 0) next
      
      # Determinar el area a usar para la densidad
      if (!is.null(tabla_areas)) {
        # Area individual desde el txt.
        # El CSV se llama "Nombre.tif-results-azul.csv", el txt tiene "Nombre.tif"
        nombre_tif <- sub("-results-azul\\.csv$", "", basename(archivo))
        fila_area  <- tabla_areas[tabla_areas$Archivo == nombre_tif, ]
        if (nrow(fila_area) == 0) {
          warning(paste("No se encontro el area para:", nombre_tif,
                        "- se omite este archivo."))
          next
        }
        area_imagen_cm2 <- fila_area$Area_cm2[1]
      } else {
        # Area global para todas las imagenes
        area_imagen_cm2 <- area_ROI_cm2
      }
      
      Mean_calibrado  <- (datos$Mean - intercepto_calibracion) / pendiente_calibracion
      
      Cantidad        <- nrow(datos)
      Area_px_prom    <- mean(datos$Area, na.rm = TRUE)
      Area_px_sd      <- sd(datos$Area, na.rm = TRUE)
      Intensidad_prom <- mean(Mean_calibrado, na.rm = TRUE)
      
      DefTotal        <- sum(Mean_calibrado * datos$Area, na.rm = TRUE)
      Densidad        <- Cantidad / area_imagen_cm2
      Area_mm2_prom   <- Area_px_prom * factor_a_mm2
      
      DefTotal_prom   <- mean(Mean_calibrado * datos$Area, na.rm = TRUE)
      Circ_prom       <- mean(datos$Circ., na.rm = TRUE)
      Round_prom      <- mean(datos$Round, na.rm = TRUE)
      Solidity_prom   <- mean(datos$Solidity, na.rm = TRUE)
      
      tabla <- rbind(tabla, data.frame(
        Archivo = basename(archivo),
        Cantidad = Cantidad,
        Densidad = Densidad,
        Area_mm2_prom = Area_mm2_prom,
        DefTotal = DefTotal,
        DefTotal_Rel = NA,
        DefTotal_prom = DefTotal_prom,
        Intensidad_prom = Intensidad_prom,
        Circ_prom = Circ_prom,
        Round_prom = Round_prom,
        Solidity_prom = Solidity_prom,
        stringsAsFactors = FALSE
      ))
    }
    
    if (nrow(tabla) > 0) {
      if (is.null(prom_DefTotal_ref)) {
        ref <- mean(tabla$DefTotal, na.rm = TRUE)
        tabla$DefTotal_Rel <- tabla$DefTotal / ref
      } else {
        tabla$DefTotal_Rel <- tabla$DefTotal / prom_DefTotal_ref
      }
    }
    
    return(tabla)
  }
  
  
  # Procesar cada grupo. Escapamos el regex de cada patron al buscar los archivos.
  grupos <- vector("list", length(patrones))
  names(grupos) <- patrones
  
  # Primero el control (grupo 1) para obtener la referencia de relativizacion
  patron_esc1 <- escapar_regex(patrones[1])
  archivos_control <- list.files(pattern = paste0(patron_esc1, ".*tif-results-azul.csv"),
                                 full.names = TRUE, ignore.case = TRUE)
  grupos[[1]] <- procesar_grupo(archivos_control, filtrar = filtrar)
  
  prom_DefTotal_ref <- if (nrow(grupos[[1]]) > 0) {
    mean(grupos[[1]]$DefTotal, na.rm = TRUE)
  } else NULL
  
  # Resto de los grupos relativizados al control
  if (length(patrones) > 1) {
    for (i in 2:length(patrones)) {
      patron_esc <- escapar_regex(patrones[i])
      archivos <- list.files(pattern = paste0(patron_esc, ".*tif-results-azul.csv"),
                             full.names = TRUE, ignore.case = TRUE)
      grupos[[i]] <- procesar_grupo(archivos,
                                    prom_DefTotal_ref = prom_DefTotal_ref,
                                    filtrar = filtrar)
    }
  }
  
  list(
    grupos = grupos,
    patrones = patrones,
    parametros = data.frame(
      modo          = ifelse(!is.null(archivo_areas), "areas_por_imagen", "area_ROI_global"),
      archivo_areas = ifelse(!is.null(archivo_areas), archivo_areas, NA),
      area_ROI_px2  = ifelse(!is.null(area_ROI), area_ROI, NA),
      area_ROI_cm2  = ifelse(!is.null(area_ROI_cm2), area_ROI_cm2, NA),
      factor_a_mm2 = factor_a_mm2
    )
  )
}

# A partir del resultado de procesar_CSVs(), arma una tabla por cada métrica
# con los grupos como columnas y las imágenes como filas — el formato 
# "ancho" que se pega directo en GraphPad, con una tabla por variable 
# en vez de todo apilado en una sola hoja.
crear_tablas_comparativas <- function(resultados) {
  
  grupos   <- resultados$grupos
  patrones <- resultados$patrones
  
  max_rows <- max(sapply(grupos, nrow), 0)
  
  crear_tabla_compar <- function(col_name) {
    df <- as.data.frame(
      lapply(grupos, function(g) {
        col <- rep(NA, max_rows)
        if (nrow(g) > 0) col[seq_len(nrow(g))] <- g[[col_name]]
        col
      }),
      stringsAsFactors = FALSE
    )
    names(df) <- patrones
    return(df)
  }
  
  tablas <- list(
    DefTotal_Rel    = crear_tabla_compar("DefTotal_Rel"),
    Densidad        = crear_tabla_compar("Densidad"),
    Area_mm2_prom   = crear_tabla_compar("Area_mm2_prom"),
    Intensidad_prom = crear_tabla_compar("Intensidad_prom"),
    DefTotal_prom   = crear_tabla_compar("DefTotal_prom"),
    Circ_prom       = crear_tabla_compar("Circ_prom"),
    Round_prom      = crear_tabla_compar("Round_prom"),
    Solidity_prom   = crear_tabla_compar("Solidity_prom"),
    Cantidad        = crear_tabla_compar("Cantidad")
  )
  
  return(tablas)
}

# Creación del archivo de excel
# Escribe una hoja en Excel donde cada tabla es una variable, y cada
# columna corresponde a un grupo/patrón.
agregar_hoja_comparativa_promedios <- function(wb, tablas_filtradas) {
  addWorksheet(wb, "Tablas Comparativas")
  start_col <- 1
  for (i in seq_along(tablas_filtradas)) {
    writeData(wb, sheet = "Tablas Comparativas", x = names(tablas_filtradas)[i], startCol = start_col)
    writeData(wb, sheet = "Tablas Comparativas", x = tablas_filtradas[[i]], startCol = start_col, startRow = 2)
    start_col <- start_col + ncol(tablas_filtradas[[i]]) + 1
  }
  wb
}

# Escribre una hoja en Excel donde se separan por patrón/grupo, y cada una 
# cuenta con una columna por cada variable.
agregar_hoja_metricas_promedios <- function(wb, resultados_filtrados) {
  addWorksheet(wb, "Metricas por grupo")
  fila_actual <- 1
  for (i in seq_along(resultados_filtrados$grupos)) {
    writeData(wb, "Metricas por grupo", resultados_filtrados$patrones[i],
              startRow = fila_actual, startCol = 1)
    writeData(wb, "Metricas por grupo", resultados_filtrados$grupos[[i]],
              startRow = fila_actual + 1, startCol = 1)
    fila_actual <- fila_actual + nrow(resultados_filtrados$grupos[[i]]) + 5
  }
  writeData(wb, "Metricas por grupo", resultados_filtrados$parametros,
            startRow = fila_actual + 3, startCol = 1)
  wb
}

# Arma el Excel completo para la descarga de las variables "por réplica"
armar_wb_promedios <- function(resultados_filtrados, tablas_filtradas, wb = NULL) {
  if (is.null(wb)) wb <- createWorkbook()
  wb <- agregar_hoja_comparativa_promedios(wb, tablas_filtradas)
  wb <- agregar_hoja_metricas_promedios(wb, resultados_filtrados)
  wb
}

# Versión de la función para copiar estos datos al portapapeles.
construir_tabla_comparativa_promedios <- function(tablas_filtradas) {
  if (length(tablas_filtradas) == 0) return(NULL)
  
  bloques <- lapply(seq_along(tablas_filtradas), function(i) {
    nombre <- names(tablas_filtradas)[i]
    tabla  <- tablas_filtradas[[i]]
    
    fila_variable <- c(nombre, rep("", ncol(tabla) - 1))
    fila_patron   <- colnames(tabla)
    
    tabla <- formatear_numeros_es(as.data.frame(tabla, check.names = FALSE))
    datos <- as.data.frame(tabla, check.names = FALSE)
    colnames(datos) <- as.character(seq_len(ncol(datos)))
    rownames(datos) <- NULL
    
    rbind(
      setNames(as.list(fila_variable), colnames(datos)),
      setNames(as.list(fila_patron),   colnames(datos)),
      datos
    )
  })
  
  # Igualar la cantidad de filas entre bloques (por si una variable tiene
  # más muestras que otra) antes de pegarlos lado a lado.
  max_filas <- max(sapply(bloques, nrow))
  bloques <- lapply(bloques, function(b) {
    faltan <- max_filas - nrow(b)
    if (faltan > 0) {
      relleno <- as.data.frame(matrix("", nrow = faltan, ncol = ncol(b)))
      colnames(relleno) <- colnames(b)
      b <- rbind(b, relleno)
    }
    b
  })
  
  separador <- as.data.frame(matrix("", nrow = max_filas, ncol = 1))
  colnames(separador) <- " "
  
  do.call(cbind, lapply(seq_along(bloques), function(i) {
    if (i < length(bloques)) cbind(bloques[[i]], separador) else bloques[[i]]
  }))
}


# ---- Extracción y procesamiento de las metricas punto a punto de cada grupo ----
# Lee y combina todos los CSVs de UN patrón/grupo en una sola tabla "punto a
# punto" (una fila por partícula detectada, en vez de una fila por imagen
# como en procesar_CSVs). Agrega columnas derivadas (CV, intensidad
# calibrada, área en mm2, IntDen calibrado) y una columna "replica" que
# identifica de qué archivo/imagen viene cada partícula.
juntar.CSVs <- function(patron, factor_a_mm2, filtrar = FALSE) {
  
  patron_esc <- escapar_regex(patron)
  archivos <- list.files(pattern = paste0(patron_esc, ".*results-azul\\.csv"),
                         full.names = TRUE, ignore.case = TRUE)
  
  csv.completo <- data.frame()
  
  for (i in 1:length(archivos)) {
    csv          <- read.csv(archivos[i])
    csv$replica  <- i
    csv$patron   <- patron
    csv$CV       <- csv$StdDev / csv$Mean
    csv$Mean_calibrado  <- (csv$Mean - 4.94) / 6.06
    csv$area.mm2 <- csv$Area * factor_a_mm2
    csv$IntDen_calibrado <- csv$Area * csv$Mean_calibrado
    
    if (!"IntDen" %in% colnames(csv)) {
      csv$IntDen <- csv$Area * csv$Mean
    }
    
    csv.completo <- dplyr::bind_rows(csv.completo, csv)
  }
  
  if (filtrar) {
    csv.completo <- dplyr::filter(csv.completo, Area > 50 & Area < 4000)
  }
  
  # Reordenar columnas
  cols_primero <- c("patron", "replica", "X.1", "Label", "IntDen", "IntDen_calibrado",
                    "area.mm2", "Mean", "Mean_calibrado", "Circ.", "Round", "Solidity")
  
  cols_ordenadas <- c(
    intersect(cols_primero, colnames(csv.completo)),
    setdiff(colnames(csv.completo), cols_primero)
  )
  
  csv.completo <- csv.completo[, cols_ordenadas]
  
  return(csv.completo)
}
  # Normaliza IntDen_calibrado contra el control (primer patrón de la lista),
  # agregando tres columnas alternativas a cada data.frame de lista_datos:
  # Tres opciones de normalización, todas calculadas y guardadas como columnas:
  #
  #   IntDen_ratio    — Opción 1: ratio al control
  #                     control = 1, los demás son múltiplos del control
  #                     Ej: 1.5 = 50% más que el control
  #
  #   IntDen_centrado — Opción 2: centrado al control  (sugerencia del tutor)
  #                     control = 0, los demás muestran la diferencia absoluta
  #                     Valores positivos = más que el control; negativos = menos
  #
  #   IntDen_zscore   — Opción 3: z-score relativo al control
  #                     Igual que el centrado pero además escala por la SD del
  #                     control; el resultado es cuántas SDs del control se aleja
  #                     cada observación. Útil si la variabilidad del control es
  #                     grande y querés relativizar también a eso.
  normalizar_intden_calibrado <- function(lista_datos) {
    if (length(lista_datos) == 0) return(lista_datos)
    
    primer_patron  <- names(lista_datos)[1]
    control_valores <- lista_datos[[primer_patron]]$IntDen_calibrado
    media_control   <- mean(control_valores, na.rm = TRUE)
    sd_control      <- sd(control_valores, na.rm = TRUE)
    
    lapply(lista_datos, function(df) {
      df$IntDen_calibrado_ratio    <- df$IntDen_calibrado / media_control
      df$IntDen_calibrado_centrado <- df$IntDen_calibrado - media_control
      df$IntDen_calibrado_zscore   <- (df$IntDen_calibrado - media_control) / sd_control
      df
    })
}

# Función principal del análisis "punto a punto": procesa todos los patrones,
# arma un Workbook con una hoja por patrón (todas sus partículas) más una 
# hoja "Comparacion" con las variables elegidas. Devuelve tanto el
# Workbook como la lista de datos por patrón (esta última se reutiliza
# después para armar el Excel filtrado y para copiar al portapapeles).
procesar_puntos <- function(patrones, filtrar = FALSE){
  
  factor_a_mm2 <- 0.000448
  
  wb <- createWorkbook()
  
  lista_datos <- list()
  
  for (patron in patrones) {
    
    message("Procesando patrón: ", patron)
    
    datos <- tryCatch(
      juntar.CSVs(patron, factor_a_mm2, filtrar),
      error = function(e) {
        warning("Error en patrón '", patron, "': ", e$message)
        NULL
      }
    )
    
    if (is.null(datos) || nrow(datos) == 0) {
      message("  Sin datos para '", patron, "' — hoja omitida.")
      next
    }
    
    lista_datos[[patron]] <- datos
  }
  
  # La normalización necesita el control (primer patrón) ya cargado, y se
  # hace ANTES de escribir las hojas para que IntDen_calibrado_ratio /
  # _centrado / _zscore ya aparezcan tanto en las hojas por patrón como en
  # "Comparacion".
  lista_datos <- normalizar_intden_calibrado(lista_datos)
  
  for (patron in names(lista_datos)) {
    # Los nombres de hoja no pueden superar 31 caracteres
    nombre_hoja <- substr(patron, 1, 31)
    addWorksheet(wb, nombre_hoja)
    writeData(wb, nombre_hoja, lista_datos[[patron]])
  }
  
  # ── Hoja de comparación ───────────────────────────────────────────────────────
  
  vars_comparar <- c("IntDen", "IntDen_calibrado", "IntDen_calibrado_ratio",
                     "IntDen_calibrado_centrado", "IntDen_calibrado_zscore",
                     "area.mm2", "Mean", "Mean_calibrado", "Circ.", "Round", "Solidity")
  
  max_filas <- max(sapply(lista_datos, nrow))
  
  bloques <- lapply(vars_comparar, function(var) {
    cols <- lapply(lista_datos, function(df) {
      vals <- if (var %in% colnames(df)) df[[var]] else rep(NA, nrow(df))
      # Padding para igualar largo
      length(vals) <- max_filas
      vals
    })
    as.data.frame(cols, check.names = FALSE)
  })
  
  # Intercalar columna en blanco entre bloques
  separador <- as.data.frame(matrix(NA, nrow = max_filas, ncol = 1))
  colnames(separador) <- " "
  
  hoja_comp <- do.call(cbind, lapply(seq_along(bloques), function(i) {
    if (i < length(bloques)) cbind(bloques[[i]], separador)
    else bloques[[i]]
  }))
  
  # Headers: nombre de variable sobre cada bloque
  header_row <- unlist(lapply(seq_along(vars_comparar), function(i) {
    c(vars_comparar[i], rep("", length(patrones) - 1),
      if (i < length(vars_comparar)) "" else NULL)  # celda del separador
  }))
  
  addWorksheet(wb, "Comparacion")
  writeData(wb, "Comparacion", t(header_row), startRow = 1, colNames = FALSE)
  writeData(wb, "Comparacion", hoja_comp, startRow = 2)
  
  return(list(
    wb = wb,
    grupos = lista_datos
  ))
}


# ---- Métricas elegidas punto a punto ----
# Versión "filtrada" de la hoja de comparación de procesar_puntos(). Arma el archivo de Excel
# y las tablas solo con las variables seleccionadas por el usuario.
agregar_hoja_comparacion_puntos <- function(wb, lista_datos, patrones, vars_comparar) {
  vars_comparar <- intersect(vars_comparar,
                             unique(unlist(lapply(lista_datos, colnames))))
  if (length(vars_comparar) == 0) return(wb)
  
  max_filas <- max(sapply(lista_datos[patrones], nrow))
  bloques <- lapply(vars_comparar, function(var) {
    cols <- lapply(lista_datos[patrones], function(df) {
      vals <- if (var %in% colnames(df)) df[[var]] else rep(NA, nrow(df))
      length(vals) <- max_filas
      vals
    })
    as.data.frame(cols, check.names = FALSE)
  })
  
  separador <- as.data.frame(matrix(NA, nrow = max_filas, ncol = 1))
  colnames(separador) <- " "
  
  hoja_comp <- do.call(cbind, lapply(seq_along(bloques), function(i) {
    if (i < length(bloques)) cbind(bloques[[i]], separador) else bloques[[i]]
  }))
  
  header_row <- unlist(lapply(seq_along(vars_comparar), function(i) {
    c(vars_comparar[i], rep("", length(patrones) - 1),
      if (i < length(vars_comparar)) "" else NULL)
  }))
  
  addWorksheet(wb, "Comparacion")
  writeData(wb, "Comparacion", t(header_row), startRow = 1, colNames = FALSE)
  writeData(wb, "Comparacion", hoja_comp, startRow = 2)
  wb
}

# ---- Función copiar al portapapeles ----
# Mantiene el formato de la hoja "comparación" de los archivos Excel. Se mantiene como función
# aparte para no alterar el formato de la hoja de Excel.
construir_tabla_comparacion_puntos <- function(lista_datos, patrones, vars_comparar) {
  vars_comparar <- intersect(vars_comparar,
                             unique(unlist(lapply(lista_datos, colnames))))
  if (length(vars_comparar) == 0) return(NULL)
  
  max_filas <- max(sapply(lista_datos[patrones], nrow))
  bloques <- lapply(vars_comparar, function(var) {
    cols <- lapply(lista_datos[patrones], function(df) {
      vals <- if (var %in% colnames(df)) df[[var]] else rep(NA, nrow(df))
      length(vals) <- max_filas
      vals
    })
    as.data.frame(cols, check.names = FALSE)
  })
  
  datos <- do.call(cbind, bloques)
  datos <- formatear_numeros_es(datos)
  
  fila_variable <- unlist(lapply(seq_along(vars_comparar), function(i) {
    c(vars_comparar[i], rep("", length(patrones) - 1))
  }))
  fila_patron <- rep(patrones, length(vars_comparar))
  
  colnames(datos) <- as.character(seq_len(ncol(datos)))  # nombres temporales, no importan
  
  # Dos filas de encabezado + los datos, todo como texto/tabla plana
  rbind(
    setNames(as.list(fila_variable), colnames(datos)),
    setNames(as.list(fila_patron),   colnames(datos)),
    datos
  )
}

# Agrega al Workbook, una hoja por patrón con su tabla punto a punto completa.
agregar_hojas_patron_puntos <- function(wb, lista_datos, patrones) {
  for (patron in patrones) {
    datos <- lista_datos[[patron]]
    if (is.null(datos) || nrow(datos) == 0) next
    nombre_hoja <- substr(patron, 1, 31)
    addWorksheet(wb, nombre_hoja)
    writeData(wb, nombre_hoja, datos)
  }
  wb
}

# Arma el Workbook completo para la descarga.
armar_wb_puntos <- function(lista_datos, patrones, vars_comparar, wb = NULL) {
  if (is.null(wb)) wb <- createWorkbook()
  wb <- agregar_hoja_comparacion_puntos(wb, lista_datos, patrones, vars_comparar)
  wb <- agregar_hojas_patron_puntos(wb, lista_datos, patrones)
  wb
}

# ---- Filtrado de resultados según la selección del usuario (modal de descarga) ----
# Filtra el objeto `resultados` (de procesar_CSVs) por muestras y columnas
filtrar_resultados <- function(resultados, muestras, parametros) {
  grupos_filtrados <- resultados$grupos[muestras]
  cols <- c("Archivo", parametros)  # Archivo siempre se mantiene
  grupos_filtrados <- lapply(grupos_filtrados, function(g) {
    g[, intersect(cols, colnames(g)), drop = FALSE]
  })
  list(
    grupos = grupos_filtrados,
    patrones = muestras,
    parametros = resultados$parametros
  )
}

# Filtra la lista de tablas comparativas (de crear_tablas_comparativas)
filtrar_tablas_comparativas <- function(tablas, muestras, parametros) {
  tablas_filtradas <- tablas[intersect(names(tablas), parametros)]                               # Solo dejamos las tablas cuyo nombre está en los parametros elegidos
  lapply(tablas_filtradas, function(df) df[, intersect(muestras, colnames(df)), drop = FALSE])   # Y de cada tabla, solo las columnas (muestras) elegidas
}

# Filtra lista_datos (de procesar_puntos) por muestras y columnas
filtrar_puntos <- function(lista_datos, muestras, parametros) {
  lista_datos[muestras] |>
    lapply(function(df) {
      cols_fijas <- intersect(c("patron", "replica", "X.1", "Label"), colnames(df))
      df[, c(cols_fijas, intersect(parametros, colnames(df))), drop = FALSE]
    })
}


# ---- Generación del archivo a partir de la elección del usuario ----
# Descarga en Excel según el "tipo" seleccionado por el usuario ("promedios", "puntos",
# o "ambos).
generar_excel_descarga <- function(tipo, muestras, parametros,
                                   resultados = NULL, wb_puntos = NULL) {
  if (tipo == "promedios") {
    resultados_filtrados <- filtrar_resultados(resultados, muestras, parametros)
    tablas <- crear_tablas_comparativas(resultados)
    tablas_filtradas <- filtrar_tablas_comparativas(tablas, muestras, parametros)
    return(armar_wb_promedios(resultados_filtrados, tablas_filtradas))
  }
  if (tipo == "puntos") {
    lista_filtrada <- filtrar_puntos(wb_puntos$grupos, muestras, parametros)
    return(armar_wb_puntos(lista_filtrada, muestras, parametros))
  }
  if (tipo == "ambos") {
    # `parametros` puede traer mezclados nombres de columna de resumen y de
    # puntos (según lo que haya elegido el usuario en el picker combinado);
    # se separan comparando contra los diccionarios de cada lado.
    parametros_prom <- intersect(parametros, vars_promedios)
    parametros_pap  <- intersect(parametros, vars_puntos)
    
    wb <- createWorkbook()
    algo_agregado <- FALSE
    
    # 1) Comparacion (puntos) y Tablas Comparativas (resumen)
    if (length(parametros_pap) > 0 && !is.null(wb_puntos)) {
      lista_filtrada <- filtrar_puntos(wb_puntos$grupos, muestras, parametros_pap)
      wb <- agregar_hoja_comparacion_puntos(wb, lista_filtrada, muestras, parametros_pap)
      algo_agregado <- TRUE
    }
    
    if (length(parametros_prom) > 0 && !is.null(resultados)) {
      tablas <- crear_tablas_comparativas(resultados)
      tablas_filtradas <- filtrar_tablas_comparativas(tablas, muestras, parametros_prom)
      wb <- agregar_hoja_comparativa_promedios(wb, tablas_filtradas)
      algo_agregado <- TRUE
    }
    
    # 2) hojas de puntos y Metricas por grupo
    if (length(parametros_pap) > 0 && !is.null(wb_puntos)) {
      lista_filtrada <- filtrar_puntos(wb_puntos$grupos, muestras, parametros_pap)
      wb <- agregar_hojas_patron_puntos(wb, lista_filtrada, muestras)
    }
    
    if (length(parametros_prom) > 0 && !is.null(resultados)) {
      resultados_filtrados <- filtrar_resultados(resultados, muestras, parametros_prom)
      wb <- agregar_hoja_metricas_promedios(wb, resultados_filtrados)
    }
    
    if (!algo_agregado) {
      stop("No se seleccionó ningún parámetro válido de resumen ni de punto a punto para 'Ambos'.")
    }
    
    return(wb)
  }
  stop("Tipo de descarga no reconocido")
}


# ---- Exportación a GraphPad Prism (.pzfx) ----------------------------------
# A diferencia del Excel (que arma hojas apiladas por archivo/réplica), Prism
# necesita cada variable como una tabla aparte, con los grupos como columnas
# ("formato ancho"). Estas funciones arman esas tablas reutilizando los
# mismos objetos que ya calcula la app al procesar los CSV (resultados() y
# wb_puntos()).

# Arma, para "puntos", una tabla ancha por variable (1 columna por grupo,
# rellenada con NA hasta la cantidad máxima de partículas de cualquier
# grupo). Es la versión "objeto" de lo que procesar_puntos()/armar_wb_puntos()
# escriben en la hoja "Comparacion" de Excel.
armar_bloques_puntos <- function(lista_datos, patrones, vars_comparar) {
  
  vars_comparar <- intersect(vars_comparar,
                             unique(unlist(lapply(lista_datos, colnames))))
  if (length(vars_comparar) == 0) return(list())
  
  max_filas <- max(sapply(lista_datos[patrones], nrow))
  
  bloques <- lapply(vars_comparar, function(var) {
    columnas <- lapply(lista_datos[patrones], function(df) {
      vals <- if (var %in% colnames(df)) df[[var]] else rep(NA_real_, nrow(df))
      length(vals) <- max_filas   # rellena con NA hasta el máximo de filas
      vals
    })
    tabla <- as.data.frame(columnas, check.names = FALSE)
    names(tabla) <- patrones
    tabla
  })
  
  names(bloques) <- vars_comparar
  bloques
}

# Una tabla se considera "vacía" si no tiene filas o no tiene ni un solo
# valor que no sea NA. Se descartan antes de escribir el .pzfx para no
# generar hojas en blanco en Prism.
tabla_pzfx_vacia <- function(df) {
  nrow(df) == 0 || all(is.na(df))
}

# Arma la lista de tablas lista para pasarle a pzfx::write_pzfx(), a partir
# de la misma selección de muestras/parámetros que usa la descarga a Excel.
# Al seleccionar "ambos", junta las tablas, y las variables se distinguen con
# el sufijo ("por réplica" o "por punto").
generar_tablas_pzfx <- function(tipo, muestras, parametros,
                                resultados = NULL, wb_puntos = NULL) {
  
  if (tipo == "promedios") {
    tablas <- crear_tablas_comparativas(resultados)
    tablas <- filtrar_tablas_comparativas(tablas, muestras, parametros)
  } else if (tipo == "puntos") {
    tablas <- armar_bloques_puntos(wb_puntos$grupos[muestras], muestras, parametros)
  } else if (tipo == "ambos") {
    parametros_prom <- intersect(parametros, vars_promedios)
    parametros_pap  <- intersect(parametros, vars_puntos)
    
    tablas_prom <- list()
    if (length(parametros_prom) > 0 && !is.null(resultados)) {
      todas <- crear_tablas_comparativas(resultados)
      tablas_prom <- filtrar_tablas_comparativas(todas, muestras, parametros_prom)
      names(tablas_prom) <- paste0(names(tablas_prom), " (por replica)")
    }
    
    tablas_pap <- list()
    if (length(parametros_pap) > 0 && !is.null(wb_puntos)) {
      tablas_pap <- armar_bloques_puntos(wb_puntos$grupos[muestras], muestras, parametros_pap)
      names(tablas_pap) <- paste0(names(tablas_pap), " (por punto)")
    }
    
    tablas <- c(tablas_prom, tablas_pap)
    
  } else {
    stop("Tipo de descarga no reconocido")
  }
  
  tablas <- tablas[!sapply(tablas, tabla_pzfx_vacia)]
  
  if (length(tablas) == 0) {
    stop("No hay datos para exportar con la selección actual de muestras y parámetros.")
  }
  
  tablas
}

