# -----------------------------------------------------
# Instalador de dependencias
# Ejecutar una única vez antes de usar la aplicación
# -----------------------------------------------------

# Lista de paquetes necesarios
paquetes <- c(
  "PMCMRplus",
  "dunn.test",
  "openxlsx",
  "ggplot2",
  "shiny",
  "shinyFiles",
  "shinyjs",
  "shinyWidgets",
  "zip",
  "pzfx"
)

# Instalar únicamente los que faltan
instalados <- rownames(installed.packages())
faltantes <- setdiff(paquetes, instalados)

if(length(faltantes) > 0){
  install.packages(faltantes, dependencies = TRUE)
} else {
  message("Todos los paquetes ya están instalados.")
}

message("Instalación finalizada.")