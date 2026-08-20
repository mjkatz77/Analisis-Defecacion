# ----- Compliar.R -----
# Este scritp permite, a partir de la app creada en la carpeta "appdir",
# crear un ejecutable que pueda ser utilizado en otra computadora sin
# necesidad de instalar nada más, solo descargar esa carpeta. Tanto 
# R como las librerías empleadas a lo largo de la app, están incluidas
# en este ejecutable que se crea.

#Paquetes necesarios para poder corres este script de forma local
# shinyelectron::install_nodejs()
# install.packages("renv")
library(shinyelectron)

# Ejecutar el empaquetado
export(
  appdir  = "C:/Users/ramig/Desktop/Proyecto Final/Optimizacion Software/InterfazGrafica/App_LabGCO_v1.0.0",
  destdir = "C:/Users/ramig/Desktop/Proyecto Final/Optimizacion Software/InterfazGrafica/AccesoDirecto_App",
  runtime_strategy = "bundled",
  overwrite = TRUE
)
