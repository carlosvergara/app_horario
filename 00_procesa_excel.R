
library(readxl)
library(data.table)

# Descargar archivo de:
# https://www.uv.es/uvweb/grado-enfermeria/es/se-estudia/horarios-examenes/horarios-examenes-1285929126385.html
# Ubicarlo en este mismo directorio
arch    <- list.files(pattern = "\\.xlsx?$", full.names = TRUE)
hoja    <- grep(pattern = "horari", x = excel_sheets(arch), ignore.case = TRUE)
horario <- setDT(read_excel(path = arch, sheet = hoja))

# Listado de códigos extraído desde el mismo archivo (tabla.csv)
# A mano al estar formateado de forma variable
codigos_enfermeria <- fread(file = "tabla.csv", colClasses = "character")$codi_base

# Definir horas
hour_cols <- sprintf("h%02d00", 8:20)

# Eliminar filas sin ninguna clase de Enfermería
horario <- horario[
  (rowSums(!is.na(horario[, ..hour_cols]) &
             horario[, ..hour_cols] != "") > 0)
]

# Mantener columnas para la app
keep_cols <- c("id", "dia", "smstr", "nd", "tipus", "codaula", hour_cols)
horario   <- horario[, ..keep_cols]

# Comprobar estructura
horario_old <- fread("horario.csv")
if (!identical(names(horario), names(horario_old))) {
  stop(
    paste0(
      "La estructura del nuevo horario no coincide con horario.csv.\n",
      "Columnas esperadas: ",
      paste(names(horario_old), collapse = ", "),
      "\nColumnas obtenidas: ",
      paste(names(horario), collapse = ", ")
    )
  )
}
if (nrow(horario) == 0L) {
  stop("El horario procesado no contiene ninguna fila.")
}

# Igual que el fichero antiguo: separado por tabuladores
fwrite(x = horario, file = "horario.csv")
