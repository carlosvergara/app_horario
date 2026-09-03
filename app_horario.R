
# Carga de paquetes y datos ---------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(data.table)
library(lubridate)
library(DT)
library(uuid)

tabla_codigos <- fread("tabla.csv", colClasses = "character")
horario       <- fread(file = "horario.csv")


# Funciones -------------------------------------------------------------------------
squish_base <- function(x) {
  x <- trimws(as.character(x))
  
  gsub("[[:space:]]+", " ", x)
}

extrae_codigo <- function(x) {
  out     <- rep(NA_character_, length(x))
  ok      <- !is.na(x) & grepl("^[0-9]{5}", x)
  out[ok] <- substr(x[ok], 1L, 5L)
  
  out
}

extrae_subgrupo <- function(x) {
  out     <- rep("", length(x))
  ok      <- !is.na(x) & grepl("^[0-9]{5}[A-Z]{1,3}", x, perl = TRUE)
  out[ok] <- sub("^[0-9]{5}([A-Z]{1,3}).*$", "\\1", x[ok], perl = TRUE)
  
  out
}

fold_ical_line <- function(x, width = 73) {
  if (nchar(x, type = "bytes") <= width) return(x)
  out  <- character()
  line <- x
  while (nchar(line, type = "bytes") > width) {
    cut <- width
    out <- c(out, substr(line, 1L, cut))
    line <- paste0(" ", substr(line, cut + 1L, nchar(line)))
  }
  
  c(out, line)
}

ics_escape <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub(";", "\\\\;", x)
  x <- gsub(",", "\\\\,", x)
  x <- gsub("\r?\n", "\\\\n", x, perl = TRUE)
  
  x
}

ics_dt_utc <- function(t) {
  t <- lubridate::with_tz(t, "UTC")
  
  format(t, "%Y%m%dT%H%M%SZ")
}

ics_event <- function(
    dt_start,
    dt_end,
    summary,
    location            = "",
    description         = "",
    alarm_display       = FALSE,
    alarm_display_min   = 15,
    alarm_email         = FALSE,
    alarm_email_min     = 15,
    alarm_email_address = ""
) {
  
  lines <- c(
    "BEGIN:VEVENT",
    paste0("UID:", uuid::UUIDgenerate()),
    paste0("DTSTAMP:", ics_dt_utc(Sys.time())),
    paste0("DTSTART:", ics_dt_utc(dt_start)),
    paste0("DTEND:", ics_dt_utc(dt_end)),
    paste0("SUMMARY:", ics_escape(summary))
  )
  
  if (nzchar(location)) {
    lines <- c(lines, paste0("LOCATION:", ics_escape(location)))
  }
  if (nzchar(description)) {
    lines <- c(lines, paste0("DESCRIPTION:", ics_escape(description)))
  }
  
  # Notificación
  if (isTRUE(alarm_display)) {
    lines <- c(
      lines,
      "BEGIN:VALARM",
      paste0(
        "TRIGGER:-PT",
        as.integer(alarm_display_min),
        "M"
      ),
      "ACTION:DISPLAY",
      paste0(
        "DESCRIPTION:",
        ics_escape(paste0("Recordatorio: ", summary))
      ),
      "END:VALARM"
    )
  }
  
  # Correo electrónico
  if (isTRUE(alarm_email)) {
    email <- gsub("[\r\n]", "", trimws(alarm_email_address))
    lines <- c(
      lines,
      "BEGIN:VALARM",
      paste0(
        "TRIGGER:-PT",
        as.integer(alarm_email_min),
        "M"
      ),
      "ACTION:EMAIL",
      paste0("ATTENDEE:MAILTO:", email),
      paste0(
        "SUMMARY:",
        ics_escape(paste0("Recordatorio: ", summary))
      ),
      paste0(
        "DESCRIPTION:",
        ics_escape(
          paste0(
            "El evento ",
            summary,
            " comienza en ",
            as.integer(alarm_email_min),
            " minutos."
          )
        )
      ),
      "END:VALARM"
    )
  }
  lines  <- c(lines, "END:VEVENT")
  folded <- unlist(lapply(lines, fold_ical_line), use.names = FALSE)
  
  paste0(paste(folded, collapse = "\r\n"), "\r\n")
}

ics_calendar <- function(events_txt, calname = "Horario") {
  head <- c(
    "BEGIN:VCALENDAR",
    "VERSION:2.0",
    "PRODID:-//Horario Shiny//ES",
    "CALSCALE:GREGORIAN",
    "METHOD:PUBLISH",
    paste0("X-WR-CALNAME:", ics_escape(calname))
  )
  tail <- "END:VCALENDAR"
  body <- paste0(events_txt, collapse = "")
  
  paste0(paste(head, collapse = "\r\n"), "\r\n", body, tail, "\r\n")
}

formatea_eventos <- function(evs) {
  d <- copy(evs)
  d[, `:=`(
    Fecha      = as.character(as.Date(ini)),
    Inicio     = format(ini, "%H:%M"),
    Fin        = format(fin, "%H:%M"),
    Aula       = codigo_aula,
    Curso      = ifelse(is.na(curso), "", paste0(curso, "º")),
    Codigo     = codi_base,
    Subgrupo   = ifelse(is.na(subgrupo) | subgrupo == "", "-", subgrupo),
    Asignatura = asignatura
  )]
  
  d[, .(Fecha, Inicio, Fin, Aula, Curso, Codigo, Subgrupo, Asignatura)]
}


# Trabajo con los datos -------------------------------------------------------------
# Renombrar 6 primeras columnas a nombres internos
setnames(
  x   = horario,
  old = names(horario)[1:6],
  new = c("id_dia_aula", "dia", "semestre", "dia_semana", "tipo_docencia", "codigo_aula")
)

# Columnas de hora (h0800..h2000)
hour_cols <- grep("^h\\d{4}$", names(horario), value = TRUE)
stopifnot(length(hour_cols) == 13L)


# Transformación a sesiones por hora
sesiones_hora <- melt(
  data            = horario,
  id.vars         = setdiff(names(horario), hour_cols),
  measure.vars    = hour_cols,
  variable.name   = "col_hora",
  value.name      = "asignatura",
  variable.factor = FALSE
)

sesiones_hora[, `:=`(
  asignatura = squish_base(asignatura),
  hora_num   = as.integer(substr(col_hora, 2L, 3L))
)]
sesiones_hora[, valida := !is.na(asignatura) & asignatura != ""]
sesiones_hora <- sesiones_hora[(valida) & !is.na(dia) & !is.na(hora_num)]
sesiones_hora[, `:=`(
  ini       = lubridate::make_datetime(
    lubridate::year(dia),
    lubridate::month(dia),
    lubridate::day(dia),
    hour = hora_num,
    tz = "Europe/Madrid"
  ),
  codi_base = extrae_codigo(asignatura),
  subgrupo  = extrae_subgrupo(asignatura)
)]

sesiones_hora[, fin := ini + lubridate::hours(1)]

# Equivalente al left_join() original con tabla_codigos
sesiones_hora[tabla_codigos, on = .(codi_base), `:=`(curso = i.curso, nom = i.nom)]


# UI --------------------------------------------------------------------------------
header <- dashboardHeader(
  title = tags$span(
    tags$span("Horarios 2026/27", class = "titulo-completo"),
    tags$span(
      icon("calendar"),
      tags$span("26/27", class = "curso-mini"),
      class = "titulo-mini"
    )
  ),
  tags$li(a(href = "mailto:fipdeganat@uv.es", icon("envelope")), class = "dropdown"),
  tags$li(a(
    href   = "https://github.com/carlosvergara/app_horario",
    target = "_blank",
    icon("github")
  ),
  class = "dropdown"
  ),
  leftUi = tagList(
    dropdownBlock(
      badgeStatus = NULL,
      id          = "seleccion",
      title       = "Titulación, curso, asignatura, grupo",
      icon        = icon("magnifying-glass"),
      selectInput(
        "titulacion",
        "Titulación",
        choices  = c("Enfermería", "Podología"),
        selected = "Enfermería"
      ),
      selectizeInput(
        "curso",
        "Curso",
        choices  = setNames(1:4, paste0(1:4, "º")),
        multiple = TRUE,
        options  = list(placeholder = "Todos")
      ),
      selectizeInput(
        "codigos",
        "Asignaturas (código + nombre)",
        choices  = NULL,
        multiple = TRUE,
        options  = list(placeholder = "Selecciona código(s) base...")
      ),
      selectInput("subgrupo", "Subgrupo", choices = "Todos", selected = "Todos")
    ),
    dropdownBlock(
      badgeStatus = NULL,
      id          = "aulas",
      title       = "Filtro por aula y fechas",
      icon        = icon("filter"),
      selectInput("aula", "Aula", choices = "Todas", selected = "Todas"),
      dateRangeInput(
        "rango",
        "Rango de fechas",
        start     = min(sesiones_hora$dia, na.rm = TRUE),
        end       = max(sesiones_hora$dia, na.rm = TRUE),
        startview = "month",
        language  = "es",
        separator = " a ",
      )
    )
  )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "lateral",
    menuItem(text = "Horario", tabName = "mitabla", icon = icon("calendar-check")),
    menuItem(text = "Exámenes", tabName = "examenes", icon = icon("file-lines"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "mitabla",
      fluidRow(
        box(
          width       = 12,
          title       = "Sesiones encontradas",
          status      = "primary",
          solidHeader = TRUE,
          DTOutput("tabla"),
          br(),
          div(
            style = "display:flex; gap:10px;",
            actionButton(
              "config_ics",
              "Descargar calendario (formato .ics)",
              icon = icon("download")
            ),
            downloadButton("dl_csv", "Descargar tabla de datos (.csv)")
          )
        )
      )
    ),
    tabItem(
      tabName = "examenes",
      fluidRow(
        box(
          width       = 12,
          title       = "Exámenes",
          status      = "primary",
          solidHeader = TRUE,
          selectInput(
            "convocatoria",
            "Convocatoria",
            choices = c(
              "Todas",
              "1ª convocatoria",
              "2ª convocatoria"
            ),
            selected = "Todas"
          ),
          DTOutput("tabla_examenes")
        )
      )
    )
  ),
  tags$head(
    tags$style(HTML("
    .titulo-mini {
      display: none;
    }

    .sidebar-mini.sidebar-collapse .titulo-completo {
      display: none;
    }

    .sidebar-mini.sidebar-collapse .main-header .logo {
      width: 50px;
      padding: 0;
      line-height: normal;
    }

    .sidebar-mini.sidebar-collapse .titulo-mini {
      display: flex;
      width: 50px;
      height: 50px;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      font-size: 15px;
      line-height: 1;
    }

    .sidebar-mini.sidebar-collapse .titulo-mini .curso-mini {
      display: block;
      margin-top: 3px;
      font-size: 9px;
      line-height: 1;
    }
    
     .main-header .logo,
    .main-header .navbar {
      background-color: #002C52 !important;
    }

    .skin-blue .main-sidebar,
    .skin-blue .left-side {
      background-color: #002C52;
    }
    
    .box.box-solid.box-primary > .box-header {
      background-color: #002C52 !important;
      color: white !important;
    }

    .box.box-solid.box-primary {
      border-color: #002C52 !important;
    }
  "))
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "blue", title = "Enfermería UVEG 2026/27")

# Idioma
idioma_dt <- list(
  lengthMenu   = "Mostrar _MENU_ registros",
  search       = "Buscar:",
  zeroRecords  = "No se encontraron registros",
  info         = "Mostrando _START_ a _END_ de _TOTAL_ registros",
  infoEmpty    = "No hay registros disponibles",
  infoFiltered = "(filtrados de _MAX_ registros)",
  paginate     = list(
    first    = "Primero",
    last     = "Último",
    `next`   = "Siguiente",
    previous = "Anterior"
  )
)

# Server ----------------------------------------------------------------------------
server <- function(input, output, session) {
  # --- Helpers reactivos para choices dependientes ---
  
  # Filtro titulación
  codigos_titulacion <- reactive({
    tabla_codigos[titulacion == input$titulacion, codi_base]
  })
  sesiones_titulacion <- reactive({
    sesiones_hora[(codi_base %in% codigos_titulacion())]
  })
  
  # Códigos presentes, restringidos por curso si procede
  codigos_presentes <- reactive({
    d <- sesiones_titulacion()
    if (!is.null(input$curso) && length(input$curso) > 0L) {
      d <- d[(!is.na(curso)) & (curso %in% input$curso)]
    }
    
    codigos <- unique(d[!is.na(codi_base), codi_base])
    cod_df  <- tabla_codigos[(codi_base %in% codigos)]
    setorder(cod_df, curso, codi_base)
    cod_df[, display := paste0(codi_base, " — ", nom, " (", curso, "º)")]
    
    cod_df
  })
  
  # Subgrupos disponibles, restringidos por curso y por códigos seleccionados si los hay
  subgrupos_presentes <- reactive({
    d <- sesiones_titulacion()
    if (!is.null(input$curso) && length(input$curso) > 0L) {
      d <- d[(!is.na(curso)) & (curso %in% input$curso)]
    }
    if (!is.null(input$codigos) && length(input$codigos) > 0L) {
      d <- d[(codi_base %in% input$codigos)]
    }
    
    sort(unique(d[!is.na(subgrupo) & subgrupo != "", subgrupo]))
  })
  
  # Aulas disponibles según los filtros activos y la pestaña mostrada
  observeEvent(
    list(
      input$lateral, input$titulacion, input$curso, input$codigos,
      input$subgrupo, input$rango, input$convocatoria
    ),
    {
      d <- sesiones_titulacion()
      
      if (!is.null(input$curso) && length(input$curso) > 0L) {
        d <- d[(!is.na(curso)) & (curso %in% input$curso)]
      }
      
      if (!is.null(input$codigos) && length(input$codigos) > 0L) {
        d <- d[(codi_base %in% input$codigos)]
      }
      
      if (!is.null(input$rango[1]) && !is.null(input$rango[2])) {
        fecha_ini <- as.Date(input$rango[1])
        fecha_fin <- as.Date(input$rango[2])
        d <- d[(dia >= fecha_ini) & (dia <= fecha_fin)]
      }
      
      if (identical(input$lateral, "examenes")) {
        d <- d[grepl("[12][ªa][[:space:]]*CONV", asignatura)]
        
        if (!is.null(input$convocatoria) && input$convocatoria == "1ª convocatoria") {
          d <- d[grepl("1[ªa][[:space:]]*CONV", asignatura)]
        } else if (!is.null(input$convocatoria) && input$convocatoria == "2ª convocatoria") {
          d <- d[grepl("2[ªa][[:space:]]*CONV", asignatura)]
        }
      } else if (!is.null(input$subgrupo) && input$subgrupo != "Todos") {
        d <- d[(subgrupo == input$subgrupo)]
      }
      
      aulas <- sort(na.omit(unique(d$codigo_aula)))
      sel_aula <- isolate(input$aula)
      if (is.null(sel_aula) || !(sel_aula %in% aulas)) sel_aula <- "Todas"
      
      updateSelectInput(
        session,
        "aula",
        choices  = c("Todas", aulas),
        selected = sel_aula
      )
    },
    ignoreInit = FALSE
  )
  
  # Actualiza choices de códigos cuando cambie el curso
  observeEvent(list(input$curso, input$titulacion), {
    cod_df      <- codigos_presentes()
    choices_cod <- setNames(cod_df$codi_base, cod_df$display)
    sel_raw     <- if (is.null(input$codigos)) character(0) else input$codigos
    sel_ok      <- intersect(sel_raw, cod_df$codi_base)
    
    updateSelectizeInput(
      session,
      "codigos",
      choices  = choices_cod,
      selected = sel_ok,
      server   = TRUE
    )
    
    # Al cambiar curso, también refrescamos subgrupos en función
    # de curso + códigos actuales
    subs    <- subgrupos_presentes()
    sel_sub <- if (!is.null(input$subgrupo) && input$subgrupo %in% subs) {
      input$subgrupo
    } else {
      "Todos"
    }
    
    updateSelectInput(
      session,
      "subgrupo",
      choices  = c("Todos", subs),
      selected = sel_sub
    )
  }, ignoreInit = FALSE)
  
  # Actualiza choices de subgrupos cuando cambien los códigos seleccionados
  observeEvent(input$codigos, {
    subs    <- subgrupos_presentes()
    sel_sub <- if (!is.null(input$subgrupo) && input$subgrupo %in% subs) {
      input$subgrupo
    } else {
      "Todos"
    }
    updateSelectInput(
      session,
      "subgrupo",
      choices  = c("Todos", subs),
      selected = sel_sub
    )
  }, ignoreInit = FALSE)
  
  # Filtro reactivo principal
  dat_filtrado <- reactive({
    d <- sesiones_titulacion()
    
    # Curso (1..4)
    if (!is.null(input$curso) && length(input$curso) > 0L) {
      d <- d[(!is.na(curso)) & (curso %in% input$curso)]
    }
    
    # Asignaturas por código base (selección múltiple)
    if (!is.null(input$codigos) && length(input$codigos) > 0L) {
      d <- d[(codi_base %in% input$codigos)]
    }
    
    # Aula
    if (input$aula != "Todas") d <- d[(codigo_aula == input$aula)]
    
    # Rango de fechas
    if (!is.null(input$rango[1]) && !is.null(input$rango[2])) {
      fecha_ini <- as.Date(input$rango[1])
      fecha_fin <- as.Date(input$rango[2])
      d <- d[(dia >= fecha_ini) & (dia <= fecha_fin)]
    }
    
    d
  })
  
  # Compactar bloques contiguos de 1 h en eventos
  eventos_base <- reactive({
    d <- dat_filtrado()
    
    if (nrow(d) == 0L) {
      return(data.table(
        dia         = as.Date(character()),
        codigo_aula = character(),
        asignatura  = character(),
        ini         = as.POSIXct(character(), tz = "Europe/Madrid"),
        fin         = as.POSIXct(character(), tz = "Europe/Madrid"),
        curso       = integer(),
        codi_base   = character(),
        subgrupo    = character()
      ))
    }
    
    d <- copy(d)
    setorder(d, dia, codigo_aula, asignatura, hora_num)
    
    d[,
      corte := cumsum(c(TRUE, diff(hora_num) != 1L)),
      by     = .(dia, codigo_aula, asignatura, curso, codi_base, subgrupo)
    ]
    
    d <- d[, .(
      ini = min(ini, na.rm = TRUE),
      fin = max(fin, na.rm = TRUE)
    ),
    by = .(dia, codigo_aula, asignatura, curso, codi_base, subgrupo, corte)]
    setorder(d, ini)
    
    d
  })
  
  eventos <- reactive({
    d <- copy(eventos_base())
    if (!is.null(input$subgrupo) && input$subgrupo != "Todos") {
      d <- d[(subgrupo == input$subgrupo)]
    }
    
    d
  })
  
  eventos_examenes <- reactive({
    d <- copy(eventos_base())
    d <- d[grepl("[12][ªa][[:space:]]*CONV", asignatura)]
    d[, convocatoria := fifelse(
      grepl("1[ªa][[:space:]]*CONV", asignatura),
      "1ª convocatoria",
      "2ª convocatoria"
    )]
    
    if (!is.null(input$convocatoria) && input$convocatoria != "Todas") {
      d <- d[(convocatoria == input$convocatoria)]
    }
    
    d
  })
  
  
  # Tabla
  output$tabla <- renderDT({
    d <- formatea_eventos(eventos())
    datatable(
      d,
      rownames = FALSE,
      options  = list(
        pageLength = 15,
        order      = list(list(0, "asc"), list(1, "asc")),
        language   = idioma_dt
      )
    )
  })
  
  output$tabla_examenes <- renderDT({
    d <- formatea_eventos(eventos_examenes())
    d[, Subgrupo := NULL]
    datatable(
      d,
      rownames = FALSE,
      options  = list(
        pageLength = 15,
        order      = list(list(0, "asc"), list(1, "asc")),
        language   = idioma_dt
      )
    )
  })
  
  # Descarga ICS
  observeEvent(input$config_ics, {
    showModal(
      modalDialog(
        title = "Exportar calendario",
        checkboxInput(
          "alarm_display",
          "Añadir notificación",
          value = FALSE
        ),
        conditionalPanel(
          condition = "input.alarm_display == true",
          numericInput(
            "alarm_display_min",
            "Avisar con esta antelación (minutos)",
            value = 15,
            min   = 1,
            step  = 5
          )
        ),
        checkboxInput(
          "alarm_email",
          "Añadir aviso por correo electrónico",
          value = FALSE
        ),
        conditionalPanel(
          condition = "input.alarm_email == true",
          numericInput(
            "alarm_email_min",
            "Enviar con esta antelación (minutos)",
            value = 15,
            min   = 1,
            step  = 5
          ),
          textInput(
            "alarm_email_address",
            "Correo electrónico",
            value       = "",
            placeholder = "nombre@dominio.es"
          )
        ),
        
        footer = tagList(
          modalButton("Cancelar"),
          downloadButton(
            "dl_ics",
            "Descargar .ics",
            icon    = icon("download"),
            onclick = "Shiny.modal.remove();"
          )
        ),
        
        easyClose = TRUE
      )
    )
  })
  
  output$dl_ics <- downloadHandler(
    filename = function() "horario_enfermeria.ics",
    content  = function(file) {
      evs <- eventos()
      
      eventos_txt <- if (nrow(evs) == 0L) {
        character(0)
      } else {
        vapply(
          seq_len(nrow(evs)),
          function(i) {
            ics_event(
              evs$ini[i],
              evs$fin[i],
              summary           = evs$asignatura[i],
              location          = evs$codigo_aula[i],
              alarm_display     = isTRUE(input$alarm_display),
              alarm_display_min = if (is.null(input$alarm_display_min)) {
                15
              } else {
                input$alarm_display_min
              },
              alarm_email       = isTRUE(input$alarm_email),
              alarm_email_min   = if (is.null(input$alarm_email_min)) {
                15
              } else {
                input$alarm_email_min
              },
              
              alarm_email_address = if (is.null(input$alarm_email_address)) {
                ""
              } else {
                input$alarm_email_address
              }
            )
          },
          character(1)
        )
      }
      
      txt <- ics_calendar(eventos_txt, calname = "Horario")
      con <- file(file, open = "wb")
      on.exit(close(con), add = TRUE)
      
      writeBin(charToRaw(txt), con)
    }
  )
  
  # Descarga CSV
  output$dl_csv <- downloadHandler(
    filename = function() paste0("horario_filtrado_", Sys.Date(), ".csv"),
    content  = function(file) {
      evs <- formatea_eventos(eventos())
      fwrite(evs, file = file)
    }
  )
}


# APP -------------------------------------------------------------------------------
shinyApp(ui, server)
