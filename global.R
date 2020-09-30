# setup -------------------------------------------------------------------
message("global.R")
pacman::p_load(shiny, bs4Dash, dplyr, readr, tidyr, stringr, forcats,
               highcharter, shinyWidgets, ggsci, cicerone, territorial,
               lubridate, zoo)

suppressPackageStartupMessages({
  library(shiny)
  library(bs4Dash)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(forcats)
  library(highcharter)
  library(shinyWidgets)
  library(ggsci)
  library(cicerone)
  library(lubridate)
  library(zoo)
  
})

# data --------------------------------------------------------------------
movid <- readRDS("output/movid.rds")

movid <- movid %>% 
  mutate(
    sexo = factor(sexo, levels = c("Femenino", "Masculino", "Otro")),
    edad_3cat = factor(edad_3cat, levels = c("18 a 39", "40 a 64", "65 y más")),
    prev = factor(prev, levels = c("ISAPRE", "FONASA", "Ninguna", "Fuerzas Armadas y de Orden", 
                                   "Otra")),
    pr3_ocupacion = factor(pr3_ocupacion, levels = c("Trabaja de manera remunerada", "Otra actividad (jubilado, pensionado, recibe pensión de invalidez u otro)", 
                                                     "Desempleado o desempleada", "Quehaceres del hogar, cuidando niños y otras personas", 
                                                     "Estudia")),
    educ_3cat = factor(educ_3cat, levels = c("Profesional", "Técnica", "Media o menos")),
    region = factor(
      territorial::estandariza_regiones(region),
      levels = c(
        "Arica y Parinacota",
        "Tarapacá",
        "Antofagasta",
        "Atacama",
        "Coquimbo",
        "Valparaíso",
        "Metropolitana de Santiago",
        "Libertador General Bernardo O'Higgins",
        "Maule",
        "Ñuble",
        "Biobío",
        "La Araucanía",
        "Los Ríos",
        "Los Lagos",
        "Aysén del General Carlos Ibañez del Campo",
        "Magallanes y la Antártica Chilena"
      )
    )
  )

# sintomas ----------------------------------------------------------------
OPTS_SINTOMAS <- c(
  `Fiebre (temperatura axilar sobre los 37,8° C)` = "snt_fiebre",
  Tos = "snt_tos",
  `Dificultad para respirar` = "snt_disnea",
  `Dolor muscular` = "snt_mialgias",
  `Dolor de garganta` = "snt_odinofagia",
  `Disminución o pérdida del olfato` = "snt_anosmia",
  `Disminución o pérdida del gusto` = "snt_disgeusia",
  `Dolor en el pecho` = "snt_dol_torax",
  `Dolor de cabeza` = "snt_cefalea",
  Diarrea = "snt_diarrea",
  `No he tenido ninguno de estos síntomas` = "snt_null"
)

OPTS_SINTOMAS_DF <- OPTS_SINTOMAS %>%
  as.list() %>% 
  tibble::enframe() %>% 
  unnest(cols = c(value)) %>% 
  mutate(value = paste0("s1_", value))

OPTS_SOSPECHOSO_DEFINICION <- c(
  `Definición MINSAL actual`  = "sosp_minsal0530",
  `Definición MINSAL inicial` = "sosp_minsal0326",
  `Definición MOVID19`        = "sosp_movid19"
)


# Sistema salud -----------------------------------------------------------
OPTS_DESAGREGAR <- c(
  "Sin desagregar" = "todo",
  "Sexo" = "sexo", 
  "Edad" = "edad_3cat",
  # región, 
  "Previsión" = "prev",
  "Ocupación" = "pr3_ocupacion", 
  "Educación" = "educ_3cat" 
  # residencia en comuna en cuarentena
)

OPTS_RAZONES <- c(
  "Porque tenía que esperar mucho tiempo" = "tiempo",
  "Porque no le pareció importante" = "nimporta",
  "Por el costo económico" = "costo",
  "Porque no sabía dónde realizarlo" = "nosabia",
  "Porque está evaluando si los síntomas empeoran antes de consultar" = "empeorar",
  "Por miedo a contagiarse" = "temor",
  "Porque los síntomas son leves o habituales" = "leves",
  "Por que el sistema de salud está muy lleno" = "sistlleno",
  "Otra" = "otra"
)

OPTS_RAZONES2 <- c(
  "Porque no le pareció importante" = "nimporta",
  "Por el costo económico" = "costo",
  "Porque no sabía dónde realizarlo" = "nosabia",
  "Porque tenía que esperar mucho tiempo" = "tiempo",
  "Porque el examen no estaba disponible" = "nodisp",
  "Porque está esperando poder realizárselo" = "espera",
  "Porque no estaba suficientemente grave" = "nograve",
  "Otra" = "otra"
)

OPTS_RAZONES2_DF <- OPTS_RAZONES2 %>%
  as.list() %>% 
  tibble::enframe() %>% 
  unnest(cols = c(value)) %>% 
  mutate(value = paste0("s8_exmn_", value))


# practicas ---------------------------------------------------------------
PRACTICAS_DF <- tibble(
  tipo = c("p1_pra_trabajo", "p1_pra_tramite", "p1_pra_recrea", "p1_pra_visita", 
           "p1_pra_invitado", "p1_pra_transporte"),
  tipo_lbl = c("Trabajar", "Trámite", "Recreación", "Visitar amigos o familiares",
               "Recibido vistias de amigos o familiares", "Utilizado transporte público")
) %>% 
  mutate(tipo_lbl = fct_inorder(tipo_lbl))

# Regiones --------------
OPTS_REGIONES <- c("Metropolitana de Santiago",
                   "Antofagasta","Coquimbo" ,"Valparaiso",
                   "La Araucania",
                   "Libertador General Bernardo OHiggins" ,
                   "Los Lagos" ,"Los Rios" ,  
                   "Maule","Tarapaca",
                   "Magallanes y de la Antartica Chilena")

OPTS_ZONA <- c("Norte",
               "Centro",
               "Gran Santiago",
               "Región Metropolitana",
               "Sur")

OPTS_REGIONES <- unique(casen::codigos_subdere$nombre_region)
# acceso no covid ------------

# highcharter -------------------------------------------------------------
hc_lang <- getOption("highcharter.lang")
hc_lang$weekdays <- c("domingo", "lunes", "martes", "miércoles", "jueves", "viernes", "sábado")
hc_lang$months <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", 
                    "agosto", "septiembre", "octubre", "noviembre", "diciembre")
hc_lang$shortMonths <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", 
                         "oct", "nov", "dic")
hc_lang$thousandsSep <- "."
hc_lang$decimalPoint <- ","
hc_lang$downloadJPEG <- "Descargar imagen"
hc_lang$downloadXLS <- "Descargar excel"
hc_lang$viewFullscreen <- "Ver en pantalla completa"


hc_chart <- getOption("highcharter.chart")
hc_chart$exporting <- list(enabled = TRUE)
hc_chart$credits <- list(
  enbled = TRUE,
  href = "https://www.movid19.cl/",
  text = "MOVID19"
)

options(
  highcharter.chart = hc_chart,
  highcharter.lang  = hc_lang,
  highcharter.google_fonts = TRUE,
  highcharter.theme = 
    hc_theme_smpl(
      chart = list(
        style = list(fontFamily = "Roboto")
      ),
      title = list(
        style = list(fontFamily = "Source Sans Pro", fontWeight  = "normal")
      ),
      subtitle = list(
        style = list(fontFamily = "Roboto", fontSize = "12px", fontWeight  = "normal")
      ),
      # colors = ggsci::pal_jama()(7)[-1],
      colors = c("#093C66",
                 "#DCA11D",
                 "#00C6BA",
                 "#887456",
                 "#ABCBFF",
                 # falta opcion, agrego rojo
                 "#d35400"
      ),
      xAxis = list(gridLineWidth = 1),
      yAxis = list(gridLineWidth = 1),
      plotOptions = list(
        line = list(marker = list(enabled = FALSE), lineWidth = 4, opacity = 0.85),
        series = list(
          marker = list(symbol = "circle")
        )
      ),
      tooltip = list(
        useHTML = TRUE,
        valueDecimals = 2
      ),
      legend = list(
        verticalAlign = "top",
        align = "left",
        itemStyle =  list(
          fontWeight = "normal"
        )
      ),
      exporting = list(
        # scale = 1.75,
        sourceWidth = 1024,
        sourceHeight = 768,
        chartOptions = list(
          credits = list(
            text = "Monitoreo Nacional de Prácticas y Síntomas COVID-19 (MOVID19). www.movid19.cl",
            style = list(fontSize = "11px")
          )
        ),
        buttons = list(
          contextButton = list(
            symbol = 'url(img/downloadicon.png)',
            # symbol = "url(https://icon-library.com/images/3-dots-icon/3-dots-icon-28.jpg)",
            symbolSize = 18,
            symbolX = 21,
            symbolY = 20,
            titleKey = "Descargar",
            y = -05,
            menuItems = c("downloadJPEG", "downloadXLS")
          )
        )
      )
    )
)


# tour --------------------------------------------------------------------
guide <- Cicerone$
  new(
    next_btn_text = "Siguiente",
    stage_background = "#F5F5F5",
    prev_btn_text = "Anterior",
    done_btn_text = "¡Listo!",
    close_btn_text = "Cerrar"
  )$ 
  step(
    el = "sidebarItemExpanded",
    title = "Secciones",
    description = "Cada sección esta asociada a ciertos aspectos relevantes
    de la encuesta MOVID19.",
    position = "right-center"
  )$
  step(
    "shiny-tab-inicio",
    "Sección",
    "Cada sección posee visualizaciones las cuales puedes controlar con 
    selectores que aparecen en la misma sección.",
    position = "mid-center"
  )$
  step(
    "inicial",
    "Gráficos",
    "Cada uno de los gráficos es interactivo, y en cada uno de ellos puedes
    descargar tanto la visualización como imagen o los datos en un archivo
    excel haciendo click en el ícono <img width='16px' src='img/downloadicon.png'/>
    ubicado en la esquina superior derecha.",
    position = "mid-center"
  )

# comunas ------------------------------------------------
movid_comunas <- movid  %>%  
  group_by(semana_fecha, tipo = comuna) %>% 
  summarise(proporcion = mean(100 * sosp_minsal0530, na.rm = TRUE),
            cantidad = sum(sosp_minsal0530, na.rm = TRUE),
            total = n(),
            .groups = "drop") %>%
  group_by(tipo) %>%
  arrange(tipo) %>% 
  mutate(mediamovil = (rollmean(proporcion, k =2, fill = NA)),
         var = proporcion - lag(proporcion)) %>% 
  filter(!is.na(tipo), var[semana_fecha == max(semana_fecha)] >= 0) %>%
  group_by(tipo)%>% filter(total >= 50) %>%
  merge(casen::codigos_subdere, by.x = c("tipo"), by.y = c("nombre_comuna")) %>%
  group_by(tipo) %>% 
  mutate(media = rollmean(proporcion, k =2, fill = NA), codigo_region = as.numeric(codigo_region),
         codigo_provincia = as.numeric(codigo_provincia),
         macrozona = case_when(codigo_region %in% c(1,2,3,4,15) ~ "Norte",
                               codigo_region %in% c(5,6,7) ~ "Centro",
                               codigo_provincia %in% c(131) ~ "Gran Santiago",
                               codigo_provincia %in% c(132,133,134,135,136) ~ "Región Metropolitana",
                               codigo_region %in% c(8,9,10,12,14,16) ~ "Sur" )) %>% 
  arrange(tipo, semana_fecha) %>% filter(semana_fecha >= "2020-05-06", tipo !="Cerro Navia")
