message("R/helpers.R")

RMD_to_HTML <- function(file) {
  
  HTML(
    markdown::markdownToHTML(
      text = knitr::knit(
        text = readLines(file, encoding = "UTF-8"),
        quiet = TRUE
      ),
      fragment.only = TRUE
    )  
  )
  
}

bs4CardMovid <- purrr::partial(
  bs4Card,
  width = 6,
  collapsible = FALSE,
  ... =
)

valueBox <- function(value, subtitle, icon = NULL, elevation = 3, status = NULL, 
                     width = 3, footer = NULL, href = NULL) {
  
  
  value <- ifelse(is.numeric(value), scales::comma(value, big.mark = ".", decimal.mark = ","), value)
  value <- tags$h1(value)
  
  bs4Dash::valueBox(
    value, subtitle, icon = icon, elevation = elevation, status = status, 
    width = width, footer = footer, href = href
  )
  
}

bs4Card <- function(
  ...,
  inputId = NULL, title = NULL, footer = NULL, status = NULL,
  elevation = 3, solidHeader = FALSE, headerBorder = TRUE,
  gradientColor = NULL, width = 6, height = NULL, collapsible = FALSE,
  collapsed = FALSE, closable = FALSE, maximizable = FALSE, 
  cardLabel = NULL, dropdownMenu = NULL, overflow = FALSE,
  sidebar = NULL) {
  
  # hack necesario para poder cambiar los defaults
  # creo que se debe al doble ellipsis
  
  bs4Dash::bs4Card(
    ..., 
    inputId = inputId, title = title, footer = footer, status = status, 
    elevation = elevation, solidHeader = solidHeader, headerBorder = headerBorder, 
    gradientColor = gradientColor, width = width, height = height, collapsible = collapsible, 
    collapsed = collapsed, closable = closable, maximizable = maximizable, 
    cardLabel = cardLabel, dropdownMenu = dropdownMenu, overflow = overflow, 
    sidebar = sidebar
  )
}

hc_demografica <- function(var = "prev") {
  
  d <- movid %>% 
    select(pob_id, all_of(var)) %>% 
    distinct(pob_id, .keep_all = TRUE) %>%
    rename_at(2, ~ "variable") %>% 
    count(variable) %>%
    filter(complete.cases(.)) %>% 
    arrange(variable) %>% 
    mutate(p = scales::percent(n/sum(n), accuracy = 0.1))
  
  d
  
  hchart(
    d,
    "column",
    hcaes(variable, n),
    colorByPoint = TRUE,
    tooltip = list(pointFormat = "<center>{point.y} ({point.p})</center>", valueDecimals = 0),
    dataLabels = list(enabled = TRUE, format = "{point.y:,.0f} ({point.p})")
  ) %>%
    hc_tooltip(table = TRUE, sort = TRUE) %>%
    hc_xAxis(title = list(text = "")) %>%
    hc_yAxis(title = list(text = ""))  
  
}

hc_tooltip_n <- function(hc, separado = TRUE, ...) {
  
  header <- "<small>{point.key}</small><table>"
  
  header <- ifelse(
    separado,
    header,
    str_c(header, "<tr><td style=\"text-align: right; color: #A9A9A9\">Cantidad de respuestas {point.cantidad:,.0f}</td></tr>")
  )
  
  point <- "<tr>
        <td style=\"color: {series.color}\">{series.name}: </td>
        <td style=\"text-align: right\"><b>{point.y}</b></td>"
  
  point <- ifelse(
    separado,
    str_c(point, "<td style=\"text-align: right; color: #A9A9A9\">({point.cantidad:,.0f})</td>"),
    point
  )
  
  point <- str_c(point, "</tr>")
  
  hc %>% 
    hc_tooltip(
      shared = TRUE,
      useHTML = TRUE,
      headerFormat = header,
      pointFormat = point,
      footerFormat = "</table>"
      ) 
}

