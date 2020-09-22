# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$sosp_comuna <- renderHighchart({
    
    movid_comunas %>% 
      filter(macrozona %in% input$macrozona) %>% 
      hchart(.,
             "line",
             hcaes(semana_fecha, round(mediamovil,1), group = tipo)) %>%
      hc_tooltip() %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), min = 0) %>% 
      hc_subtitle(text = " Porcentaje de personas que cumplen con definición 
                  vigente de casos sospechosos")  
  })
  
}