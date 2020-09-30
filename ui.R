# Define UI for app that draws a histogram ----
ui <- bs4DashPage(
    enable_preloader = FALSE,
    loading_duration = 1.5,
    loading_background = "white",
    sidebar_collapsed = TRUE,
    navbar = bs4DashNavbar(),
    # sidebar -----------------------------------------------------------------
    sidebar = bs4DashSidebar(
        disable = T,
        title = NULL,
        expand_on_hover = F,
        fixed = FALSE,
        skin = "light",
        bs4SidebarMenu(
            id = "current_tab",
            bs4SidebarMenuItem(
                text = "MOVID-19",
                tabName = "inicio",
                # icon =  "tachometer-alt",
                icon = "hospital-o"
            )
        )
    ),
    # body --------------------------------------------------------------------
    body = bs4DashBody(
        useSweetAlert(theme = "minimal"),
        use_cicerone(),
        tags$head(tags$link(rel="shortcut icon", href="fa.png")),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/movid19.css")),
        tags$script(src = "js/movid19.js"),
        
        bs4TabItems(
            # inicio ------------------------------------------------------------------
            bs4TabItem(
                tabName = "inicio",
                fluidRow(
                    column(
                        12,
                        tags$hr()
                    ),
                    bs4Card(
                        title = "Casos sospechosos por comuna",
                        width =12,
                        selectizeInput(
                            "macrozona", NULL, 
                            choices = OPTS_ZONA,
                            selected = "Gran Santiago",
                            multiple = TRUE, 
                            width = "100%",
                            options = list(maxItems = 5)
                        ),
                        highchartOutput("sosp_comuna", height = 345)
                    ),
                ),
                
            )
        )
    )
)    