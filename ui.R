ui <- shinyUI(
    tagList(
        tags$head(
            tags$style(
                type = "text/css",
                "
                .navbar-brand {
                    display: none;
                }
                .navbar {
                        font-family: Optima;
                        font-weight: 300;
                        font-size: 20px;
                        padding-top: 5px;
                        padding-bottom: 5px;
                }
                "
            )
        ),
        navbarPage(
            "Ársreikningar sveitarfélaga",
            theme = bs_global_get(),
            
            tabPanel(
                title = "Þróun",
                throun_ui("throun"), 
            ),
            tabPanel(
                title = "Dreifing",
                dreifing_ui("dreifing")
            ),
            tabPanel(
                title = "Viðmið",
                vidmid_ui("vidmid")
            ),
            
        )
    )
    
)