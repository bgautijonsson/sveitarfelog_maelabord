hafdusamband_ui <- function(id) {
    
        fluidPage(
            tags$style(type="text/css", "body {padding-top: 80px;}"),
            fluidRow(
                div(
                column(
                    width = 5,
                    br(" "),
                    wellPanel(
                        p("Ef þú hefur spurningar eða ábendingar um mælaborðið, endilega hafðu samband:"),
                        HTML("<a href='https://twitter.com/bggjonsson' target='_blank'> Twitter </a><br>"),
                        HTML("<a href='https://github.com/bgautijonsson/skattagogn/issues' target='_blank'> GitHub </a><br>"),
                        markdown("[Tölvupóstur](mailto:bgautijonsson@gmail.com)")
                        
                    )
                ),
                class = "center", align = "middle"
            )
        )
    )
}


hafdusamband_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
    })
}