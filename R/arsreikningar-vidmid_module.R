vidmid_ui <- function(id) {
    
    sidebarLayout(
        sidebarPanel(
            width = 3,
            selectInput(
                inputId = NS(id, "sveitarfelag"),
                label = "Sveitarfélag",
                choices = unique(d$sveitarfelag),
                selected = c("Reykjavíkurborg", "Kópavogsbær", "Hafnarfjarðarkaupstaður",
                             "Garðabær", "Mosfellsbær", "Seltjarnarnesbær"),
                multiple = TRUE,
                selectize = TRUE
            ),
            selectInput(
                inputId = NS(id, "hluti"),
                label = "Hluti",
                choices = c("A-hluti", "A og B-hluti"),
                selected = c("A-hluti")
            ),
            div(
                actionButton(
                    inputId = NS(id, "goButton"),
                    label = "Sækja gögn",
                    width = "120px"
                ),
                class = "center", align = "middle"
            ),
            HTML(sidebar_info)
            
        ),
        
        
        mainPanel(
            plotlyOutput(NS(id, "plot_vidmid"), height = 900, width = "100%") |> withSpinner()
        )
    )
    
}

vidmid_server <-function(id) {
    moduleServer(id, function(input, output, session) {
        
        my_plot_vidmid <- reactive({
            
            make_vidmid_ggplot(input = input)
            
        })
        
        output$plot_vidmid <- renderPlotly({
            
            my_plot_vidmid() |> 
                make_vidmid_ggplotly(input = input)
            
        }) |> 
            bindCache(input$sveitarfelag, 
                      input$hluti) |> 
            bindEvent(input$goButton, ignoreNULL = FALSE)
        
        outputOptions(output, "plot_vidmid", suspendWhenHidden = FALSE)
    })
}