fasteignagjold_gjold_ui <- function(id) {
    
    sidebarLayout(
        sidebarPanel(
            width = 3,
            selectInput(
                inputId = NS(id, "vidmid"),
                label = "Sveitarfélag til viðmiðunar",
                choices = unique(fasteignamat$sveitarfelag),
                selected = c("Reykjavíkurborg"),
                multiple = FALSE,
                selectize = FALSE
            ),
            numericInput(
                inputId = NS(id, "fasteignamat"),
                label = "Fasteignamat",
                value = 37500000,
                min = 0, max = 1e10
            ),
            numericInput(
                inputId = NS(id, "lodarmat"),
                label = "Lóðarmat",
                value = 5350000,
                min = 0, max = 1e10
            ),
            numericInput(
                inputId = NS(id, "fermetrar"),
                label = "Fermetrar",
                value = 70.9,
                min = 0, max = 1e4
            ),
            div(
                actionButton(
                    inputId = NS(id, "goButton"),
                    label = "Reikna",
                    width = "120px"
                ),
                class = "center", align = "middle"
            ),
            HTML(sidebar_info)
            
        ),
        mainPanel(
            tabPanel("Myndrit", plotOutput(NS(id, "haekkun_plot"), height = 1000))
        )
    )
    
}

fasteignagjold_ggjold_server <-function(id) {
    moduleServer(id, function(input, output, session) {
        
        haekkun_plot <- reactive({
            
            plot_dat <- fasteignamat |> 
                mutate(y = input$haekkun * fasteignamat,
                       my_colour = 1 * (sveitarfelag %in% input$vidmid),
                       sveitarfelag = case_when(sveitarfelag == input$vidmid ~ str_c("<b style='color:#2171b5'>", sveitarfelag, "</b>"),
                                                TRUE ~ str_c(sveitarfelag)),
                       sveitarfelag = fct_reorder(sveitarfelag, y))
            
            p <- plot_dat |> 
                ggplot(aes(y, sveitarfelag)) +
                geom_vline(xintercept = 0, lty = 2) +
                geom_segment(aes(xend = 0, yend = sveitarfelag, col = factor(my_colour)), size = 0.3) +
                geom_point(aes(col = factor(my_colour), size = factor(my_colour))) +
                scale_x_continuous(limits = c(0, NA),
                                   breaks = seq(0, 1e5, by = 1e3),
                                   expand = expansion()) +
                scale_colour_manual(values = c("#525252", "#2171b5", "#b2182b")) +
                scale_size_manual(values = c(2, 4, 4)) +
                coord_cartesian(clip = "off") +
                theme(legend.position = "none",
                      axis.text.y = element_markdown(),
                      plot.margin = margin(t = 5, r = 15, b = 5, l = 5)) +
                labs(x = NULL,
                     y = NULL,
                     col = NULL,
                     title = "Hvað hækka fasteignagjöld mikið út frá hækkun fasteignamats?",
                     subtitle = "Reiknað miðað við að forsendur breytist ekki",
                     caption = "Mynd var fengin frá: https://bggj.shinyapps.io/maelabord_arsreikninga_sveitarfelaga/")
            
            p
            
            
        }) |> 
            bindEvent(input$goButton)
        
        output$haekkun_plot <- renderPlot({
            haekkun_plot()
        })
        
        
    })
}