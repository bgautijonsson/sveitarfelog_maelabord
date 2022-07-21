fasteignir_fasteignagjold_ui <- function(id) {
    
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
            selectInput(
                inputId = NS(id, "tegund_eignar"),
                label = "Tegund Eignar",
                choices = unique(fasteignamat$tegund_eigna),
                selected = "Íbúðarhúsnæði",
                multiple = FALSE,
                selectize = FALSE
            ),
            numericInput(
                inputId = NS(id, "haekkun"),
                label = "Hækkun fasteignamats",
                value = 6450000,
                min = 0, max = 1e10
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
            plotlyOutput(NS(id, "haekkun_plot"),  height = 1000, width = "100%")
            
        )
    )
    
}

fasteignir_fasteignagjold_server <-function(id) {
    moduleServer(id, function(input, output, session) {
        
        haekkun_plot <- reactive({
            
            plot_dat <- fasteignamat |> 
                filter(tegund_eigna == input$tegund_eignar) |> 
                mutate(y = input$haekkun * fasteignamat,
                       my_colour = 1 * (sveitarfelag %in% input$vidmid),
                       text = str_c("Sveitarfélag: ", sveitarfelag, "\n",
                                    "Hækkun: ", number(y, suffix = " kr", big.mark = ".", decimal.mark = ","), " á ári"),
                       sveitarfelag = case_when(sveitarfelag == input$vidmid ~ str_c("<b style='color:#2171b5'>", sveitarfelag, "</b>"),
                                                TRUE ~ str_c(sveitarfelag)),
                       sveitarfelag = fct_reorder(sveitarfelag, y))
            
            p <- plot_dat |> 
                ggplot(aes(y, sveitarfelag, text = text)) +
                geom_vline(xintercept = 0, lty = 2) +
                geom_segment(aes(xend = 0, yend = sveitarfelag, col = factor(my_colour)), size = 0.3) +
                geom_point(aes(col = factor(my_colour), size = factor(my_colour))) +
                scale_x_continuous(limits = c(0, max(plot_dat$y) * 1.01),
                                   breaks = pretty_breaks(8),
                                   labels = label_number(suffix = " kr", big.mark = ".", decimal.mark = ","),
                                   expand = expansion()) +
                scale_colour_manual(values = c("#525252", "#2171b5", "#b2182b")) +
                scale_size_manual(values = c(2, 4, 4)) +
                coord_cartesian(clip = "off") +
                theme(legend.position = "none",
                      axis.text.y = element_markdown(),
                      plot.margin = margin(t = 5, r = 15, b = 5, l = 5),
                      plot.title = element_text(size = 12),
                      axis.text = element_text(size = 10)) +
                labs(x = NULL,
                     y = NULL,
                     col = NULL,
                     title = str_c("Aukning árlegra fasteigna-, vatns- og fráveitugjalda miðað við ",
                                   # "\n",
                                   number(input$haekkun, big.mark = ".", decimal.mark = ","), 
                                   " kr hækkun fasteignamats." ))
            
            ggplotly(p,
                     tooltip = "text",
                     height = 1000,
                     width = 1100) |> 
                layout(
                   title = list(
                        y = 0.91, yanchor = "top",
                        x = 0, xref = "paper",
                        font = list(
                            size = 15
                        )
                    ),
                    margin = list(
                        t = 105,
                        r = 0,
                        b = 120,
                        l = 0
                    ),
                    autosize = FALSE,
                   annotations = list(
                       list(x = 0.9, xanchor = "right", xref = "paper",
                            y = -0.10, yanchor = "bottom", yref = "paper",
                            showarrow = FALSE,
                            text = caption,
                            font = list(size = 14))
                   )
                )
            
            
        }) |> 
            bindEvent(input$goButton)
        
        output$haekkun_plot <- renderPlotly({
            haekkun_plot()
        })
        
        
    })
}