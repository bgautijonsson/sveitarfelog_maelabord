fasteignir_kaupverd_ui <- function(id) {
    
    sidebarLayout(
        sidebarPanel(
            width = 3,
            selectInput(
                inputId = NS(id, "sveitarfelag"),
                label = "Sveitarfélag",
                choices = unique(kaupskra$sveitarfelag),
                selected = c("Reykjavíkurborg", "Garðabær", "Kópavogsbær", 
                             "Seltjarnarnesbær", "Hafnarfjarðarkaupstaður", "Mosfellsbær"),
                multiple = TRUE,
                selectize = TRUE
            ),
            numericInput(
              inputId = NS(id, "byggar"),
              label = "Byggingarár (frá og með)",
              min = 2000, max = 2022, step = 1,
              value = 2018
            ),
            selectInput(
                inputId = NS(id, "verdlag"),
                label = "Verðlag",
                choices = c("Í dag", "Hvers árs"),
                selected = "Í dag", 
                multiple = FALSE, 
                selectize = FALSE
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
            plotOutput(NS(id, "verd_vs_fm_plot"),  height = 800, width = 1000)
            
        )
    )
    
}

fasteignir_kaupverd_server <-function(id) {
    moduleServer(id, function(input, output, session) {
        
        verd_vs_fm_df <- reactive({
            
            # print(ymd(input$dags[1]))
            
            kaupskra |> 
                dplyr::filter(byggar >= input$byggar, sveitarfelag %in% input$sveitarfelag) |> 
                dplyr::group_by(sveitarfelag) |> 
                dplyr::mutate(n_fastnum = n()) |> 
                dplyr::ungroup() |> 
                dplyr::mutate(sveitarfelag = str_c("<b>", sveitarfelag, "</b>", "<br>", "(Fasteignir = ", n_fastnum, ")"),
                              kaupverd = (input$verdlag == "Í dag") * (kaupverd/visitala) + (input$verdlag != "Í dag") * kaupverd) 
            
            
            
        }) |> 
            bindEvent(input$goButton)
        
        verd_vs_fm_plot <- reactive({
            
            
           dat <- verd_vs_fm_df()
            
            p <- dat |> 
                ggplot(aes(einflm, kaupverd)) +
                geom_point(data = dat |> rename(svf = sveitarfelag), aes(group = svf), col = "grey", alpha = 0.2) +
                geom_point(aes(col = sveitarfelag, fill = sveitarfelag), alpha = 0.5) +
                # geom_rangeframe() +
                scale_x_log10(labels = label_number(suffix = "m2", big.mark = ".", decimal.mark = ",")) +
                scale_y_log10(labels = label_number(suffix = " mkr", big.mark = ".", decimal.mark = ",")) +
                scale_fill_brewer(type = "qual", palette = "Dark2") +
                scale_colour_brewer(type = "qual", palette = "Dark2") +
                # theme_tufte() +
                theme(strip.background = element_rect( fill = "grey95"),
                      strip.text = element_markdown(),
                      # axis.text.y = element_blank(),
                      # axis.ticks.y = element_blank(),
                      plot.title = element_text(face = "bold")) +
                guides(color = "none", fill = "none") +
                facet_wrap(~ sveitarfelag, nrow = 2, scales = "free_x") +
                labs(x = NULL, y = NULL,
                     title = str_c("Dreifing fyrsta kaupverðs og stærðar fasteigna"),
                     subtitle = str_c("Sýnt fyrir fasteignir byggðar frá ", 
                                      input$byggar,
                                      ". Borið saman við heildardreifingu í völdum sveitarfélögum."))
            
            p
            
            
        }) |> 
            bindEvent(input$goButton)
        
        output$verd_vs_fm_plot <- renderPlot({
            
            verd_vs_fm_plot()
            
        })
        
        
    })
}