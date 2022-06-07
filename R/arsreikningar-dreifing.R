##### UI #####

dreifing_ui <- function(id) {
    tabPanel("Dreifing",
             
             sidebarLayout(
                 sidebarPanel(
                     width = 3,
                     selectInput(
                         inputId = NS(id, "vidmid"),
                         label = "Sveitarfélag til viðmiðunar",
                         choices = unique(d$sveitarfelag),
                         selected = c("Reykjavíkurborg"),
                         multiple = FALSE,
                         selectize = FALSE
                     ),
                     selectInput(
                         inputId = NS(id, "hluti"),
                         label = "Hluti",
                         choices = c("A-hluti", "A og B-hluti"),
                         selected = c("A-hluti")
                     ),
                     
                     selectInput(
                         inputId = NS(id, "y_var"),
                         label = "Myndrit",
                         choices = c(
                             "Eiginfjárhlutfall",
                             "Handbært fé per íbúi",
                             "Jöfnunarsjóðsframlög per íbúi",
                             "Jöfnunarsjóðsframlög sem hlutfall af skatttekjum",
                             "Launa- og launatengd gjöld per íbúi",
                             "Launa- og launatengd gjöld sem hlutfall af útgjöldum",
                             "Nettó jöfnunarsjóðsframlög per íbúi",
                             "Nettóskuldir sem hlutfall af tekjum",
                             "Rekstrarniðurstaða per íbúi (kjörtímabil í heild)",
                             "Rekstrarniðurstaða sem hlutfall af tekjum (kjörtímabil í heild)",
                             "Skuldir per íbúi", 
                             "Skuldir sem hlutfall af tekjum",
                             "Skuldaaukning á kjörtímabili (leiðrétt fyrir verðbólgu)",
                             "Skuldahlutfall",
                             "Útsvar og fasteignaskattur per íbúi",
                             "Veltufé frá rekstri sem hlutfall af tekjum",
                             "Veltufjárhlutfall"
                         ),
                         selected = c("Skuldaaukning á kjörtímabili (leiðrétt fyrir verðbólgu)")
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
                     h3("Tölur miða við síðasta aðgengilega ársreikning sveitarfélags"),
                     br(" "),
                     tabsetPanel(
                         tabPanel("Myndrit", plotOutput(NS(id, "dreifing_plot"), height = 1200)),
                         tabPanel("Tafla", DTOutput(NS(id, "dreifing_tafla")))
                     )
                 )
             )
             
    )
}


##### SERVER #####


dreifing_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        dreifing_df <- reactive({
            

            y_var <- ui_name_to_data_name(input$y_var)
            
            plot_dat <- d |> 
                group_by(sveitarfelag) |> 
                filter(ar == max(ar), hluti == input$hluti) |> 
                ungroup() |> 
                select(sveitarfelag, ar, y = all_of(y_var)) |> 
                drop_na(y)  
            
            plot_dat
            
        }) |> 
            bindEvent(input$goButton)
        
        dreifing_plot <- reactive({
            
            plot_dat <- dreifing_df() |> 
                mutate(my_colour = 1 * (sveitarfelag %in% input$vidmid) + 2 * (sveitarfelag == "Heild"),
                       text = text_tooltip_dreifing(sveitarfelag, y),
                       sveitarfelag = case_when(sveitarfelag == input$vidmid ~ str_c("<b style='color:#2171b5'>", sveitarfelag, " (Til ", ar, ")", "</b>"),
                                                sveitarfelag == "Heild" ~ str_c("<b style='color:#b2182b'>", sveitarfelag, " (Til ", ar, ")", "</b>"),
                                                TRUE ~ str_c(sveitarfelag, " (Til ", ar, ")")),
                       sveitarfelag = fct_reorder(sveitarfelag, y))
            
            x_scale <- make_x_scale(input$y_var)
            subtitles <- make_subtitles(input$y_var)
            vline_and_segments <- make_vline_and_segments(input$y_var)
            coords <- make_coords_dreifing(input$y_var, plot_dat$y)
            
            p <- plot_dat |> 
                ggplot(aes(y, sveitarfelag)) +
                vline_and_segments + 
                geom_point(aes(col = factor(my_colour), size = factor(my_colour))) +
                x_scale +
                scale_colour_manual(values = c("#525252", "#2171b5", "#b2182b")) +
                scale_size_manual(values = c(2, 4, 4)) +
                coords +
                theme(legend.position = "none",
                      axis.text.y = element_markdown(),
                      plot.margin = margin(t = 5, r = 15, b = 5, l = 5)) +
                labs(x = NULL,
                     y = NULL,
                     col = NULL,
                     title = str_c(input$y_var, " (", input$hluti, ")"),
                     subtitle = subtitles,
                     caption = "Mynd var fengin frá: https://bggj.shinyapps.io/maelabord_arsreikninga_sveitarfelaga/")
            
           p
            
            
        }) |> 
            bindCache(input$y_var, input$hluti, input$vidmid) |> 
            bindEvent(input$goButton)
        
        output$dreifing_plot <- renderPlot({
            dreifing_plot()
        })
        
        dreifing_tafla <- eventReactive(input$goButton, {
            
            
            
            my_digits <- list(
                "Eiginfjárhlutfall" = 2,
                "Framlegð per íbúi (kjörtímabil í heild)" = 0,
                "Framlegð sem hlutfall af tekjum (kjörtímabil í heild)" = 2,
                "Handbært fé per íbúi" = 0,
                "Jöfnunarsjóðsframlög per íbúi" = 0,
                "Jöfnunarsjóðsframlög sem hlutfall af skatttekjum" = 2,
                "Launa- og launatengd gjöld per íbúi" = 0,
                "Launa- og launatengd gjöld sem hlutfall af útgjöldum" = 2,
                "Nettó jöfnunarsjóðsframlög per íbúi" = 0,
                "Nettóskuldir sem hlutfall af tekjum" = 2,
                "Rekstrarniðurstaða per íbúi (kjörtímabil í heild)" = 0,
                "Rekstrarniðurstaða sem hlutfall af tekjum (kjörtímabil í heild)" = 2,
                "Útsvar og fasteignaskattur per íbúi" = 0,
                "Skuldir per íbúi"  = 0,
                "Skuldir sem hlutfall af tekjum" = 2,
                "Skuldaaukning á kjörtímabili (leiðrétt fyrir verðbólgu)" = 3,
                "Skuldahlutfall" = 2,
                "Veltufé frá rekstri sem hlutfall af tekjum" = 2,
                "Veltufjárhlutfall" = 2
            )
            
            if (is.null(my_digits[[input$y_var]])) my_digits[[input$y_var]] <- 0
            
            y_name <- input$y_var
            
            table_dat <- dreifing_df() |> 
                select(sveitarfelag, ar, y) |> 
                arrange(desc(y)) |> 
                mutate(y = round(y, digits = my_digits[[input$y_var]]),
                       nr = str_c(str_pad(row_number(), width = 2, side = "left", pad = "0"), "/", n())) |> 
                select(nr, sveitarfelag, ar, y) |> 
                rename(Röðun = nr, Sveitarfélag = sveitarfelag, "Síðasti ársreikningur" = ar, !!y_name := y)
            
            caption <- str_c(input$y_var)
            
            datatable(
                table_dat,
                extensions = "Buttons",
                rownames = FALSE,
                caption = htmltools::tags$caption(
                    style = "caption-side: top",
                    h4(caption)
                    ),
                options = list(
                    dom = "fBrtip",
                    buttons = c("csv", "excel", "pdf"),
                    pageLength = 68,
                    lengthChange = FALSE,
                    searching = TRUE,
                    autoWidth = TRUE,
                    captionSide = "top",
                    language = list(
                        decimal = ",",
                        thousands = ".",
                        url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Icelandic.json'
                    )
                )
            ) |> 
                formatStyle(
                    target = 'row', columns = 'Sveitarfélag',  
                    backgroundColor = styleEqual(input$vidmid, c("#2171b5")),
                    color = styleEqual(input$vidmid, "#ffffff")
                ) |> 
                formatStyle(
                    target = 'row', columns = 'Sveitarfélag',  
                    backgroundColor = styleEqual("Heild", c("#b2182b")),
                    color = styleEqual("Heild", "#ffffff")
                )
            
        })
        
        output$dreifing_tafla <- renderDT({
            
            datatable(
                fasteigna_alogur,
                extensions = "Buttons",
                rownames = FALSE,
                caption = htmltools::tags$caption(
                    style = "caption-side: top",
                    h4(caption)
                ),
                options = list(
                    dom = "fBrtip",
                    buttons = c("csv", "excel", "pdf"),
                    pageLength = 68,
                    lengthChange = FALSE,
                    searching = TRUE,
                    autoWidth = TRUE,
                    captionSide = "top",
                    language = list(
                        decimal = ",",
                        thousands = ".",
                        url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Icelandic.json'
                    )
                )
            ) |> 
                formatStyle(
                    target = 'row', columns = 'Sveitarfélag',  
                    backgroundColor = styleEqual(input$vidmid, c("#2171b5")),
                    color = styleEqual(input$vidmid, "#ffffff")
                ) |> 
                formatStyle(
                    target = 'row', columns = 'Sveitarfélag',  
                    backgroundColor = styleEqual("Heild", c("#b2182b")),
                    color = styleEqual("Heild", "#ffffff")
                )
        })
    })
}