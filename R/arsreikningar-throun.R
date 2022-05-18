##### UI #####

throun_ui <- function(id) {
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
            selectInput(
                inputId = NS(id, "y_var"),
                label = "Myndrit",
                choices = c(
                    "Árafjöldi til niðurgreiðslu nettó skulda",
                    "Eiginfjárhlutfall",
                    "Framlegð sem hlutfall af tekjum",
                    "Handbært fé per íbúi",
                    "Jöfnunarsjóðsframlög per íbúi",
                    "Jöfnunarsjóðsframlög sem hlutfall af skatttekjum",
                    "Launa- og launatengd gjöld per íbúi",
                    "Launa- og launatengd gjöld sem hlutfall af útgjöldum",
                    "Nettó jöfnunarsjóðsframlög per íbúi",
                    "Nettóskuldir sem hlutfall af tekjum",
                    "Rekstrarniðurstaða sem hlutfall af tekjum",
                    "Rekstrarniðurstaða undanfarinna 3 ára  sem hlutfall af tekjum",
                    "Skuldir",
                    "Skuldir per íbúi", 
                    "Skuldir sem hlutfall af tekjum",
                    "Skuldaaukning",
                    "Skuldahlutfall",
                    "Útsvar og fasteignaskattur per íbúi",
                    "Veltufé frá rekstri sem hlutfall af tekjum",
                    "Veltufjárhlutfall"
                ),
                selected = c("Nettóskuldir sem hlutfall af tekjum")
            ),
            selectInput(
                inputId = NS(id, "ar_fra"), 
                label = "Frá hvaða ári viltu sjá þróunina?", 
                choices = c(2002, 2006, 2010, 2014, 2018), 
                selected = 2002, 
                multiple = FALSE, 
                selectize = FALSE
            ),
            selectInput(
                inputId = NS(id, "verdlag"),
                label = "Verðlag",
                choices = c("Verðlag hvers árs", "Verðlag 2021"),
                selected = "Verðlag 2021"
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
            tabsetPanel(
                type = "tabs",
                tabPanel("Myndrit", plotOutput(NS(id, "throun_plot"), height = 500)),
                tabPanel("Tafla", DTOutput(NS(id, "throun_tafla")))
            )
            
        )
    )
}



##### SERVER #####


throun_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        ##### Make the dataframe used in the plot and table ######
        
        throun_df <- eventReactive(input$goButton, {
            
            y_var <- ui_name_to_data_name(input$y_var)
            
            plot_dat <- d |> 
                filter(hluti %in% input$hluti, 
                       sveitarfelag %in% input$sveitarfelag,
                       ar >= input$ar_fra) |> 
                select(ar, sveitarfelag, y = all_of(y_var), visitala_2021) |> 
                mutate(x = ar) |> 
                adjust_for_inflation(input$y_var, input$verdlag, input$ar_fra)
            
            plot_dat
            
        }) 
        
        ##### Plot #####
        
        throun_plot <- eventReactive(input$goButton, {
            
            plot_dat <- throun_df()
            
            y_scale <- make_y_scale(input$y_var)
            subtitles <- make_subtitles(input$y_var)
            hlines <- make_hlines(input$y_var)
            coords <- make_coords(input$y_var, plot_dat$y)
            
            p <- plot_dat |> 
                ggplot(aes(ar, y, col = sveitarfelag)) +
                hlines +
                geom_line() +
                geom_point() +
                scale_x_continuous() +
                y_scale +
                scale_colour_brewer(type = "qual", palette = "Set1") +
                coords +
                theme(legend.position = "top") +
                labs(x = NULL,
                     y = NULL,
                     col = NULL,
                     title = input$y_var,
                     subtitle = subtitles,
                     caption = caption)
            p
        })
        
        ##### Table #####
        throun_tafla <- eventReactive(input$goButton, {
            
            
            
            my_digits <- list(
                "Eiginfjárhlutfall" = 3,
                "Framlegð sem hlutfall af tekjum" = 3,
                "Handbært fé per íbúi" = 0,
                "Jöfnunarsjóðsframlög per íbúi" = 0,
                "Jöfnunarsjóðsframlög sem hlutfall af skatttekjum" = 3,
                "Launa- og launatengd gjöld per íbúi" = 0,
                "Launa- og launatengd gjöld sem hlutfall af útgjöldum" = 3,
                "Nettó jöfnunarsjóðsframlög per íbúi" = 0,
                "Nettóskuldir sem hlutfall af tekjum" = 3,
                "Rekstrarniðurstaða sem hlutfall af tekjum" = 3,
                "Skuldir per íbúi"  = 0,
                "Skuldir sem hlutfall af tekjum" = 3,
                "Skuldaaukning" = 3,
                "Skuldahlutfall" = 3,
                "Útsvar og fasteignaskattur per íbúi" = 0,
                "Veltufé frá rekstri sem hlutfall af tekjum" = 3,
                "Veltufjárhlutfall" = 3
            )
            
            if (is.null(my_digits[[input$y_var]])) my_digits[[input$y_var]] <- 0
            
            table_dat <- throun_df() |> 
                select(-visitala_2021, -x) |> 
                select(Ár = ar, sveitarfelag, y) |> 
                mutate(y = round(y, digits = my_digits[[input$y_var]])) |> 
                pivot_wider(names_from = sveitarfelag, values_from = y)
            
            caption <- str_c(input$y_var, " (", input$verdlag, ")")
            
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
            )
            
        })
        
        output$throun_plot <- renderPlot({
            throun_plot()
        })
        
        output$throun_tafla <- renderDT({
            
            throun_tafla()
        })
    })
}