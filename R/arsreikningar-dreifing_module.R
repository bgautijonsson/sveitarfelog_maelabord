##### UI #####

dreifing_ui <- function(id) {
    tabPanel("Dreifing",
             
             sidebarLayout(
                 sidebarPanel(
                     width = 3,
                     tags$style(type="text/css", "body {padding-top: 80px;}"),
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
                         tabPanel("Myndrit", plotlyOutput(NS(id, "dreifing_plot"), height = 1200, width = "100%") |> withSpinner()),
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
            
            
            make_dreifing_df(input = input)
            
        }) 
        
        dreifing_plot <- reactive({
            
            dreifing_df() |> 
                make_dreifing_ggplot(input = input)
            
            
        })
        
        output$dreifing_plot <- renderPlotly({
            dreifing_plot() |> 
                make_dreifing_ggplotly(input =)
            
        }) |> 
            bindCache(input$y_var, input$hluti, input$vidmid) |> 
            bindEvent(input$goButton, ignoreNULL = FALSE) 
        
        outputOptions(output, "dreifing_plot", suspendWhenHidden = FALSE)
        
        dreifing_tafla <- reactive({
            
            dreifing_df() |> 
                make_dreifing_table(input = input)
            
        })
        
        
        
        output$dreifing_tafla <- renderDT({
            
            dreifing_tafla()
            
        }) |> 
            bindEvent(input$goButton, ignoreNULL = FALSE)
        
        outputOptions(output, "dreifing_tafla", suspendWhenHidden = FALSE)
    })
}