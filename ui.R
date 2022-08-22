ui <- navbarPage("Ársreikningar sveitarfélaga",
                 theme = light,
                 position = "fixed-top",

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
                 )

                 
                 
)