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
            plotlyOutput(NS(id, "plot_vidmid"), height = 1200, width = 1200)
        )
    )
    
}

vidmid_server <-function(id) {
    moduleServer(id, function(input, output, session) {
        
        my_plot_vidmid <- eventReactive(input$goButton, {
            
            fjarf_skuldir_function <- function(x) {
                case_when(
                    x <= 1.5 ~ 0.05,
                    x < 2 ~ (x - 1.5)/0.5 * 0.025 + (2 - x)/0.5 * 0.05,
                    x <= 2.5 ~ (x - 2)/0.5 * 0.005 + (2.5 - x)/0.5 * 0.025,
                    TRUE ~ 0.005
                )
            }
            
            
            plot_dat <- d |> 
                filter(ar >= 2010,
                       sveitarfelag %in% input$sveitarfelag,
                       hluti %in% input$hluti) |> 
                select(sveitarfelag, ar, 
                       nettoskuldir_obs = nettoskuldir_hlutf_tekjur, 
                       rekstrarnidurstada_obs = rekstur_3_ar_hlutf_tekjur, 
                       framlegd_obs = framlegd_hlutf, 
                       veltufe_obs = veltufe_hlutf_tekjur,
                       veltufjarhlutfall_obs = veltufjarhlutfall) |> 
                mutate(framlegd_vidmid = nettoskuldir_obs/10,
                       veltufe_vidmid = nettoskuldir_obs/20,
                       rekstrarnidurstada_vidmid = 0,
                       veltufjarhlutfall_vidmid = 1,
                       nettoskuldir_vidmid = 1) |> 
                pivot_longer(c(-sveitarfelag, -ar), names_to = c("name", "type"), values_to = "value", names_sep = "_") |> 
                pivot_wider(names_from = type, values_from  = value) |> 
                mutate(diff = obs - vidmid,
                       colour = diff > 0,
                       text = text_tooltip_vidmid(sveitarfelag, name, ar, obs, vidmid),
                       name = fct_recode(name,
                                         "Framlegðarhlutfall" = "framlegd",
                                         "Rekstrarniðurstaða\n(síðustu 3 ára)" = "rekstrarnidurstada",
                                         "Veltufé frá rekstri\n(hlutfall af tekjum)" = "veltufe",
                                         "Nettóskuldir\n(hlutfall af tekjum)" = "nettoskuldir",
                                         "Veltufjárhlutfall" = "veltufjarhlutfall") |> 
                           fct_relevel("Nettóskuldir\n(hlutfall af tekjum)")) 
            
            
            p <- plot_dat |> 
                filter(name != "Nettóskuldir\n(hlutfall af tekjum)") |> 
                ggplot() +
                geom_line(aes(ar, vidmid, group = paste(name, sveitarfelag)), lty = 2) +
                geom_point(aes(ar, obs, col = colour, text = text)) +
                # geom_line(data = plot_dat |> filter(name == "Nettóskuldahlutfall"), 
                #           aes(ar, vidmid), lty = 2, inherit.aes = FALSE, col = "black") +
                geom_point(data = plot_dat |> filter(name == "Nettóskuldir\n(hlutfall af tekjum)"),
                           aes(ar, obs, text = text), inherit.aes = FALSE) + 
                geom_line(data = plot_dat |> filter(name == "Nettóskuldir\n(hlutfall af tekjum)"),
                          aes(ar, obs), inherit.aes = FALSE) + 
                scale_x_continuous(breaks = c(2021, 2018, 2014)) +
                scale_y_continuous(labels = label_percent(),
                                   breaks = pretty_breaks(4)) +
                scale_colour_brewer(type = "qual", palette = "Set1") +
                facet_grid(name ~ sveitarfelag, scales = "free_y") +
                theme(legend.position = "none", 
                      panel.spacing.y = unit(0.02, units= "npc"),
                      strip.placement = "outside",
                      strip.text = element_text(margin = margin(t = 5, r = 10, b = 5, l = 10), 
                                                  hjust = 0.5,
                                                  vjust = 0.5, 
                                                  size = 10)) +
                labs(x = NULL,
                     y = NULL,
                     title = str_c("Hvernig gengur sveitarfélögum að standast viðmið Eftirlitsnefndar með fjármálum sveitarfélaga (", input$hluti,")?"),
                     subtitle = "Brotin lína er viðmið og er reiknuð út frá nettóskuldum sem hlutfall af tekjum",
                     caption = caption)
            
            
            p
        })
        
        output$plot_vidmid <- renderPlotly({
            ggplotly(
                my_plot_vidmid(),
                tooltip = "text",
                height = 1200,
                width = 1200,
            ) |> 
                layout(
                    title = list(
                        y = 0.95, yanchor = "top",
                        x = 0, xref = "paper",
                        font = list(
                            size = 14
                        )
                    ),
                    margin = list(
                        t = 105,
                        r = 40,
                        b = 110,
                        l = 0
                    ),
                    autosize = FALSE,
                    annotations = list(
                        list(x = 1, xanchor = "right", xref = "paper",
                             y = -0.05, yanchor = "bottom", yref = "paper",
                             showarrow = FALSE,
                             text = caption)
                    )
                ) 
        })
    })
}