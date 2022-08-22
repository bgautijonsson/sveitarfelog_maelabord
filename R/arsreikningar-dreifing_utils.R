make_dreifing_df <- function(input) {
    y_var <- ui_name_to_data_name(input$y_var)
    
    d |> 
        group_by(sveitarfelag) |> 
        filter(ar == max(ar), hluti == input$hluti) |> 
        ungroup() |> 
        select(sveitarfelag, ar, y = all_of(y_var)) |> 
        drop_na(y)  
}


make_dreifing_ggplot <- function(dreifing_df, input) {
    
    plot_dat <- dreifing_df |> 
        mutate(my_colour = 1 * (sveitarfelag %in% input$vidmid) + 2 * (sveitarfelag == "Heild"),
               text = text_tooltip_throun(sveitarfelag, y, ar, input$y_var),
               sveitarfelag = case_when(sveitarfelag == input$vidmid ~ str_c("<b style='color:#2171b5'>", sveitarfelag, " (Til ", ar, ")", "</b>"),
                                        sveitarfelag == "Heild" ~ str_c("<b style='color:#b2182b'>", sveitarfelag, " (Til ", ar, ")", "</b>"),
                                        TRUE ~ str_c(sveitarfelag, " (Til ", ar, ")")),
               sveitarfelag = fct_reorder(sveitarfelag, y))
    
    x_scale <- make_x_scale(input$y_var)
    subtitles <- make_subtitles(input$y_var)
    vline_and_segments <- make_vline_and_segments(input$y_var)
    coords <- make_coords_dreifing(input$y_var, plot_dat$y)
    
    plot_dat |> 
        ggplot(aes(y, sveitarfelag, text = text)) +
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
             caption = caption)
}


make_dreifing_ggplotly <- function(dreifing_ggplot, input) {
    ggplotly(
        dreifing_ggplot,
        tooltip = "text",
        height = 1200,
        width = 1000
    ) |> 
        layout(
            title = list(
                y = 0.95, yanchor = "top",
                x = 0, xref = "paper"
            ),
            margin = list(
                t = 105,
                r = 0,
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
}

make_dreifing_table <- function(dreifing_df, input) {
    
    
    table_dat <- dreifing_df |> 
        select(sveitarfelag, ar, y) |> 
        arrange(desc(y)) |> 
        mutate(y = round(y, digits = get_digits_yvar(input$y_var)),
               y = format_number(input$y_var, y),
               nr = str_c(str_pad(row_number(), width = 2, side = "left", pad = "0"), "/", n())) |> 
        select(nr, sveitarfelag, ar, y) |> 
        rename(Röðun = nr, Sveitarfélag = sveitarfelag, "Síðasti ársreikningur" = ar, !!input$y_var := y)
    
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
}