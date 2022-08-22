make_throun_df <- function(input) {
    
    y_var <- ui_name_to_data_name(input$y_var)
    
    d |> 
        filter(hluti %in% input$hluti, 
               sveitarfelag %in% input$sveitarfelag,
               ar >= input$ar_fra) |> 
        select(ar, sveitarfelag, y = all_of(y_var), visitala_2021) |> 
        mutate(x = ar,
               tooltip = text_tooltip_throun(sveitarfelag, y, ar, input$y_var),
               sveitarfelag = str_c(sveitarfelag, "     ")) |> 
        adjust_for_inflation(input$y_var, input$verdlag, input$ar_fra)
    
}


make_throun_ggplot <- function(throun_df, input) {
    
    
    y_scale <- make_y_scale(input$y_var)
    subtitles <- make_subtitles(input$y_var)
    hlines <- make_hlines(input$y_var)
    coords <- make_coords(input$y_var, throun_df$y)
    
    p <- throun_df |> 
        ggplot(aes(ar, y, group = sveitarfelag, col = sveitarfelag, text = tooltip)) +
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
}


make_throun_table <- function(throun_df, input) {
    
    table_dat <- throun_df |> 
        select(-visitala_2021, -x) |> 
        select(Ár = ar, sveitarfelag, y) |> 
        mutate(y = round(y, digits = get_digits_yvar(input$y_var))) |> 
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
}


make_throun_plotly <- function(throun_plot, input) {
    
    
    ggplotly(
        throun_plot,
        tooltip = "text",
        height = 600,
        width = 1000
    ) |> 
        layout(
            legend = list(
                orientation = "h",
                yanchor = "bottom",
                y = 1.02, 
                x = 0
            ),
            title = list(
                y = 0.95, yanchor = "top",
                x = 0, xref = "paper"
            ),
            margin = list(
                t = 105,
                r = 0,
                b = 120,
                l = 0
            ),
            autosize = FALSE,
            annotations = list(
                list(x = 0.8, xanchor = "right", xref = "paper",
                     y = -0.15, yanchor = "bottom", yref = "paper",
                     showarrow = FALSE,
                     text = caption)
            )
        )
}