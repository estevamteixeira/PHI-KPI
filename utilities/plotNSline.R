## lineplot for total -------
plot_line <- function(data, var, ylab){
 # Use 'import' from the 'modules' package.
 # These listed imports are made available inside the module scope.
 import("dplyr")
 import("ggplot2")
 import("plotly")

 dta <- data
 var <- unlist(var)
 delta <- unlist(names(dta)[endsWith(names(dta),"delta")])

 pal <- "#44AD99"

 plotly::plot_ly(
  data = dta %>% arrange(BrthYear),
  x = ~BrthYear,
  y = ~.data[[var]],
  type = "scatter",
  mode = "lines+markers",
  hovertemplate = ~ifelse(
   !is.na(.data[[delta]]),
   paste(paste0(
    "<b>",.data[["label"]]," in ",BrthYear,"</b>",
    "<br><br>",
    scales::percent(.data[[var]], accuracy = 0.01),
    " (<b><span style='color:",
    ifelse(.data[[delta]] < 0, "#D9715F",
           ifelse(.data[[delta]] > 0, "#44AD99", "#F2C577")),
    "'>",
    ifelse(.data[[delta]] < 0, "\u2B07",
           ifelse(.data[[delta]] > 0, "\u2B06", "-")),
    " ",
    scales::percent(.data[[delta]], accuracy = 0.01),"</span></b>",
    ", year on year)"),
    "<extra></extra>"),
   paste(paste0(
    "<b>",.data[["label"]]," in ",BrthYear,"</b>",
    "<br><br>",
    scales::percent(.data[[var]], accuracy = 0.01),
    " (-, year on year)"),
    "<extra></extra>")
  ),
  line = list(color = pal),
  marker = list(color = pal)) %>%
  plotly::style(hoverlabel = list(
   bgcolor  = "black",
   bordercolor = "transparent",
   font = list(
    color = "white",
    size = 14,
    face = "bold"
   )
  )) %>%
  plotly::layout(
   #showlegend = FALSE,
   legend = list(
    orientation = "h",
    y = -0.25, x = 0.5,
    xanchor = "center", yanchor = "middle",
    font = list(
     size = 10,
     face = "bold"
    )
   ),
   xaxis = list(
    color = "#307972",
    title = list(
     text = "Year",
     face = "bold",
     size = 14
    ),
    tickfont = list(
     face = "bold",
     size = 14
    ),
    tickformat = "%Y"
   ),
   yaxis = list(
    color = "#307972",
    title = list(
     text = ylab,
     face = "bold",
     size = 14,
     rangemode = "tozero"
    ),
    tickfont = list(
     face = "bold",
     size = 14
    ),
    tickformat = ".1%"
   ),
   font = list(
    family = "Montserrat",
    size = 14,
    color = "#307972"
   )
  ) %>%
  plotly::config(displaylogo = FALSE,
                 modeBarButtonsToRemove = c(
                  "select2d",
                  "zoomIn2D",
                  "zoomOut2d",
                  "zoom2d",
                  "pan2d",
                  "lasso2d",
                  "autoScale2d",
                  "resetScale2d",
                  "hoverClosestCartesian",
                  "hoverCompareCartesian"
                 ))
 }

## area plot for reactable ----

plot_line_index <- function(data){
 # Use 'import' from the 'modules' package.
 # These listed imports are made available inside the module scope.
 import("dplyr")
 import("ggplot2")
 import("ggiraph")

 ggplot(
  data = data,
  aes(
  x = BrthYear,
  y = rate)) +
  geom_area(
   fill = "#44AD99",
   color = "#44AD99",
   alpha = 0.1) +
  geom_point_interactive(
   fill = "#44AD99",
   color = "#44AD99",
   size = 1.5) +
  theme_void()
}
