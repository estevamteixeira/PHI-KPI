## lineplot for total -------
plot_line <- function(data, var, ylab){
 # Use 'import' from the 'modules' package.
 # These listed imports are made available inside the module scope.
 import("dplyr")
 import("ggplot2")
 import("plotly")

 dta <- data
 var <- unlist(var)

 pal <- "#44ad99"

 plotly::plot_ly(
  data = dta %>% arrange(BrthYear),
  x = ~BrthYear,
  y = ~.data[[var]],
  type = "scatter",
  mode = "lines+markers",
  hovertemplate = ~paste(
   "<b>", BrthYear, "</b>",
   "<br> Prevalence:",
   scales::percent(.data[[var]],
                 accuracy = 0.01),
   "<extra></extra>"
  ),
  line = list(color = pal),
  marker = list(color = pal)) %>%
  plotly::style(hoverlabel = list(
   bgcolor  = "black",
   bordercolor = "transparent",
   font = list(
    color = "white",
    size = 12,
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
     size = 12
    ),
    tickfont = list(
     face = "bold",
     size = 12
    ),
    tickformat = "%Y"
   ),
   yaxis = list(
    color = "#307972",
    title = list(
     text = ylab,
     face = "bold",
     size = 12,
     rangemode = "tozero"
    ),
    tickfont = list(
     face = "bold",
     size = 12
    ),
    tickformat = ".1%"
   ),
   font = list(
    family = "Montserrat",
    size = 12,
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
