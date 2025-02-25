## lineplot for total -------
plot_line <- function(data, var, alt = NULL, focus, ylab, xlab, add_smooth, add_ci = FALSE, alpha = 0.05) {
 import("dplyr")
 import("ggplot2")
 import("plotly")
 import("stats")
 import("stringr")

 dta <- data
 var <- unlist(var)
 alt <- unlist(alt)
 focus <- unlist(focus)
 delta <- unlist(names(dta)[endsWith(names(dta), "delta")])

 # Get the range of your y variable
 y_range <- range(dta[[var]], na.rm = TRUE)
 y_range <- c(y_range[1] - 0.2 * diff(y_range), y_range[2] + 0.2 * diff(y_range))

 tick_format <- if ((y_range[2] - y_range[1]) < 0.002) {
  ".2%"    # 2 decimal places for small values
 } else if ((y_range[2] - y_range[1]) < 0.015) {
  ".1%"    # 1 decimal places for medium-small values
 } else {
  "1%"      # no decimal places for large values
 }



 if (is.null(alt)) {
  pal <- "#44AD99"
 } else if (tolower(alt) %in% "hruid") {
  dta$HRuid <- factor(
   dta$HRuid,
   levels = c("01", "02", "03", "04"),
   labels = c("Western", "Northern", "Eastern", "Central")
  )

  pal <- c(
   "Western" = "#44AD99", # tranquil teal
   "Northern" = "#196FA3", # coastal blue
   "Eastern" = "#D9715F", # coral comfort
   "Central" = "#F2C577" # mellow yellow
  )
 } else if (tolower(alt) %in% "dlhosp") {
  # Mapping hospital codes to names
  hospital_map <- c(
   "1" = "All Nova Scotia",
   "99" = "Other Regional Hospitals",
   "11" = "Aberdeen Hospital",
   "14" = "South Shore Regional",
   "18" = "Colchester Regional",
   "30" = "Cumberland Regional",
   "43" = "St. Martha's Regional",
   "56" = "Western Kings Memorial",
   "67" = "Valley Regional",
   "86" = "IWK Health",
   "87" = "Cape Breton Regional"
  )

  # Convert focus from code to name
  focus_name <- hospital_map[focus]

  dta$DLHosp <- factor(case_when(
   as.character(dta$DLHosp) %in% "1" ~ "All Nova Scotia",
   as.character(dta$DLHosp) %in% "99" ~ "Other Regional Hospitals",
   as.character(dta$DLHosp) %in% "11" ~ "Aberdeen Hospital",
   as.character(dta$DLHosp) %in% "14" ~ "South Shore Regional",
   as.character(dta$DLHosp) %in% "18" ~ "Colchester Regional",
   as.character(dta$DLHosp) %in% "30" ~ "Cumberland Regional",
   as.character(dta$DLHosp) %in% "43" ~ "St. Martha's Regional",
   as.character(dta$DLHosp) %in% "56" ~ "Western Kings Memorial",
   as.character(dta$DLHosp) %in% "67" ~ "Valley Regional",
   as.character(dta$DLHosp) %in% "86" ~ "IWK Health",
   as.character(dta$DLHosp) %in% "87" ~ "Cape Breton Regional",
   TRUE ~ "Unknown"
  ))

  # Reorder factor levels
  dta$DLHosp <- factor(dta$DLHosp, levels = c(focus_name, setdiff(levels(dta$DLHosp), focus_name)))

  pal <- c("#44AD99", "#196FA3")
  pal <- stats::setNames(pal[seq_along(levels(dta$DLHosp))], levels(dta$DLHosp))
 }

 # Create the base plot
 plt <- plotly::plot_ly(
  data = dta %>% arrange(period),
  x = ~period,
  y = ~.data[[var]],
  type = "scatter",
  mode = "markers",
  color = if (!is.null(alt)) ~.data[[alt]] else NULL,
  colors = if (!is.null(alt)) pal else NULL,
  symbol = if (!is.null(alt)) ~.data[[alt]] else NULL,
  legendgroup = if (!is.null(alt)) ~.data[[alt]] else "NSAll",
  hovertemplate = ~ifelse(
   !is.na(.data[[delta]]),
   paste(paste0(
    "<b>", stringr::str_wrap(.data[["label"]], width = 35), " in",
    "<br>", period, "<br>",
    if (!is.null(alt)) paste("<br>", .data[[alt]], "</b>") else NULL,
    "<br>",
    scales::percent(.data[[var]], accuracy = 0.01),
    " (<b><span style='color:",
    ifelse(.data[[delta]] < 0, "#D9715F",
           ifelse(.data[[delta]] > 0, "#44AD99", "#F2C577")),
    "'>",
    ifelse(.data[[delta]] < 0, "\u2B07",
           ifelse(.data[[delta]] > 0, "\u2B06", "-")),
    " ",
    scales::percent(.data[[delta]], accuracy = 0.01), "</span></b>",
    ", year on year)"),
    "<extra></extra>"),
   paste(paste0(
    "<b>", stringr::str_wrap(.data[["label"]], width = 35), " in",
    "<br>", period, "<br>",
    if (!is.null(alt)) paste("<br>", .data[[alt]], "</b>") else NULL,
    "<br>",
    scales::percent(.data[[var]], accuracy = 0.01),
    " (-, year on year)"),
    "<extra></extra>")
  ),
  marker = if (is.null(alt)) list(color = pal, size = 8) else list(col = pal, size = 8)
 )

 # Add LOESS smoothing if enabled
 if (add_smooth) {
  zscore <- qnorm(1-alpha/2)

  library(dplyr)
  library(tidyr)

  smooth_data <- dta %>%
   group_by(group = if (!is.null(alt)) .data[[alt]] else 1) %>%
   tidyr::nest() %>%
   mutate(
    smooth_results = purrr::map(data, ~ {
     loess_fit <- loess(.x[[var]] ~ as.numeric(.x$period), data = .x, span = 1.5)
     fit <- predict(loess_fit)

     if (add_ci) {
      se_fit <- predict(loess_fit, se = TRUE)
      z <- qnorm(1-alpha/2)  # Assuming 95% confidence intervals
      tibble(
       period = .x$period,
       smooth = fit,
       upper = se_fit$fit + z * se_fit$se.fit,
       lower = se_fit$fit - z * se_fit$se.fit
      )
      } else {
      tibble(
       period = .x$period,
       smooth = fit,
       upper = NA,
       lower = NA
      )
     }
    })
   ) %>%
   select(-data) %>%
   tidyr::unnest(smooth_results) %>%
   {
    if (!is.null(alt)) {
     rename(., !!alt := group)
    } else {
     .
    }
   }

  plt <- plt %>%
   add_ribbons(
    data = smooth_data,
    x = ~period,
    ymin = ~lower,
    ymax = ~upper,
    fillcolor = if (!is.null(alt)) ~scales::alpha(pal[.data[[alt]]], 0.2) else ~scales::alpha(pal, 0.2), # Map group to fillcolor with transparency
    color = if (!is.null(alt)) ~.data[[alt]] else NULL, # Match group for color aesthetic
    line = list(width = 0),
    inherit = FALSE,
    hoverinfo = "skip", # Disable hover for the ribbon
    legendgroup = if (!is.null(alt)) ~.data[[alt]] else "NSAll",
    showlegend = FALSE
   ) %>%
   add_lines(
    data = smooth_data,
    x = ~period,
    y = ~smooth,
    line = list(width = 2),
    mode = "lines",
    color = if (!is.null(alt)) ~.data[[alt]] else I(pal), # Assuming 'group' holds the region (e.g., Western, Northern, etc.)
    colors = pal,
    inherit = FALSE,
    showlegend = FALSE,
    legendgroup = if (!is.null(alt)) ~.data[[alt]] else "NSAll",
    hoverinfo = "skip" # Disable hover for the ribbon
   )
 } else{
  plt <- plt %>%
   add_lines(
    data = dta,
    x = ~period,
    y = ~.data[[var]],
    mode = "lines+markers", # Add markers to the mode
    line = list(width = 2, color = ~pal),
    showlegend = FALSE
   )
 }

 # Layout configuration
 plt <- plt %>%
  plotly::style(hoverlabel = list(
   bgcolor = "black",
   bordercolor = "transparent",
   font = list(
    color = "white",
    size = 14,
    face = "bold"
   )
  )) %>%
  plotly::layout(
   legend = list(
    orientation = "h",
    y = 100, x = 0.5, # Position the legend outside the plot area. Align the legend to the middle
    xanchor = "center", yanchor = "middle",
    font = list(
     size = 12,
     face = "bold"
    )
   ),
   xaxis = list(
    color = "#307972",
    title = list(
     text = xlab,
     face = "bold",
     size = 14
    ),
    tickfont = list(
     face = "bold",
     size = 14
    ),
    automargin = TRUE,
    tickformat = "%Y"
   ),
   yaxis = list(
    color = "#307972",
    rangemode = "tozero",
    range = list(0, y_range[2]), # Lock the y-axis range to the original data range
    title = list(
     text = ylab %>% stringr::str_replace_all("\n", "<br>"),
     face = "bold",
     size = 14
    ),
    tickfont = list(
     face = "bold",
     size = 14
    ),
    tickformat = tick_format,
    #title_standoff = 20,
    automargin = TRUE
   ),
   font = list(
    family = "Poppins",
    size = 14,
    color = "#307972"
   )
  ) %>%
  plotly::config(displaylogo = FALSE,
                 displayModeBar = TRUE,
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

 return(plt)
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
  x = period,
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
