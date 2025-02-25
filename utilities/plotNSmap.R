nsmap <- function(df, var){
 import("leaflet")
 import("stringr")
 import("htmltools")

 dta <- df
 var <- unlist(var)

 # color palette

 pal <- leaflet::colorBin(
  c("#D8FFFB", "#44AD99", "#307972"), # RCP green colors - https://www.color-hex.com/
  domain = dta[[var]],
  bins = 8,
  na.color = "#CCCCCC",
  pretty = TRUE
 )

 # Create label tag

 lab <- ifelse(
  is.na(dta[[var]])| dta[[var]] %in% 0,
  paste(
   "<b>",stringr::str_wrap(dta[["label"]], width = 40) |>
    stringr::str_replace_all("\n", "<br>"),"</b>",
   "<br><b>",dta[["name"]],
   "<br>",dta[["period"]],"</b>",
   "<br>",
   "0%"
  ),
  paste(
   "<b>",stringr::str_wrap(dta[["label"]], width = 40) |>
    stringr::str_replace_all("\n", "<br>"),"</b>",
   "<br><b>",dta[["name"]],
   "<br>",dta[["period"]],"</b>",
   "<br>",
   scales::percent(dta[[var]], accuracy = 0.01)
  )
  )|> lapply(htmltools::HTML)

 # Build map

 leaflet::leaflet(
  data = dta,
  padding = 0,
  options = leafletOptions(
   minZoom = 6.45,
   zoomControl = FALSE,
   attributionControl = FALSE) # Removes leaflet link in the bottom right corner of the map
 ) |>
  # addControl(
  #  html = tags$div(
  #   HTML("&copy; "),  # This generates the copyright symbol
  #   # format(Sys.Date(), "%Y"),  # Displays the current year dynamically
  #   tags$a(
  #    href = "https://rcp.nshealth.ca/",
  #    target = "_blank",
  #    rel = "nofollow noreferrer",
  #    "Reproductive Care Program of Nova Scotia"
  #    ),
  #   class = "leaflet-control-attribution leaflet-control"
  #   ), position = "bottomright"
  #  ) |>
  leaflet::addPolygons(
   layerId = ~GeoUID,
   stroke = TRUE,
   fillColor = ~pal(dta[[var]]),
   weight = 0.5,
   opacity = 1,
   color = "#800080", # bright purple
   dashArray = "1",
   fillOpacity = 1,
   highlightOptions = highlightOptions(
    weight = 2,
    color = "#FE9B00", # orange
    dashArray = "",
    bringToFront = FALSE,   # Move polygon back instead of bringing forward
    sendToBack = TRUE       # Explicitly send polygon to back
    ),
   label = lab,
   labelOptions = labelOptions(
    style = list(
     "font-weight" = "normal",
     padding = "3px 8px",
     "background-color" = "black",
     "color" = "white"
    ),
    textsize = "15px",
    direction = "auto")) %>%
  leaflet::addLegend(
   pal = pal,
   values = ~dta[[var]],
   opacity = 1,
   labFormat = labelFormat(
    suffix = "%",
    between = "% - ",
    transform = function(x) 100*x
   ),
   title = "",
   position = "bottomright",
   na.label = "= 0%"
  )
}
