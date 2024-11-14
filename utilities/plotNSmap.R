nsmap <- function(df, var){
 import("leaflet")

 dta <- df
 var <- unlist(var)

 # color palette

 pal <- leaflet::colorBin(
  c("#D8FFFB", "#44AD99", "#307972"), # RCP green colors - https://www.color-hex.com/
  domain = dta[[var]]
 )

 # Create label tag

 lab <- ifelse(
  is.na(dta[[var]]) | dta[[var]] %in% 0,
  paste(
   "<b>",dta[["label"]],"</b>",
   "<br><b>",dta[["name"]],
   "<br>",dta[["period"]],"</b>",
   "<br>",
   "No information provided"
  ),
  paste(
   "<b>",dta[["label"]],"</b>",
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
  options = leafletOptions(minZoom = 6.45)
 ) |>
  leaflet::addPolygons(
   layerId = ~GeoUID,
   stroke = TRUE,
   fillColor = ~pal(dta[[var]]),
   weight = 0.5,
   opacity = 1,
   color = "white",
   dashArray = "1",
   fillOpacity = 1,
   highlightOptions = highlightOptions(
    weight = 2,
    color = "#FE9B00", # orange
    dashArray = "",
    bringToFront = TRUE),
   label = lab,
   labelOptions = labelOptions(
    style = list(
     "font-weight" = "normal",
     padding = "3px 8px",
     "background-color" = "black",
     "color" = "white"
    ),
    textsize = "15px",
    direction = "auto"))
}
