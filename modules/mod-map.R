# Use 'import' from the 'modules' package ----
# These listed imports are made available inside the module scope
modules::import("arrow")
modules::import("bslib")
modules::import("dplyr")
modules::import("DT")
modules::import("leaflet")
modules::import("mapview")
modules::import("sf")
modules::import("shiny")

# Define which objects from the module you make available to a user ----
# All other objects are kept private, local, to the module.
modules::export("mapUI")
modules::export("mapServer")


# It is a variation of 'use' where instead of returning a module
# as return value, the elements are copied to the calling environment.
modules::expose("utilities/plotNSmap.R")

mapUI <- function(id){
 # The `ns <- NS()` structure creates a "namespacing" function, that will
 # prefix all ids with a string
 ns <- NS(id)

 layout_sidebar(
  shinyjs::useShinyjs(),
  sidebar = sidebar(
   ## Select which data to analyze:
   ## Calendar/Fiscal year
   input_switch(ns("fdata"), "Fiscal Year", FALSE),
   ## Indicator option to select ----
   # Options are categorized in groups
   selectInput(
    inputId = ns("metric"),
    label = "Indicator",
    choices = NULL,
    selected = NULL
   ),
   selectInput(
    inputId = ns("geo"),
    label = tooltip(
     span("Geographic unit",
          bsicons::bs_icon("question-circle")
          ),
     "Geographical links were determined based on",strong("mothers'"),
     "postal codes at the time of admission for delivery."
                    ),
    choices = NULL,
    selected = NULL
   ),
   ## Put year options side-by-side ----
   layout_column_wrap(
    width = 1/2,
    fill = FALSE,
    selectInput(
     inputId = ns("t0"),
     label = "Year",
     choices = NULL,
     selected = NULL
    )
   ),
   ## conditionally show or hide UI elements based on a JavaScript expression
   ## checks if the geo_selected_data input exists and is not undefined
   ## If both conditions are true, the UI elements will be displayed
   conditionalPanel("typeof input.geotable_rows_selected() !== 'undefined' ||
                     input.geotable_rows_selected.length() > 0",
                    uiOutput(ns("controls"))
   )
  ),
  layout_columns(
   col_widths = c(7,5),
   card(
    full_screen = TRUE,
    card_header(
     div(
      style = "white-space: nowrap; width: auto;", # Dynamically adjusts to content width
      uiOutput(ns("map_title"))
     ),
     div(
      style = "display: inline; width: 100%; margin-left: 5px",  # Adjust margin as needed
      tooltip(
       bsicons::bs_icon("info-circle"),
       uiOutput(ns("map_btn"))
      )
     ),
     tooltip(
      downloadButton(ns("map_down"),
                     label = "",
                     inline = TRUE,
                     icon = shiny::icon("camera")
      ),
      "Download map as png",
      style = "margin-left: auto;"  # Move download button to the right
     ),
     class = "d-flex justify-content-between align-items-center"
    ),
    card_body(
     leafletOutput(ns("geomap")),
    )
   ),
   card(
    full_screen = TRUE,
    card_header(
     div(
      style = "white-space: nowrap; width: auto;", # Dynamically adjusts to content width
      uiOutput(ns("tab_title"))
     ),
     div(
      style = "display: inline; width: 100%; margin-left: 5px",  # Adjust margin as needed
      tooltip(
       bsicons::bs_icon("info-circle"),
       uiOutput(ns("tab_btn"))
       )
      ),
     class = "d-flex justify-content-between align-items-center"
     ),
    card_body(
     DT::dataTableOutput(ns("geotable"))
     )
    )
   )
 )
}

mapServer <- function(id, df1, df2, df3, df4, df5,
                      df6, df7, df8, df9 ,df10, df11){
 moduleServer(id, function(input, output, session){
  ## Setting id for session
  ns <- session$ns

  # Using a "server-side selectize" option that massively improves
  # performance and efficiency (for large numbers of choices)
  updateSelectInput(
   session,
   inputId = "metric",
   choices = df1,
   selected = c("prehyp_rate")
  )

  # Using a "server-side selectize" option that massively improves
  # performance and efficiency (for large numbers of choices)
  updateSelectInput(
   session,
   inputId = "geo",
   choices = df3,
   selected = c("cd")
  )

  # Select the shape file depending on the selected geographic unit ----
  shape <- reactive({
   if(input$geo %in% "cd")  return(sf::st_make_valid(df8))
   if(input$geo %in% "cl")  return(sf::st_make_valid(df9))
   if(input$geo %in% "chn") return(sf::st_make_valid(df10))
   if(input$geo %in% "hr")  return(sf::st_make_valid(df11))
  })

  # Select the stats file depending on the selected geographic unit ----
  stats <- reactive({

   # Ensure input$geo is available before proceeding
   req(input$geo)

   if(input$geo %in% "cd")  return(df4)
   if(input$geo %in% "cl")  return(df5)
   if(input$geo %in% "chn") return(df6)
   if(input$geo %in% "hr")  return(df7)
  })

  ## Update the options for the initial year selectInput()
  ## The values should reflect the data availability
  observeEvent(input$geo,{

   # Ensure stats() is not NULL
   req(stats())

   df <- stats()

   updateSelectInput(
    session,
    inputId = "t0",
    choices = sort(
     unique(
      df %>%
       select(BrthYear) %>%
       distinct() %>%
       pull()
      )
     ),
    selected = min(
     sort(
      unique(
       df %>%
        select(BrthYear) %>%
        distinct() %>%
        pull()
      )
     ),
     na.rm = TRUE)
   )
  })

  # Get the list name to make the
  # Card header dynamic
  lbl <- reactive({
   metric <- input$metric
   label <- names(unlist(unname(df1)))[unlist(df1) %in% metric]
   return(label)
  })

  output$map_title <- renderUI({
   lbl()
  })

  output$tab_title <- renderUI({
   lbl()
  })

  # Get the list name to make the
  # Tooltip (i button) dynamic

  ibtn <- reactive({
   metric <- input$metric
   text <- unlist(unname(df2))[names(unlist(df2)) %in% metric]
   return(HTML(text))
  })

  output$map_btn <- renderUI({
   ibtn()
  })

  output$tab_btn <- renderUI({
   ibtn()
  })

  # `selected_dta` is a reactive expression whose results will depend on ----
  # the t0, indicator, and geo
  selected_dta <- reactive({
   return(
    stats() %>%
     select(contains("id"), BrthYear, input$metric) %>%
     filter(BrthYear == as.numeric(input$t0)) %>%
     distinct() %>%
     mutate(label = lbl()) %>%
     collect()
   )
  })

  # Create `geodta` data: merge `selected_dta()` with the shape file `shape()` to draw the map ----

  geodta <- reactive({

   req(selected_dta())

   merge(
    shape(),
    selected_dta(),
    by.x = names(shape())[endsWith(tolower(names(shape())),"id")],
    by.y = names(selected_dta())[endsWith(tolower(names(selected_dta())),"id")],
    all.x = TRUE
   ) %>%
    arrange(desc(across(ends_with("rate"))))
  })

  # Create an object for storing reactive values ----
  map <- reactiveValues(dat = NULL, mapView = NULL)

  # Map output ----
  output$geomap <- renderLeaflet({
   req(geodta())
   validate(need(nrow(geodta()) > 0,
                 "Sorry, there is no data available for the selected options.
             \nPlease, choose different years and/or conditions."))

   map$dat <- nsmap(geodta(),
                    input$metric)
  })

  # Map download ----

  output$map_down <- downloadHandler(

   filename = function(){

    # Ensure inputs are available
    req(input$metric, input$t0)

    paste0(sub("_.*", "", input$metric),
           "_", input$t0, ".png")
   },

   content = function(file){
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))

    if(!is.null(map$mapView)){

     withProgress(message = "Creating file", value = 0,{

      Sys.sleep(0.25)
      incProgress(1/10)

      dta <- geodta() %>% filter(GeoUID %in% map$mapView$GeoUID)

      Sys.sleep(0.25)
      incProgress(3/10)

      # color palette
      pal <- leaflet::colorBin(
       c("#D8FFFB", "#44AD99", "#307972"), # https://colors.muz.li/
       domain = geodta()[[input$metric]]
      )

      Sys.sleep(0.25)
      incProgress(5/10)

      # Popup label
      lab <- ifelse(
       is.na(dta[[input$metric]]) | dta[[input$metric]] %in% 0,
       paste(
        "<b>",dta[["label"]],"</b>",
        "<br><b>",dta[["name"]],"-",dta[["BrthYear"]],"</b>",
        "<br>",
        "No information provided"
       ),
       paste(
        "<b>",dta[["label"]],"</b>",
        "<br><b>",dta[["name"]],"-",dta[["BrthYear"]],"</b>",
        "<br>",
        scales::percent(dta[[input$metric]], accuracy = 0.01)
       )
      )|> lapply(htmltools::HTML)

      # Map title
      tlt_txt <- paste(
       lbl(),
       "<br>",
       dta$name,
       "<br>",
       input$t0) |>
       htmltools::HTML()

      tlt <- tags$div(
       tags$style(
        HTML(
         ".leaflet-control.map-title {
    background: rgba(221,221,221,0.5);
    font-weight: bold;
    font-size: 20px;
    font-family: Montserrat;
  }"
        )
       ), tlt_txt)

      # Footnote
      foot_txt <- tags$p(style = "bottom: 0; width: 100%; font-family: Montserrat;",
                         tags$span(HTML("&copy;"),
                                   paste(format(Sys.Date(), "%Y"),
                                         c("| All Rights Reserved | Built with")), HTML("&#x2764;"),paste("by"),
                                   tags$a(href = "http://rcp.nshealth.ca/", style = "color: #000000;",
                                          tags$strong("Reproductive Care Program of Nova Scotia")
                                   )
                         ))

      foot <- tags$div(
       tags$style(
        HTML(
         ".leaflet-control.map-foot {
    background: rgba(221,221,221,0.5);
    font-weight: bold;
    font-size: 12px;
  }"
        )
       ), foot_txt)

      Sys.sleep(0.25)
      incProgress(7/10)

      mapview::mapshot(
       nsmap(dta, input$metric) |>
        leaflet::addLegend(
         pal = pal,
         values = ~geodta()[[input$metric]],
         opacity = 1,
         title = "",
         position = "bottomright",
         na.label = "Not informed"
        ) |>
        leaflet::setView(
         lat = mean(sf::st_bbox(dta)[c(2,4)]),
         lng = mean(sf::st_bbox(dta)[c(1,3)]),
         zoom = 9) |>
        leaflet::addPolygons(data = dta,
                             color = "#FF0000",# red
                             opacity = 1,
                             fill = FALSE,
                             weight = 3,
                             layerId = "bounds") |>
        leaflet::addPopups(
         lat = mean(sf::st_bbox(dta)[c(2,4)]),
         lng = mean(sf::st_bbox(dta)[c(1,3)]),
         popup = lab,
         options = leaflet::popupOptions(
          closeButton = FALSE,
          maxWidth = 600)) |>
        leaflet::addControl(tlt, position = "topleft", className = "map-title") |>
        leaflet::addControl(foot, position = "bottomleft", className = "map-foot"),
       file = file)

      Sys.sleep(0.25)
      incProgress(10/10)

     })
    } else{
     withProgress(message = "Creating file, please wait.", value = 0,{
      Sys.sleep(0.25)
      incProgress(1/10)

      # color palette
      pal <- leaflet::colorBin(
       c("#D8FFFB", "#44AD99", "#307972"), # RCP green colors - https://www.color-hex.com/
       domain = geodta()[[input$metric]]
      )

      # Map title
      tlt_txt <- paste(
       lbl(),
       "<br>",
       input$t0) |>
       htmltools::HTML()

      tlt <- tags$div(
       tags$style(
        HTML(
         ".leaflet-control.map-title {
    background: rgba(221,221,221,0.5);
    font-weight: bold;
    font-size: 20px;
    font-family: Montserrat;
  }"
        )
       ), tlt_txt)

      # Footnote
      foot_txt <- tags$p(style = "bottom: 0; width: 100%;",
                         tags$span(HTML("&copy;"),
                                   paste(format(Sys.Date(), "%Y"),
                                         c("| All Rights Reserved | Built with")), HTML("&#x2764;"),paste("by"),
                                   tags$a(href = "http://rcp.nshealth.ca/", style = "color: #000000;",
                                          tags$strong("Reproductive Care Program of Nova Scotia")
                                   )
                         ))

      foot <- tags$div(
       tags$style(
        HTML(
         ".leaflet-control.map-foot {
    background: rgba(221,221,221,0.5);
    font-weight: bold;
    font-size: 12px;
    font-family: Montserrat;
  }"
        )
       ), foot_txt)

      Sys.sleep(0.25)
      incProgress(5/10)

      mapview::mapshot(
       map$dat |>
        leaflet::addLegend(
         pal = pal,
         values = ~geodta()[[input$metric]],
         opacity = 1,
         title = "",
         position = "bottomright",
         na.label = "Not informed"
        ) |>
        leaflet::addControl(tlt, position = "topleft", className = "map-title") |>
        leaflet::addControl(foot, position = "bottomleft", className = "map-foot"),
       file = file)

      Sys.sleep(0.25)
      incProgress(10/10)
     }) }
   }
  )

  # This allows to add footer with totals
  # Here we need to make use of the isolate() function
  # Otherwise, this object should be put inside the DT::renderDT() call

  field <- reactive({

   # Ensure input$geo is available before proceeding
   req(input$geo)

   if(input$geo %in% "cd")  return("County")
   if(input$geo %in% "cl")  return("Community <br> Cluster")
   if(input$geo %in% "chn") return("Community <br> Health Network")
   if(input$geo %in% "hr")  return("Health <br> Management Zone")
  })

  sketch <- reactive({

   # Ensure field() is available before proceeding
   req(field())

   htmltools::withTags(table(
    tableHeader(c("GeoUID", "BrthYear", field(), "Rate"),
                escape = FALSE)
   ))

  })

  # DataTable object
  output$geotable <- DT::renderDT({
   req(geodta())
   validate(need(nrow(geodta()) > 0,
                 "Sorry, there is no data available for the selected options.
            \nPlease, choose different year and/or indicator."))

   DT::datatable(
    geodta() %>% as_tibble() %>% select(GeoUID, BrthYear, name, ends_with("rate")),
    container = sketch(),
    rownames = FALSE,
    style = "auto",
    selection = 'single',
    extensions = "Buttons",
    # caption = lbl(),
    options = list(
     dom = 'B<t>ftp',
     extensions = "Buttons",
     search = list(regex = TRUE, caseInsensitive = TRUE),
     paging = TRUE,
     pageLength = 5,
     ordering = TRUE,
     stateSave = TRUE,
     columnDefs = list(list(visible = FALSE,
                            targets = c(0,1)))
    )
   ) |>
    DT::formatPercentage(
     columns = names(geodta())[endsWith(names(geodta()),"rate")],
     digits = 2,
     mark = ","
    )
  }, server = FALSE)

  # DataTable proxy object ----
  DTproxy <- DT::dataTableProxy("geotable")

  observeEvent(input$geotable_rows_selected,{

   l <- input$geotable_rows_selected

   # Show the controls when a row is selected
   shinyjs::show("controls")

   # Update map with selected line
   leaflet::leafletProxy("geomap", session, data = geodta()[l,]) |>
    leaflet::removeShape("bounds") |>
    leaflet::setView(
     lat = mean(sf::st_bbox(geodta()[l,])[c(2,4)]),
     lng = mean(sf::st_bbox(geodta()[l,])[c(1,3)]),
     zoom = 8) |>
    leaflet::addPolygons(data = geodta()[l,],
                         color = "#FF0000",
                         opacity = 1,
                         fill = FALSE,
                         weight = 3,
                         layerId = "bounds")

   # Update reactiveValues with current map view
   map$mapView <- leaflet::getMapData(
    leaflet::leafletProxy("geomap", session, data = geodta()[l,])
   )
  })

  ## Reset button ----
  output$controls <- renderUI({
   req(input$geotable_rows_selected)
   absolutePanel(id = "controls",
                 actionButton(inputId = ns("reset"),
                              label = "Reset map"))

  })

  ## Resetting map to original value when reset button is clicked ----
  observeEvent(input$reset, {

   # Hiding the control button
   shinyjs::hide("controls")

   # plotting original map
   leaflet::leafletProxy("geomap", data = geodta()) |>
    leaflet::removeShape("bounds") |>
    leaflet::setView(
     lat = mean(sf::st_bbox(geodta())[c(2,4)]),
     lng = mean(sf::st_bbox(geodta())[c(1,3)]),
     zoom = 6.45)

   # clear row selection ----
   selectRows(DTproxy, selected = NULL)
   # go to page 1
   selectPage(DTproxy, 1)

   # Reset mapView
   map$mapView = NULL
  })

  observeEvent(input$geomap_shape_click,{
   sc <- input$geomap_shape_click$id

   selectRows(DTproxy, selected = which(geodta()[["GeoUID"]] %in% sc))
   selectPage(DTproxy, ceiling(which(geodta()[["GeoUID"]] %in% sc) / input$geotable_state[["length"]]))
  })

 })
}
