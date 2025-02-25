# Use 'import' from the 'modules' package ----
# These listed imports are made available inside the module scope
modules::import("arrow")
modules::import("bslib")
modules::import("dplyr")
modules::import("DT")
modules::import("htmlwidgets")
modules::import("leaflet")
modules::import("sf")
modules::import("shiny")
modules::import("shinycustomloader")
modules::import("stringr")

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
   selectInput(
    inputId = ns("focus"),
    label = "Place of Incidence",
    choices = NULL,
    selected = NULL
   ),
   selectInput(
    inputId = ns("t0"),
    label = "Period",
    choices = NULL,
    selected = NULL
   )
  ),
  layout_columns(
   col_widths = c(12),
   layout_columns(
    col_widths = c(7,5),
    card(
     full_screen = TRUE,
     card_header(
      div(
       style = "display: inline-flex; align-items: center; flex-wrap: wrap; gap: 0.25rem; word-wrap: break-word; word-break: break-word;",
       span(
        style = "display: inline-flex; align-items: center; gap: 0.25rem; flex-wrap: wrap;",
        uiOutput(ns("map_title")),
        div(
         style = "display: inline-flex; align-items: center;",
         tooltip(
          bsicons::bs_icon("info-circle"),
          uiOutput(ns("map_btn"))
         )
        )
       )
      ),
      tooltip(
       downloadButton(ns("map_down"),
                      label = "",
                      inline = TRUE,
                      icon = shiny::icon("camera")
       ),
       "Download map as html",
       style = "margin-left: auto;"  # Move download button to the right
      ),
      class = "d-flex justify-content-between align-items-center"
     ),
     card_body(
      tags$style(HTML(".leaflet-container { background: #FBFBFB !important; }")),
      shinycustomloader::withLoader(
       leafletOutput(ns("geomap")),
       type = "html",
       loader = "loader1"
      ),
      ## conditionally show or hide UI elements based on a JavaScript expression
      ## checks if the geo_selected_data input exists and is not undefined
      ## If both conditions are true, the UI elements will be displayed
      conditionalPanel(
       style = "
       position: absolute;
       z-index: 1000; /* Ensure it appears above the map */
       top: 80% !important; /* Position at the top */
       left: 20px; /* position at the left */
       white-space: nowrap; /* Prevent wrapping inside the button */
       text-align: center; /* Center the button content */
      ",

      condition = "typeof input.geotable_rows_selected !== 'undefined' ||
      input.geotable_rows_selected.length > 0",
      uiOutput(ns("controls"))
      )
     )
    ),
    card(
     full_screen = TRUE,
     card_header(
      div(
       style = "display: inline-flex; align-items: center; flex-wrap: wrap; gap: 0.25rem; word-wrap: break-word; word-break: break-word;",
       span(
        style = "display: inline-flex; align-items: center; gap: 0.25rem; flex-wrap: wrap;",
        uiOutput(ns("tab_title")),
        div(
         style = "display: inline-flex; align-items: center;",
         tooltip(
          bsicons::bs_icon("info-circle"),
          uiOutput(ns("tab_btn"))
         )
        )
       )
      ),
      class = "d-flex justify-content-between align-items-center"
     ),
     card_body(
      shinycustomloader::withLoader(
       DT::dataTableOutput(ns("geotable")),
       type = "html",
       loader = "loader1"
      )
     )
    )
   )
  ),
  # Include the leaflet-image library
  tags$script(src = "https://rawcdn.githack.com/mapbox/leaflet-image/gh-pages/leaflet-image.js")
 )
}

mapServer <- function(id, df1, df2, df3, df4, df5,
                      df6, df7, df8, df9 ,df10, df11,
                      df12, df13 ,df14, df15, df16){
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

  ## Update the options for the `Choose focus` selectInput
  ## The data should reflect data availability
  observeEvent(c(input$geo, input$t0, input$tn),{

   # Choices
   ch <- c("1","11","14","18","30","43","56","67","86","87")

   # Name choices
   lab <- c("All Nova Scotia",
            "Aberdeen Hospital",
            "South Shore Regional ",
            "Colchester Regional",
            "Cumberland Regional",
            "St. Martha's Regional",
            "Western Kings Memorial",
            "Valley Regional",
            "IWK Health",
            "Cape Breton Regional")

   names(ch) <- lab

   ch <- list(
    "Region" = ch[1],
    "Hospital" = ch[2:10]
   )

   updateSelectInput(
    session,
    inputId = "focus",
    choices = ch,
    selected = "1"
   )
  })

  # Select the shape file depending on the selected geographic unit ----
  shape <- reactive({
   if(input$geo %in% "cd")  return(sf::st_make_valid(df8))
   if(input$geo %in% "cl")  return(sf::st_make_valid(df9))
   if(input$geo %in% "chn") return(sf::st_make_valid(df10))
   if(input$geo %in% "hr")  return(sf::st_make_valid(df11))
  })

  hosp_shape <- reactiveVal(NULL) # Stores hospital data reactively

 observeEvent(input$focus, {
   req(input$focus != "1")  # Only proceed if input$focus is NOT 1

   hosp <- sf::st_make_valid(df16) %>%
    filter(GeoUID %in% input$focus) %>%
    distinct()

   hosp_shape(hosp) # Store the result reactively
  })

  # Select the stats file depending on the selected geographic unit ----
  # and by calendar/fiscal year
  stats <- reactive({

   # Ensure input$geo is available before proceeding
   req(input$geo)

   if(input$geo %in% "cd"){
    if(isTRUE(input$fdata)){
     return(df12 %>%
             rename("period" = "FiscalYear")
     )
    } else {
     return(df4 %>%
             rename("period" = "BrthYear")
     )
    }
   }

   if(input$geo %in% "cl"){
    if(isTRUE(input$fdata)){
     return(df13 %>%
             rename("period" = "FiscalYear")
     )
    } else {
     return(df5 %>%
             rename("period" = "BrthYear")
     )
    }
   }

   if(input$geo %in% "chn"){
    if(isTRUE(input$fdata)){
     return(df14 %>%
             rename("period" = "FiscalYear")
     )
    } else {
     return(df6 %>%
             rename("period" = "BrthYear")
     )
    }
   }

   if(input$geo %in% "hr"){
    if(isTRUE(input$fdata)){
     return(df15 %>%
             rename("period" = "FiscalYear")
     )
    } else {
     return(df7 %>%
             rename("period" = "BrthYear")
     )
    }
   }
  })

  ## Update the options for the initial year selectInput()
  ## The values should reflect the data availability
  observeEvent(c(input$fdata,input$metric, input$geo),{

   updateSelectInput(
    session,
    inputId = "t0",
    choices = unique(
     stats() %>%
      select(period, !!sym(input$metric)) %>%
      collect() %>%
      dplyr::filter(stats::complete.cases(.)) %>%
      pull(period) %>%
      as.character() %>%
      sort(decreasing = TRUE)
    ),
    selected = c(
     stats() %>%
      select(period, !!sym(input$metric)) %>%
      collect() %>%
      dplyr::filter(stats::complete.cases(.)) %>%
      pull(period) %>%
      as.character() %>%
      max()
    )
   )
  })

  # Get the list name to make the
  # Card header dynamic
  vals <- reactiveValues(metric = NULL, label = NULL, ibtn= NULL)

  # Observe input and update metric and label within vals
  observeEvent(input$metric, {
   vals$metric <- input$metric

   # Calculate label based on the updated metric
   vals$label <- names(unlist(unname(df1)))[unlist(df1) %in% vals$metric]

   vals$ibtn <- unlist(unname(df2))[names(unlist(df2)) %in% vals$metric]

   # Validate that a label is found
   validate(need(length(vals$label) > 0, "Label not found for selected metric"))
  })

  output$map_title <- renderUI({
   req(vals$label)  # Ensure label is available

   vals$label # Output label as UI element
  })

  output$tab_title <- renderUI({# Output label as UI element
   req(vals$label)  # Ensure label is available

   vals$label # Output label as UI element
  })

  # Get the list name to make the
  # Tooltip (i button) dynamic

  output$map_btn <- renderUI({
   HTML(vals$ibtn)
  })

  output$tab_btn <- renderUI({
   HTML(vals$ibtn)
  })

  fields <- reactive({
   # List of variables to be dynamically selected
   c(
    paste0(sub("_.*", "", vals$metric), "_count"),
    paste0(sub("_.*", "", vals$metric), "_total"),
    paste0(sub("_.*", "", vals$metric), "_rate")
   )
  })

  # `selected_dta` is a reactive expression whose results will depend on ----
  # the t0, indicator, and geo
  selected_dta <- reactive({
   req(stats())

   if(!input$focus %in% 1){
    # Individual hospitals
    ndta <- stats() %>%
     select(contains("id"),DLHosp, period, all_of(fields())) %>%
     filter(DLHosp %in% c(input$focus),
            as.character(period) == as.character(input$t0)) %>%
     distinct() %>%
     mutate(#across(ends_with(c("rate","count")), ~ coalesce(., 0)),
            !!sym(fields()[endsWith(fields(),"total")]) := sum(!!sym(fields()[endsWith(fields(),"total")]), na.rm = TRUE)) %>%
     group_by(across(contains("id"))) %>%
     mutate(!!sym(fields()[endsWith(fields(),"rate")]) := !!sym(fields()[endsWith(fields(),"count")])/!!sym(fields()[endsWith(fields(),"total")])) %>%
     collect()

    return(ndta)
   } else{
    ndta <- stats() %>%
     select(contains("id"),DLHosp, period, all_of(fields())) %>%
     filter(as.character(period) == as.character(input$t0)) %>%
     distinct() %>%
     mutate(DLHosp = 1) %>%
     group_by(across(contains("id"))) %>%
     mutate(
      !!sym(fields()[endsWith(fields(),"total")]) := sum(!!sym(fields()[endsWith(fields(),"total")]), na.rm = TRUE),
      !!sym(fields()[endsWith(fields(),"count")]) := sum(!!sym(fields()[endsWith(fields(),"count")]), na.rm = TRUE),
      !!sym(fields()[endsWith(fields(),"rate")]) := !!sym(fields()[endsWith(fields(),"count")])/!!sym(fields()[endsWith(fields(),"total")])
     ) %>%
     ungroup() %>%  # Ungroup before applying window functions
     distinct() %>%
     collect()

    return(ndta)
   }
  })

  # Create `geodta` data: merge `selected_dta()` with the shape file `shape()` to draw the map ----

  geodta <- reactive({

   req(selected_dta(), shape())
   validate(need(nrow(selected_dta()) > 0,
                 "Sorry, there is no data available for the selected options.
             \nPlease, choose different years, geographies, and/or conditions."))

   merge(
    shape(),
    selected_dta(),
    by.x = names(shape())[endsWith(tolower(names(shape())),"id")],
    by.y = names(selected_dta())[endsWith(tolower(names(selected_dta())),"id")],
    all.x = TRUE
   ) %>%
    arrange(desc(across(ends_with("rate"))))
    # mutate(across(ends_with("_rate"), ~replace(., is.na(.), 0)))
  })

  hospital_icon <- leaflet::makeIcon(
   iconUrl = "www/hospital-solid.svg", # hospital icon
   iconWidth = 25, iconHeight = 25
  )

  # Create an object for storing reactive values ----
  map <- reactiveValues(dat = NULL, mapView = NULL)

  # Map output ----
  output$geomap <- renderLeaflet({
   req(geodta())
   validate(need(nrow(geodta()) > 0,
                 "Sorry, there is no data available for the selected options.
             \nPlease, choose different years and/or conditions."))

   if(!input$focus %in% 1){
    # Individual hospitals
    map$dat <- nsmap(
     geodta() ,
     input$metric
    ) %>%
     addCircleMarkers(
      data = hosp_shape(),
      color = "#D9715F",
      opacity = 0.8,
      fillOpacity = 0.8,
      label = ~FACILITY,
      labelOptions = labelOptions(
       style = list(
        "font-weight" = "bold",
        padding = "3px 8px",
        "background-color" = "black",
        "color" = "white"
       ),
       textsize = "15px",
       direction = "auto"
      ),
      group = "Hospital"
     )
     # addLayersControl(
     #  overlayGroups = c("Hospital"),
     #  options = layersControlOptions(collapsed = FALSE)
     # )
   } else{
    map$dat <- nsmap(
     geodta() ,
     input$metric
    )
   }
  })

  # Map download ----

  output$map_down <- downloadHandler(

   filename = function(){

    # Ensure inputs are available
    req(input$metric, input$t0)

    paste0(sub("_.*", "", input$metric),
           "_", toupper(input$geo),
           "_",input$t0, ".html")
   },

   content = function(file){
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))

    if(!is.null(map$mapView)){

     withProgress(message = "Creating file", value = 0,{

      Sys.sleep(0.25)
      incProgress(1/8)

      dta <- geodta() %>% filter(GeoUID %in% map$mapView$GeoUID)

      Sys.sleep(0.25)
      incProgress(2/8)

      # color palette
      pal <- leaflet::colorBin(
       c("#D8FFFB", "#44AD99", "#307972"), # https://colors.muz.li/
       domain = geodta()[[input$metric]]
      )

      Sys.sleep(0.25)
      incProgress(3/8)

      # Popup label
      lab <- ifelse(
       is.na(dta[[input$metric]]) | dta[[input$metric]] %in% 0,
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
        scales::percent(dta[[input$metric]], accuracy = 0.01)
       )
      )|> lapply(htmltools::HTML)

      Sys.sleep(0.25)
      incProgress(4/8)

      # Map title
      # tlt_txt <- paste(
      #  stringr::str_wrap(vals$label, width = 40),
      #  "<br>",
      #  dta$name,
      #  "<br>",
      #  input$t0) |>
      #  stringr::str_replace_all("\n", "<br>") |>
      #  htmltools::HTML()

      Sys.sleep(0.25)
      incProgress(5/8)

      #     tlt <- tags$div(
      #      tags$style(
      #       HTML(
      #        ".leaflet-control.map-title {
      #   background: rgba(221,221,221,0.5);
      #   font-weight: bold;
      #   font-size: 20px;
      #   font-family: Poppins;
      # }"
      #       )
      #      ), tlt_txt)

      Sys.sleep(0.25)
      incProgress(6/8)

      # Footnote
      foot_txt <- tags$p(style = "bottom: 0; width: 100%; font-family: Poppins;",
                         tags$span(HTML("&copy;"),
                                   paste(format(Sys.Date(), "%Y"),
                                         c("| Built with")), HTML("&#x2764;"),paste("by"),
                                   tags$a(href = "http://rcp.nshealth.ca/", style = "color: #000000;",
                                          tags$strong("RCP-NS")
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
      incProgress(7/8)

      htmlwidgets::saveWidget(
       nsmap(geodta(), input$metric) |>
       leaflet::fitBounds(
        lng1 = sf::st_bbox(dta)[["xmin"]], lat1 = sf::st_bbox(dta)[["ymin"]],
        lng2 = sf::st_bbox(dta)[["xmax"]], lat2 = sf::st_bbox(dta)[["ymax"]]
       ) |>
        leaflet::addPolygons(
         data = dta,
         color = "#FF0000",# red
         opacity = 1,
         fill = FALSE,
         weight = 3,
         label = lab,
         labelOptions = labelOptions(
          style = list(
           "font-weight" = "bold",
           padding = "3px 8px",
           "background-color" = "black",
           "color" = "white"
          ),
          textsize = "15px",
          direction = "auto",
          permanent = TRUE # open the tooltip permanently
         ),
         layerId = "bounds"
         ) |>
        addCircleMarkers(
         data = hosp_shape(),
         color = "#D9715F",
         opacity = 0.8,
         fillOpacity = 0.8,
         label = ~FACILITY,
         labelOptions = labelOptions(
          style = list(
           "font-weight" = "bold",
           padding = "3px 8px",
           "background-color" = "black",
           "color" = "white"
          ),
          textsize = "15px",
          direction = "auto"
          # permanent = TRUE # open the tooltip permanently
         ),
         group = "Hospital"
        ) |>
        # leaflet::addControl(tlt, position = "topleft", className = "map-title") |>
        leaflet::addControl(foot, position = "bottomleft", className = "map-foot"),
       file = file, selfcontained = TRUE)

      Sys.sleep(0.25)
      incProgress(8/8)

     })
    } else{

     withProgress(message = "Creating file, please wait.", value = 0,{
      Sys.sleep(0.25)
      incProgress(1/7)

      # color palette
      pal <- leaflet::colorBin(
       c("#D8FFFB", "#44AD99", "#307972"), # RCP green colors - https://www.color-hex.com/
       domain = geodta()[[input$metric]]
      )

      Sys.sleep(0.25)
      incProgress(2/7)

      # Map title
      tlt_txt <- paste(
       stringr::str_wrap(vals$label, width = 40),
       stringr::str_replace_all(field(), "<br> ", ""),
       input$t0,
       sep = "<br>") |>
       stringr::str_replace_all("\n", "<br>") |>
       htmltools::HTML()

      Sys.sleep(0.25)
      incProgress(3/7)

      tlt <- tags$div(
       tags$style(
        HTML(
         ".leaflet-control.map-title {
    background: rgba(221,221,221,0.5);
    font-weight: bold;
    font-size: 20px;
    font-family: Poppins;
  }"
        )
       ), tlt_txt)

      Sys.sleep(0.25)
      incProgress(4/7)

      # Footnote
      foot_txt <- tags$p(style = "bottom: 0; width: 100%;",
                         tags$span(HTML("&copy;"),
                                   paste(format(Sys.Date(), "%Y"),
                                         c("| Built with")), HTML("&#x2764;"),paste("by"),
                                   tags$a(href = "http://rcp.nshealth.ca/", style = "color: #000000;",
                                          tags$strong("RCP-NS")
                                   )
                         ))

      Sys.sleep(0.25)
      incProgress(5/7)

      foot <- tags$div(
       tags$style(
        HTML(
         ".leaflet-control.map-foot {
    background: rgba(221,221,221,0.5);
    font-weight: bold;
    font-size: 12px;
    font-family: Poppins;
  }"
        )
       ), foot_txt)

      Sys.sleep(0.25)
      incProgress(6/7)

      htmlwidgets::saveWidget(
       map$dat |>
        # leaflet::addLegend(
        #  pal = pal,
        #  values = ~geodta()[[input$metric]],
        #  labFormat = labelFormat(
        #   suffix = "%",
        #   between = "% - ",
        #   transform = function(x) 100*x
        #  ),
        #  opacity = 1,
        #  title = "",
        #  position = "bottomright",
       #  na.label = "Not informed"
       # ) |>
       leaflet::addControl(tlt, position = "topleft", className = "map-title") |>
        leaflet::addControl(foot, position = "bottomleft", className = "map-foot"),
       file = file, selfcontained = TRUE)

      Sys.sleep(0.25)
      incProgress(7/7)
     }) }
   }
  )

  field <- reactive({

   # Ensure input$geo is available before proceeding
   req(input$geo)

   if(input$geo %in% "cd")  return("Census <br> Division")
   if(input$geo %in% "cl")  return("Community <br> Cluster")
   if(input$geo %in% "chn") return("Community <br> Health Network")
   if(input$geo %in% "hr")  return("Health <br> Management Zone")
  })

  sketch <- reactive({

   # Ensure field() is available before proceeding
   req(field())

   htmltools::withTags(table(
    tableHeader(c("GeoUID", "period", field(), "Rate"),
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
    geodta() %>% as_tibble() %>% select(GeoUID, period, name, ends_with("rate")) %>% mutate(across(ends_with("_rate"), ~replace(., is.na(.), 0))),
    container = sketch(),
    rownames = FALSE,
    style = "auto",
    selection = 'single',
    extensions = "Buttons",
    # caption = lbl(),
    options = list(
     dom = 'B<t>ftp',
     extensions = "Buttons",
     buttons = c("csv"),
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
    leaflet::fitBounds(
     lng1 = sf::st_bbox(geodta()[l,])[["xmin"]], lat1 = sf::st_bbox(geodta()[l,])[["ymin"]],
     lng2 = sf::st_bbox(geodta()[l,])[["xmax"]], lat2 = sf::st_bbox(geodta()[l,])[["ymax"]]
    ) |>
    leaflet::addPolygons(data = geodta()[l,],
                         color = "#FF0000", #red
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

   absolutePanel(
    id = "controls",
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
