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
   ## Put year options side-by-side ----
   layout_column_wrap(
    width = 1/1,
    fill = FALSE,
    selectInput(
     inputId = ns("t0"),
     label = "Period",
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
                      df6, df7, df8, df9 ,df10, df11,
                      df12, df13 ,df14, df15){
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
      sort()
    ),
    selected = c(
     stats() %>%
      select(period, !!sym(input$metric)) %>%
      collect() %>%
      dplyr::filter(stats::complete.cases(.)) %>%
      pull(period) %>%
      min()
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

   stringr::str_wrap(vals$label, width = 40) %>% # Break labels longer than 40 chars
    stringr::str_replace_all("\n", "<br>") %>%
    HTML()
  })

  output$tab_title <- renderUI({# Output label as UI element
   req(vals$label)  # Ensure label is available

   stringr::str_wrap(vals$label, width = 40) %>% # Break labels longer than 40 chars
    stringr::str_replace_all("\n", "<br>") %>%
    HTML()
  })

  # Get the list name to make the
  # Tooltip (i button) dynamic

  output$map_btn <- renderUI({
   HTML(vals$ibtn)
  })

  output$tab_btn <- renderUI({
   HTML(vals$ibtn)
  })

  # `selected_dta` is a reactive expression whose results will depend on ----
  # the t0, indicator, and geo
  selected_dta <- reactive({
   return(
    stats() %>%
     select(contains("id"), period, all_of(input$metric)) %>%
     filter(as.character(period) == as.character(input$t0)) %>% # for some reason, using `%in%` gives this message: Warning: Error in compute.arrow_dplyr_query: Type error: Array type doesn't match type of values set: double vs string
     distinct() %>%
     mutate(label = as.character(vals$label)) %>%
     collect()
   )
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
           "_", toupper(input$geo),
           "_",input$t0, ".png")
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
        "No information provided"
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
  #   font-family: Montserrat;
  # }"
  #       )
  #      ), tlt_txt)

      Sys.sleep(0.25)
      incProgress(6/8)

      # Footnote
      foot_txt <- tags$p(style = "bottom: 0; width: 100%; font-family: Montserrat;",
                         tags$span(HTML("&copy;"),
                                   paste(format(Sys.Date(), "%Y"),
                                         c("| Built with")), HTML("&#x2764;"),paste("by"),
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
      incProgress(7/8)

      mapview::mapshot(
       nsmap(geodta(), input$metric) |>
        leaflet::addLegend(
         pal = pal,
         values = ~geodta()[[input$metric]],
         opacity = 1,
         labFormat = labelFormat(
          suffix = "%",
          between = "% - ",
          transform = function(x) 100*x
          ),
         title = "",
         position = "bottomright",
         na.label = "Not informed"
        ) |>
        leaflet::fitBounds(
         lng1 = sf::st_bbox(dta)[["xmin"]], lat1 = sf::st_bbox(dta)[["ymin"]],
         lng2 = sf::st_bbox(dta)[["xmax"]], lat2 = sf::st_bbox(dta)[["ymax"]]
         ) |>
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
        # leaflet::addControl(tlt, position = "topleft", className = "map-title") |>
        leaflet::addControl(foot, position = "bottomleft", className = "map-foot"),
       file = file)

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
       field(),
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
    font-family: Montserrat;
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
                                          tags$strong("Reproductive Care Program of Nova Scotia")
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
    font-family: Montserrat;
  }"
        )
       ), foot_txt)

      Sys.sleep(0.25)
      incProgress(6/7)

      mapview::mapshot(
       map$dat |>
        leaflet::addLegend(
         pal = pal,
         values = ~geodta()[[input$metric]],
         labFormat = labelFormat(
          suffix = "%",
          between = "% - ",
          transform = function(x) 100*x
         ),
         opacity = 1,
         title = "",
         position = "bottomright",
         na.label = "Not informed"
        ) |>
        leaflet::addControl(tlt, position = "topleft", className = "map-title") |>
        leaflet::addControl(foot, position = "bottomleft", className = "map-foot"),
       file = file)

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
    geodta() %>% as_tibble() %>% select(GeoUID, period, name, ends_with("rate")),
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
    leaflet::fitBounds(
     lng1 = sf::st_bbox(geodta()[l,])[["xmin"]], lat1 = sf::st_bbox(geodta()[l,])[["ymin"]],
     lng2 = sf::st_bbox(geodta()[l,])[["xmax"]], lat2 = sf::st_bbox(geodta()[l,])[["ymax"]]
    ) |>
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
