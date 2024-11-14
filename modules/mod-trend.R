# Use 'import' from the 'modules' package ----
# These listed imports are made available inside the module scope
modules::import("arrow")
modules::import("bsicons")
modules::import("bslib")
modules::import("dplyr")
modules::import("plotly")
modules::import("shiny")

# Define which objects from the module you make available to a user ----
# All other objects are kept private, local, to the module.
modules::export("trendUI")
modules::export("trendServer")


# It is a variation of 'use' where instead of returning a module
# as return value, the elements are copied to the calling environment.
modules::expose("utilities/plotNSline.R")

trendUI <- function(id){
 # The `ns <- NS()` structure creates a "namespacing" function, that will
 # prefix all ids with a string
 ns <- NS(id)

 layout_sidebar(
  shinyjs::useShinyjs(),
  sidebar = sidebar(
   ## Select which data to analyze:
   ## Calendar/Fiscal year
   input_switch(ns("fdata"), "Fiscal Year", FALSE),
   ## Metric option to select ----
   # Options are categorized in groups
   selectInput(
    inputId = ns("metric"),
    label = "Indicator",
    choices = NULL,
    selected = NULL
   ),
   ## Put year options side-by-side ----
   layout_column_wrap(
    width = 1/2,
    fill = FALSE,
    selectInput(
     inputId = ns("t0"),
     label = "Initial period",
     choices = NULL,
     selected = NULL
    ),
    selectInput(
     inputId = ns("tn"),
     label = "Final period",
     choices = NULL,
     selected = NULL
    )
   )
   # The picture only shows if running the app through the 'Run App' btn
   # tags$span(
   #  style = "display: flex; justify-content: center; align-items: center; width: 100%;position: absolute; bottom: 0; left: 50%;transform: translateX(-50%);",
   #  tags$a(
   #   href = "https://rcp.nshealth.ca/",
   #   tags$img(
   #    src = "logo/rcp-logo-transparent.svg",
   #    width = "50%",
   #    height = "auto",
   #    alt = "RCP logo"
   #    )
   #   )
   #  )
   ),
  layout_columns(
   col_widths = c(12),
   card(
    full_screen = TRUE,
    card_header(
     div(
      style = "white-space: nowrap; width: auto;", # Dynamically adjusts to content width
      uiOutput(ns("card_title"))
     ),
     tooltip(
      bsicons::bs_icon("info-circle"),
      uiOutput(ns("i_btn")),
      placement = "right"
     ),
     class = "d-flex align-items-center gap-1"
    ),
    card_body(
     plotlyOutput(ns("line"))
    )
   )
  ))
}

trendServer <- function(id, df1, df2, df3, df4){
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

  # `selected_dta` is a reactive expression whose results will depend on ----
  # the fdata input selection: Calendar or Fiscal year.
  selected_dta <- reactive({
   if(isTRUE(input$fdata)){
    return(df4 %>%
            rename("period" = "FiscalYear"))
   } else {
    return(df2 %>%
            rename("period" = "BrthYear"))
   }
  })

  ## Update the options for the initial year selectInput
  ## The values should reflect the data availability
  observeEvent(c(input$fdata,input$metric),{

   updateSelectInput(
    session,
    inputId = "t0",
    choices = unique(
      selected_dta() %>%
       select(period, !!sym(input$metric)) %>%
       collect() %>%
       pull(period) %>%
       as.character() %>%
       sort()
     ),
    selected = c(
     selected_dta() %>%
      select(period, !!sym(input$metric)) %>%
      collect() %>%
      pull(period) %>%
      as.character() %>%
      min()
    )
   )
  })

  ## Make the final year option greater than or equal the
  ## initial year option
  observeEvent(c(input$fdata, input$metric, input$t0),{

   updateSelectInput(
    session,
    inputId = "tn",
    choices = c(
     sort(
      unique(
       selected_dta() %>%
        select(period, !!sym(input$metric)) %>%
        collect() %>%
        pull(period) %>%
        as.character()
      )))[
       c(
        sort(
         unique(
          selected_dta() %>%
           select(period, !!sym(input$metric)) %>%
           collect() %>%
           pull(period) %>%
           as.character()
         )
        )
       ) >= input$t0],
    selected = c(
     selected_dta() %>%
      select(period, !!sym(input$metric)) %>%
      collect() %>%
      pull(period) %>%
      as.character() %>%
      max()
     )
   )
  })

  # Get the list name to make the
  # Card header dynamic
  vals <- reactiveValues(metric = NULL, label = NULL,
                         ibtn = NULL, xlbl = NULL
                         )

  # Observe input and update metric and label within vals
  observeEvent(input$metric, {
   vals$metric <- input$metric

   # Calculate label based on the updated metric
   vals$label <- names(unlist(unname(df1)))[unlist(df1) %in% vals$metric]

   vals$ibtn <- unlist(unname(df3))[names(unlist(df3)) %in% vals$metric]

   # Validate that a label is found
   validate(need(length(vals$label) > 0, "Label not found for selected metric"))
  })

  observeEvent(input$fdata, {
   vals$xlbl <- if(isTRUE(input$fdata)) "Period" else "Year"
  })

  output$card_title <- renderUI({
   req(vals$label)  # Ensure label is available

   vals$label  # Output label as UI element
  })

  # Get the list name to make the
  # Tooltip (i button) dynamic

  output$i_btn <- renderUI({
   HTML(vals$ibtn)
  })

  # `selected_dta` is a reactive expression whose results will depend on ----
  # the t0, tn, metric
  final_dta <- reactive({
   req(vals$label, input$t0, input$tn)

   return(
    selected_dta() %>%
     select(period, matches(paste0("^", sub("_.*", "", input$metric), "_(rate|delta|deltap)$"))) %>%
     filter(as.character(period) >= as.character(input$t0),
            as.character(period) <= as.character(input$tn)) %>%
     distinct() %>%
     mutate(label = as.character(vals$label)) %>%
     collect()
   )
  })


  # Line plot output ----
  output$line <- renderPlotly({
   req(final_dta())
   validate(need(nrow(final_dta()) > 0,
                 "Sorry, there is no data available for the selected options.
             \nPlease, choose different years and/or indicators."))

   plot_line(final_dta(),
             input$metric,
             ylab = paste("Proportion of patients with",
                          stringr::str_wrap(as.character(vals$label), width = 35),
                          sep = "<br>") %>% stringr::str_replace_all("\n", "<br>"),
             xlab = vals$xlbl
             )
   })
 })
}
