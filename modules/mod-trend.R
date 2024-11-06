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
  lbl <- reactive({
   metric <- input$metric
   label <- names(unlist(unname(df1)))[unlist(df1) %in% metric]
   return(label)
  })

  output$card_title <- renderUI({
   lbl()
  })

  # Get the list name to make the
  # Tooltip (i button) dynamic

  ibtn <- reactive({
   metric <- input$metric
   text <- unlist(unname(df3))[names(unlist(df3)) %in% metric]
   return(HTML(text))
  })

  output$i_btn <- renderUI({
   ibtn()
  })

  # `selected_dta` is a reactive expression whose results will depend on ----
  # the t0, tn, metric
  final_dta <- reactive({
   return(
    selected_dta() %>%
     select(period, starts_with(sub("_.*", "", input$metric))) %>%
     filter(as.character(period) >= as.character(input$t0),
            as.character(period) <= as.character(input$tn)) %>%
     distinct() %>%
     mutate(label = lbl()) %>%
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
             ylab = paste("Proportion of patients with",lbl(),sep = "<br>")
             )
   })
 })
}
