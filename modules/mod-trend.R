# Use 'import' from the 'modules' package ----
# These listed imports are made available inside the module scope
modules::import("arrow")
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
     label = "Initial year",
     choices = NULL,
     selected = NULL
    ),
    selectInput(
     inputId = ns("tn"),
     label = "Final year",
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
     uiOutput(ns("card_title"))
    ),
    card_body(
     plotlyOutput(ns("line"))
    )
   )
  )
 )
}

trendServer <- function(id, df1, df2){
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
  ## Update the options for the initial year selectInput
  ## The values should reflect the data availability
  observeEvent(input$metric,{

   updateSelectInput(
    session,
    inputId = "t0",
    choices = sort(
     unique(
      df2 %>%
       select(BrthYear, !!sym(input$metric)) %>%
       collect() %>%
       pull(BrthYear)
     )),
    selected = c(
     min(
      df2 %>%
       select(BrthYear, !!sym(input$metric)) %>%
       collect() %>%
       pull(BrthYear), na.rm = TRUE
     )
    )
   )
  })

  ## Make the final year option greater than or equal the
  ## initial year option
  observeEvent(c(input$metric, input$t0),{

   updateSelectInput(
    session,
    inputId = "tn",
    choices = c(
     sort(
      unique(
       df2 %>%
        select(BrthYear, !!sym(input$metric)) %>%
        collect() %>%
        pull(BrthYear)
      )))[
       c(
        sort(
         unique(
          df2 %>%
           select(BrthYear, !!sym(input$metric)) %>%
           collect() %>%
           pull(BrthYear)
         )
        )
       ) >= input$t0],
    selected = c(
     max(
      df2 %>%
       select(BrthYear, !!sym(input$metric)) %>%
       collect() %>%
       pull(BrthYear), na.rm = TRUE
     )
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

    # `selected_dta` is a reactive expression whose results will depend on ----
  # the t0, tn, metric
  selected_dta <- reactive({
   return(
    df2 %>%
     select(BrthYear, !!sym(input$metric)) %>%
     filter(BrthYear >= as.numeric(input$t0),
            BrthYear <= as.numeric(input$tn)) %>%
     distinct() %>%
     collect()
   )
  })


  # Line plot output ----
  output$line <- renderPlotly({
   req(selected_dta())
   validate(need(nrow(selected_dta()) > 0,
                 "Sorry, there is no data available for the selected options.
             \nPlease, choose different years and/or indicators."))

   plot_line(selected_dta(),
             input$metric,
             ylab = paste("Proportion of patients")
             )
   })
 })
}
