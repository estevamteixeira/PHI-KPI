# Use 'import' from the 'modules' package ----
# These listed imports are made available inside the module scope
modules::import("arrow")
modules::import("bsicons")
modules::import("bslib")
modules::import("dplyr")
modules::import("plotly")
modules::import("shiny")
modules::import("shinycustomloader")
modules::import("stringr")

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
   layout_column_wrap(
    width = "200px",
    layout_column_wrap(
     width = 1/2,
     input_switch(ns("smth"), "Smoothing Line", FALSE),
     # Conditional panel for 'Compare to' selectInput
     conditionalPanel(
      condition = "input['smth'] == true",
      ns = ns, # Add the `ns` argument here to namespace the inputs
      input_switch(ns("ci"), "Add Confidence Interval", FALSE)
     )
    ),
    selectInput(
     inputId = ns("focus"),
     label = "Choose Focus",
     choices = NULL,
     selected = NULL
     ),
  # Conditional panel for 'Compare to' selectInput
  conditionalPanel(
   condition = "input['focus'] != '1' && input['focus'] != '2'",
   ns = ns, # Add the `ns` argument here to namespace the inputs
   selectInput(
    inputId = ns("compareto"),
    label = "Compare to",
    choices = NULL,
    multiple = FALSE,
    selected = NULL
    )
   )
  ),
  card(
   full_screen = TRUE,
   card_header(
    div(
     style = "display: inline-flex; align-items: center; flex-wrap: wrap; gap: 0.25rem; word-wrap: break-word;",
     span(
      style = "display: inline-flex; align-items: center; gap: 0.25rem;",
      uiOutput(ns("card_title")),  # Title output
      tooltip(
       bsicons::bs_icon("info-circle"),  # Info icon
       uiOutput(ns("i_btn")),  # Tooltip content
       placement = "right"
      )
     )
    ),
    class = "d-flex align-items-center"
   ),
   card_body(
    shinycustomloader::withLoader(
     plotlyOutput(ns("line")),
     type = "html",
     loader = "loader1"
    )
   )
  )
  ))
}

trendServer <- function(id, df1, df2, df3, df4, df5, df6, df7, df8){
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

   # Define a mapping of data frames for Calendar and Fiscal years
   data_map <- list(
    "1" = list(fiscal = df3, calendar = df2),
    "2" = list(fiscal = df5, calendar = df4),
    "default" = list(fiscal = df7, calendar = df6)
   )

selected_df <- if (input$focus %in% c("1", "2")) {
    if (isTRUE(input$fdata)){
     data_map[[input$focus]]$fiscal
    } else {
     data_map[[input$focus]]$calendar
     }
   } else {
    if (isTRUE(input$fdata)){
     data_map[["default"]]$fiscal
    } else {
     data_map[["default"]]$calendar
      }
   }
   # Select the correct data frame based on input$focus and input$fdata


   # Rename the `period` column
   return(
    selected_df %>%
     rename("period" = if (isTRUE(input$fdata)) "FiscalYear" else "BrthYear")
    )

  })

  ## Update the options for the initial year selectInput (t0)
  observeEvent(c(input$fdata, input$metric), {
   # Get the current selection of t0
   current_t0 <- input$t0

   # Get the available periods
   available_periods <- selected_dta() %>%
    select(period, !!sym(input$metric)) %>%
    collect() %>%
    filter(stats::complete.cases(.)) %>%
    pull(period) %>%
    as.character() %>%
    sort()

   # Determine the selected value (persist current selection if valid)
   selected_t0 <- if (!is.null(current_t0) && current_t0 %in% available_periods) {
    current_t0
   } else {
    min(available_periods) # Fallback to the earliest period
   }

   # Update the selectInput for t0
   updateSelectInput(
    session,
    inputId = "t0",
    choices = available_periods,
    selected = selected_t0
   )
  })


  ## Update the options for the final year selectInput (tn)
  observeEvent(c(input$fdata, input$metric, input$t0), {
   # Get the current selection of tn
   current_tn <- input$tn

   # Get the available periods
   available_periods <- selected_dta() %>%
    select(period, !!sym(input$metric)) %>%
    collect() %>%
    filter(stats::complete.cases(.)) %>%
    pull(period) %>%
    as.character() %>%
    sort()

   # Filter the periods to include only those >= t0
   valid_periods <- available_periods[available_periods >= input$t0]

   # Determine the selected value (persist current selection if valid)
   selected_tn <- if (!is.null(current_tn) && current_tn %in% valid_periods) {
    current_tn
   } else {
    max(valid_periods) # Fallback to the latest valid period
   }

   # Update the selectInput for tn
   updateSelectInput(
    session,
    inputId = "tn",
    choices = valid_periods,
    selected = selected_tn
   )
  })

  ## Update the options for the `Choose focus` selectInput
  ## The data should reflect data availability
  observeEvent(c(input$metric, input$t0, input$tn), {
   # Get the current value of 'focus'
   current_focus <- input$focus

   # Choices
   ch <- c("1", "2", "11", "14", "18", "30", "43", "56", "67", "86", "87")

   # Name choices
   lab <- c(
    "All Nova Scotia",
    "Health Management Zone",
    "Aberdeen Hospital",
    "South Shore Regional",
    "Colchester Regional",
    "Cumberland Regional",
    "St. Martha's Regional",
    "Western Kings Memorial",
    "Valley Regional",
    "IWK Health",
    "Cape Breton Regional"
   )

   names(ch) <- lab

   ch <- list(
    "Region" = ch[1:2],
    "Hospital" = ch[3:11]
   )

   # Check if the current focus is still a valid choice
   new_selected <- if (current_focus %in% unlist(ch)) {
    current_focus
   } else {
    "1" # Default fallback
   }

   updateSelectInput(
    session,
    inputId = "focus",
    choices = ch,
    selected = new_selected
   )
  })


  ## Update the options for the `Choose focus` selectInput
  ## The data should reflect data availability
  observeEvent(input$focus, {
   # Get the current value of 'compareto'
   current_compareto <- input$compareto

   # Name choices
   lab <- c("All Nova Scotia" = "1",
            "Other Regional Hospitals" = "99",
            "Aberdeen Hospital" = "11",
            "South Shore Regional" = "14",
            "Colchester Regional" = "18",
            "Cumberland Regional" = "30",
            "St. Martha's Regional" = "43",
            "Western Kings Memorial" = "56",
            "Valley Regional" = "67",
            "IWK Health" = "86",
            "Cape Breton Regional" = "87")

   # Dynamically set choices by removing `input$focus` value
   ch <- setdiff(lab,input$focus)

   # Subset the `lab` table based on `ch`
   ch <- lab[lab %in% ch]

   # Labels for SelectInput() ----
   ch <- list(
    "Region" = ch[ch %in% c("1","99")],
    "Hospital" = ch[!ch %in% c("1","99")]
   )

   # Check if the current compareto is still a valid choice
   new_selected <- if (current_compareto %in% unlist(ch)) {
    current_compareto
   } else {
    "1" # Default fallback
   }

   updateSelectInput(
    session,
    inputId = "compareto",
    choices = ch,
    selected = new_selected
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

   vals$ibtn <- unlist(unname(df8))[names(unlist(df8)) %in% vals$metric]

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

  # Select field depending on the selected focus

  opt <- reactive({
   if (input$focus == "1") return(NULL)
   if (input$focus == "2") return("HRuid")
   if (!input$focus %in% c("1", "2")) return("DLHosp")
  })

  # `tmp_dta` is a reactive expression whose results will depend on ----
  # t0, tn, metric, focus
  tmp_dta <- reactive({
   req(vals$label, input$t0, input$tn, input$focus)

   # Select relevant columns dynamically
   selected_dta() %>%
    select(
     all_of(opt()),
     period,
     matches(paste0("^", sub("_.*", "", input$metric), "_(rate|delta|deltap|count|total)$"))
     ) %>%
    filter(
     as.character(period) >= as.character(input$t0),
     as.character(period) <= as.character(input$tn)
     ) %>%
    # distinct() %>%
    mutate(label = as.character(vals$label)) %>%
    as_arrow_table()
   })

  fields <- reactive({
   # List of variables to be dynamically selected
   c(
     tolower(names(tmp_dta()))[endsWith(tolower(names(tmp_dta())), "count")],
     tolower(names(tmp_dta()))[endsWith(tolower(names(tmp_dta())), "total")],
     tolower(names(tmp_dta()))[endsWith(tolower(names(tmp_dta())), "rate")],
     tolower(names(tmp_dta()))[endsWith(tolower(names(tmp_dta())), "delta")],
     tolower(names(tmp_dta()))[endsWith(tolower(names(tmp_dta())), "deltap")],
     tolower(names(tmp_dta()))[endsWith(tolower(names(tmp_dta())), "label")]
    )
   })

  # `final_dta` is a reactive expression whose results will depend on ----
  # t0, tn, metric, focus
  final_dta <- reactive({
   req(tmp_dta())

   if(!input$focus %in% c("1","2")){
    # Other regionals
    if(input$compareto %in% "99"){
     # Compute stats for "Other Regional Hospitals" (sum of all except input$focus)
     ohosp <- setdiff(unique(pull(tmp_dta(), DLHosp)), input$focus)

     tmp <- tmp_dta() %>%
      select(DLHosp, period, all_of(fields())) %>%
      filter(DLHosp %in% ohosp) %>%
      mutate(
       DLHosp = 99,
       !!sym(fields()[endsWith(fields(),"delta")]) := NA,
       !!sym(fields()[endsWith(fields(),"deltap")]) := NA
      ) %>%
      group_by(period) %>%
      mutate(
       !!sym(fields()[endsWith(fields(),"total")]) := sum(!!sym(fields()[endsWith(fields(),"total")]), na.rm = TRUE),
       !!sym(fields()[endsWith(fields(),"count")]) := sum(!!sym(fields()[endsWith(fields(),"count")]), na.rm = TRUE),
       !!sym(fields()[endsWith(fields(),"rate")]) := !!sym(fields()[endsWith(fields(),"count")])/!!sym(fields()[endsWith(fields(),"total")])
      ) %>%
      ungroup() %>%  # Ungroup before applying window functions
      distinct() %>%
      collect() %>%  # Collect the data from Arrow into R
      group_by(!!sym(opt())) %>%
      mutate(
       across(ends_with(c("rate")), ~ coalesce(., 0)),
       !!sym(fields()[endsWith(fields(),"delta")]) := (!!sym(fields()[endsWith(fields(),"rate")]) - lag(!!sym(fields()[endsWith(fields(),"rate")]))) / lag(!!sym(fields()[endsWith(fields(),"rate")])),
       !!sym(fields()[endsWith(fields(),"delta")]) := ifelse(period == 2014, NA, !!sym(fields()[endsWith(fields(),"delta")])),
       !!sym(fields()[endsWith(fields(),"deltap")]) := (!!sym(fields()[endsWith(fields(),"rate")]) - lag(!!sym(fields()[endsWith(fields(),"rate")]), 9)) / lag(!!sym(fields()[endsWith(fields(),"rate")]), 9)
      ) %>%
      arrange(DLHosp, period)

     ndta <- tmp_dta() %>%
      filter(DLHosp %in% input$focus) %>%
      collect() %>%
      bind_rows(tmp) %>%
      arrange(DLHosp, period) %>%
      mutate(# Handle `Inf` replacement in delta columns with corresponding rate column values
       across(ends_with(c("rate")), ~ coalesce(., 0)),
       !!sym(fields()[endsWith(fields(),"delta")]) := ifelse(period == 2014, NA, !!sym(fields()[endsWith(fields(),"delta")])),
       across(
        all_of(fields()[endsWith(fields(), "delta")]),
        ~ ifelse(is.na(.), get(fields()[endsWith(fields(), "rate")]), .)
       ),
       across(
        all_of(fields()[endsWith(fields(), "delta")]),
        ~ ifelse(lag(.) == 0,
                 get(fields()[endsWith(fields(), "rate")]) - lag(get(fields()[endsWith(fields(), "rate")])),
                 (get(fields()[endsWith(fields(), "rate")]) - lag(get(fields()[endsWith(fields(), "rate")])))/lag(get(fields()[endsWith(fields(), "rate")])))
       )) %>%
      distinct()

     return(ndta)

    } else if(input$compareto %in% "1"){
     tmp <- tmp_dta() %>%
      select(DLHosp, period, all_of(fields())) %>%
      # filter(DLHosp %in% ohosp) %>%
      mutate(
       DLHosp = 1,
       !!sym(fields()[endsWith(fields(),"delta")]) := NA,
       !!sym(fields()[endsWith(fields(),"deltap")]) := NA
      ) %>%
      group_by(period) %>%
      mutate(
       !!sym(fields()[endsWith(fields(),"total")]) := sum(!!sym(fields()[endsWith(fields(),"total")]), na.rm = TRUE),
       !!sym(fields()[endsWith(fields(),"count")]) := sum(!!sym(fields()[endsWith(fields(),"count")]), na.rm = TRUE),
       !!sym(fields()[endsWith(fields(),"rate")]) := !!sym(fields()[endsWith(fields(),"count")])/!!sym(fields()[endsWith(fields(),"total")])
      ) %>%
      ungroup() %>%  # Ungroup before applying window functions
      distinct() %>%
      collect() %>%  # Collect the data from Arrow into R
      group_by(!!sym(opt())) %>%
      mutate(
       across(ends_with(c("rate")), ~ coalesce(., 0)),
       !!sym(fields()[endsWith(fields(),"delta")]) := (!!sym(fields()[endsWith(fields(),"rate")]) - lag(!!sym(fields()[endsWith(fields(),"rate")]))) / lag(!!sym(fields()[endsWith(fields(),"rate")])),
       !!sym(fields()[endsWith(fields(),"delta")]) := ifelse(period == 2014, NA, !!sym(fields()[endsWith(fields(),"delta")])),
       !!sym(fields()[endsWith(fields(),"deltap")]) := (!!sym(fields()[endsWith(fields(),"rate")]) - lag(!!sym(fields()[endsWith(fields(),"rate")]), 9)) / lag(!!sym(fields()[endsWith(fields(),"rate")]), 9)
      ) %>%
      arrange(DLHosp, period)

     ndta <- tmp_dta() %>%
      filter(DLHosp %in% input$focus) %>%
      collect() %>%
      bind_rows(tmp) %>%
      arrange(DLHosp, period) %>%
      mutate(# Handle `Inf` replacement in delta columns with corresponding rate column values
       across(ends_with(c("rate")), ~ coalesce(., 0)),
       !!sym(fields()[endsWith(fields(),"delta")]) := ifelse(period == 2014, NA, !!sym(fields()[endsWith(fields(),"delta")])),
       across(
        all_of(fields()[endsWith(fields(), "delta")]),
        ~ ifelse(is.na(.), get(fields()[endsWith(fields(), "rate")]), .)
       ),
       across(
        all_of(fields()[endsWith(fields(), "delta")]),
        ~ ifelse(lag(.) == 0,
                 get(fields()[endsWith(fields(), "rate")]) - lag(get(fields()[endsWith(fields(), "rate")])),
                 (get(fields()[endsWith(fields(), "rate")]) - lag(get(fields()[endsWith(fields(), "rate")])))/lag(get(fields()[endsWith(fields(), "rate")])))
       )) %>%
      distinct()

     return(ndta)
    } else{
     # Individual hospitals
     ndta <- tmp_dta() %>%
      select(DLHosp, period, all_of(fields())) %>%
      filter(DLHosp %in% c(input$focus, input$compareto)) %>%
      collect() %>%
      mutate(# Handle `Inf` replacement in delta columns with corresponding rate column values
       across(ends_with(c("rate")), ~ coalesce(., 0)),
       !!sym(fields()[endsWith(fields(),"delta")]) := ifelse(period == 2014, NA, !!sym(fields()[endsWith(fields(),"delta")])),
       across(
        all_of(fields()[endsWith(fields(), "delta")]),
        ~ ifelse(is.na(.), get(fields()[endsWith(fields(), "rate")]), .)
       ),
       across(
        all_of(fields()[endsWith(fields(), "delta")]),
        ~ ifelse(lag(.) == 0,
                 get(fields()[endsWith(fields(), "rate")]) - lag(get(fields()[endsWith(fields(), "rate")])),
                 (get(fields()[endsWith(fields(), "rate")]) - lag(get(fields()[endsWith(fields(), "rate")])))/lag(get(fields()[endsWith(fields(), "rate")])))
        )) %>%
      arrange(DLHosp, period)

     return(ndta)
    }
   } else if(input$focus %in% "2"){
    ndta <- tmp_dta() %>%
     mutate(
      !!sym(fields()[endsWith(fields(),"delta")]) := NA,
      !!sym(fields()[endsWith(fields(),"deltap")]) := NA
     ) %>%
     group_by(HRuid, period) %>%
     mutate(
      !!sym(fields()[endsWith(fields(),"total")]) := sum(!!sym(fields()[endsWith(fields(),"total")]), na.rm = TRUE),
      !!sym(fields()[endsWith(fields(),"count")]) := sum(!!sym(fields()[endsWith(fields(),"count")]), na.rm = TRUE),
      !!sym(fields()[endsWith(fields(),"rate")]) := !!sym(fields()[endsWith(fields(),"count")])/!!sym(fields()[endsWith(fields(),"total")])
     ) %>%
     ungroup() %>%  # Ungroup before applying window functions
     distinct() %>%
     collect() %>%  # Collect the data from Arrow into R
     group_by(!!sym(opt())) %>%
     mutate(
      across(ends_with(c("rate")), ~ coalesce(., 0)),
      !!sym(fields()[endsWith(fields(),"delta")]) := (!!sym(fields()[endsWith(fields(),"rate")]) - lag(!!sym(fields()[endsWith(fields(),"rate")]))) / lag(!!sym(fields()[endsWith(fields(),"rate")])),
      !!sym(fields()[endsWith(fields(),"delta")]) := ifelse(period == 2014, NA, !!sym(fields()[endsWith(fields(),"delta")])),
      !!sym(fields()[endsWith(fields(),"deltap")]) := (!!sym(fields()[endsWith(fields(),"rate")]) - lag(!!sym(fields()[endsWith(fields(),"rate")]), 9)) / lag(!!sym(fields()[endsWith(fields(),"rate")]), 9)
     ) %>%
     arrange(HRuid, period) %>%
     mutate(across(
      all_of(fields()[endsWith(fields(), "delta")]),
      ~ ifelse(is.na(.), get(fields()[endsWith(fields(), "rate")]), .)
     ),
     across(
      all_of(fields()[endsWith(fields(), "delta")]),
      ~ ifelse(lag(.) == 0,
               get(fields()[endsWith(fields(), "rate")]) - lag(get(fields()[endsWith(fields(), "rate")])),
               (get(fields()[endsWith(fields(), "rate")]) - lag(get(fields()[endsWith(fields(), "rate")])))/lag(get(fields()[endsWith(fields(), "rate")])))
     ))


    return(ndta)
   } else{
    return(tmp_dta())
    }
   })

  # Line plot output ----
  output$line <- renderPlotly({
   req(final_dta())
   validate(need(nrow(final_dta() %>% collect()) > 0,
                 "Sorry, there is no data available for the selected options.
             \nPlease, choose different years and/or indicators."))

   plot_line(final_dta() %>%
              collect(),
             input$metric,
             alt = opt(),
             focus = input$focus,
             ylab = paste("Proportion of",
                          stringr::str_wrap(as.character(vals$label), width = 35),
                          sep = "<br>") %>% stringr::str_replace_all("\n", "<br>"),
             xlab = vals$xlbl,
             add_smooth = input$smth,
             add_ci = input$ci
             )
   })
 })
}
