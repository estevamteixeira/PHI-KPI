# Use 'import' from the 'modules' package ----
# These listed imports are made available inside the module scope
modules::import("arrow")
modules::import("bslib")
modules::import("dplyr")
modules::import("ggiraph")
modules::import("reactable")
modules::import("reactablefmtr")
modules::import("shiny")

# Define which objects from the module you make available to a user ----
# All other objects are kept private, local, to the module.
modules::export("tableUI")
modules::export("tableServer")

# It is a variation of 'use' where instead of returning a module
# as return value, the elements are copied to the calling environment.
# modules::expose("utilities/plotNSline.R")

tableUI <- function(id){
 # The `ns <- NS()` structure creates a "namespacing" function, that will
 # prefix all ids with a string
 ns <- NS(id)

 layout_columns(
  col_widths = 12,
  card(
   full_screen = TRUE,
   card_body(
    input_switch(ns("fdata"), "Fiscal Year", FALSE),
    reactableOutput(ns("tab_overview"))
   )
  )
 )
}

tableServer <- function(id, df1, df2){
 moduleServer(id, function(input, output, session){
  ## Setting id for session
  ns <- session$ns

  # Table ----

  ## group data
  grp <- reactive({
   if(isTRUE(input$fdata)){
    df2 %>%
     group_by(category) %>%
     mutate(size = n()) %>%
     select(category, size) %>%
     distinct()
   } else {
    df1 %>%
     group_by(category) %>%
     mutate(size = n()) %>%
     select(category, size) %>%
     distinct()
   }
  })


  # `selected_dta` is a reactive expression whose results will depend on ----
  # the fdata input selection: Calendar or Fiscal year.
  selected_dta <- reactive({
   if(isTRUE(input$fdata)){
   return(df2 %>%
           rename("period" = "FiscalYear"))
    } else {
     return(df1 %>%
             rename("period" = "BrthYear"))
   }
  })

  lab <- reactive({
   if(isTRUE(input$fdata)){
    return(
     list(
      paste0(
       "(",
       max(levels(selected_dta()[[1]])),
       ")"
       ),
      paste0(
       "(",
       min(levels(selected_dta()[[1]])),
       " \u2B62 \ufe0f",
       max(levels(selected_dta()[[1]])),
       ")"
       ),
      paste0(
       "(",
       levels(selected_dta()[[1]])[length(levels(selected_dta()[[1]]))-1],
       " \u2B62 \ufe0f",
       max(levels(selected_dta()[[1]])),
       ")"
       )
     )
    )
   } else{
    return(
     list(
      paste0(
       "(",
       max(selected_dta()[[1]]),
       ")"
      ),
      paste0(
       "(",
       max(selected_dta()[[1]])-9,
       " \u2B62 \ufe0f",
       max(selected_dta()[[1]]),
       ")"
      ),
      paste0(
       "(",
       max(selected_dta()[[1]])-1,
       " \u2B62 \ufe0f",
       max(selected_dta()[[1]]),
       ")"
      )
     )
    )
   }
  })

  output$tab_overview <- renderReactable({
   reactable(
    data = grp(),
    defaultExpanded = TRUE,
    columns = list(
     size = colDef(show = FALSE),
     category = colDef(
      name = "",
      align = "left",
      style = list(fontWeight = "bold")
     )
    ),
    details = function(index){
     # input:
     #   - index, the row index
     #   - name, the column name (optional)
     #
     # output:
     #   - content to render (e.g., an HTML tag or nested table), or NULL to hide details
     dta <- filter(selected_dta(), category %in% grp()$category[index])
     reactable(
      data = dta,
     highlight = TRUE,
     compact = TRUE,
     defaultColDef = colDef(
      align = "center",
      vAlign = "center",
      headerVAlign = "center"
     ),
     columns = list(
      period = colDef(show = FALSE),
      icon_colors = colDef(show = FALSE),
      icon10_colors = colDef(show = FALSE),
      index_rate = colDef(show = FALSE),
      category = colDef(show = FALSE),
      delta = colDef(show = FALSE),
      cond = colDef(show = FALSE),
      name = colDef(
       name = "Indicator",
       html = TRUE,
       filterable = TRUE,
       align = "left",
       style = list(fontWeight = "bold")
      ),
      rate = colDef(
       name = paste0(
        "Most recent value <br>",
        lab()[[1]]
        ),
       html = TRUE,
       cell = data_bars(
        data = selected_dta(),
        fill_color = c("#44AD99"),
        fill_opacity = 0.5,
        text_position = "outside-base",
        round_edges = FALSE,
        number_fmt = scales::percent_format(accuracy = 0.1),
        box_shadow = FALSE,
        bold_text = TRUE
       )
      ),
      delta10 = colDef(
       name = paste0(
        "10-year % change <br>",
        lab()[[2]]
        ),
       html = TRUE,
       cell = data_bars(
        data = dta,
        # fill_color = c("#d9715f","#44ad99"),
        fill_color_ref = "icon10_colors",
        fill_opacity = 0.5,
        text_position = "above",
        round_edges = FALSE,
        number_fmt = scales::percent_format(accuracy = 1),
        box_shadow = FALSE,
        background = "transparent",
        bold_text = TRUE
       )
      ),
      status = colDef(
       name = paste0(
        "Status <br>",
        lab()[[3]]
       ),
       html = TRUE,
       cell = pill_buttons(
        data = dta,
        color_ref = "icon_colors",
        opacity = 0.2,
        text_color_ref = "icon_colors",
        bold_text = TRUE
       ))
      )
     )
     }
    )
  })
 })
}
