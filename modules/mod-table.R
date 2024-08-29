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
modules::expose("utilities/plotNSline.R")

tableUI <- function(id){
 # The `ns <- NS()` structure creates a "namespacing" function, that will
 # prefix all ids with a string
 ns <- NS(id)

 layout_columns(
  col_widths = 12,
  card(
   full_screen = TRUE,
   card_body(
    reactableOutput(ns("tab_overview"))
   )
  )
 )
}

tableServer <- function(id, df1){
 moduleServer(id, function(input, output, session){
  ## Setting id for session
  ns <- session$ns

  # Table ----

  output$tab_overview <- renderReactable({
   reactable(
    data = df1,
    highlight = TRUE,
    compact = TRUE,
    columns = list(
     BrthYear = colDef(show = FALSE),
     icon_colors = colDef(show = FALSE),
     icon10_colors = colDef(show = FALSE),
     delta = colDef(show = FALSE),
     name = colDef(
      name = "Indicator",
      html = TRUE,
      filterable = TRUE,
      vAlign ="center",
      style = list(fontWeight = "bold")
     ),
     rate = colDef(
      name = paste0(
       "Most recent value","<br>","(",
       as.numeric(format(as.Date(Sys.Date()),"%Y"))-1,")"),
      html = TRUE,
      align = "center",
      vAlign = "center",
      cell = data_bars(
       data = df1,
       fill_color = c("#44ad99"),
       fill_opacity = 0.5,
       text_position = "above",
       round_edges = FALSE,
       number_fmt = scales::percent_format(accuracy = 1),
       box_shadow = FALSE,
       bold_text = TRUE
      )
     ),
     delta10 = colDef(
      name = paste0(
       "10-year % change","<br>","(",
       as.numeric(format(as.Date(Sys.Date()),"%Y"))-10,"-",
       as.numeric(format(as.Date(Sys.Date()),"%Y"))-1,")"),
      html = TRUE,
      align = "center",
      vAlign = "center",
      # format = colFormat(
      #  percent = TRUE,
      #  locales = "en-CA",
      #  digits = 1
      # ),
      cell = data_bars(
       data = df1,
       fill_color = c("#d9715f","#44ad99"),
       # fill_color_ref = "icon_colors",
       fill_opacity = 0.5,
       text_position = "above",
       round_edges = FALSE,
       number_fmt = scales::percent_format(accuracy = 1),
       box_shadow = FALSE,
       background = "transparent",
       bold_text = TRUE
      )
     ),
     index_rate = colDef(
      show = FALSE
      # name = "",
      # align = "center",
      # vAlign = "center",
      # cell = function(value){
       # input:
       #   - value, the cell value
       #   - index, the row index (optional)
       #   - name, the column name (optional)
      #  plot_line_index(data = df2 %>% filter(index_rate %in% value)) |>
      #   girafe(code = print)
      # }
     ),
     status = colDef(
      name = paste0("Status <br>(",
                    as.numeric(format(as.Date(Sys.Date()),"%Y"))-2,
                    "\u2B62 \ufe0f",
                    as.numeric(format(as.Date(Sys.Date()),"%Y"))-1,")"),
      align = "center",
      vAlign = "center",
      html = TRUE,
      cell = pill_buttons(
       data = df1,
       color_ref = "icon_colors",
       opacity = 0.2,
       text_color_ref = "icon_colors",
       bold_text = TRUE
      )
     )
    )
   )
  })
 })
}
