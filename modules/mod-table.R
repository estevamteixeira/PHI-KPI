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

tableServer <- function(id, df1, df2){
 moduleServer(id, function(input, output, session){
  ## Setting id for session
  ns <- session$ns

  # Table ----

  ## group data

  grp <- df1 %>%
   group_by(Category) %>%
   mutate(size = n(),
          date = Sys.Date()) %>%
   select(Category, size, date) %>%
   distinct()

  output$tab_overview <- renderReactable({
   reactable(
    data = grp,
    columns = list(
     size = colDef(show = FALSE),
     date = colDef(show = FALSE),
     Category = colDef(
      name = "Category",
      align = "left"
     )
    ),
    details = function(index){
     # input:
     #   - index, the row index
     #   - name, the column name (optional)
     #
     # output:
     #   - content to render (e.g., an HTML tag or nested table), or NULL to hide details
     dta <- filter(df1, Category %in% grp$Category[index])
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
      BrthYear = colDef(show = FALSE),
      icon_colors = colDef(show = FALSE),
      icon10_colors = colDef(show = FALSE),
      icon10 = colDef(show = FALSE),
      index_rate = colDef(show = FALSE),
      Category = colDef(show = FALSE),
      delta = colDef(show = FALSE),
      name = colDef(
       name = "Indicator",
       html = TRUE,
       filterable = TRUE,
       align = "left",
       style = list(fontWeight = "bold")
      ),
      rate = colDef(
       name = paste0(
        "Most recent value","<br>","(",
        as.numeric(format(as.Date(Sys.Date()),"%Y"))-1,")"),
       html = TRUE,
       cell = data_bars(
        data = df1,
        fill_color = c("#44ad99"),
        fill_opacity = 0.5,
        text_position = "outside-end",
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
        "Status <br>(",
        as.numeric(format(as.Date(Sys.Date()),"%Y"))-2,
        "\u2B62 \ufe0f",
        as.numeric(format(as.Date(Sys.Date()),"%Y"))-1,")"
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
     },
    defaultExpanded = TRUE
    )
  })
 })
}
