# Use 'import' from the 'modules' package ----
# These listed imports are made available inside the module scope
modules::import("arrow")
modules::import("bslib")
modules::import("dplyr")
modules::import("fontawesome")
modules::import("reactable")
modules::import("reactablefmtr")
modules::import("shiny")
modules::import("shinycustomloader")
modules::import("stringr")
modules::import("tippy")


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
    input_switch(ns("fdata"), "Fiscal Year", FALSE), # switch data btn
    # Add a div with custom CSS for horizontal alignment
    tags$div(
     style = "display: flex;gap: 2px;flex-wrap: wrap;align-items: center;justify-content: center;",
     # Btn to filter table data
     actionButton(ns("all"), "All", icon = shiny::icon("arrows-rotate"), class = "btn"),
     actionButton(ns("c1"), "Antenatal", icon = shiny::icon("person-pregnant")),
     actionButton(ns("c3"), "Intrapartum", icon = shiny::icon("user-doctor")),
     actionButton(ns("c5"), "Postpartum", icon = shiny::icon("baby-carriage")),
     actionButton(ns("c4"), "Newborn", icon = shiny::icon("baby")),
     actionButton(ns("c2"), "Infant Feeding", icon = shiny::icon("person-breastfeeding")),
     actionButton(ns("c6"), "Other", icon = shiny::icon("hospital"))
    ),
    # Reactable output
    shinycustomloader::withLoader(
     reactableOutput(ns("tab_overview")),
     type = "html",
     loader = "loader1"
     )
   )
  )
 )
}

tableServer <- function(id, df1, df2, df3){
 moduleServer(id, function(input, output, session){
  ## Setting id for session
  ns <- session$ns

  # Reactive value to track the selected filter
  selected_filter <- reactiveVal("all") # Default to "Show All"

  # Observe button clicks and update the selected filter
  observeEvent(input$all, { selected_filter("all") })
  observeEvent(input$c1, { selected_filter("c1") })
  observeEvent(input$c2, { selected_filter("c2") })
  observeEvent(input$c3, { selected_filter("c3") })
  observeEvent(input$c4, { selected_filter("c4") })
  observeEvent(input$c5, { selected_filter("c5") })
  observeEvent(input$c6, { selected_filter("c6") })

  # Table ----

  ## group data
  grp <- reactive({
   if(isTRUE(input$fdata)){
    df2 %>%
     group_by(category) %>%
     mutate(
      size = n(),
      #comp = paste(name, collapse = ","),
      icons = case_when(
       tolower(category) %in% "antenatal" ~ "person-pregnant",
       tolower(category) %in% "intrapartum" ~ "user-doctor",
       tolower(category) %in% "postpartum" ~ "baby-carriage",
       tolower(category) %in% "newborn" ~ "baby",
       tolower(category) %in% "other" ~ "hospital",
       TRUE ~ "person-breastfeeding"
      )) %>%
     select(icons, category, size) %>%
     distinct()
   } else {
    df1 %>%
     group_by(category) %>%
     mutate(
      size = n(),
      #comp = paste(name, collapse = ","),
      icons = case_when(
       tolower(category) %in% "antenatal" ~ "person-pregnant",
       tolower(category) %in% "intrapartum" ~ "user-doctor",
       tolower(category) %in% "postpartum" ~ "baby-carriage",
       tolower(category) %in% "newborn" ~ "baby",
       tolower(category) %in% "other" ~ "hospital",
       TRUE ~ "person-breastfeeding"
      )) %>%
     select(icons, category, size) %>%
     distinct()
   }
  })

  # Reactive data for filtering
  filtered_dta <- reactive({
   switch(selected_filter(),
          "all" = grp(),
          "c1" = grp() %>% filter(category %in% "Antenatal"),
          "c2" = grp() %>% filter(category %in% "Infant Feeding"),
          "c3" = grp() %>% filter(category %in% "Intrapartum"),
          "c4" = grp() %>% filter(category %in% "Newborn"),
          "c5" = grp() %>% filter(category %in% "Postpartum"),
          "c6" = grp() %>% filter(category %in% "Other"),
          grp()) # Default to all
  })


  # `tmp_dta` is a reactive expression whose results will depend on ----
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
       max(levels(selected_dta()[[1]]), na.rm = TRUE),
       ")"
       ),
      paste0(
       "(",
       min(levels(selected_dta()[[1]]), na.rm = TRUE),
       " \u2B62 \ufe0f",
       max(levels(selected_dta()[[1]]), na.rm = TRUE),
       ")"
       ),
      paste0(
       "(",
       levels(selected_dta()[[1]])[length(levels(selected_dta()[[1]]))-1],
       " \u2B62 \ufe0f",
       max(levels(selected_dta()[[1]]), na.rm = TRUE),
       ")"
       )
     )
    )
   } else{
    return(
     list(
      paste0(
       "(",
       max(selected_dta()[[1]], na.rm = TRUE),
       ")"
      ),
      paste0(
       "(",
       2014,
       " \u2B62 \ufe0f",
       max(selected_dta()[[1]], na.rm = TRUE),
       ")"
      ),
      paste0(
       "(",
       max(selected_dta()[[1]], na.rm = TRUE)-1,
       " \u2B62 \ufe0f",
       max(selected_dta()[[1]], na.rm = TRUE),
       ")"
      )
     )
    )
   }
  })

  output$tab_overview <- renderReactable({
   reactable(
    data = filtered_dta(),
    highlight = TRUE,
    wrap = TRUE,
    defaultExpanded = FALSE,
    defaultPageSize = nrow(filtered_dta()),
    defaultColDef = colDef(
     vAlign = "center",
     align = "center",
     headerVAlign = "center",
     sortable = FALSE,
     headerStyle = list(
      fontWeight = "bold"
     )
    ),
    columns = list(
     icons = colDef(
      name = "",
      maxWidth = 50,
      sortable = FALSE,
      html = TRUE,
      cell = icon_sets(
       data = filtered_dta(),
       icon_ref = "icons",
       icon_size = 24,
       icon_position = "over",
       colors = c("#44AD99"))
     ),
     category = colDef(
      name = "Category",
      align = "left",
      html = TRUE
     ),
     size = colDef(
      name = "Number of Indicators",
      cell = data_bars(
       data = filtered_dta(),
       text_position = "outside-base",
       number_fmt = scales::comma,
       fill_color = c("#44AD99"),
       fill_opacity = 0.5,
       round_edges = FALSE,
       box_shadow = FALSE,
       bold_text = TRUE
      ),
      sortable = TRUE
     )
    ),
    details = function(index){
     # input:
     #   - value, the cell value
     #   - index, the row index (optional)
     #   - name, the column name (optional)
     # output:
     #   - content to render (e.g., an HTML tag or nested table), or NULL to hide details
     # Preparing data to be used in the inner table
     dta <- filter(selected_dta(), category %in% filtered_dta()$category[index]) %>%
      mutate(
       icon = case_when(
        delta > 0 ~ "circle-chevron-up",
        delta < 0 ~ "circle-chevron-down",
        TRUE ~ "circle-minus"
       ),
       icon10 = case_when(
        delta10 > 0 ~ "circle-chevron-up", # Duotone up arrow
        delta10 < 0 ~ "circle-chevron-down",
        TRUE ~ "circle-minus"
       )
      ) %>%
      relocate(status, .before = "delta")
     # Inner table
     reactable(
      data = dta,
      outlined = FALSE,
      style = list(
       fontSize = "0.875rem"
      ),
      defaultPageSize = nrow(dta),
      defaultColDef = colDef(
       vAlign = "center",
       align = "center",
       headerVAlign = "center",
       sortable = FALSE
      ),
      columns = list(
       period = colDef(show = FALSE),
       cond = colDef(show = FALSE),
       index_rate = colDef(show = FALSE),
       icon_colors = colDef(show = FALSE),
       icon10_colors = colDef(show = FALSE),
       icon = colDef(show = FALSE),
       icon10 = colDef(show = FALSE),
       category = colDef(show = FALSE),
       delta = colDef(show = FALSE),
       group = colDef(show = FALSE),
       name = colDef(
        name = "Indicator",
        filterable = TRUE,
        align = "left",
        cell = function(value, index){
         # input:
         #   - value, the cell value
         #   - index, the row index (optional)
         #   - name, the column name (optional)
         #
         # output:
         #   - content to render (e.g. an HTML tag or widget)
         cl <- tibble(
          metric = names(df3),
          html = unlist(df3)
         ) %>%
          filter(metric == dta$index_rate[index]) %>%
          mutate(
           definition = str_extract(html, "(?<=<b>Definition:</b> ).*?(?=</li>)")
          )

         tippy::tippy(value, cl$definition)
        },
        details = function(index) {
         # input:
         #   - value, the cell value
         #   - index, the row index (optional)
         #   - name, the column name (optional)
         #
         # output:
         #   - content to render (e.g. an HTML tag or widget)
         # Create details row content
         dt <- tibble(
          metric = names(df3),
          html = unlist(df3)
         ) %>%
          filter(metric == dta$index_rate[index]) %>%
          mutate(
           definition = str_extract(html, "(?<=<b>Definition:</b> ).*?(?=</li>)"),
           numerator = str_extract(html, "(?<=<b>Numerator:</b> ).*?(?=</li>)"),
           denominator = str_extract(html, "(?<=<b>Denominator:</b> ).*?(?=</li>)")
          )

         pkg_field <- function(name, ...) {
          if (any(is.na(...))) NULL
          else tagList(div(style = "margin: 1.25rem 0 0.25rem;font-size: 0.875rem; color: rgba(68, 173, 153, 0.6);",
                           name), ...)
         }

         as.character(tagList(
          div(style = "padding: 24px;box-shadow: inset 0 1px 3px #D4E2E0;background: hsl(169, 20%, 99%);",
              pkg_field("Definition", HTML(dt$definition)),
              pkg_field("Numerator", HTML(dt$numerator)),
              pkg_field("Denominator", HTML(dt$denominator))
          )))
        },
        html = TRUE
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
        bold_text = TRUE,
        max_value = 1
       )
      ),
      delta10 = colDef(
       name = paste0(
        "10-year % change <br>",
        lab()[[2]]
        ),
       html = TRUE,
       cell = function(value, index) {
        icon <- dta$icon10[index]
        icon_color <- dta$icon10_colors[index]
        value_text <- scales::percent_format(accuracy = 0.1)(value)

        htmltools::div(
         style = "display: flex; align-items: center; justify-content: center;",
         htmltools::span(
          shiny::icon(icon),
          style = paste0("color: ", icon_color, "; font-size: 20px; margin-right: 5px;")
         ),
         htmltools::span(
          value_text,
          style = paste0("color:#000000; font-weight: bold;")
         )
        )
       }
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
        box_shadow = TRUE,
        bold_text = TRUE
        )
       )
      ))
     }
    )}
   )}
  )
}
