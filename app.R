library(bslib)
library(dplyr)
library(modules)
library(shiny)
library(webshot)
library(markdown)

tableTab <- use("modules/mod-table.R")
trendTab <- use("modules/mod-trend.R")
mapTab <- use("modules/mod-map.R")
consts <- modules::use("modules/constants.R")

# To download the map
Sys.setenv("OPENSSL_CONF"="/dev/null")
webshot::install_phantomjs()

ui <- page_navbar(
 title = consts$app_title,
 id = "navbar",
 collapsible = TRUE,
 fillable = TRUE,
 fillable_mobile = TRUE,
 theme = bslib::bs_theme(
  version = 5,
  bootswatch = "minty",
  bg = "#FFFFFF",
  fg = "#307972",
  primary = "#44AD99",
  secondary = "#AAAAAA",
  base_font = font_google("Montserrat", local = TRUE)
 ) %>%
  bs_add_rules(".optgroup-header {font-size: 1.125rem !important; color: #AAAAAA !important;}"
   ),
 # nav_panel("Home",
 #           tableTab$tableUI("table"),
 #           icon = bsicons::bs_icon("house-fill")
 # ),
 nav_panel("Trend",
           trendTab$trendUI("trend"),
           icon = bsicons::bs_icon("graph-up-arrow")
 )
 # nav_panel("Map Tool",
 #           mapTab$mapUI("map"),
 #           icon = bsicons::bs_icon("geo-fill")
 # )
)

server <- function(input, output, session) {

 # Home tab server ----
 # session$userData$tableTab <- tableTab$tableServer(
 #  id = "table",
 #  df1 = consts$ctab_stats %>% collect(),
 #  df2 = consts$ftab_stats %>% collect()
 #  )

 # Lineplot tab server ----
 session$userData$trendTab <- trendTab$trendServer(
  id = "trend",
  df1 = consts$metrics_lbl,
  df2 = consts$cyearly_stats,
  df3 = consts$ttip,
  df4 = consts$fyearly_stats)

 # Map tab server ----
 # session$userData$mapTab <- mapTab$mapServer(
 #  id = "map",
 #  df1 = consts$metrics_lbl,
 #  df2 = consts$ttip,
 #  df3 = consts$geo_lbl,
 #  df4 = consts$ccd_stats,
 #  df5 = consts$ccl_stats,
 #  df6 = consts$cchn_stats,
 #  df7 = consts$chr_stats,
 #  df8 = consts$cd_shp,
 #  df9 = consts$cl_shp,
 #  df10 = consts$chn_shp,
 #  df11 = consts$hr_shp)

}


# profvis::profvis({runApp(
shinyApp(ui, server)
# )})

