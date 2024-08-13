# Use 'import' from the 'modules' package.
# These listed imports are made available inside the module scope.
modules::import("arrow")
options(arrow.pull_as_vector = TRUE)
modules::import("dplyr")
modules::import("shiny")
modules::import("shinydashboard")

# App title ----
app_title <- "Perinatal Health Indicators"

# App version ----
app_version <- "0.0.0"

# urls and names ----
rcp_website <- "http://rcp.nshealth.ca/"
rcp_name <- "Reproductive Care Program of Nova Scotia"
dev_website  <- "https://www.linkedin.com/in/estevam-caixeta/"
dev_name <- "Estevam Teixeira"
rcp_contact <- "https://rcp.nshealth.ca/contact"
iwk_website <- "https://www.iwk.nshealth.ca/"
iwk_name <- "IWK Health Centre"

# Importing shapefiles ----

## Census district (CD) ----

cd_shp <-  sf::read_sf("./data/NSC_cd.shp") %>%
 select(GeoUID, name, geometry)
# sf::st_make_valid() %>%
# rmapshaper::ms_simplify()
# sf::st_make_valid()
# geoarrow_collect_sf()

## Community Clusters (CL) ----

cl_shp <- sf::read_sf("./data/NSC_cl.shp") %>%
 rename(GeoUID = clusterid)

## Community Health Networks (CHN) ----

chn_shp <- sf::read_sf("./data/NSC_chn.shp") %>%
 rename(GeoUID = network_id)

## Health Authority Zones (HR) ----

hr_shp <- sf::read_sf("./data/NSC_hr.shp") %>%
 select(ZoneID, Name, geometry) %>%
 rename(GeoUID = ZoneID)

## Health Authority Zones (HR) ----

urb_shp <- sf::read_sf("./data/NSC_urban.shp") %>%
 rename(GeoUID = FID) %>%
 mutate(GeoUID = 1)
# sf::st_make_valid() %>%
# rmapshaper::ms_simplify() %>%
# sf::st_make_valid()

# Import dashboard data ----

cyearly_stats <- arrow::read_parquet("./data/cyearly_stats.parquet", as_data_frame = FALSE)
fyearly_stats <- arrow::read_parquet("./data/fyearly_stats.parquet", as_data_frame = FALSE)

# Data time range ----
app_time_range <- paste(min(cyearly_stats %>% select(BrthYear) %>% collect(), na.rm = TRUE),
                        "-",
                        max(cyearly_stats %>% select(BrthYear) %>% collect(), na.rm = TRUE))

# Labels for SelectInput() ----
## Indicator ----

metrics_lbl <- list(
 ### Hypertension
 stats::setNames(
  cyearly_stats %>% select(ends_with("rate")) %>% collect() %>% names(),
  c("Pre-existing Hypertension","Gestational Hypertension", "Any Hypertension (Pre-existing/Gestational)")
 )
)

# List name ----
## The outermost names will be used as the group labels

names(metrics_lbl) <- c("Hypertension during pregnancy")

## Geographies for maps ----

geo_lbl <- c(
 "cd", "cl", "chn", "hr", "urb"
)

# The outermost names will be used as label in the dash

names(geo_lbl) <- c(
 "Census Divisions", "Community Clusters", "Community Health Networks", "Health Authority Zones", "Urban x Rural"
)

