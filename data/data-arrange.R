# Script to prepare data for the
# Perinatal Health Indicators and Key Performance Indicators ----
## Time frame: 10 years if possible
## Geography: provincial, compared with national data if available

# Loading required packages ----

library(haven) # read SAS data
library(dplyr) # data manipulation
library(arrow) # integration to Apache arrow: in-memory data structure
library(sf) # Support for simple features, a standardized way to encode spatial vector data

# Vector with variables of interest ----

myvec <- c(
 "BIRTHID",# Birth ID
 "MOTHERID",# Mother's Person ID
 "CONTCTID",# Contact ID
 "DLMUNCOD",# Municipality Code
 "DLPSTCOD",# Postal Code
 "BTBrthDT",# Birth Date and Time UTC
 "BrthYear",# Birth Year
 "BTDethDT",# Death Date and Time UTC
 "BTSEX",# Phenotypic Sex
 "BTOUTCOM",# Outcome at Discharge from Birth Admission
 "PreExisting_Hypertension",# Pre-existing/Pre-pregnancy Hypertension
 "Gestational_Hypertension",# Gestational Hypertension
 "Any_Hypertension",# Any Hypertension
 "Proteinuria",# Any protein in urine
 "Pre_Eclampsia"# Pre_Eclampsia incl. HELLP Syndrome, Eclampsia
)

# Load dataset ----
dta <- haven::read_sas("H:/RCP/RCP_Data/TeixeiEC/Monster/Monster.sas7bdat"
                           #, n_max = 2
                           ,col_select = myvec
) %>%
 # filter data by birth year
 filter(BrthYear >= 2009) %>%
 mutate(
  # Create a fiscal year variable
  FiscalYear = case_when(
   between(BTBrthDT, as.POSIXct("2008-04-01 00:00:00.0000"), as.POSIXct("2009-03-31 23:59:59.0000")) ~ "2008-2009",
   between(BTBrthDT, as.POSIXct("2009-04-01 00:00:00.0000"), as.POSIXct("2010-03-31 23:59:59.0000")) ~ "2009-2010",
   between(BTBrthDT, as.POSIXct("2010-04-01 00:00:00.0000"), as.POSIXct("2011-03-31 23:59:59.0000")) ~ "2010-2011",
   between(BTBrthDT, as.POSIXct("2011-04-01 00:00:00.0000"), as.POSIXct("2012-03-31 23:59:59.0000")) ~ "2011-2012",
   between(BTBrthDT, as.POSIXct("2012-04-01 00:00:00.0000"), as.POSIXct("2013-03-31 23:59:59.0000")) ~ "2012-2013",
   between(BTBrthDT, as.POSIXct("2013-04-01 00:00:00.0000"), as.POSIXct("2014-03-31 23:59:59.0000")) ~ "2013-2014",
   between(BTBrthDT, as.POSIXct("2014-04-01 00:00:00.0000"), as.POSIXct("2015-03-31 23:59:59.0000")) ~ "2014-2015",
   between(BTBrthDT, as.POSIXct("2015-04-01 00:00:00.0000"), as.POSIXct("2016-03-31 23:59:59.0000")) ~ "2015-2016",
   between(BTBrthDT, as.POSIXct("2016-04-01 00:00:00.0000"), as.POSIXct("2017-03-31 23:59:59.0000")) ~ "2016-2017",
   between(BTBrthDT, as.POSIXct("2017-04-01 00:00:00.0000"), as.POSIXct("2018-03-31 23:59:59.0000")) ~ "2017-2018",
   between(BTBrthDT, as.POSIXct("2018-04-01 00:00:00.0000"), as.POSIXct("2019-03-31 23:59:59.0000")) ~ "2018-2019",
   between(BTBrthDT, as.POSIXct("2019-04-01 00:00:00.0000"), as.POSIXct("2020-03-31 23:59:59.0000")) ~ "2019-2020",
   between(BTBrthDT, as.POSIXct("2020-04-01 00:00:00.0000"), as.POSIXct("2021-03-31 23:59:59.0000")) ~ "2020-2021",
   between(BTBrthDT, as.POSIXct("2021-04-01 00:00:00.0000"), as.POSIXct("2022-03-31 23:59:59.0000")) ~ "2021-2022",
   between(BTBrthDT, as.POSIXct("2022-04-01 00:00:00.0000"), as.POSIXct("2023-03-31 23:59:59.0000")) ~ "2022-2023",
   between(BTBrthDT, as.POSIXct("2023-04-01 00:00:00.0000"), as.POSIXct("2024-03-31 23:59:59.0000")) ~ "2023-2024",
   between(BTBrthDT, as.POSIXct("2024-04-01 00:00:00.0000"), as.POSIXct("2025-03-31 23:59:59.0000")) ~ "2024-2025",
   TRUE ~ NA_character_
 ))

# Load PCCF dataset ----

pccf <- arrow::read_parquet("H:/RCP/RCP_Data/Prod/Geocodes/Data/PCCF8A1_Monster.parquet")

# Bring geographical information to dta

dta <- merge(
 dta,
 pccf %>%
  select(BIRTHID, DAuid, CSDuid, CSDname, HRuid, HRename),
 by = "BIRTHID",
 all.x = TRUE)

# Access canadian shapefiles through cancensus package ----

# key to access cancensus datasets
key = "CensusMapper_f505397ff4bb63467541085d028c9be8"

## Census subdistrict ----
csd_shp <- cancensus::get_census(
 dataset = "CA21",
 regions = list(PR = "12"),
 level = "CSD",
 geo_format = "sf",
 api_key = key
) %>%
 mutate(
 csd_type = case_when(
  tolower(stringr::str_extract(name, "\\(([^()]*)\\)$")) %in% "(iri)" ~ "Indian reserve",
  tolower(stringr::str_extract(name, "\\(([^()]*)\\)$")) %in% "(md)" ~ "Municipal district",
  tolower(stringr::str_extract(name, "\\(([^()]*)\\)$")) %in% "(rgm)" ~ "Regional municipality",
  tolower(stringr::str_extract(name, "\\(([^()]*)\\)$")) %in% "(rm)" ~ "Rural municipality",
  tolower(stringr::str_extract(name, "\\(([^()]*)\\)$")) %in% "(sc)" ~ "Subdivision of county municipality",
  tolower(stringr::str_extract(name, "\\(([^()]*)\\)$")) %in% "(t)" ~ "Town"
 ),
 name = stringr::str_remove(name, "\\(([^()]*)\\)$")) %>%
 select("GeoUID", "CMA_UID", "name","csd_type","Population","geometry")

## RuralUrbanFlag: All regions and regional aggregates outside census ----
## metropolitan areas/census agglomerations were coded as rural.
## All regions within census metropolitan areas/census agglomerations
## and regional aggregates were coded as urban.
## https://www150.statcan.gc.ca/n1/pub/71-607-x/71-607-x2021023-eng.htm
## "Rural" or "rural and small town" refers to areas outside census metropolitan ## areas or census agglomerations.
## https://www.statcan.gc.ca/en/subjects-start/society_and_community/rural_canada

urb <- unique(csd_shp[!is.na(csd_shp$CMA_UID),][["GeoUID"]])

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

# Export dashboard dataset ----

arrow::write_parquet(
 dta,
 "./data/data.parquet")

# Color palettes ----
# https://colors.muz.li/
## Dark blue - #1a70a3: c("#125d72","#1a70a3","#94e8ff","#c9f3ff","#ffffff")
## Light blue - #85c2e8: c("#5d92a2","#85c2e8","#c9f2ff","#e4f8ff","#ffffff")
## Dark green - #45ad99: c("#307972","#45ad99","#b2fff8","#d9fffb","#ffffff")
## Light green - #75d6c4: c("#52968f","#75d6c4","#c5fffa","#e2fffc","#ffffff")
## Red - #db7370: c("#99504e","#db7370","#ffc2c1","#ffe1e0","#ffffff")
## Yellow - #f2c478: c("#a99554","#f2c478","#fff0bf","#fff7df","#ffffff")
## Dark grey - #abb8b8
## Light grey - #f5f5f5
## Black - #d72230
