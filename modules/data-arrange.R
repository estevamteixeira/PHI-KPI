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
 "DMMATAGE",# Mother's Age
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
  FiscalYear = factor(case_when(
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
 ), levels = c(
  "2008-2009","2009-2010","2010-2011","2011-2012","2012-2013","2013-2014",
  "2014-2015","2015-2016","2016-2017","2017-2018","2018-2019","2019-2020",
  "2020-2021","2021-2022","2022-2023","2023-2024","2024-2025"
 )),
 matagegrp = factor(case_when(
  floor(DMMATAGE) < 15 ~ "< 15",
  between(floor(DMMATAGE), 15, 19) ~ "15-19",
  between(floor(DMMATAGE), 20, 24) ~ "20-24",
  between(floor(DMMATAGE), 25, 29) ~ "25-29",
  between(floor(DMMATAGE), 30, 34) ~ "30-34",
  between(floor(DMMATAGE), 35, 39) ~ "35-39",
  between(floor(DMMATAGE), 40, 44) ~ "40-44",
  between(floor(DMMATAGE), 45, 49) ~ "45-49",
  floor(DMMATAGE) >= 50 ~ "50+",
  TRUE ~ NA_character_
 ), levels = c(
  "< 15","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"
 )))

# Load PCCF dataset ----

pccf <- arrow::read_parquet("H:/RCP/RCP_Data/Prod/Geocodes/Data/PCCF8A1_Monster.parquet")

# Load DA-CC conversion file ----

load("./data/DA-CC Conversion Environment.Rdata")

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

# Bring geographical information to dta ----

dta <- merge(
 dta %>%
  mutate(
  ID = 1:nrow(dta)
 ),
 pccf %>%
  select(CONTCTID, DAuid, CSDuid, CSDname, HRuid, HRename),
 by = "CONTCTID",
 all.x = TRUE) %>%
 # Keep only NS mums for mapping
 filter(grepl("^12", DAuid)) %>%
 mutate(
  URBuid = ifelse(CSDuid %in% urb | (as.integer(DAuid) %% 1000 != 999), 1, ifelse(!CSDuid %in% urb & (as.integer(CSDuid) %% 1000 != 999), 0, NA_character_)),
  URBname = ifelse(URBuid %in% 1,"Urban",ifelse(URBuid %in% 0, "Rural", NA_character_)),
  CDuid = substr(CSDuid, 1, 4),
  # remove the last occurrence of a parenthesis and the text within it
  CSDname = stringr::str_remove(CSDname, "\\(([^()]*)\\)$"),
  CDname = case_when(
   substr(CSDuid, 1, 4) %in% "1201" ~ "Shelburne",
   substr(CSDuid, 1, 4) %in% "1202" ~ "Yarmouth",
   substr(CSDuid, 1, 4) %in% "1203" ~ "Digby",
   substr(CSDuid, 1, 4) %in% "1204" ~ "Queens",
   substr(CSDuid, 1, 4) %in% "1205" ~ "Annapolis",
   substr(CSDuid, 1, 4) %in% "1206" ~ "Lunenburg",
   substr(CSDuid, 1, 4) %in% "1207" ~ "Kings",
   substr(CSDuid, 1, 4) %in% "1208" ~ "Hants",
   substr(CSDuid, 1, 4) %in% "1209" ~ "Halifax",
   substr(CSDuid, 1, 4) %in% "1210" ~ "Colchester",
   substr(CSDuid, 1, 4) %in% "1211" ~ "Cumberland",
   substr(CSDuid, 1, 4) %in% "1212" ~ "Pictou",
   substr(CSDuid, 1, 4) %in% "1213" ~ "Guysborough",
   substr(CSDuid, 1, 4) %in% "1214" ~ "Antigonish",
   substr(CSDuid, 1, 4) %in% "1215" ~ "Inverness",
   substr(CSDuid, 1, 4) %in% "1216" ~ "Richmond",
   substr(CSDuid, 1, 4) %in% "1217" ~ "Cape Breton",
   substr(CSDuid, 1, 4) %in% "1218" ~ "Victoria",
   TRUE ~ NA_character_
  ),
  Hypertension = case_when(
   PreExisting_Hypertension > 0 | Gestational_Hypertension > 0 ~ 1,
   PreExisting_Hypertension %in% 0 & Gestational_Hypertension %in% 0 ~ 0,
   TRUE ~ NA_integer_
  )
 ) %>%
 distinct(ID, .keep_all = TRUE)

# Bring Community cluster information to dta ----
dta <- merge(
 dta,
 conv_file[,c("Community.Cluster","DA")],
 by.x = "DAuid",
 by.y = "DA",
 all.x = TRUE
) %>%
 distinct(ID, .keep_all = TRUE) %>%
 rename(c("CLname" = "Community.Cluster")) %>%
 # Try to assign community cluster to new DAuid not yet contemplated in the file
 mutate(
  CLname = case_when(
   DAuid %in% c("12040051","12040053") ~ "Liverpool",
   DAuid %in% c("12080123","12080127","12080128","12080129") ~ "Sackville North and Area",
   DAuid %in% c("12091004","12091005") ~ "Cole Harbour/Eastern Passage",
   DAuid %in% c("12091006","12091007","12091008","12091024","12091025") ~ "Fall River and Area",
   DAuid %in% c("12091010","12091011","12091012") ~ "Tantallon/ Timberlea/ SMB",
   DAuid %in% c("12091013","12091014") ~ "Armdale/ Spryfield/ Herring Cove",
   DAuid %in% c("12091015","12091016","12091017","12091018","12091021","12091022","12091023") ~ "Fairview",
   DAuid %in% c("12091019","12091020") ~ "Tantallon/ Timberlea/ SMB",
   DAuid %in% c("12150074","12150075","12160034") ~ "Port Hawkesbury / L'Ardoise / Isle Madame",
   DAuid %in% c("12170547") ~ "Sydney and Area",
   TRUE ~ CLname
  ),
  # Assign community cluster ID (CLuid)
  CLuid = case_when(
   CLname %in% "Bridgewater" ~ "010101",
   CLname %in% "Chester and Area" ~ "010102",
   CLname %in% "Liverpool" ~ "010103",
   CLname %in% "Lunenburg / Mahone Bay" ~ "010104",
   CLname %in% "Digby / Clare / Weymouth" ~ "010205",
   CLname %in% "Shelburne/Lockport" ~ "010206",
   CLname %in% "Yarmouth" ~ "010207",
   CLname %in% "Annapolis Royal" ~ "010308",
   CLname %in% "Berwick" ~ "010309",
   CLname %in% "Kentville" ~ "010310",
   CLname %in% "Middleton" ~ "010311",
   CLname %in% "Wolfville" ~ "010312",
   CLname %in% "East Hants Corridor" ~ "020413",
   CLname %in% "Economy / Glenholme" ~ "020414",
   CLname %in% "Hants North" ~ "020415",
   CLname %in% "South Colchester" ~ "020416",
   CLname %in% "Truro and Area" ~ "020417",
   CLname %in% "Amherst" ~ "020518",
   CLname %in% "Cumberland North/ North Shore" ~ "020519",
   CLname %in% "South Cumberland" ~ "020520",
   CLname %in% "Springhill" ~ "020521",
   CLname %in% "New Glasgow / Westville / Stellarton" ~ "020622",
   CLname %in% "Pictou West" ~ "020623",
   CLname %in% "Antigonish" ~ "030724",
   CLname %in% "Guysborough / Canso" ~ "030725",
   CLname %in% "Sherbrooke" ~ "030726",
   CLname %in% "Port Hawkesbury / L'Ardoise / Isle Madame" ~ "030827",
   CLname %in% "Baddeck / Whycocomagh" ~ "030828",
   CLname %in% "Cheticamp" ~ "030829",
   CLname %in% "Dingwall" ~ "030830",
   CLname %in% "Inverness" ~ "030831",
   CLname %in% "New Waterford" ~ "030932",
   CLname %in% "Sydney and Area" ~ "030933",
   CLname %in% "Dominion / Glace Bay" ~ "030934",
   CLname %in% "Florence / Sydney Mines / North Sydney" ~ "030935",
   CLname %in% "Dartmouth North" ~ "041036",
   CLname %in% "Dartmouth South" ~ "041037",
   CLname %in% "Dartmouth East" ~ "041038",
   CLname %in% "Cole Harbour/Eastern Passage" ~ "041039",
   CLname %in% "Preston/Lawrencetown/Lake Echo" ~ "041040",
   CLname %in% "Tantallon/ Timberlea/ SMB" ~ "041141",
   CLname %in% "Sambro Rural Loop" ~ "041142",
   CLname %in% "Armdale/ Spryfield/ Herring Cove" ~ "041143",
   CLname %in% "Halifax Needham" ~ "041144",
   CLname %in% "Halifax Chebucto" ~ "041145",
   CLname %in% "Fairview" ~ "041146",
   CLname %in% "Clayton Park" ~ "041147",
   CLname %in% "Halifax Citadel" ~ "041148",
   CLname %in% "Bedford / Hammonds Plains" ~ "041249",
   CLname %in% "Sackville South" ~ "041250",
   CLname %in% "Sackville North and Area" ~ "041251",
   CLname %in% "Fall River and Area" ~ "041252",
   CLname %in% "Eastern Shore/ Musquodoboit" ~ "041353",
   CLname %in% "West Hants" ~ "041454",
   TRUE ~ NA_character_
  ),
  # Assign community health network name and ID
  CHNuid = substr(CLuid, 1, 4),
  CHNname =case_when(
   CHNuid %in% "0101" ~ "Lunenburg & Queens",
   CHNuid %in% "0102" ~ "Yarmouth, Shelburne & Digby",
   CHNuid %in% "0103" ~ "Annapolis and Kings",
   CHNuid %in% "0204" ~ "Colchester / East Hants",
   CHNuid %in% "0205" ~ "Cumberland",
   CHNuid %in% "0206" ~ "Pictou",
   CHNuid %in% "0307" ~ "Antigonish & Guysborough",
   CHNuid %in% "0308" ~ "Inverness, Victoria & Richmond",
   CHNuid %in% "0309" ~ "Cape Breton",
   CHNuid %in% "0410" ~ "Dartmouth / Southeastern",
   CHNuid %in% "0411" ~ "Halifax Peninsula / Chebucto",
   CHNuid %in% "0412" ~ "Bedford / Sackville",
   CHNuid %in% "0413" ~ "Eastern Shore / Musquodoboit",
   CHNuid %in% "0414" ~ "West Hants",
   TRUE ~ NA_character_
  ),
  HRuid = ifelse(HRuid %in% c("10 ","11 "), NA_character_, HRuid)
 ) %>%
 group_by(DAuid) %>%
 tidyr::fill(c(HRename, HRuid, CSDname, CSDuid), .direction = "updown") %>%
 group_by(CLuid) %>%
 tidyr::fill(c(HRename, HRuid, CSDname, CSDuid), .direction = "updown") %>%
 ungroup() %>%
 mutate(
  CSDname = ifelse(as.integer(DAuid) %% 10000 == 9999 & as.integer(CSDuid) %% 1000 == 999, NA_character_, CSDname),
  HRuid = ifelse(as.integer(DAuid) %% 10000 == 9999 & as.integer(CSDuid) %% 1000 == 999, NA_character_, HRuid),
  HRename = ifelse(as.integer(DAuid) %% 10000 == 9999 & as.integer(CSDuid) %% 1000 == 999, NA_character_, HRename)
 )

# Computing stats ----
## Calendar year stats ----
cyearly_stats <- dta %>%
 group_by(BrthYear, PreExisting_Hypertension) %>%
 mutate(
  prehyp_count = n()
 ) %>%
 group_by(BrthYear, Gestational_Hypertension) %>%
 mutate(
  gesthyp_count = n()
 ) %>%
 group_by(BrthYear, Hypertension) %>%
 mutate(
  hyp_count = n()
 ) %>%
 group_by(BrthYear) %>%
 mutate(
  total_year = n(),
  prehyp_rate = prehyp_count/total_year,
  gesthyp_rate = gesthyp_count/total_year,
  hyp_rate = hyp_count/total_year
 ) %>%
 ungroup() %>%
 filter(PreExisting_Hypertension %in% 1,Gestational_Hypertension %in% 1, Hypertension %in% 1) %>%
 select(BrthYear,prehyp_count,prehyp_rate,gesthyp_count,gesthyp_rate,hyp_count,hyp_rate, total_year) %>%
 arrange(BrthYear) %>%
 distinct()

## Fiscal year stats ----
fyearly_stats <- dta %>%
 group_by(FiscalYear, PreExisting_Hypertension) %>%
 mutate(
  prehyp_count = n()
 ) %>%
 group_by(FiscalYear, Gestational_Hypertension) %>%
 mutate(
  gesthyp_count = n()
 ) %>%
 group_by(FiscalYear, Hypertension) %>%
 mutate(
  hyp_count = n()
 ) %>%
 group_by(FiscalYear) %>%
 mutate(
  total_year = n(),
  prehyp_rate = prehyp_count/total_year,
  gesthyp_rate = gesthyp_count/total_year,
  hyp_rate = hyp_count/total_year
 ) %>%
 ungroup() %>%
 filter(PreExisting_Hypertension %in% 1,Gestational_Hypertension %in% 1, Hypertension %in% 1) %>%
 select(FiscalYear,prehyp_count,prehyp_rate,gesthyp_count,gesthyp_rate,hyp_count,hyp_rate,total_year) %>%
 arrange(FiscalYear) %>%
 distinct()

# Export dashboard dataset ----

arrow::write_parquet(
 dta,
 "./data/data.parquet")

arrow::write_parquet(
 cyearly_stats,
 "./data/cyearly_stats.parquet")

arrow::write_parquet(
 fyearly_stats,
 "./data/fyearly_stats.parquet")

# Color palettes ----
# https://colors.muz.li/
## Dark blue - #000054: c("#125d72","#1a70a3","#94e8ff","#c9f3ff","#ffffff")
## Light blue - #196f03: c("#5d92a2","#85c2e8","#c9f2ff","#e4f8ff","#ffffff")
## Dark green - #44ad99: c("#307972","#45ad99","#b2fff8","#d9fffb","#ffffff")
## Light green - #75d6c4: c("#52968f","#75d6c4","#c5fffa","#e2fffc","#ffffff")
## Red - #d9715f: c("#99504e","#db7370","#ffc2c1","#ffe1e0","#ffffff")
## Yellow - #f2c577: c("#a99554","#f2c478","#fff0bf","#fff7df","#ffffff")
## Dark grey - #a1a1bb
## Light grey - #f2ebe2
## Black - #2f2f3f
