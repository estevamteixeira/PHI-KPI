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
 "DMMETHOD",# Method of Delivery (most serious)
 ## Hypertension ----
 "PreExisting_Hypertension",# Pre-existing/Pre-pregnancy Hypertension
 "Gestational_Hypertension",# Gestational Hypertension
 "Any_Hypertension",# Any Hypertension
 #"Proteinuria",# Any protein in urine
 #"Pre_Eclampsia",# Pre_Eclampsia incl. HELLP Syndrome, Eclampsia
 ## Diabetes ----
 # Pre-pregnancy diabetes
 "PreExisting_Diabetes",# Pre-existing diabetes
 # Gestational diabetes
 "GDM",# Gestational diabetes mellitus
 "Any_Diabetes",# Any diabetes: Pre + GDM + any other different type
 ## Robson group ----
 "RobsnGrp",# Robson classification
 ## Postpartum readmission ----
 "NumPPAdm",# Number of postpartum admissions
 ## Skin to skin ----
 "BRSTCON1",# Breast contact within one hour of birth
 ## Neonatal readmission rate ----
 "NeoAdmit",# Number of neonatal transfers/readmissions
 ## Breastfeeding ----
 "BRSTFDIS",# Breastfeeding during Birth Admission
 "DLINTBFD"# Intend to Breastfeed
)

# Load dataset ----
dta <- haven::read_sas("H:/RCP/RCP_Data/TeixeiEC/Monster/Monster.sas7bdat"
                           #, n_max = 2
                           ,col_select = myvec
) %>%
 # filter data by birth year
 filter(between(BrthYear,
                as.numeric(format(as.Date(Sys.Date()),"%Y"))-10,
                as.numeric(format(as.Date(Sys.Date()),"%Y"))-1)) %>%
 mutate(
  # Create a fiscal year variable
  FiscalYear = factor(case_when(
   between(BTBrthDT, as.POSIXct("2013-04-01 00:00:00.0000", tz="UTC"), as.POSIXct("2014-03-31 23:59:59.0000", tz="UTC")) ~ "2013-2014",
   between(BTBrthDT, as.POSIXct("2014-04-01 00:00:00.0000", tz="UTC"), as.POSIXct("2015-03-31 23:59:59.0000", tz="UTC")) ~ "2014-2015",
   between(BTBrthDT, as.POSIXct("2015-04-01 00:00:00.0000", tz="UTC"), as.POSIXct("2016-03-31 23:59:59.0000", tz="UTC")) ~ "2015-2016",
   between(BTBrthDT, as.POSIXct("2016-04-01 00:00:00.0000", tz="UTC"), as.POSIXct("2017-03-31 23:59:59.0000", tz="UTC")) ~ "2016-2017",
   between(BTBrthDT, as.POSIXct("2017-04-01 00:00:00.0000", tz="UTC"), as.POSIXct("2018-03-31 23:59:59.0000", tz="UTC")) ~ "2017-2018",
   between(BTBrthDT, as.POSIXct("2018-04-01 00:00:00.0000", tz="UTC"), as.POSIXct("2019-03-31 23:59:59.0000", tz="UTC")) ~ "2018-2019",
   between(BTBrthDT, as.POSIXct("2019-04-01 00:00:00.0000", tz="UTC"), as.POSIXct("2020-03-31 23:59:59.0000", tz="UTC")) ~ "2019-2020",
   between(BTBrthDT, as.POSIXct("2020-04-01 00:00:00.0000", tz="UTC"), as.POSIXct("2021-03-31 23:59:59.0000", tz="UTC")) ~ "2020-2021",
   between(BTBrthDT, as.POSIXct("2021-04-01 00:00:00.0000", tz="UTC"), as.POSIXct("2022-03-31 23:59:59.0000", tz="UTC")) ~ "2021-2022",
   between(BTBrthDT, as.POSIXct("2022-04-01 00:00:00.0000", tz="UTC"), as.POSIXct("2023-03-31 23:59:59.0000", tz="UTC")) ~ "2022-2023",
   between(BTBrthDT, as.POSIXct("2023-04-01 00:00:00.0000", tz="UTC"), as.POSIXct("2024-03-31 23:59:59.0000", tz="UTC")) ~ "2023-2024",
   between(BTBrthDT, as.POSIXct("2024-04-01 00:00:00.0000", tz="UTC"), as.POSIXct("2025-03-31 23:59:59.0000", tz="UTC")) ~ "2024-2025",
   TRUE ~ NA_character_
 ), levels = c(
  "2013-2014","2014-2015","2015-2016","2016-2017","2017-2018","2018-2019",
  "2019-2020","2020-2021","2021-2022","2022-2023","2023-2024","2024-2025"
 )),
 # Maternal age group
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
 )),
 # Numerator
 rbs1 = case_when(
  RobsnGrp %in% 1 & tolower(DMMETHOD) %in% "cst" ~ 1,
  TRUE ~ 0
 ),
 rbs21 = case_when(
  RobsnGrp %in% 2.1 & tolower(DMMETHOD) %in% "cst" ~ 1,
  TRUE ~ 0
 ),
 rbs51 = case_when(
  RobsnGrp %in% 5.1 & tolower(DMMETHOD) %in% "cst" ~ 1,
  TRUE ~ 0
 ),
 ppreadm = case_when(
  NumPPAdm > 0 ~ 1,
  TRUE ~ 0
 ),
 sknskn = case_when(
  tolower(BRSTCON1) %in% "y" ~ 1,
  TRUE ~ 0
 ),
 sknsknvg = case_when(
  tolower(BRSTCON1) %in% "y" & tolower(DMMETHOD) %in% c("biv","spt","vac","low","mid") ~ 1,
  TRUE ~ 0
 ),
 sknskncs = case_when(
  tolower(BRSTCON1) %in% "y" & tolower(DMMETHOD) %in% c("cst") ~ 1,
  TRUE ~ 0
 ),
 neoreadm = case_when(
  NeoAdmit > 0 ~ 1,
  TRUE ~ 0
 ),
 excbrst = case_when(
  tolower(BRSTFDIS) %in% "e" ~ 1,
  TRUE ~ 0
 ),
 nexcbrst = case_when(
  tolower(BRSTFDIS) %in% "s" ~ 1,
  TRUE ~ 0
 ),
 nbrst = case_when(
  tolower(BRSTFDIS) %in% "n" & tolower(DLINTBFD) %in% "y" ~ 1,
  TRUE ~ 0
 ),
 brstinit = case_when(
  tolower(BRSTFDIS) %in% c("s","e") ~ 1,
  TRUE ~ 0
 ),
 # Denominator
 lvb = case_when(
  !tolower(BTOUTCOM) %in% c("ftd") ~ 1,
  TRUE ~ 0
 ),
 lvbvg = case_when(
  !tolower(BTOUTCOM) %in% c("ftd") & tolower(DMMETHOD) %in% c("biv","spt","vac","low","mid") ~ 1,
  TRUE ~ 0
 ),
 lvbcs = case_when(
  !tolower(BTOUTCOM) %in% c("ftd") & tolower(DMMETHOD) %in% c("cst") ~ 1,
  TRUE ~ 0
 ),
 lvbint = case_when(
  !tolower(BTOUTCOM) %in% c("ftd") & tolower(DLINTBFD) %in% "y" ~ 1,
  TRUE ~ 0
 )
 )

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

## Urban x Rural (urb) ----

urb_shp <- sf::read_sf("./data/NSC_urban.shp") %>%
 rename(GeoUID = FID) %>%
 mutate(GeoUID = 1)
# sf::st_make_valid() %>%
# rmapshaper::ms_simplify() %>%
# sf::st_make_valid()

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
### Hypertension ----

c1 <- dta %>%
 group_by(BrthYear, PreExisting_Hypertension) %>%
 mutate(
  prehyp_count = n()
 ) %>%
 group_by(BrthYear) %>%
 mutate(
  total_year = n(),
  prehyp_rate = prehyp_count/total_year
 ) %>%
 ungroup() %>%
 filter(PreExisting_Hypertension %in% 1) %>%
 select(BrthYear, PreExisting_Hypertension, prehyp_count,prehyp_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  prehyp_delta = (prehyp_rate - lag(prehyp_rate))/lag(prehyp_rate),
  prehyp_deltap = (prehyp_rate - lag(prehyp_rate,9))/lag(prehyp_rate,9)
 )

c2 <- dta %>%
 group_by(BrthYear, Gestational_Hypertension) %>%
 mutate(
  gesthyp_count = n()
 ) %>%
 group_by(BrthYear) %>%
 mutate(
  total_year = n(),
  gesthyp_rate = gesthyp_count/total_year
 ) %>%
 ungroup() %>%
 filter(Gestational_Hypertension %in% 1) %>%
 select(BrthYear, Gestational_Hypertension, gesthyp_count,gesthyp_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  gesthyp_delta = (gesthyp_rate - lag(gesthyp_rate))/lag(gesthyp_rate),
  gesthyp_deltap = (gesthyp_rate - lag(gesthyp_rate,9))/lag(gesthyp_rate,9)
 )

c3 <- dta %>%
 group_by(BrthYear, Any_Hypertension) %>%
 mutate(
  hyp_count = n()
 ) %>%
 group_by(BrthYear) %>%
 mutate(
  total_year = n(),
  hyp_rate = hyp_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Hypertension %in% 1) %>%
 select(BrthYear, Any_Hypertension, hyp_count,hyp_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  hyp_delta = (hyp_rate - lag(hyp_rate))/lag(hyp_rate),
  hyp_deltap = (hyp_rate - lag(hyp_rate,9))/lag(hyp_rate,9)
 )

### Diabetes ----

c4 <- dta %>%
 group_by(BrthYear, PreExisting_Diabetes) %>%
 mutate(
  prediab_count = n()
 ) %>%
 group_by(BrthYear) %>%
 mutate(
  total_year = n(),
  prediab_rate = prediab_count/total_year
 ) %>%
 ungroup() %>%
 filter(PreExisting_Diabetes %in% 1) %>%
 select(BrthYear, PreExisting_Diabetes, prediab_count,prediab_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  prediab_delta = (prediab_rate - lag(prediab_rate))/lag(prediab_rate),
  prediab_deltap = (prediab_rate - lag(prediab_rate,9))/lag(prediab_rate,9)
 )

c5 <- dta %>%
 group_by(BrthYear, GDM) %>%
 mutate(
  gestdiab_count = n()
 ) %>%
 group_by(BrthYear) %>%
 mutate(
  total_year = n(),
  gestdiab_rate = gestdiab_count/total_year
 ) %>%
 ungroup() %>%
 filter(GDM %in% 1) %>%
 select(BrthYear, GDM, gestdiab_count,gestdiab_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  gestdiab_delta = (gestdiab_rate - lag(gestdiab_rate))/lag(gestdiab_rate),
  gestdiab_deltap = (gestdiab_rate - lag(gestdiab_rate,9))/lag(gestdiab_rate,9)
 )

c6 <- dta %>%
 group_by(BrthYear, Any_Diabetes) %>%
 mutate(
  diab_count = n()
 ) %>%
 group_by(BrthYear) %>%
 mutate(
  total_year = n(),
  diab_rate = diab_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Diabetes %in% 1) %>%
 select(BrthYear,Any_Diabetes,diab_count,diab_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  diab_delta = (diab_rate - lag(diab_rate))/lag(diab_rate),
  diab_deltap = (diab_rate - lag(diab_rate,9))/lag(diab_rate,9)
 )

### Robson group ----

c7 <- dta %>%
 group_by(BrthYear, rbs1) %>%
 mutate(
  rbs1_count = n()
 ) %>%
 group_by(BrthYear, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs1_rate = rbs1_count/total_year
 ) %>%
 ungroup() %>%
 filter(RobsnGrp %in% 1, rbs1 %in% 1) %>%
 select(BrthYear, RobsnGrp, rbs1_count,rbs1_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  rbs1_delta = (rbs1_rate - lag(rbs1_rate))/lag(rbs1_rate),
  rbs1_deltap = (rbs1_rate - lag(rbs1_rate,9))/lag(rbs1_rate,9)
 )

c8 <- dta %>%
 group_by(BrthYear, rbs21) %>%
 mutate(
  rbs21_count = n()
 ) %>%
 group_by(BrthYear, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs21_rate = rbs21_count/total_year
 ) %>%
 ungroup() %>%
 filter(RobsnGrp %in% 2.1, rbs21 %in% 1) %>%
 select(BrthYear, RobsnGrp, rbs21_count,rbs21_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  rbs21_delta = (rbs21_rate - lag(rbs21_rate))/lag(rbs21_rate),
  rbs21_deltap = (rbs21_rate - lag(rbs21_rate,9))/lag(rbs21_rate,9)
 )

c9 <- dta %>%
 group_by(BrthYear, rbs51) %>%
 mutate(
  rbs51_count = n()
 ) %>%
 group_by(BrthYear, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs51_rate = rbs51_count/total_year
 ) %>%
 ungroup() %>%
 filter(RobsnGrp %in% 5.1, rbs51 %in% 1) %>%
 select(BrthYear, RobsnGrp, rbs51_count,rbs51_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  rbs51_delta = (rbs51_rate - lag(rbs51_rate))/lag(rbs51_rate),
  rbs51_deltap = (rbs51_rate - lag(rbs51_rate,9))/lag(rbs51_rate,9)
 )

### Postpartum readmission ----

c10 <- dta %>%
 group_by(BrthYear, ppreadm) %>%
 mutate(
  ppreadm_count = n()
 ) %>%
 group_by(BrthYear) %>%
 mutate(
  total_year = n(),
  ppreadm_rate = ppreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(ppreadm %in% 1) %>%
 select(BrthYear, ppreadm, ppreadm_count,ppreadm_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  ppreadm_delta = (ppreadm_rate - lag(ppreadm_rate))/lag(ppreadm_rate),
  ppreadm_deltap = (ppreadm_rate - lag(ppreadm_rate,9))/lag(ppreadm_rate,9)
 )

### Skin to skin ----

c11 <- dta %>%
 group_by(BrthYear, sknskn) %>%
 mutate(
  sknskn_count = n()
 ) %>%
 group_by(BrthYear, lvb) %>%
 mutate(
  total_year = n(),
  sknskn_rate = sknskn_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskn %in% 1) %>%
 select(BrthYear, sknskn, sknskn_count,sknskn_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  sknskn_delta = (sknskn_rate - lag(sknskn_rate))/lag(sknskn_rate),
  sknskn_deltap = (sknskn_rate - lag(sknskn_rate,9))/lag(sknskn_rate,9)
 )

c12 <- dta %>%
 group_by(BrthYear, sknsknvg) %>%
 mutate(
  sknsknvg_count = n()
 ) %>%
 group_by(BrthYear, lvbvg) %>%
 mutate(
  total_year = n(),
  sknsknvg_rate = sknsknvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknsknvg %in% 1) %>%
 select(BrthYear, sknsknvg, sknsknvg_count,sknsknvg_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  sknsknvg_delta = (sknsknvg_rate - lag(sknsknvg_rate))/lag(sknsknvg_rate),
  sknsknvg_deltap = (sknsknvg_rate - lag(sknsknvg_rate,9))/lag(sknsknvg_rate,9)
 )

c13 <- dta %>%
 group_by(BrthYear, sknskncs) %>%
 mutate(
  sknskncs_count = n()
 ) %>%
 group_by(BrthYear, lvbcs) %>%
 mutate(
  total_year = n(),
  sknskncs_rate = sknskncs_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskncs %in% 1) %>%
 select(BrthYear, sknskncs, sknskncs_count,sknskncs_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  sknskncs_delta = (sknskncs_rate - lag(sknskncs_rate))/lag(sknskncs_rate),
  sknskncs_deltap = (sknskncs_rate - lag(sknskncs_rate,9))/lag(sknskncs_rate,9)
 )

### Neonatal readmission ----

c14 <- dta %>%
 group_by(BrthYear, neoreadm) %>%
 mutate(
  neoreadm_count = n()
 ) %>%
 group_by(BrthYear, lvb) %>%
 mutate(
  total_year = n(),
  neoreadm_rate = neoreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(neoreadm %in% 1) %>%
 select(BrthYear, neoreadm, neoreadm_count,neoreadm_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  neoreadm_delta = (neoreadm_rate - lag(neoreadm_rate))/lag(neoreadm_rate),
  neoreadm_deltap = (neoreadm_rate - lag(neoreadm_rate,9))/lag(neoreadm_rate,9)
 )

### Milk feeding ----

c15 <- dta %>%
 group_by(BrthYear, excbrst) %>%
 mutate(
  excbrst_count = n()
 ) %>%
 group_by(BrthYear, lvb) %>%
 mutate(
  total_year = n(),
  excbrst_rate = excbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(excbrst %in% 1) %>%
 select(BrthYear, excbrst, excbrst_count,excbrst_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  excbrst_delta = (excbrst_rate - lag(excbrst_rate))/lag(excbrst_rate),
  excbrst_deltap = (excbrst_rate - lag(excbrst_rate,9))/lag(excbrst_rate,9)
 )

c16 <- dta %>%
 group_by(BrthYear, nexcbrst) %>%
 mutate(
  nexcbrst_count = n()
 ) %>%
 group_by(BrthYear, lvb) %>%
 mutate(
  total_year = n(),
  nexcbrst_rate = nexcbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nexcbrst %in% 1) %>%
 select(BrthYear, nexcbrst, nexcbrst_count,nexcbrst_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  nexcbrst_delta = (nexcbrst_rate - lag(nexcbrst_rate))/lag(nexcbrst_rate),
  nexcbrst_deltap = (nexcbrst_rate - lag(nexcbrst_rate,9))/lag(nexcbrst_rate,9)
 )

c17 <- dta %>%
 group_by(BrthYear, nbrst) %>%
 mutate(
  nbrst_count = n()
 ) %>%
 group_by(BrthYear, lvbint) %>%
 mutate(
  total_year = n(),
  nbrst_rate = nbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nbrst %in% 1) %>%
 select(BrthYear, nbrst, nbrst_count,nbrst_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  nbrst_delta = (nbrst_rate - lag(nbrst_rate))/lag(nbrst_rate),
  nbrst_deltap = (nbrst_rate - lag(nbrst_rate,9))/lag(nbrst_rate,9)
 )

c18 <- dta %>%
 group_by(BrthYear, brstinit) %>%
 mutate(
  brstinit_count = n()
 ) %>%
 group_by(BrthYear, lvb) %>%
 mutate(
  total_year = n(),
  brstinit_rate = brstinit_count/total_year
 ) %>%
 ungroup() %>%
 filter(brstinit %in% 1) %>%
 select(BrthYear, brstinit, brstinit_count,brstinit_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  brstinit_delta = (brstinit_rate - lag(brstinit_rate))/lag(brstinit_rate),
  brstinit_deltap = (brstinit_rate - lag(brstinit_rate,9))/lag(brstinit_rate,9)
 )

cyearly_stats <- cbind(
 c1 %>% select(-PreExisting_Hypertension),
 c2 %>% select(-Gestational_Hypertension,-BrthYear),
 c3 %>% select(-Any_Hypertension,-BrthYear),
 c4 %>% select(-PreExisting_Diabetes,-BrthYear),
 c5 %>% select(-GDM,-BrthYear),
 c6 %>% select(-Any_Diabetes,-BrthYear),
 c7 %>% select(-RobsnGrp,-BrthYear),
 c8 %>% select(-RobsnGrp,-BrthYear),
 c9 %>% select(-RobsnGrp,-BrthYear),
 c10 %>% select(-ppreadm,-BrthYear),
 c11 %>% select(-sknskn,-BrthYear),
 c12 %>% select(-sknsknvg,-BrthYear),
 c13 %>% select(-sknskncs,-BrthYear),
 c14 %>% select(-neoreadm,-BrthYear),
 c15 %>% select(-excbrst,-BrthYear),
 c16 %>% select(-nexcbrst,-BrthYear),
 c17 %>% select(-nbrst,-BrthYear),
 c18 %>% select(-brstinit,-BrthYear)
)

## Fiscal year stats ----
### Hypertension ----

c1 <- dta %>%
 group_by(FiscalYear, PreExisting_Hypertension) %>%
 mutate(
  prehyp_count = n()
 ) %>%
 group_by(FiscalYear) %>%
 mutate(
  total_year = n(),
  prehyp_rate = prehyp_count/total_year
 ) %>%
 ungroup() %>%
 filter(PreExisting_Hypertension %in% 1) %>%
 select(FiscalYear, PreExisting_Hypertension, prehyp_count,prehyp_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  prehyp_delta = (prehyp_rate - lag(prehyp_rate))/lag(prehyp_rate),
  prehyp_deltap = (prehyp_rate - lag(prehyp_rate,9))/lag(prehyp_rate,9)
 )

c2 <- dta %>%
 group_by(FiscalYear, Gestational_Hypertension) %>%
 mutate(
  gesthyp_count = n()
 ) %>%
 group_by(FiscalYear) %>%
 mutate(
  total_year = n(),
  gesthyp_rate = gesthyp_count/total_year
 ) %>%
 ungroup() %>%
 filter(Gestational_Hypertension %in% 1) %>%
 select(FiscalYear, Gestational_Hypertension, gesthyp_count,gesthyp_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  gesthyp_delta = (gesthyp_rate - lag(gesthyp_rate))/lag(gesthyp_rate),
  gesthyp_deltap = (gesthyp_rate - lag(gesthyp_rate,9))/lag(gesthyp_rate,9)
 )

c3 <- dta %>%
 group_by(FiscalYear, Any_Hypertension) %>%
 mutate(
  hyp_count = n()
 ) %>%
 group_by(FiscalYear) %>%
 mutate(
  total_year = n(),
  hyp_rate = hyp_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Hypertension %in% 1) %>%
 select(FiscalYear, Any_Hypertension, hyp_count,hyp_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  hyp_delta = (hyp_rate - lag(hyp_rate))/lag(hyp_rate),
  hyp_deltap = (hyp_rate - lag(hyp_rate,9))/lag(hyp_rate,9)
 )

### Diabetes ----

c4 <- dta %>%
 group_by(FiscalYear, PreExisting_Diabetes) %>%
 mutate(
  prediab_count = n()
 ) %>%
 group_by(FiscalYear) %>%
 mutate(
  total_year = n(),
  prediab_rate = prediab_count/total_year
 ) %>%
 ungroup() %>%
 filter(PreExisting_Diabetes %in% 1) %>%
 select(FiscalYear, PreExisting_Diabetes, prediab_count,prediab_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  prediab_delta = (prediab_rate - lag(prediab_rate))/lag(prediab_rate),
  prediab_deltap = (prediab_rate - lag(prediab_rate,9))/lag(prediab_rate,9)
 )

c5 <- dta %>%
 group_by(FiscalYear, GDM) %>%
 mutate(
  gestdiab_count = n()
 ) %>%
 group_by(FiscalYear) %>%
 mutate(
  total_year = n(),
  gestdiab_rate = gestdiab_count/total_year
 ) %>%
 ungroup() %>%
 filter(GDM %in% 1) %>%
 select(FiscalYear, GDM, gestdiab_count,gestdiab_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  gestdiab_delta = (gestdiab_rate - lag(gestdiab_rate))/lag(gestdiab_rate),
  gestdiab_deltap = (gestdiab_rate - lag(gestdiab_rate,9))/lag(gestdiab_rate,9)
 )

c6 <- dta %>%
 group_by(FiscalYear, Any_Diabetes) %>%
 mutate(
  diab_count = n()
 ) %>%
 group_by(FiscalYear) %>%
 mutate(
  total_year = n(),
  diab_rate = diab_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Diabetes %in% 1) %>%
 select(FiscalYear,Any_Diabetes,diab_count,diab_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  diab_delta = (diab_rate - lag(diab_rate))/lag(diab_rate),
  diab_deltap = (diab_rate - lag(diab_rate,9))/lag(diab_rate,9)
 )

### Robson group ----

c7 <- dta %>%
 group_by(FiscalYear, rbs1) %>%
 mutate(
  rbs1_count = n()
 ) %>%
 group_by(FiscalYear, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs1_rate = rbs1_count/total_year
 ) %>%
 ungroup() %>%
 filter(RobsnGrp %in% 1, rbs1 %in% 1) %>%
 select(FiscalYear, RobsnGrp, rbs1_count,rbs1_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  rbs1_delta = (rbs1_rate - lag(rbs1_rate))/lag(rbs1_rate),
  rbs1_deltap = (rbs1_rate - lag(rbs1_rate,9))/lag(rbs1_rate,9)
 )

c8 <- dta %>%
 group_by(FiscalYear, rbs21) %>%
 mutate(
  rbs21_count = n()
 ) %>%
 group_by(FiscalYear, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs21_rate = rbs21_count/total_year
 ) %>%
 ungroup() %>%
 filter(RobsnGrp %in% 2.1, rbs21 %in% 1) %>%
 select(FiscalYear, RobsnGrp, rbs21_count,rbs21_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  rbs21_delta = (rbs21_rate - lag(rbs21_rate))/lag(rbs21_rate),
  rbs21_deltap = (rbs21_rate - lag(rbs21_rate,9))/lag(rbs21_rate,9)
 )

c9 <- dta %>%
 group_by(FiscalYear, rbs51) %>%
 mutate(
  rbs51_count = n()
 ) %>%
 group_by(FiscalYear, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs51_rate = rbs51_count/total_year
 ) %>%
 ungroup() %>%
 filter(RobsnGrp %in% 5.1, rbs51 %in% 1) %>%
 select(FiscalYear, RobsnGrp, rbs51_count,rbs51_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  rbs51_delta = (rbs51_rate - lag(rbs51_rate))/lag(rbs51_rate),
  rbs51_deltap = (rbs51_rate - lag(rbs51_rate,9))/lag(rbs51_rate,9)
 )

### Postpartum readmission ----

c10 <- dta %>%
 group_by(FiscalYear, ppreadm) %>%
 mutate(
  ppreadm_count = n()
 ) %>%
 group_by(FiscalYear) %>%
 mutate(
  total_year = n(),
  ppreadm_rate = ppreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(ppreadm %in% 1) %>%
 select(FiscalYear, ppreadm, ppreadm_count,ppreadm_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  ppreadm_delta = (ppreadm_rate - lag(ppreadm_rate))/lag(ppreadm_rate),
  ppreadm_deltap = (ppreadm_rate - lag(ppreadm_rate,9))/lag(ppreadm_rate,9)
 )

### Skin to skin ----

c11 <- dta %>%
 group_by(FiscalYear, sknskn) %>%
 mutate(
  sknskn_count = n()
 ) %>%
 group_by(FiscalYear, lvb) %>%
 mutate(
  total_year = n(),
  sknskn_rate = sknskn_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskn %in% 1) %>%
 select(FiscalYear, sknskn, sknskn_count,sknskn_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  sknskn_delta = (sknskn_rate - lag(sknskn_rate))/lag(sknskn_rate),
  sknskn_deltap = (sknskn_rate - lag(sknskn_rate,9))/lag(sknskn_rate,9)
 )

c12 <- dta %>%
 group_by(FiscalYear, sknsknvg) %>%
 mutate(
  sknsknvg_count = n()
 ) %>%
 group_by(FiscalYear, lvbvg) %>%
 mutate(
  total_year = n(),
  sknsknvg_rate = sknsknvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknsknvg %in% 1) %>%
 select(FiscalYear, sknsknvg, sknsknvg_count,sknsknvg_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  sknsknvg_delta = (sknsknvg_rate - lag(sknsknvg_rate))/lag(sknsknvg_rate),
  sknsknvg_deltap = (sknsknvg_rate - lag(sknsknvg_rate,9))/lag(sknsknvg_rate,9)
 )

c13 <- dta %>%
 group_by(FiscalYear, sknskncs) %>%
 mutate(
  sknskncs_count = n()
 ) %>%
 group_by(FiscalYear, lvbcs) %>%
 mutate(
  total_year = n(),
  sknskncs_rate = sknskncs_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskncs %in% 1) %>%
 select(FiscalYear, sknskncs, sknskncs_count,sknskncs_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  sknskncs_delta = (sknskncs_rate - lag(sknskncs_rate))/lag(sknskncs_rate),
  sknskncs_deltap = (sknskncs_rate - lag(sknskncs_rate,9))/lag(sknskncs_rate,9)
 )

### Neonatal readmission ----

c14 <- dta %>%
 group_by(FiscalYear, neoreadm) %>%
 mutate(
  neoreadm_count = n()
 ) %>%
 group_by(FiscalYear, lvb) %>%
 mutate(
  total_year = n(),
  neoreadm_rate = neoreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(neoreadm %in% 1) %>%
 select(FiscalYear, neoreadm, neoreadm_count,neoreadm_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  neoreadm_delta = (neoreadm_rate - lag(neoreadm_rate))/lag(neoreadm_rate),
  neoreadm_deltap = (neoreadm_rate - lag(neoreadm_rate,9))/lag(neoreadm_rate,9)
 )

### Milk feeding ----

c15 <- dta %>%
 group_by(FiscalYear, excbrst) %>%
 mutate(
  excbrst_count = n()
 ) %>%
 group_by(FiscalYear, lvb) %>%
 mutate(
  total_year = n(),
  excbrst_rate = excbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(excbrst %in% 1) %>%
 select(FiscalYear, excbrst, excbrst_count,excbrst_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  excbrst_delta = (excbrst_rate - lag(excbrst_rate))/lag(excbrst_rate),
  excbrst_deltap = (excbrst_rate - lag(excbrst_rate,9))/lag(excbrst_rate,9)
 )

c16 <- dta %>%
 group_by(FiscalYear, nexcbrst) %>%
 mutate(
  nexcbrst_count = n()
 ) %>%
 group_by(FiscalYear, lvb) %>%
 mutate(
  total_year = n(),
  nexcbrst_rate = nexcbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nexcbrst %in% 1) %>%
 select(FiscalYear, nexcbrst, nexcbrst_count,nexcbrst_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  nexcbrst_delta = (nexcbrst_rate - lag(nexcbrst_rate))/lag(nexcbrst_rate),
  nexcbrst_deltap = (nexcbrst_rate - lag(nexcbrst_rate,9))/lag(nexcbrst_rate,9)
 )

c17 <- dta %>%
 group_by(FiscalYear, nbrst) %>%
 mutate(
  nbrst_count = n()
 ) %>%
 group_by(FiscalYear, lvbint) %>%
 mutate(
  total_year = n(),
  nbrst_rate = nbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nbrst %in% 1) %>%
 select(FiscalYear, nbrst, nbrst_count,nbrst_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  nbrst_delta = (nbrst_rate - lag(nbrst_rate))/lag(nbrst_rate),
  nbrst_deltap = (nbrst_rate - lag(nbrst_rate,9))/lag(nbrst_rate,9)
 )

c18 <- dta %>%
 group_by(FiscalYear, brstinit) %>%
 mutate(
  brstinit_count = n()
 ) %>%
 group_by(FiscalYear, lvb) %>%
 mutate(
  total_year = n(),
  brstinit_rate = brstinit_count/total_year
 ) %>%
 ungroup() %>%
 filter(brstinit %in% 1) %>%
 select(FiscalYear, brstinit, brstinit_count,brstinit_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  brstinit_delta = (brstinit_rate - lag(brstinit_rate))/lag(brstinit_rate),
  brstinit_deltap = (brstinit_rate - lag(brstinit_rate,9))/lag(brstinit_rate,9)
 )

fyearly_stats <- cbind(
 c1 %>% select(-PreExisting_Hypertension),
 c2 %>% select(-Gestational_Hypertension,-FiscalYear),
 c3 %>% select(-Any_Hypertension,-FiscalYear),
 c4 %>% select(-PreExisting_Diabetes,-FiscalYear),
 c5 %>% select(-GDM,-FiscalYear),
 c6 %>% select(-Any_Diabetes,-FiscalYear),
 c7 %>% select(-RobsnGrp,-FiscalYear),
 c8 %>% select(-RobsnGrp,-FiscalYear),
 c9 %>% select(-RobsnGrp,-FiscalYear),
 c10 %>% select(-ppreadm,-FiscalYear),
 c11 %>% select(-sknskn,-FiscalYear),
 c12 %>% select(-sknsknvg,-FiscalYear),
 c13 %>% select(-sknskncs,-FiscalYear),
 c14 %>% select(-neoreadm,-FiscalYear),
 c15 %>% select(-excbrst,-FiscalYear),
 c16 %>% select(-nexcbrst,-FiscalYear),
 c17 %>% select(-nbrst,-FiscalYear),
 c18 %>% select(-brstinit,-FiscalYear)
)

# Table stats ----
## Calendar year ----

ctab_stats <- cyearly_stats %>%
 filter(BrthYear %in% 2023) %>%
 select(BrthYear, ends_with(c("rate","deltap","delta"))) %>%
 tidyr::pivot_longer(
  ends_with(c("rate")),
  names_to = c("index_rate"),
  values_to = "rate"
 ) %>%
 tidyr::pivot_longer(
  ends_with(c("delta")),
  names_to = c("index_delta"),
  values_to = "delta"
 ) %>%
 tidyr::pivot_longer(
  ends_with(c("deltap")),
  names_to = c("index_delta10"),
  values_to = "delta10"
 ) %>%
 filter(gsub("_rate","",index_rate) == gsub("_deltap","",index_delta10),
        gsub("_rate","",index_rate) == gsub("_delta","",index_delta)) %>%
 select(-index_delta10, -index_delta) %>%
 mutate(
  name = case_when(
   startsWith(tolower(index_rate),"prehyp")  ~ "Pre-existing Hypertension",
   startsWith(tolower(index_rate),"gesthyp") ~ "Gestational Hypertension",
   startsWith(tolower(index_rate),"hyp") ~ "Any Hypertension<br>(Pre-existing/Gestational)",
   startsWith(tolower(index_rate),"prediab") ~ "Pre-existing Diabetes",
   startsWith(tolower(index_rate),"gestdiab") ~ "Gestational Diabetes",
   startsWith(tolower(index_rate),"diab") ~ "Any Diabetes<br>(Pre-existing/Gestational)",
   startsWith(tolower(index_rate),"rbs1") ~ "Robson Group 1",
   startsWith(tolower(index_rate),"rbs21") ~ "Robson Group 2a",
   startsWith(tolower(index_rate),"rbs51") ~ "Robson Group 5a",
   startsWith(tolower(index_rate),"ppreadm") ~ "Postpartum Readmission",
   startsWith(tolower(index_rate),"sknskn_rate") ~ "Skin to Skin",
   startsWith(tolower(index_rate),"sknsknvg_rate") ~ "Skin to Skin <br> following Vaginal birth",
   startsWith(tolower(index_rate),"sknskncs_rate") ~ "Skin to Skin  <br> following Caesarean birth",
   startsWith(tolower(index_rate),"neoreadm") ~ "Neonatal Readmission",
   startsWith(tolower(index_rate),"excbrst") ~ "Exclusive Breastfeeding",
   startsWith(tolower(index_rate),"nexcbrst") ~ "Non-exclusive Breastfeeding",
   startsWith(tolower(index_rate),"nbrst") ~ "No Breastfeeding",
   startsWith(tolower(index_rate),"brstinit") ~ "Breastfeeding Initiation"
  ),
  status = case_when(
   delta > 0 ~ "Increased",
   delta < 0 ~ "Decreased",
   delta == 0 ~ "Stable"
  )
 ) %>%
 relocate(name, .after = "BrthYear") %>%
 relocate(status, .after = "rate") %>%
 relocate(index_rate, .after = "delta10") %>%
 mutate(
  icon10_colors = case_when(
   grepl("^sknskn|excbrst|nexcbrst|brstinit",tolower(index_rate)) & delta10 > 0 ~ "#44AD99",
   grepl("^sknskn|excbrst|nexcbrst|brstinit",tolower(index_rate)) & delta10 < 0 ~ "#D9715F",
   !grepl("^sknskn|excbrst|nexcbrst|brstinit",tolower(index_rate)) & delta10 > 0 ~ "#D9715F",
   !grepl("^sknskn|excbrst|nexcbrst|brstinit",tolower(index_rate)) & delta10 < 0 ~ "#44AD99",
   delta10 == 0 ~ "#F2C577"
  ),
  icon10 = case_when(
   tolower(status) %in% "increased" ~ "circle-arrow-up",
   tolower(status) %in% "decreased" ~ "circle-arrow-down",
   TRUE ~ "-"
  ),
  icon_colors = case_when(
   grepl("^sknskn|excbrst|nexcbrst|brstinit",tolower(index_rate)) & delta > 0 ~ "#44AD99",
   grepl("^sknskn|excbrst|nexcbrst|brstinit",tolower(index_rate)) & delta < 0 ~ "#D9715F",
   !grepl("^sknskn|excbrst|nexcbrst|brstinit",tolower(index_rate)) & delta > 0 ~ "#D9715F",
   !grepl("^sknskn|excbrst|nexcbrst|brstinit",tolower(index_rate)) & delta < 0 ~ "#44AD99",
   delta10 == 0 ~ "#F2C577"
  )
  # grp = case_when(
  #  startsWith(tolower(index_rate),"prehyp")  ~ "Hypertension during pregnancy",
  #  startsWith(tolower(index_rate),"gesthyp") ~ "Hypertension during pregnancy",
  #  startsWith(tolower(index_rate),"hyp") ~ "Hypertension during pregnancy",
  #  startsWith(tolower(index_rate),"prediab") ~ "Diabetes during pregnancy",
  #  startsWith(tolower(index_rate),"gestdiab") ~ "Diabetes during pregnancy",
  #  startsWith(tolower(index_rate),"diab") ~ "Diabetes during pregnancy",
  #  startsWith(tolower(index_rate),"rbs1") ~ "Robson classification",
  #  startsWith(tolower(index_rate),"rbs21") ~ "Robson classification",
  #  startsWith(tolower(index_rate),"rbs51") ~ "Robson classification",
  #  startsWith(tolower(index_rate),"ppreadm") ~ "Maternal health",
  #  startsWith(tolower(index_rate),"sknskn_rate") ~ "Skin to Skin",
  #  startsWith(tolower(index_rate),"sknsknvg_rate") ~ "Skin to Skin",
  #  startsWith(tolower(index_rate),"sknskncs_rate") ~ "Skin to Skin",
  #  startsWith(tolower(index_rate),"neoreadm") ~ "Fetal and Infant health",
  #  startsWith(tolower(index_rate),"excbrst") ~ "Breastfeeding",
  #  startsWith(tolower(index_rate),"nexcbrst") ~ "Breastfeeding",
  #  startsWith(tolower(index_rate),"nbrst") ~ "Breastfeeding",
  #  startsWith(tolower(index_rate),"brstinit") ~ "Breastfeeding"
  # )
 )

## Fiscal year ----

ftab_stats <- fyearly_stats %>%
 filter(FiscalYear %in% "2023-2024") %>%
 select(FiscalYear, ends_with(c("rate","deltap","delta"))) %>%
 tidyr::pivot_longer(
  ends_with(c("rate")),
  names_to = c("index_rate"),
  values_to = "rate"
 ) %>%
 tidyr::pivot_longer(
  ends_with(c("delta")),
  names_to = c("index_delta"),
  values_to = "delta"
 ) %>%
 tidyr::pivot_longer(
  ends_with(c("deltap")),
  names_to = c("index_delta10"),
  values_to = "delta10"
 ) %>%
 filter(gsub("_rate","",index_rate) == gsub("_deltap","",index_delta10)) %>%
 select(-index_delta10) %>%
 mutate(
  name = case_when(
   startsWith(tolower(index_rate),"prehyp")  ~ "Pre-existing Hypertension",
   startsWith(tolower(index_rate),"gesthyp") ~ "Gestational Hypertension",
   startsWith(tolower(index_rate),"hyp") ~ "Any Hypertension<br>(Pre-existing/Gestational)",
   startsWith(tolower(index_rate),"prediab") ~ "Pre-existing Diabetes",
   startsWith(tolower(index_rate),"gestdiab") ~ "Gestational Diabetes",
   startsWith(tolower(index_rate),"diab") ~ "Any Diabetes<br>(Pre-existing/Gestational)",
   startsWith(tolower(index_rate),"rbs1") ~ "Robson Group 1",
   startsWith(tolower(index_rate),"rbs21") ~ "Robson Group 2a",
   startsWith(tolower(index_rate),"rbs51") ~ "Robson Group 5a",
   startsWith(tolower(index_rate),"ppreadm") ~ "Postpartum Readmission",
   startsWith(tolower(index_rate),"sknskn_rate") ~ "Skin to Skin",
   startsWith(tolower(index_rate),"sknsknvg_rate") ~ "Skin to Skin <br> following Vaginal birth",
   startsWith(tolower(index_rate),"sknskncs_rate") ~ "Skin to Skin  <br> following Caesarean birth",
   startsWith(tolower(index_rate),"neoreadm") ~ "Neonatal Readmission",
   startsWith(tolower(index_rate),"excbrst") ~ "Exclusive Breastfeeding",
   startsWith(tolower(index_rate),"nexcbrst") ~ "Non-exclusive Breastfeeding",
   startsWith(tolower(index_rate),"nbrst") ~ "No Breastfeeding",
   startsWith(tolower(index_rate),"brstinit") ~ "Breastfeeding Initiation"
  ),status = case_when(
   delta > 0 ~ "Increased",
   delta < 0 ~ "Decreased",
   delta == 0 ~ "Stable"
  )
 ) %>%
 relocate(name, .after = "FiscalYear") %>%
 relocate(status, .after = "rate") %>%
 relocate(index_rate, .after = "delta10") %>%
 mutate(
  icon10_colors = case_when(
   grepl("^sknskn|excbrst|nexcbrst|brstinit",tolower(index_rate)) & delta10 > 0 ~ "#44AD99",
   grepl("^sknskn|excbrst|nexcbrst|brstinit",tolower(index_rate)) & delta10 < 0 ~ "#D9715F",
   !grepl("^sknskn|excbrst|nexcbrst|brstinit",tolower(index_rate)) & delta10 > 0 ~ "#D9715F",
   !grepl("^sknskn|excbrst|nexcbrst|brstinit",tolower(index_rate)) & delta10 < 0 ~ "#44AD99",
   delta10 == 0 ~ "#F2C577"
  ),
  icon_colors = case_when(
   grepl("^sknskn|excbrst|nexcbrst|brstinit",tolower(index_rate)) & delta > 0 ~ "#44AD99",
   grepl("^sknskn|excbrst|nexcbrst|brstinit",tolower(index_rate)) & delta < 0 ~ "#D9715F",
   !grepl("^sknskn|excbrst|nexcbrst|brstinit",tolower(index_rate)) & delta > 0 ~ "#D9715F",
   !grepl("^sknskn|excbrst|nexcbrst|brstinit",tolower(index_rate)) & delta < 0 ~ "#44AD99",
   delta10 == 0 ~ "#F2C577"
  ),
  status = case_when(
   delta > 0 ~ "Increased",
   delta < 0 ~ "Decreased",
   delta == 0 ~ "Stable"
  )
  # grp = case_when(
  #  startsWith(tolower(index_rate),"prehyp")  ~ "Hypertension during pregnancy",
  #  startsWith(tolower(index_rate),"gesthyp") ~ "Hypertension during pregnancy",
  #  startsWith(tolower(index_rate),"hyp") ~ "Hypertension during pregnancy",
  #  startsWith(tolower(index_rate),"prediab") ~ "Diabetes during pregnancy",
  #  startsWith(tolower(index_rate),"gestdiab") ~ "Diabetes during pregnancy",
  #  startsWith(tolower(index_rate),"diab") ~ "Diabetes during pregnancy",
  #  startsWith(tolower(index_rate),"rbs1") ~ "Robson classification",
  #  startsWith(tolower(index_rate),"rbs21") ~ "Robson classification",
  #  startsWith(tolower(index_rate),"rbs51") ~ "Robson classification",
  #  startsWith(tolower(index_rate),"ppreadm") ~ "Maternal health",
  #  startsWith(tolower(index_rate),"sknskn_rate") ~ "Skin to Skin",
  #  startsWith(tolower(index_rate),"sknsknvg_rate") ~ "Skin to Skin",
  #  startsWith(tolower(index_rate),"sknskncs_rate") ~ "Skin to Skin",
  #  startsWith(tolower(index_rate),"neoreadm") ~ "Fetal and Infant health",
  #  startsWith(tolower(index_rate),"excbrst") ~ "Breastfeeding",
  #  startsWith(tolower(index_rate),"nexcbrst") ~ "Breastfeeding",
  #  startsWith(tolower(index_rate),"nbrst") ~ "Breastfeeding",
  #  startsWith(tolower(index_rate),"brstinit") ~ "Breastfeeding"
  # )
 )

# Table distribution ----
## Calendar year ----

ctab_dist <- cyearly_stats %>%
 select(BrthYear, ends_with(c("rate"))) %>%
 tidyr::pivot_longer(
  ends_with(c("rate")),
  names_to = c("index_rate"),
  values_to = "rate"
 )

## Fiscal year ----

ftab_dist <- fyearly_stats %>%
 select(FiscalYear, ends_with(c("rate"))) %>%
 tidyr::pivot_longer(
  ends_with(c("rate")),
  names_to = c("index_rate"),
  values_to = "rate"
 )

# Map stats ----
## Calendar year ----
### Census division (CD) ----
#### Hypertension ----

c1 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, PreExisting_Hypertension, CDuid) %>%
 distinct() %>%
 group_by(BrthYear, PreExisting_Hypertension, CDuid) %>%
 mutate(
  prehyp_count = n()
 ) %>%
 group_by(BrthYear, CDuid) %>%
 mutate(
  total_year = n(),
  prehyp_rate = prehyp_count/total_year
 ) %>%
 filter(PreExisting_Hypertension %in% 1,
        !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, PreExisting_Hypertension, prehyp_count, prehyp_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CDuid, PreExisting_Hypertension)) %>%
 group_by(CDuid) %>%
 mutate(
  prehyp_delta = (prehyp_rate - lag(prehyp_rate))/lag(prehyp_rate),
  prehyp_delta = ifelse(BrthYear == 2014, NA, prehyp_delta),
  prehyp_deltap = (prehyp_rate - lag(prehyp_rate,9))/lag(prehyp_rate,9)
 ) %>%
 ungroup()

c2 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, Gestational_Hypertension, CDuid) %>%
 distinct() %>%
 group_by(BrthYear, Gestational_Hypertension, CDuid) %>%
 mutate(
  gesthyp_count = n()
 ) %>%
 group_by(BrthYear, CDuid) %>%
 mutate(
  total_year = n(),
  gesthyp_rate = gesthyp_count/total_year
 ) %>%
 filter(Gestational_Hypertension %in% 1,
        !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, Gestational_Hypertension, gesthyp_count, gesthyp_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CDuid, Gestational_Hypertension)) %>%
 group_by(CDuid) %>%
 mutate(
  gesthyp_delta = (gesthyp_rate - lag(gesthyp_rate))/lag(gesthyp_rate),
  gesthyp_delta = ifelse(BrthYear == 2014, NA, gesthyp_delta),
  gesthyp_deltap = (gesthyp_rate - lag(gesthyp_rate,9))/lag(gesthyp_rate,9)
 ) %>%
 ungroup()

c3 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, Any_Hypertension, CDuid) %>%
 distinct() %>%
 group_by(BrthYear, Any_Hypertension, CDuid) %>%
 mutate(
  hyp_count = n()
 ) %>%
 group_by(BrthYear, CDuid) %>%
 mutate(
  total_year = n(),
  hyp_rate = hyp_count/total_year
 ) %>%
 filter(Any_Hypertension %in% 1,
        !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, Any_Hypertension, hyp_count, hyp_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CDuid, Any_Hypertension)) %>%
 group_by(CDuid) %>%
 mutate(
  hyp_delta = (hyp_rate - lag(hyp_rate))/lag(hyp_rate),
  hyp_delta = ifelse(BrthYear == 2014, NA, hyp_delta),
  hyp_deltap = (hyp_rate - lag(hyp_rate,9))/lag(hyp_rate,9)
 ) %>%
 ungroup()

#### Diabetes ----

c4 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, PreExisting_Diabetes, CDuid) %>%
 distinct() %>%
 group_by(BrthYear, PreExisting_Diabetes, CDuid) %>%
 mutate(
  prediab_count = n()
 ) %>%
 group_by(BrthYear, CDuid) %>%
 mutate(
  total_year = n(),
  prediab_rate = prediab_count/total_year
 ) %>%
 filter(PreExisting_Diabetes %in% 1,
        !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, PreExisting_Diabetes, prediab_count, prediab_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CDuid, PreExisting_Diabetes)) %>%
 group_by(CDuid) %>%
 mutate(
  prediab_delta = (prediab_rate - lag(prediab_rate))/lag(prediab_rate),
  prediab_delta = ifelse(BrthYear == 2014, NA, prediab_delta),
  prediab_deltap = (prediab_rate - lag(prediab_rate,9))/lag(prediab_rate,9)
 ) %>%
 ungroup()

c5 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, GDM, CDuid) %>%
 distinct() %>%
 group_by(BrthYear, GDM, CDuid) %>%
 mutate(
  gestdiab_count = n()
 ) %>%
 group_by(BrthYear, CDuid) %>%
 mutate(
  total_year = n(),
  gestdiab_rate = gestdiab_count/total_year
 ) %>%
 filter(GDM %in% 1,
        !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, GDM, gestdiab_count, gestdiab_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CDuid, GDM)) %>%
 group_by(CDuid) %>%
 mutate(
  gestdiab_delta = (gestdiab_rate - lag(gestdiab_rate))/lag(gestdiab_rate),
  gestdiab_delta = ifelse(BrthYear == 2014, NA, gestdiab_delta),
  gestdiab_deltap = (gestdiab_rate - lag(gestdiab_rate,9))/lag(gestdiab_rate,9)
 ) %>%
 ungroup()

c6 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, Any_Diabetes, CDuid) %>%
 distinct() %>%
 group_by(BrthYear, Any_Diabetes, CDuid) %>%
 mutate(
  diab_count = n()
 ) %>%
 group_by(BrthYear, CDuid) %>%
 mutate(
  total_year = n(),
  diab_rate = diab_count/total_year
 ) %>%
 filter(Any_Diabetes %in% 1,
        !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, Any_Diabetes, diab_count, diab_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CDuid, Any_Diabetes)) %>%
 group_by(CDuid) %>%
 mutate(
  diab_delta = (diab_rate - lag(diab_rate))/lag(diab_rate),
  diab_delta = ifelse(BrthYear == 2014, NA, diab_delta),
  diab_deltap = (diab_rate - lag(diab_rate,9))/lag(diab_rate,9)
 ) %>%
 ungroup()

#### Robson group ----

c7 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs1, RobsnGrp, CDuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs1, CDuid) %>%
 mutate(
  rbs1_count = n()
 ) %>%
 group_by(BrthYear, CDuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs1_rate = rbs1_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs1 %in% 1,
        RobsnGrp %in% 1,
        !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, rbs1, rbs1_count, rbs1_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CDuid, rbs1)) %>%
 group_by(CDuid) %>%
 mutate(
  rbs1_delta = (rbs1_rate - lag(rbs1_rate))/lag(rbs1_rate),
  rbs1_delta = ifelse(BrthYear == 2014, NA, rbs1_delta),
  rbs1_deltap = (rbs1_rate - lag(rbs1_rate,9))/lag(rbs1_rate,9)
 ) %>%
 ungroup()

c8 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs21, RobsnGrp, CDuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs21, CDuid) %>%
 mutate(
  rbs21_count = n()
 ) %>%
 group_by(BrthYear, CDuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs21_rate = rbs21_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs21 %in% 1,
        RobsnGrp %in% 2.1,
        !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, rbs21, rbs21_count, rbs21_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CDuid, rbs21)) %>%
 group_by(CDuid) %>%
 mutate(
  rbs21_delta = (rbs21_rate - lag(rbs21_rate))/lag(rbs21_rate),
  rbs21_delta = ifelse(BrthYear == 2014, NA, rbs21_delta),
  rbs21_deltap = (rbs21_rate - lag(rbs21_rate,9))/lag(rbs21_rate,9)
 ) %>%
 ungroup()

c9 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs51, RobsnGrp, CDuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs51, CDuid) %>%
 mutate(
  rbs51_count = n()
 ) %>%
 group_by(BrthYear, CDuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs51_rate = rbs51_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs51 %in% 1,
        RobsnGrp %in% 5.1,
        !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, rbs51, rbs51_count, rbs51_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CDuid, rbs51)) %>%
 group_by(CDuid) %>%
 mutate(
  rbs51_delta = (rbs51_rate - lag(rbs51_rate))/lag(rbs51_rate),
  rbs51_delta = ifelse(BrthYear == 2014, NA, rbs51_delta),
  rbs51_deltap = (rbs51_rate - lag(rbs51_rate,9))/lag(rbs51_rate,9)
 ) %>%
 ungroup()

#### Postpartum readmission ----

c10 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, ppreadm, CDuid) %>%
 distinct() %>%
 group_by(BrthYear, ppreadm, CDuid) %>%
 mutate(
  ppreadm_count = n()
 ) %>%
 group_by(BrthYear, CDuid) %>%
 mutate(
  total_year = n(),
  ppreadm_rate = ppreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(ppreadm %in% 1,
        !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, ppreadm, ppreadm_count, ppreadm_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CDuid, ppreadm)) %>%
 group_by(CDuid) %>%
 mutate(
  ppreadm_delta = (ppreadm_rate - lag(ppreadm_rate))/lag(ppreadm_rate),
  ppreadm_delta = ifelse(BrthYear == 2014, NA, ppreadm_delta),
  ppreadm_deltap = (ppreadm_rate - lag(ppreadm_rate,9))/lag(ppreadm_rate,9)
 ) %>%
 ungroup()

#### Skin to skin ----

c11 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknskn, CDuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, sknskn, CDuid) %>%
 mutate(
  sknskn_count = n()
 ) %>%
 group_by(BrthYear, CDuid, lvb) %>%
 mutate(
  total_year = n(),
  sknskn_rate = sknskn_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskn %in% 1,
        !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, sknskn, sknskn_count, sknskn_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CDuid, sknskn)) %>%
 group_by(CDuid) %>%
 mutate(
  sknskn_delta = (sknskn_rate - lag(sknskn_rate))/lag(sknskn_rate),
  sknskn_delta = ifelse(BrthYear == 2014, NA, sknskn_delta),
  sknskn_deltap = (sknskn_rate - lag(sknskn_rate,9))/lag(sknskn_rate,9)
 ) %>%
 ungroup()

c12 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknsknvg, CDuid, lvbvg) %>%
 distinct() %>%
 group_by(BrthYear, sknsknvg, CDuid) %>%
 mutate(
  sknsknvg_count = n()
 ) %>%
 group_by(BrthYear, CDuid, lvbvg) %>%
 mutate(
  total_year = n(),
  sknsknvg_rate = sknsknvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknsknvg %in% 1,
        !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, sknsknvg, sknsknvg_count, sknsknvg_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CDuid, sknsknvg)) %>%
 group_by(CDuid) %>%
 mutate(
  sknsknvg_delta = (sknsknvg_rate - lag(sknsknvg_rate))/lag(sknsknvg_rate),
  sknsknvg_delta = ifelse(BrthYear == 2014, NA, sknsknvg_delta),
  sknsknvg_deltap = (sknsknvg_rate - lag(sknsknvg_rate,9))/lag(sknsknvg_rate,9)
 ) %>%
 ungroup()

c13 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknskncs, CDuid, lvbcs) %>%
 distinct() %>%
 group_by(BrthYear, sknskncs, CDuid) %>%
 mutate(
  sknskncs_count = n()
 ) %>%
 group_by(BrthYear, CDuid, lvbcs) %>%
 mutate(
  total_year = n(),
  sknskncs_rate = sknskncs_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskncs %in% 1,
        !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, sknskncs, sknskncs_count, sknskncs_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CDuid, sknskncs)) %>%
 group_by(CDuid) %>%
 mutate(
  sknskncs_delta = (sknskncs_rate - lag(sknskncs_rate))/lag(sknskncs_rate),
  sknskncs_delta = ifelse(BrthYear == 2014, NA, sknskncs_delta),
  sknskncs_deltap = (sknskncs_rate - lag(sknskncs_rate,9))/lag(sknskncs_rate,9)
 ) %>%
 ungroup()

#### Neonatal readmission ----

c14 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, neoreadm, CDuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, neoreadm, CDuid) %>%
 mutate(
  neoreadm_count = n()
 ) %>%
 group_by(BrthYear, CDuid, lvb) %>%
 mutate(
  total_year = n(),
  neoreadm_rate = neoreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(neoreadm %in% 1,
        !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, neoreadm, neoreadm_count, neoreadm_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CDuid, neoreadm)) %>%
 group_by(CDuid) %>%
 mutate(
  neoreadm_delta = (neoreadm_rate - lag(neoreadm_rate))/lag(neoreadm_rate),
  neoreadm_delta = ifelse(BrthYear == 2014, NA, neoreadm_delta),
  neoreadm_deltap = (neoreadm_rate - lag(neoreadm_rate,9))/lag(neoreadm_rate,9)
 ) %>%
 ungroup()

#### Milk feeding ----

c15 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, excbrst, CDuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, excbrst, CDuid) %>%
 mutate(
  excbrst_count = n()
 ) %>%
 group_by(BrthYear, CDuid, lvb) %>%
 mutate(
  total_year = n(),
  excbrst_rate = excbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(excbrst %in% 1,
        !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, excbrst, excbrst_count, excbrst_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CDuid, excbrst)) %>%
 group_by(CDuid) %>%
 mutate(
  excbrst_delta = (excbrst_rate - lag(excbrst_rate))/lag(excbrst_rate),
  excbrst_delta = ifelse(BrthYear == 2014, NA, excbrst_delta),
  excbrst_deltap = (excbrst_rate - lag(excbrst_rate,9))/lag(excbrst_rate,9)
 ) %>%
 ungroup()

c16 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, nexcbrst, CDuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, nexcbrst, CDuid) %>%
 mutate(
  nexcbrst_count = n()
 ) %>%
 group_by(BrthYear, CDuid, lvb) %>%
 mutate(
  total_year = n(),
  nexcbrst_rate = nexcbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nexcbrst %in% 1,
        !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, nexcbrst, nexcbrst_count, nexcbrst_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CDuid, nexcbrst)) %>%
 group_by(CDuid) %>%
 mutate(
  nexcbrst_delta = (nexcbrst_rate - lag(nexcbrst_rate))/lag(nexcbrst_rate),
  nexcbrst_delta = ifelse(BrthYear == 2014, NA, nexcbrst_delta),
  nexcbrst_deltap = (nexcbrst_rate - lag(nexcbrst_rate,9))/lag(nexcbrst_rate,9)
 ) %>%
 ungroup()

c17 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, nbrst, CDuid, lvbint) %>%
 distinct() %>%
 group_by(BrthYear, nbrst, CDuid) %>%
 mutate(
  nbrst_count = n()
 ) %>%
 group_by(BrthYear, CDuid, lvbint) %>%
 mutate(
  total_year = n(),
  nbrst_rate = nbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nbrst %in% 1,
        !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, nbrst, nbrst_count, nbrst_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CDuid, nbrst)) %>%
 group_by(CDuid) %>%
 mutate(
  nbrst_delta = (nbrst_rate - lag(nbrst_rate))/lag(nbrst_rate),
  nbrst_delta = ifelse(BrthYear == 2014, NA, nbrst_delta),
  nbrst_deltap = (nbrst_rate - lag(nbrst_rate,9))/lag(nbrst_rate,9)
 ) %>%
 ungroup()

c18 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, brstinit, CDuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, brstinit, CDuid) %>%
 mutate(
  brstinit_count = n()
 ) %>%
 group_by(BrthYear, CDuid, lvb) %>%
 mutate(
  total_year = n(),
  brstinit_rate = brstinit_count/total_year
 ) %>%
 ungroup() %>%
 filter(brstinit %in% 1,
        !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, brstinit, brstinit_count, brstinit_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CDuid, brstinit)) %>%
 group_by(CDuid) %>%
 mutate(
  brstinit_delta = (brstinit_rate - lag(brstinit_rate))/lag(brstinit_rate),
  brstinit_delta = ifelse(BrthYear == 2014, NA, brstinit_delta),
  brstinit_deltap = (brstinit_rate - lag(brstinit_rate,9))/lag(brstinit_rate,9)
 ) %>%
 ungroup()

ccd_stats <- cbind(
 c1 %>% select(-PreExisting_Hypertension),
 c2 %>% select(-Gestational_Hypertension,-BrthYear, -CDuid),
 c3 %>% select(-Any_Hypertension,-BrthYear, -CDuid),
 c4 %>% select(-PreExisting_Diabetes,-BrthYear, -CDuid),
 c5 %>% select(-GDM,-BrthYear, -CDuid),
 c6 %>% select(-Any_Diabetes,-BrthYear, -CDuid),
 c7 %>% select(-rbs1,-BrthYear, -CDuid),
 c8 %>% select(-rbs21,-BrthYear, -CDuid),
 c9 %>% select(-rbs51,-BrthYear, -CDuid),
 c10 %>% select(-ppreadm,-BrthYear, -CDuid),
 c11 %>% select(-sknskn,-BrthYear, -CDuid),
 c12 %>% select(-sknsknvg,-BrthYear, -CDuid),
 c13 %>% select(-sknskncs,-BrthYear, -CDuid),
 c14 %>% select(-neoreadm,-BrthYear, -CDuid),
 c15 %>% select(-excbrst,-BrthYear, -CDuid),
 c16 %>% select(-nexcbrst,-BrthYear, -CDuid),
 c17 %>% select(-nbrst,-BrthYear, -CDuid),
 c18 %>% select(-brstinit,-BrthYear, -CDuid)
)

### Community cluster (CL) ----
#### Hypertension ----

c1 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, PreExisting_Hypertension, CLuid) %>%
 distinct() %>%
 group_by(BrthYear, PreExisting_Hypertension, CLuid) %>%
 mutate(
  prehyp_count = n()
 ) %>%
 group_by(BrthYear, CLuid) %>%
 mutate(
  total_year = n(),
  prehyp_rate = prehyp_count/total_year
 ) %>%
 filter(PreExisting_Hypertension %in% 1,
        !is.na(CLuid)
        ) %>%
 select(BrthYear, CLuid, PreExisting_Hypertension, prehyp_count, prehyp_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CLuid, PreExisting_Hypertension)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(BrthYear, PreExisting_Hypertension)) %>%
 group_by(CLuid) %>%
 mutate(
  prehyp_delta = (prehyp_rate - lag(prehyp_rate))/lag(prehyp_rate),
  prehyp_delta = ifelse(BrthYear == 2014, NA, prehyp_delta),
  prehyp_deltap = (prehyp_rate - lag(prehyp_rate,9))/lag(prehyp_rate,9)
 ) %>%
 ungroup()

c2 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, Gestational_Hypertension, CLuid) %>%
 distinct() %>%
 group_by(BrthYear, Gestational_Hypertension, CLuid) %>%
 mutate(
  gesthyp_count = n()
 ) %>%
 group_by(BrthYear, CLuid) %>%
 mutate(
  total_year = n(),
  gesthyp_rate = gesthyp_count/total_year
 ) %>%
 filter(Gestational_Hypertension %in% 1,
        !is.na(CLuid)
        ) %>%
 select(BrthYear, CLuid, Gestational_Hypertension, gesthyp_count, gesthyp_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CLuid, Gestational_Hypertension)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(BrthYear, Gestational_Hypertension)) %>%
 group_by(CLuid) %>%
 mutate(
  gesthyp_delta = (gesthyp_rate - lag(gesthyp_rate))/lag(gesthyp_rate),
  gesthyp_delta = ifelse(BrthYear == 2014, NA, gesthyp_delta),
  gesthyp_deltap = (gesthyp_rate - lag(gesthyp_rate,9))/lag(gesthyp_rate,9)
 ) %>%
 ungroup()

c3 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, Any_Hypertension, CLuid) %>%
 distinct() %>%
 group_by(BrthYear, Any_Hypertension, CLuid) %>%
 mutate(
  hyp_count = n()
 ) %>%
 group_by(BrthYear, CLuid) %>%
 mutate(
  total_year = n(),
  hyp_rate = hyp_count/total_year
 ) %>%
 filter(Any_Hypertension %in% 1,
        !is.na(CLuid)
        ) %>%
 select(BrthYear, CLuid, Any_Hypertension, hyp_count, hyp_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CLuid, Any_Hypertension)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(BrthYear, Any_Hypertension)) %>%
 group_by(CLuid) %>%
 mutate(
  hyp_delta = (hyp_rate - lag(hyp_rate))/lag(hyp_rate),
  hyp_delta = ifelse(BrthYear == 2014, NA, hyp_delta),
  hyp_deltap = (hyp_rate - lag(hyp_rate,9))/lag(hyp_rate,9)
 ) %>%
 ungroup()

#### Diabetes ----

c4 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, PreExisting_Diabetes, CLuid) %>%
 distinct() %>%
 group_by(BrthYear, PreExisting_Diabetes, CLuid) %>%
 mutate(
  prediab_count = n()
 ) %>%
 group_by(BrthYear, CLuid) %>%
 mutate(
  total_year = n(),
  prediab_rate = prediab_count/total_year
 ) %>%
 filter(PreExisting_Diabetes %in% 1,
        !is.na(CLuid)
        ) %>%
 select(BrthYear, CLuid, PreExisting_Diabetes, prediab_count, prediab_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CLuid, PreExisting_Diabetes)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(BrthYear, PreExisting_Diabetes)) %>%
 group_by(CLuid) %>%
 mutate(
  prediab_delta = (prediab_rate - lag(prediab_rate))/lag(prediab_rate),
  prediab_delta = ifelse(BrthYear == 2014, NA, prediab_delta),
  prediab_deltap = (prediab_rate - lag(prediab_rate,9))/lag(prediab_rate,9)
 ) %>%
 ungroup()

c5 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, GDM, CLuid) %>%
 distinct() %>%
 group_by(BrthYear, GDM, CLuid) %>%
 mutate(
  gestdiab_count = n()
 ) %>%
 group_by(BrthYear, CLuid) %>%
 mutate(
  total_year = n(),
  gestdiab_rate = gestdiab_count/total_year
 ) %>%
 filter(GDM %in% 1,
        !is.na(CLuid)
        ) %>%
 select(BrthYear, CLuid, GDM, gestdiab_count, gestdiab_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CLuid, GDM)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(BrthYear, GDM)) %>%
 group_by(CLuid) %>%
 mutate(
  gestdiab_delta = (gestdiab_rate - lag(gestdiab_rate))/lag(gestdiab_rate),
  gestdiab_delta = ifelse(BrthYear == 2014, NA, gestdiab_delta),
  gestdiab_deltap = (gestdiab_rate - lag(gestdiab_rate,9))/lag(gestdiab_rate,9)
 ) %>%
 ungroup()

c6 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, Any_Diabetes, CLuid) %>%
 distinct() %>%
 group_by(BrthYear, Any_Diabetes, CLuid) %>%
 mutate(
  diab_count = n()
 ) %>%
 group_by(BrthYear, CLuid) %>%
 mutate(
  total_year = n(),
  diab_rate = diab_count/total_year
 ) %>%
 filter(Any_Diabetes %in% 1,
        !is.na(CLuid)
        ) %>%
 select(BrthYear, CLuid, Any_Diabetes, diab_count, diab_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CLuid, Any_Diabetes)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(BrthYear, Any_Diabetes)) %>%
 group_by(CLuid) %>%
 mutate(
  diab_delta = (diab_rate - lag(diab_rate))/lag(diab_rate),
  diab_delta = ifelse(BrthYear == 2014, NA, diab_delta),
  diab_deltap = (diab_rate - lag(diab_rate,9))/lag(diab_rate,9)
 ) %>%
 ungroup()

#### Robson group ----

c7 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs1, RobsnGrp, CLuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs1, CLuid) %>%
 mutate(
  rbs1_count = n()
 ) %>%
 group_by(BrthYear, CLuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs1_rate = rbs1_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs1 %in% 1,
        RobsnGrp %in% 1,
        !is.na(CLuid)
        ) %>%
 select(BrthYear, CLuid, rbs1, rbs1_count, rbs1_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CLuid, rbs1)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(BrthYear, rbs1)) %>%
 group_by(CLuid) %>%
 mutate(
  rbs1_delta = (rbs1_rate - lag(rbs1_rate))/lag(rbs1_rate),
  rbs1_delta = ifelse(BrthYear == 2014, NA, rbs1_delta),
  rbs1_deltap = (rbs1_rate - lag(rbs1_rate,9))/lag(rbs1_rate,9)
 ) %>%
 ungroup()

c8 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs21, RobsnGrp, CLuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs21, CLuid) %>%
 mutate(
  rbs21_count = n()
 ) %>%
 group_by(BrthYear, CLuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs21_rate = rbs21_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs21 %in% 1,
        RobsnGrp %in% 2.1,
        !is.na(CLuid)
        ) %>%
 select(BrthYear, CLuid, rbs21, rbs21_count, rbs21_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CLuid, rbs21)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(BrthYear, rbs21)) %>%
 group_by(CLuid) %>%
 mutate(
  rbs21_delta = (rbs21_rate - lag(rbs21_rate))/lag(rbs21_rate),
  rbs21_delta = ifelse(BrthYear == 2014, NA, rbs21_delta),
  rbs21_deltap = (rbs21_rate - lag(rbs21_rate,9))/lag(rbs21_rate,9)
 ) %>%
 ungroup()

c9 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs51, RobsnGrp, CLuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs51, CLuid) %>%
 mutate(
  rbs51_count = n()
 ) %>%
 group_by(BrthYear, CLuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs51_rate = rbs51_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs51 %in% 1,
        RobsnGrp %in% 5.1,
        !is.na(CLuid)
        ) %>%
 select(BrthYear, CLuid, rbs51, rbs51_count, rbs51_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CLuid, rbs51)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(BrthYear, rbs51)) %>%
 group_by(CLuid) %>%
 mutate(
  rbs51_delta = (rbs51_rate - lag(rbs51_rate))/lag(rbs51_rate),
  rbs51_delta = ifelse(BrthYear == 2014, NA, rbs51_delta),
  rbs51_deltap = (rbs51_rate - lag(rbs51_rate,9))/lag(rbs51_rate,9)
 ) %>%
 ungroup()

#### Postpartum readmission ----

c10 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, ppreadm, CLuid) %>%
 distinct() %>%
 group_by(BrthYear, ppreadm, CLuid) %>%
 mutate(
  ppreadm_count = n()
 ) %>%
 group_by(BrthYear, CLuid) %>%
 mutate(
  total_year = n(),
  ppreadm_rate = ppreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(ppreadm %in% 1,
        !is.na(CLuid)
        ) %>%
 select(BrthYear, CLuid, ppreadm, ppreadm_count, ppreadm_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CLuid, ppreadm)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(BrthYear, ppreadm)) %>%
 group_by(CLuid) %>%
 mutate(
  ppreadm_delta = (ppreadm_rate - lag(ppreadm_rate))/lag(ppreadm_rate),
  ppreadm_delta = ifelse(BrthYear == 2014, NA, ppreadm_delta),
  ppreadm_deltap = (ppreadm_rate - lag(ppreadm_rate,9))/lag(ppreadm_rate,9)
 ) %>%
 ungroup()

#### Skin to skin ----

c11 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknskn, CLuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, sknskn, CLuid) %>%
 mutate(
  sknskn_count = n()
 ) %>%
 group_by(BrthYear, CLuid, lvb) %>%
 mutate(
  total_year = n(),
  sknskn_rate = sknskn_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskn %in% 1,
        !is.na(CLuid)
        ) %>%
 select(BrthYear, CLuid, sknskn, sknskn_count, sknskn_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CLuid, sknskn)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(BrthYear, sknskn)) %>%
 group_by(CLuid) %>%
 mutate(
  sknskn_delta = (sknskn_rate - lag(sknskn_rate))/lag(sknskn_rate),
  sknskn_delta = ifelse(BrthYear == 2014, NA, sknskn_delta),
  sknskn_deltap = (sknskn_rate - lag(sknskn_rate,9))/lag(sknskn_rate,9)
 ) %>%
 ungroup()

c12 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknsknvg, CLuid, lvbvg) %>%
 distinct() %>%
 group_by(BrthYear, sknsknvg, CLuid) %>%
 mutate(
  sknsknvg_count = n()
 ) %>%
 group_by(BrthYear, CLuid, lvbvg) %>%
 mutate(
  total_year = n(),
  sknsknvg_rate = sknsknvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknsknvg %in% 1,
        !is.na(CLuid)
        ) %>%
 select(BrthYear, CLuid, sknsknvg, sknsknvg_count, sknsknvg_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CLuid, sknsknvg)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(BrthYear, sknsknvg)) %>%
 group_by(CLuid) %>%
 mutate(
  sknsknvg_delta = (sknsknvg_rate - lag(sknsknvg_rate))/lag(sknsknvg_rate),
  sknsknvg_delta = ifelse(BrthYear == 2014, NA, sknsknvg_delta),
  sknsknvg_deltap = (sknsknvg_rate - lag(sknsknvg_rate,9))/lag(sknsknvg_rate,9)
 ) %>%
 ungroup()

c13 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknskncs, CLuid, lvbcs) %>%
 distinct() %>%
 group_by(BrthYear, sknskncs, CLuid) %>%
 mutate(
  sknskncs_count = n()
 ) %>%
 group_by(BrthYear, CLuid, lvbcs) %>%
 mutate(
  total_year = n(),
  sknskncs_rate = sknskncs_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskncs %in% 1,
        !is.na(CLuid)
        ) %>%
 select(BrthYear, CLuid, sknskncs, sknskncs_count, sknskncs_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CLuid, sknskncs)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(BrthYear, sknskncs)) %>%
 group_by(CLuid) %>%
 mutate(
  sknskncs_delta = (sknskncs_rate - lag(sknskncs_rate))/lag(sknskncs_rate),
  sknskncs_delta = ifelse(BrthYear == 2014, NA, sknskncs_delta),
  sknskncs_deltap = (sknskncs_rate - lag(sknskncs_rate,9))/lag(sknskncs_rate,9)
 ) %>%
 ungroup()

#### Neonatal readmission ----

c14 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, neoreadm, CLuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, neoreadm, CLuid) %>%
 mutate(
  neoreadm_count = n()
 ) %>%
 group_by(BrthYear, CLuid, lvb) %>%
 mutate(
  total_year = n(),
  neoreadm_rate = neoreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(neoreadm %in% 1,
        !is.na(CLuid)
        ) %>%
 select(BrthYear, CLuid, neoreadm, neoreadm_count, neoreadm_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CLuid, neoreadm)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(BrthYear, neoreadm)) %>%
 group_by(CLuid) %>%
 mutate(
  neoreadm_delta = (neoreadm_rate - lag(neoreadm_rate))/lag(neoreadm_rate),
  neoreadm_delta = ifelse(BrthYear == 2014, NA, neoreadm_delta),
  neoreadm_deltap = (neoreadm_rate - lag(neoreadm_rate,9))/lag(neoreadm_rate,9)
 ) %>%
 ungroup()

#### Milk feeding ----

c15 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, excbrst, CLuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, excbrst, CLuid) %>%
 mutate(
  excbrst_count = n()
 ) %>%
 group_by(BrthYear, CLuid, lvb) %>%
 mutate(
  total_year = n(),
  excbrst_rate = excbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(excbrst %in% 1,
        !is.na(CLuid)
        ) %>%
 select(BrthYear, CLuid, excbrst, excbrst_count, excbrst_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CLuid, excbrst)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(BrthYear, excbrst)) %>%
 group_by(CLuid) %>%
 mutate(
  excbrst_delta = (excbrst_rate - lag(excbrst_rate))/lag(excbrst_rate),
  excbrst_delta = ifelse(BrthYear == 2014, NA, excbrst_delta),
  excbrst_deltap = (excbrst_rate - lag(excbrst_rate,9))/lag(excbrst_rate,9)
 ) %>%
 ungroup()

c16 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, nexcbrst, CLuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, nexcbrst, CLuid) %>%
 mutate(
  nexcbrst_count = n()
 ) %>%
 group_by(BrthYear, CLuid, lvb) %>%
 mutate(
  total_year = n(),
  nexcbrst_rate = nexcbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nexcbrst %in% 1,
        !is.na(CLuid)
        ) %>%
 select(BrthYear, CLuid, nexcbrst, nexcbrst_count, nexcbrst_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CLuid, nexcbrst)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(BrthYear, nexcbrst)) %>%
 group_by(CLuid) %>%
 mutate(
  nexcbrst_delta = (nexcbrst_rate - lag(nexcbrst_rate))/lag(nexcbrst_rate),
  nexcbrst_delta = ifelse(BrthYear == 2014, NA, nexcbrst_delta),
  nexcbrst_deltap = (nexcbrst_rate - lag(nexcbrst_rate,9))/lag(nexcbrst_rate,9)
 ) %>%
 ungroup()

c17 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, nbrst, CLuid, lvbint) %>%
 distinct() %>%
 group_by(BrthYear, nbrst, CLuid) %>%
 mutate(
  nbrst_count = n()
 ) %>%
 group_by(BrthYear, CLuid, lvbint) %>%
 mutate(
  total_year = n(),
  nbrst_rate = nbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nbrst %in% 1,
        !is.na(CLuid)
        ) %>%
 select(BrthYear, CLuid, nbrst, nbrst_count, nbrst_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CLuid, nbrst)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(BrthYear, nbrst)) %>%
 group_by(CLuid) %>%
 mutate(
  nbrst_delta = (nbrst_rate - lag(nbrst_rate))/lag(nbrst_rate),
  nbrst_delta = ifelse(BrthYear == 2014, NA, nbrst_delta),
  nbrst_deltap = (nbrst_rate - lag(nbrst_rate,9))/lag(nbrst_rate,9)
 ) %>%
 ungroup()

c18 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, brstinit, CLuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, brstinit, CLuid) %>%
 mutate(
  brstinit_count = n()
 ) %>%
 group_by(BrthYear, CLuid, lvb) %>%
 mutate(
  total_year = n(),
  brstinit_rate = brstinit_count/total_year
 ) %>%
 ungroup() %>%
 filter(brstinit %in% 1,
        !is.na(CLuid)
        ) %>%
 select(BrthYear, CLuid, brstinit, brstinit_count, brstinit_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CLuid, brstinit)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(BrthYear, brstinit)) %>%
 group_by(CLuid) %>%
 mutate(
  brstinit_delta = (brstinit_rate - lag(brstinit_rate))/lag(brstinit_rate),
  brstinit_delta = ifelse(BrthYear == 2014, NA, brstinit_delta),
  brstinit_deltap = (brstinit_rate - lag(brstinit_rate,9))/lag(brstinit_rate,9)
 ) %>%
 ungroup()

ccl_stats <- cbind(
 c1 %>% select(-PreExisting_Hypertension),
 c2 %>% select(-Gestational_Hypertension,-BrthYear, -CLuid),
 c3 %>% select(-Any_Hypertension,-BrthYear, -CLuid),
 c4 %>% select(-PreExisting_Diabetes,-BrthYear, -CLuid),
 c5 %>% select(-GDM,-BrthYear, -CLuid),
 c6 %>% select(-Any_Diabetes,-BrthYear, -CLuid),
 c7 %>% select(-rbs1,-BrthYear, -CLuid),
 c8 %>% select(-rbs21,-BrthYear, -CLuid),
 c9 %>% select(-rbs51,-BrthYear, -CLuid),
 c10 %>% select(-ppreadm,-BrthYear, -CLuid),
 c11 %>% select(-sknskn,-BrthYear, -CLuid),
 c12 %>% select(-sknsknvg,-BrthYear, -CLuid),
 c13 %>% select(-sknskncs,-BrthYear, -CLuid),
 c14 %>% select(-neoreadm,-BrthYear, -CLuid),
 c15 %>% select(-excbrst,-BrthYear, -CLuid),
 c16 %>% select(-nexcbrst,-BrthYear, -CLuid),
 c17 %>% select(-nbrst,-BrthYear, -CLuid),
 c18 %>% select(-brstinit,-BrthYear, -CLuid)
)

### Community health network (CHN) ----
#### Hypertension ----

c1 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, PreExisting_Hypertension, CHNuid) %>%
 distinct() %>%
 group_by(BrthYear, PreExisting_Hypertension, CHNuid) %>%
 mutate(
  prehyp_count = n()
 ) %>%
 group_by(BrthYear, CHNuid) %>%
 mutate(
  total_year = n(),
  prehyp_rate = prehyp_count/total_year
 ) %>%
 filter(PreExisting_Hypertension %in% 1,
        !is.na(CHNuid)
        ) %>%
 select(BrthYear, CHNuid, PreExisting_Hypertension, prehyp_count, prehyp_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CHNuid, PreExisting_Hypertension)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(BrthYear, PreExisting_Hypertension)) %>%
 group_by(CHNuid) %>%
 mutate(
  prehyp_delta = (prehyp_rate - lag(prehyp_rate))/lag(prehyp_rate),
  prehyp_delta = ifelse(BrthYear == 2014, NA, prehyp_delta),
  prehyp_deltap = (prehyp_rate - lag(prehyp_rate,9))/lag(prehyp_rate,9)
 ) %>%
 ungroup()

c2 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, Gestational_Hypertension, CHNuid) %>%
 distinct() %>%
 group_by(BrthYear, Gestational_Hypertension, CHNuid) %>%
 mutate(
  gesthyp_count = n()
 ) %>%
 group_by(BrthYear, CHNuid) %>%
 mutate(
  total_year = n(),
  gesthyp_rate = gesthyp_count/total_year
 ) %>%
 filter(Gestational_Hypertension %in% 1,
        !is.na(CHNuid)
        ) %>%
 select(BrthYear, CHNuid, Gestational_Hypertension, gesthyp_count, gesthyp_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CHNuid, Gestational_Hypertension)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(BrthYear, Gestational_Hypertension)) %>%
 group_by(CHNuid) %>%
 mutate(
  gesthyp_delta = (gesthyp_rate - lag(gesthyp_rate))/lag(gesthyp_rate),
  gesthyp_delta = ifelse(BrthYear == 2014, NA, gesthyp_delta),
  gesthyp_deltap = (gesthyp_rate - lag(gesthyp_rate,9))/lag(gesthyp_rate,9)
 ) %>%
 ungroup()

c3 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, Any_Hypertension, CHNuid) %>%
 distinct() %>%
 group_by(BrthYear, Any_Hypertension, CHNuid) %>%
 mutate(
  hyp_count = n()
 ) %>%
 group_by(BrthYear, CHNuid) %>%
 mutate(
  total_year = n(),
  hyp_rate = hyp_count/total_year
 ) %>%
 filter(Any_Hypertension %in% 1,
        !is.na(CHNuid)
        ) %>%
 select(BrthYear, CHNuid, Any_Hypertension, hyp_count, hyp_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CHNuid, Any_Hypertension)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(BrthYear, Any_Hypertension)) %>%
 group_by(CHNuid) %>%
 mutate(
  hyp_delta = (hyp_rate - lag(hyp_rate))/lag(hyp_rate),
  hyp_delta = ifelse(BrthYear == 2014, NA, hyp_delta),
  hyp_deltap = (hyp_rate - lag(hyp_rate,9))/lag(hyp_rate,9)
 ) %>%
 ungroup()

#### Diabetes ----

c4 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, PreExisting_Diabetes, CHNuid) %>%
 distinct() %>%
 group_by(BrthYear, PreExisting_Diabetes, CHNuid) %>%
 mutate(
  prediab_count = n()
 ) %>%
 group_by(BrthYear, CHNuid) %>%
 mutate(
  total_year = n(),
  prediab_rate = prediab_count/total_year
 ) %>%
 filter(PreExisting_Diabetes %in% 1,
        !is.na(CHNuid)
        ) %>%
 select(BrthYear, CHNuid, PreExisting_Diabetes, prediab_count, prediab_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CHNuid, PreExisting_Diabetes)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(BrthYear, PreExisting_Diabetes)) %>%
 group_by(CHNuid) %>%
 mutate(
  prediab_delta = (prediab_rate - lag(prediab_rate))/lag(prediab_rate),
  prediab_delta = ifelse(BrthYear == 2014, NA, prediab_delta),
  prediab_deltap = (prediab_rate - lag(prediab_rate,9))/lag(prediab_rate,9)
 ) %>%
 ungroup()

c5 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, GDM, CHNuid) %>%
 distinct() %>%
 group_by(BrthYear, GDM, CHNuid) %>%
 mutate(
  gestdiab_count = n()
 ) %>%
 group_by(BrthYear, CHNuid) %>%
 mutate(
  total_year = n(),
  gestdiab_rate = gestdiab_count/total_year
 ) %>%
 filter(GDM %in% 1,
        !is.na(CHNuid)
        ) %>%
 select(BrthYear, CHNuid, GDM, gestdiab_count, gestdiab_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CHNuid, GDM)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(BrthYear, GDM)) %>%
 group_by(CHNuid) %>%
 mutate(
  gestdiab_delta = (gestdiab_rate - lag(gestdiab_rate))/lag(gestdiab_rate),
  gestdiab_delta = ifelse(BrthYear == 2014, NA, gestdiab_delta),
  gestdiab_deltap = (gestdiab_rate - lag(gestdiab_rate,9))/lag(gestdiab_rate,9)
 ) %>%
 ungroup()

c6 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, Any_Diabetes, CHNuid) %>%
 distinct() %>%
 group_by(BrthYear, Any_Diabetes, CHNuid) %>%
 mutate(
  diab_count = n()
 ) %>%
 group_by(BrthYear, CHNuid) %>%
 mutate(
  total_year = n(),
  diab_rate = diab_count/total_year
 ) %>%
 filter(Any_Diabetes %in% 1,
        !is.na(CHNuid)
        ) %>%
 select(BrthYear, CHNuid, Any_Diabetes, diab_count, diab_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CHNuid, Any_Diabetes)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(BrthYear, Any_Diabetes)) %>%
 group_by(CHNuid) %>%
 mutate(
  diab_delta = (diab_rate - lag(diab_rate))/lag(diab_rate),
  diab_delta = ifelse(BrthYear == 2014, NA, diab_delta),
  diab_deltap = (diab_rate - lag(diab_rate,9))/lag(diab_rate,9)
 ) %>%
 ungroup()

#### Robson group ----

c7 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs1, RobsnGrp, CHNuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs1, CHNuid) %>%
 mutate(
  rbs1_count = n()
 ) %>%
 group_by(BrthYear, CHNuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs1_rate = rbs1_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs1 %in% 1,
        RobsnGrp %in% 1,
        !is.na(CHNuid)
        ) %>%
 select(BrthYear, CHNuid, rbs1, rbs1_count, rbs1_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CHNuid, rbs1)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(BrthYear, rbs1)) %>%
 group_by(CHNuid) %>%
 mutate(
  rbs1_delta = (rbs1_rate - lag(rbs1_rate))/lag(rbs1_rate),
  rbs1_delta = ifelse(BrthYear == 2014, NA, rbs1_delta),
  rbs1_deltap = (rbs1_rate - lag(rbs1_rate,9))/lag(rbs1_rate,9)
 ) %>%
 ungroup()

c8 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs21, RobsnGrp, CHNuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs21, CHNuid) %>%
 mutate(
  rbs21_count = n()
 ) %>%
 group_by(BrthYear, CHNuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs21_rate = rbs21_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs21 %in% 1,
        RobsnGrp %in% 2.1,
        !is.na(CHNuid)
        ) %>%
 select(BrthYear, CHNuid, rbs21, rbs21_count, rbs21_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CHNuid, rbs21)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(BrthYear, rbs21)) %>%
 group_by(CHNuid) %>%
 mutate(
  rbs21_delta = (rbs21_rate - lag(rbs21_rate))/lag(rbs21_rate),
  rbs21_delta = ifelse(BrthYear == 2014, NA, rbs21_delta),
  rbs21_deltap = (rbs21_rate - lag(rbs21_rate,9))/lag(rbs21_rate,9)
 ) %>%
 ungroup()

c9 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs51, RobsnGrp, CHNuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs51, CHNuid) %>%
 mutate(
  rbs51_count = n()
 ) %>%
 group_by(BrthYear, CHNuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs51_rate = rbs51_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs51 %in% 1,
        RobsnGrp %in% 5.1,
        !is.na(CHNuid)
        ) %>%
 select(BrthYear, CHNuid, rbs51, rbs51_count, rbs51_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CHNuid, rbs51)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(BrthYear, rbs51)) %>%
 group_by(CHNuid) %>%
 mutate(
  rbs51_delta = (rbs51_rate - lag(rbs51_rate))/lag(rbs51_rate),
  rbs51_delta = ifelse(BrthYear == 2014, NA, rbs51_delta),
  rbs51_deltap = (rbs51_rate - lag(rbs51_rate,9))/lag(rbs51_rate,9)
 ) %>%
 ungroup()

#### Postpartum readmission ----

c10 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, ppreadm, CHNuid) %>%
 distinct() %>%
 group_by(BrthYear, ppreadm, CHNuid) %>%
 mutate(
  ppreadm_count = n()
 ) %>%
 group_by(BrthYear, CHNuid) %>%
 mutate(
  total_year = n(),
  ppreadm_rate = ppreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(ppreadm %in% 1,
        !is.na(CHNuid)
        ) %>%
 select(BrthYear, CHNuid, ppreadm, ppreadm_count, ppreadm_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CHNuid, ppreadm)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(BrthYear, ppreadm)) %>%
 group_by(CHNuid) %>%
 mutate(
  ppreadm_delta = (ppreadm_rate - lag(ppreadm_rate))/lag(ppreadm_rate),
  ppreadm_delta = ifelse(BrthYear == 2014, NA, ppreadm_delta),
  ppreadm_deltap = (ppreadm_rate - lag(ppreadm_rate,9))/lag(ppreadm_rate,9)
 ) %>%
 ungroup()

#### Skin to skin ----

c11 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknskn, CHNuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, sknskn, CHNuid) %>%
 mutate(
  sknskn_count = n()
 ) %>%
 group_by(BrthYear, CHNuid, lvb) %>%
 mutate(
  total_year = n(),
  sknskn_rate = sknskn_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskn %in% 1,
        !is.na(CHNuid)
        ) %>%
 select(BrthYear, CHNuid, sknskn, sknskn_count, sknskn_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CHNuid, sknskn)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(BrthYear, sknskn)) %>%
 group_by(CHNuid) %>%
 mutate(
  sknskn_delta = (sknskn_rate - lag(sknskn_rate))/lag(sknskn_rate),
  sknskn_delta = ifelse(BrthYear == 2014, NA, sknskn_delta),
  sknskn_deltap = (sknskn_rate - lag(sknskn_rate,9))/lag(sknskn_rate,9)
 ) %>%
 ungroup()

c12 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknsknvg, CHNuid, lvbvg) %>%
 distinct() %>%
 group_by(BrthYear, sknsknvg, CHNuid) %>%
 mutate(
  sknsknvg_count = n()
 ) %>%
 group_by(BrthYear, CHNuid, lvbvg) %>%
 mutate(
  total_year = n(),
  sknsknvg_rate = sknsknvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknsknvg %in% 1,
        !is.na(CHNuid)
        ) %>%
 select(BrthYear, CHNuid, sknsknvg, sknsknvg_count, sknsknvg_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CHNuid, sknsknvg)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(BrthYear, sknsknvg)) %>%
 group_by(CHNuid) %>%
 mutate(
  sknsknvg_delta = (sknsknvg_rate - lag(sknsknvg_rate))/lag(sknsknvg_rate),
  sknsknvg_delta = ifelse(BrthYear == 2014, NA, sknsknvg_delta),
  sknsknvg_deltap = (sknsknvg_rate - lag(sknsknvg_rate,9))/lag(sknsknvg_rate,9)
 ) %>%
 ungroup()

c13 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknskncs, CHNuid, lvbcs) %>%
 distinct() %>%
 group_by(BrthYear, sknskncs, CHNuid) %>%
 mutate(
  sknskncs_count = n()
 ) %>%
 group_by(BrthYear, CHNuid, lvbcs) %>%
 mutate(
  total_year = n(),
  sknskncs_rate = sknskncs_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskncs %in% 1,
        !is.na(CHNuid)
        ) %>%
 select(BrthYear, CHNuid, sknskncs, sknskncs_count, sknskncs_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CHNuid, sknskncs)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(BrthYear, sknskncs)) %>%
 group_by(CHNuid) %>%
 mutate(
  sknskncs_delta = (sknskncs_rate - lag(sknskncs_rate))/lag(sknskncs_rate),
  sknskncs_delta = ifelse(BrthYear == 2014, NA, sknskncs_delta),
  sknskncs_deltap = (sknskncs_rate - lag(sknskncs_rate,9))/lag(sknskncs_rate,9)
 ) %>%
 ungroup()

#### Neonatal readmission ----

c14 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, neoreadm, CHNuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, neoreadm, CHNuid) %>%
 mutate(
  neoreadm_count = n()
 ) %>%
 group_by(BrthYear, CHNuid, lvb) %>%
 mutate(
  total_year = n(),
  neoreadm_rate = neoreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(neoreadm %in% 1,
        !is.na(CHNuid)
        ) %>%
 select(BrthYear, CHNuid, neoreadm, neoreadm_count, neoreadm_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CHNuid, neoreadm)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(BrthYear, neoreadm)) %>%
 group_by(CHNuid) %>%
 mutate(
  neoreadm_delta = (neoreadm_rate - lag(neoreadm_rate))/lag(neoreadm_rate),
  neoreadm_delta = ifelse(BrthYear == 2014, NA, neoreadm_delta),
  neoreadm_deltap = (neoreadm_rate - lag(neoreadm_rate,9))/lag(neoreadm_rate,9)
 ) %>%
 ungroup()

#### Milk feeding ----

c15 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, excbrst, CHNuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, excbrst, CHNuid) %>%
 mutate(
  excbrst_count = n()
 ) %>%
 group_by(BrthYear, CHNuid, lvb) %>%
 mutate(
  total_year = n(),
  excbrst_rate = excbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(excbrst %in% 1,
        !is.na(CHNuid)
        ) %>%
 select(BrthYear, CHNuid, excbrst, excbrst_count, excbrst_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CHNuid, excbrst)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(BrthYear, excbrst)) %>%
 group_by(CHNuid) %>%
 mutate(
  excbrst_delta = (excbrst_rate - lag(excbrst_rate))/lag(excbrst_rate),
  excbrst_delta = ifelse(BrthYear == 2014, NA, excbrst_delta),
  excbrst_deltap = (excbrst_rate - lag(excbrst_rate,9))/lag(excbrst_rate,9)
 ) %>%
 ungroup()

c16 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, nexcbrst, CHNuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, nexcbrst, CHNuid) %>%
 mutate(
  nexcbrst_count = n()
 ) %>%
 group_by(BrthYear, CHNuid, lvb) %>%
 mutate(
  total_year = n(),
  nexcbrst_rate = nexcbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nexcbrst %in% 1,
        !is.na(CHNuid)
        ) %>%
 select(BrthYear, CHNuid, nexcbrst, nexcbrst_count, nexcbrst_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CHNuid, nexcbrst)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(BrthYear, nexcbrst)) %>%
 group_by(CHNuid) %>%
 mutate(
  nexcbrst_delta = (nexcbrst_rate - lag(nexcbrst_rate))/lag(nexcbrst_rate),
  nexcbrst_delta = ifelse(BrthYear == 2014, NA, nexcbrst_delta),
  nexcbrst_deltap = (nexcbrst_rate - lag(nexcbrst_rate,9))/lag(nexcbrst_rate,9)
 ) %>%
 ungroup()

c17 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, nbrst, CHNuid, lvbint) %>%
 distinct() %>%
 group_by(BrthYear, nbrst, CHNuid) %>%
 mutate(
  nbrst_count = n()
 ) %>%
 group_by(BrthYear, CHNuid, lvbint) %>%
 mutate(
  total_year = n(),
  nbrst_rate = nbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nbrst %in% 1,
        !is.na(CHNuid)
        ) %>%
 select(BrthYear, CHNuid, nbrst, nbrst_count, nbrst_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CHNuid, nbrst)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(BrthYear, nbrst)) %>%
 group_by(CHNuid) %>%
 mutate(
  nbrst_delta = (nbrst_rate - lag(nbrst_rate))/lag(nbrst_rate),
  nbrst_delta = ifelse(BrthYear == 2014, NA, nbrst_delta),
  nbrst_deltap = (nbrst_rate - lag(nbrst_rate,9))/lag(nbrst_rate,9)
 ) %>%
 ungroup()

c18 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, brstinit, CHNuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, brstinit, CHNuid) %>%
 mutate(
  brstinit_count = n()
 ) %>%
 group_by(BrthYear, CHNuid, lvb) %>%
 mutate(
  total_year = n(),
  brstinit_rate = brstinit_count/total_year
 ) %>%
 ungroup() %>%
 filter(brstinit %in% 1,
        !is.na(CHNuid)
        ) %>%
 select(BrthYear, CHNuid, brstinit, brstinit_count, brstinit_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(CHNuid, brstinit)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(BrthYear, brstinit)) %>%
 group_by(CHNuid) %>%
 mutate(
  brstinit_delta = (brstinit_rate - lag(brstinit_rate))/lag(brstinit_rate),
  brstinit_delta = ifelse(BrthYear == 2014, NA, brstinit_delta),
  brstinit_deltap = (brstinit_rate - lag(brstinit_rate,9))/lag(brstinit_rate,9)
 ) %>%
 ungroup()

cchn_stats <- cbind(
 c1 %>% select(-PreExisting_Hypertension),
 c2 %>% select(-Gestational_Hypertension,-BrthYear, -CHNuid),
 c3 %>% select(-Any_Hypertension,-BrthYear, -CHNuid),
 c4 %>% select(-PreExisting_Diabetes,-BrthYear, -CHNuid),
 c5 %>% select(-GDM,-BrthYear, -CHNuid),
 c6 %>% select(-Any_Diabetes,-BrthYear, -CHNuid),
 c7 %>% select(-rbs1,-BrthYear, -CHNuid),
 c8 %>% select(-rbs21,-BrthYear, -CHNuid),
 c9 %>% select(-rbs51,-BrthYear, -CHNuid),
 c10 %>% select(-ppreadm,-BrthYear, -CHNuid),
 c11 %>% select(-sknskn,-BrthYear, -CHNuid),
 c12 %>% select(-sknsknvg,-BrthYear, -CHNuid),
 c13 %>% select(-sknskncs,-BrthYear, -CHNuid),
 c14 %>% select(-neoreadm,-BrthYear, -CHNuid),
 c15 %>% select(-excbrst,-BrthYear, -CHNuid),
 c16 %>% select(-nexcbrst,-BrthYear, -CHNuid),
 c17 %>% select(-nbrst,-BrthYear, -CHNuid),
 c18 %>% select(-brstinit,-BrthYear, -CHNuid)
)

### Health authority zone (HR) ----
#### Hypertension ----

c1 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, PreExisting_Hypertension, HRuid) %>%
 distinct() %>%
 group_by(BrthYear, PreExisting_Hypertension, HRuid) %>%
 mutate(
  prehyp_count = n()
 ) %>%
 group_by(BrthYear, HRuid) %>%
 mutate(
  total_year = n(),
  prehyp_rate = prehyp_count/total_year
 ) %>%
 filter(PreExisting_Hypertension %in% 1,
        !is.na(HRuid)
 ) %>%
 select(BrthYear, HRuid, PreExisting_Hypertension, prehyp_count, prehyp_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(HRuid, PreExisting_Hypertension)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(BrthYear, PreExisting_Hypertension)) %>%
 group_by(HRuid) %>%
 mutate(
  prehyp_delta = (prehyp_rate - lag(prehyp_rate))/lag(prehyp_rate),
  prehyp_delta = ifelse(BrthYear == 2014, NA, prehyp_delta),
  prehyp_deltap = (prehyp_rate - lag(prehyp_rate,9))/lag(prehyp_rate,9)
 ) %>%
 ungroup()

c2 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, Gestational_Hypertension, HRuid) %>%
 distinct() %>%
 group_by(BrthYear, Gestational_Hypertension, HRuid) %>%
 mutate(
  gesthyp_count = n()
 ) %>%
 group_by(BrthYear, HRuid) %>%
 mutate(
  total_year = n(),
  gesthyp_rate = gesthyp_count/total_year
 ) %>%
 filter(Gestational_Hypertension %in% 1,
        !is.na(HRuid)
 ) %>%
 select(BrthYear, HRuid, Gestational_Hypertension, gesthyp_count, gesthyp_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(HRuid, Gestational_Hypertension)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(BrthYear, Gestational_Hypertension)) %>%
 group_by(HRuid) %>%
 mutate(
  gesthyp_delta = (gesthyp_rate - lag(gesthyp_rate))/lag(gesthyp_rate),
  gesthyp_delta = ifelse(BrthYear == 2014, NA, gesthyp_delta),
  gesthyp_deltap = (gesthyp_rate - lag(gesthyp_rate,9))/lag(gesthyp_rate,9)
 ) %>%
 ungroup()

c3 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, Any_Hypertension, HRuid) %>%
 distinct() %>%
 group_by(BrthYear, Any_Hypertension, HRuid) %>%
 mutate(
  hyp_count = n()
 ) %>%
 group_by(BrthYear, HRuid) %>%
 mutate(
  total_year = n(),
  hyp_rate = hyp_count/total_year
 ) %>%
 filter(Any_Hypertension %in% 1,
        !is.na(HRuid)
 ) %>%
 select(BrthYear, HRuid, Any_Hypertension, hyp_count, hyp_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(HRuid, Any_Hypertension)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(BrthYear, Any_Hypertension)) %>%
 group_by(HRuid) %>%
 mutate(
  hyp_delta = (hyp_rate - lag(hyp_rate))/lag(hyp_rate),
  hyp_delta = ifelse(BrthYear == 2014, NA, hyp_delta),
  hyp_deltap = (hyp_rate - lag(hyp_rate,9))/lag(hyp_rate,9)
 ) %>%
 ungroup()

#### Diabetes ----

c4 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, PreExisting_Diabetes, HRuid) %>%
 distinct() %>%
 group_by(BrthYear, PreExisting_Diabetes, HRuid) %>%
 mutate(
  prediab_count = n()
 ) %>%
 group_by(BrthYear, HRuid) %>%
 mutate(
  total_year = n(),
  prediab_rate = prediab_count/total_year
 ) %>%
 filter(PreExisting_Diabetes %in% 1,
        !is.na(HRuid)
 ) %>%
 select(BrthYear, HRuid, PreExisting_Diabetes, prediab_count, prediab_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(HRuid, PreExisting_Diabetes)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(BrthYear, PreExisting_Diabetes)) %>%
 group_by(HRuid) %>%
 mutate(
  prediab_delta = (prediab_rate - lag(prediab_rate))/lag(prediab_rate),
  prediab_delta = ifelse(BrthYear == 2014, NA, prediab_delta),
  prediab_deltap = (prediab_rate - lag(prediab_rate,9))/lag(prediab_rate,9)
 ) %>%
 ungroup()

c5 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, GDM, HRuid) %>%
 distinct() %>%
 group_by(BrthYear, GDM, HRuid) %>%
 mutate(
  gestdiab_count = n()
 ) %>%
 group_by(BrthYear, HRuid) %>%
 mutate(
  total_year = n(),
  gestdiab_rate = gestdiab_count/total_year
 ) %>%
 filter(GDM %in% 1,
        !is.na(HRuid)
 ) %>%
 select(BrthYear, HRuid, GDM, gestdiab_count, gestdiab_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(HRuid, GDM)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(BrthYear, GDM)) %>%
 group_by(HRuid) %>%
 mutate(
  gestdiab_delta = (gestdiab_rate - lag(gestdiab_rate))/lag(gestdiab_rate),
  gestdiab_delta = ifelse(BrthYear == 2014, NA, gestdiab_delta),
  gestdiab_deltap = (gestdiab_rate - lag(gestdiab_rate,9))/lag(gestdiab_rate,9)
 ) %>%
 ungroup()

c6 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, Any_Diabetes, HRuid) %>%
 distinct() %>%
 group_by(BrthYear, Any_Diabetes, HRuid) %>%
 mutate(
  diab_count = n()
 ) %>%
 group_by(BrthYear, HRuid) %>%
 mutate(
  total_year = n(),
  diab_rate = diab_count/total_year
 ) %>%
 filter(Any_Diabetes %in% 1,
        !is.na(HRuid)
 ) %>%
 select(BrthYear, HRuid, Any_Diabetes, diab_count, diab_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(HRuid, Any_Diabetes)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(BrthYear, Any_Diabetes)) %>%
 group_by(HRuid) %>%
 mutate(
  diab_delta = (diab_rate - lag(diab_rate))/lag(diab_rate),
  diab_delta = ifelse(BrthYear == 2014, NA, diab_delta),
  diab_deltap = (diab_rate - lag(diab_rate,9))/lag(diab_rate,9)
 ) %>%
 ungroup()

#### Robson group ----

c7 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs1, RobsnGrp, HRuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs1, HRuid) %>%
 mutate(
  rbs1_count = n()
 ) %>%
 group_by(BrthYear, HRuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs1_rate = rbs1_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs1 %in% 1,
        RobsnGrp %in% 1,
        !is.na(HRuid)
 ) %>%
 select(BrthYear, HRuid, rbs1, rbs1_count, rbs1_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(HRuid, rbs1)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(BrthYear, rbs1)) %>%
 group_by(HRuid) %>%
 mutate(
  rbs1_delta = (rbs1_rate - lag(rbs1_rate))/lag(rbs1_rate),
  rbs1_delta = ifelse(BrthYear == 2014, NA, rbs1_delta),
  rbs1_deltap = (rbs1_rate - lag(rbs1_rate,9))/lag(rbs1_rate,9)
 ) %>%
 ungroup()

c8 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs21, RobsnGrp, HRuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs21, HRuid) %>%
 mutate(
  rbs21_count = n()
 ) %>%
 group_by(BrthYear, HRuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs21_rate = rbs21_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs21 %in% 1,
        RobsnGrp %in% 2.1,
        !is.na(HRuid)
 ) %>%
 select(BrthYear, HRuid, rbs21, rbs21_count, rbs21_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(HRuid, rbs21)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(BrthYear, rbs21)) %>%
 group_by(HRuid) %>%
 mutate(
  rbs21_delta = (rbs21_rate - lag(rbs21_rate))/lag(rbs21_rate),
  rbs21_delta = ifelse(BrthYear == 2014, NA, rbs21_delta),
  rbs21_deltap = (rbs21_rate - lag(rbs21_rate,9))/lag(rbs21_rate,9)
 ) %>%
 ungroup()

c9 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs51, RobsnGrp, HRuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs51, HRuid) %>%
 mutate(
  rbs51_count = n()
 ) %>%
 group_by(BrthYear, HRuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs51_rate = rbs51_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs51 %in% 1,
        RobsnGrp %in% 5.1,
        !is.na(HRuid)
 ) %>%
 select(BrthYear, HRuid, rbs51, rbs51_count, rbs51_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(HRuid, rbs51)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(BrthYear, rbs51)) %>%
 group_by(HRuid) %>%
 mutate(
  rbs51_delta = (rbs51_rate - lag(rbs51_rate))/lag(rbs51_rate),
  rbs51_delta = ifelse(BrthYear == 2014, NA, rbs51_delta),
  rbs51_deltap = (rbs51_rate - lag(rbs51_rate,9))/lag(rbs51_rate,9)
 ) %>%
 ungroup()

#### Postpartum readmission ----

c10 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, ppreadm, HRuid) %>%
 distinct() %>%
 group_by(BrthYear, ppreadm, HRuid) %>%
 mutate(
  ppreadm_count = n()
 ) %>%
 group_by(BrthYear, HRuid) %>%
 mutate(
  total_year = n(),
  ppreadm_rate = ppreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(ppreadm %in% 1,
        !is.na(HRuid)
 ) %>%
 select(BrthYear, HRuid, ppreadm, ppreadm_count, ppreadm_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(HRuid, ppreadm)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(BrthYear, ppreadm)) %>%
 group_by(HRuid) %>%
 mutate(
  ppreadm_delta = (ppreadm_rate - lag(ppreadm_rate))/lag(ppreadm_rate),
  ppreadm_delta = ifelse(BrthYear == 2014, NA, ppreadm_delta),
  ppreadm_deltap = (ppreadm_rate - lag(ppreadm_rate,9))/lag(ppreadm_rate,9)
 ) %>%
 ungroup()

#### Skin to skin ----

c11 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknskn, HRuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, sknskn, HRuid) %>%
 mutate(
  sknskn_count = n()
 ) %>%
 group_by(BrthYear, HRuid, lvb) %>%
 mutate(
  total_year = n(),
  sknskn_rate = sknskn_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskn %in% 1,
        !is.na(HRuid)
 ) %>%
 select(BrthYear, HRuid, sknskn, sknskn_count, sknskn_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(HRuid, sknskn)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(BrthYear, sknskn)) %>%
 group_by(HRuid) %>%
 mutate(
  sknskn_delta = (sknskn_rate - lag(sknskn_rate))/lag(sknskn_rate),
  sknskn_delta = ifelse(BrthYear == 2014, NA, sknskn_delta),
  sknskn_deltap = (sknskn_rate - lag(sknskn_rate,9))/lag(sknskn_rate,9)
 ) %>%
 ungroup()

c12 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknsknvg, HRuid, lvbvg) %>%
 distinct() %>%
 group_by(BrthYear, sknsknvg, HRuid) %>%
 mutate(
  sknsknvg_count = n()
 ) %>%
 group_by(BrthYear, HRuid, lvbvg) %>%
 mutate(
  total_year = n(),
  sknsknvg_rate = sknsknvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknsknvg %in% 1,
        !is.na(HRuid)
 ) %>%
 select(BrthYear, HRuid, sknsknvg, sknsknvg_count, sknsknvg_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(HRuid, sknsknvg)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(BrthYear, sknsknvg)) %>%
 group_by(HRuid) %>%
 mutate(
  sknsknvg_delta = (sknsknvg_rate - lag(sknsknvg_rate))/lag(sknsknvg_rate),
  sknsknvg_delta = ifelse(BrthYear == 2014, NA, sknsknvg_delta),
  sknsknvg_deltap = (sknsknvg_rate - lag(sknsknvg_rate,9))/lag(sknsknvg_rate,9)
 ) %>%
 ungroup()

c13 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknskncs, HRuid, lvbcs) %>%
 distinct() %>%
 group_by(BrthYear, sknskncs, HRuid) %>%
 mutate(
  sknskncs_count = n()
 ) %>%
 group_by(BrthYear, HRuid, lvbcs) %>%
 mutate(
  total_year = n(),
  sknskncs_rate = sknskncs_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskncs %in% 1,
        !is.na(HRuid)
 ) %>%
 select(BrthYear, HRuid, sknskncs, sknskncs_count, sknskncs_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(HRuid, sknskncs)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(BrthYear, sknskncs)) %>%
 group_by(HRuid) %>%
 mutate(
  sknskncs_delta = (sknskncs_rate - lag(sknskncs_rate))/lag(sknskncs_rate),
  sknskncs_delta = ifelse(BrthYear == 2014, NA, sknskncs_delta),
  sknskncs_deltap = (sknskncs_rate - lag(sknskncs_rate,9))/lag(sknskncs_rate,9)
 ) %>%
 ungroup()

#### Neonatal readmission ----

c14 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, neoreadm, HRuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, neoreadm, HRuid) %>%
 mutate(
  neoreadm_count = n()
 ) %>%
 group_by(BrthYear, HRuid, lvb) %>%
 mutate(
  total_year = n(),
  neoreadm_rate = neoreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(neoreadm %in% 1,
        !is.na(HRuid)
 ) %>%
 select(BrthYear, HRuid, neoreadm, neoreadm_count, neoreadm_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(HRuid, neoreadm)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(BrthYear, neoreadm)) %>%
 group_by(HRuid) %>%
 mutate(
  neoreadm_delta = (neoreadm_rate - lag(neoreadm_rate))/lag(neoreadm_rate),
  neoreadm_delta = ifelse(BrthYear == 2014, NA, neoreadm_delta),
  neoreadm_deltap = (neoreadm_rate - lag(neoreadm_rate,9))/lag(neoreadm_rate,9)
 ) %>%
 ungroup()

#### Milk feeding ----

c15 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, excbrst, HRuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, excbrst, HRuid) %>%
 mutate(
  excbrst_count = n()
 ) %>%
 group_by(BrthYear, HRuid, lvb) %>%
 mutate(
  total_year = n(),
  excbrst_rate = excbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(excbrst %in% 1,
        !is.na(HRuid)
 ) %>%
 select(BrthYear, HRuid, excbrst, excbrst_count, excbrst_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(HRuid, excbrst)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(BrthYear, excbrst)) %>%
 group_by(HRuid) %>%
 mutate(
  excbrst_delta = (excbrst_rate - lag(excbrst_rate))/lag(excbrst_rate),
  excbrst_delta = ifelse(BrthYear == 2014, NA, excbrst_delta),
  excbrst_deltap = (excbrst_rate - lag(excbrst_rate,9))/lag(excbrst_rate,9)
 ) %>%
 ungroup()

c16 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, nexcbrst, HRuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, nexcbrst, HRuid) %>%
 mutate(
  nexcbrst_count = n()
 ) %>%
 group_by(BrthYear, HRuid, lvb) %>%
 mutate(
  total_year = n(),
  nexcbrst_rate = nexcbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nexcbrst %in% 1,
        !is.na(HRuid)
 ) %>%
 select(BrthYear, HRuid, nexcbrst, nexcbrst_count, nexcbrst_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(HRuid, nexcbrst)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(BrthYear, nexcbrst)) %>%
 group_by(HRuid) %>%
 mutate(
  nexcbrst_delta = (nexcbrst_rate - lag(nexcbrst_rate))/lag(nexcbrst_rate),
  nexcbrst_delta = ifelse(BrthYear == 2014, NA, nexcbrst_delta),
  nexcbrst_deltap = (nexcbrst_rate - lag(nexcbrst_rate,9))/lag(nexcbrst_rate,9)
 ) %>%
 ungroup()

c17 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, nbrst, HRuid, lvbint) %>%
 distinct() %>%
 group_by(BrthYear, nbrst, HRuid) %>%
 mutate(
  nbrst_count = n()
 ) %>%
 group_by(BrthYear, HRuid, lvbint) %>%
 mutate(
  total_year = n(),
  nbrst_rate = nbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nbrst %in% 1,
        !is.na(HRuid)
 ) %>%
 select(BrthYear, HRuid, nbrst, nbrst_count, nbrst_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(HRuid, nbrst)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(BrthYear, nbrst)) %>%
 group_by(HRuid) %>%
 mutate(
  nbrst_delta = (nbrst_rate - lag(nbrst_rate))/lag(nbrst_rate),
  nbrst_delta = ifelse(BrthYear == 2014, NA, nbrst_delta),
  nbrst_deltap = (nbrst_rate - lag(nbrst_rate,9))/lag(nbrst_rate,9)
 ) %>%
 ungroup()

c18 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, brstinit, HRuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, brstinit, HRuid) %>%
 mutate(
  brstinit_count = n()
 ) %>%
 group_by(BrthYear, HRuid, lvb) %>%
 mutate(
  total_year = n(),
  brstinit_rate = brstinit_count/total_year
 ) %>%
 ungroup() %>%
 filter(brstinit %in% 1,
        !is.na(HRuid)
 ) %>%
 select(BrthYear, HRuid, brstinit, brstinit_count, brstinit_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(HRuid, brstinit)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(BrthYear, brstinit)) %>%
 group_by(HRuid) %>%
 mutate(
  brstinit_delta = (brstinit_rate - lag(brstinit_rate))/lag(brstinit_rate),
  brstinit_delta = ifelse(BrthYear == 2014, NA, brstinit_delta),
  brstinit_deltap = (brstinit_rate - lag(brstinit_rate,9))/lag(brstinit_rate,9)
 ) %>%
 ungroup()

chr_stats <- cbind(
 c1 %>% select(-PreExisting_Hypertension),
 c2 %>% select(-Gestational_Hypertension,-BrthYear, -HRuid),
 c3 %>% select(-Any_Hypertension,-BrthYear, -HRuid),
 c4 %>% select(-PreExisting_Diabetes,-BrthYear, -HRuid),
 c5 %>% select(-GDM,-BrthYear, -HRuid),
 c6 %>% select(-Any_Diabetes,-BrthYear, -HRuid),
 c7 %>% select(-rbs1,-BrthYear, -HRuid),
 c8 %>% select(-rbs21,-BrthYear, -HRuid),
 c9 %>% select(-rbs51,-BrthYear, -HRuid),
 c10 %>% select(-ppreadm,-BrthYear, -HRuid),
 c11 %>% select(-sknskn,-BrthYear, -HRuid),
 c12 %>% select(-sknsknvg,-BrthYear, -HRuid),
 c13 %>% select(-sknskncs,-BrthYear, -HRuid),
 c14 %>% select(-neoreadm,-BrthYear, -HRuid),
 c15 %>% select(-excbrst,-BrthYear, -HRuid),
 c16 %>% select(-nexcbrst,-BrthYear, -HRuid),
 c17 %>% select(-nbrst,-BrthYear, -HRuid),
 c18 %>% select(-brstinit,-BrthYear, -HRuid)
) %>%
 filter(!HRuid %in% "12 ") %>%
 mutate(HRuid = as.character(trimws(substr(HRuid,3,4))))

### Urban x Rural (URB) ----

#### Hypertension ----

c1 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, PreExisting_Hypertension, URBuid) %>%
 distinct() %>%
 group_by(BrthYear, PreExisting_Hypertension, URBuid) %>%
 mutate(
  prehyp_count = n()
 ) %>%
 group_by(BrthYear, URBuid) %>%
 mutate(
  total_year = n(),
  prehyp_rate = prehyp_count/total_year
 ) %>%
 filter(PreExisting_Hypertension %in% 1,
        !is.na(URBuid)
 ) %>%
 select(BrthYear, URBuid, PreExisting_Hypertension, prehyp_count, prehyp_rate) %>%
 distinct() %>%
 arrange(URBuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(URBuid, PreExisting_Hypertension)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(BrthYear, PreExisting_Hypertension)) %>%
 group_by(URBuid) %>%
 mutate(
  prehyp_delta = (prehyp_rate - lag(prehyp_rate))/lag(prehyp_rate),
  prehyp_delta = ifelse(BrthYear == 2014, NA, prehyp_delta),
  prehyp_deltap = (prehyp_rate - lag(prehyp_rate,9))/lag(prehyp_rate,9)
 ) %>%
 ungroup()

c2 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, Gestational_Hypertension, URBuid) %>%
 distinct() %>%
 group_by(BrthYear, Gestational_Hypertension, URBuid) %>%
 mutate(
  gesthyp_count = n()
 ) %>%
 group_by(BrthYear, URBuid) %>%
 mutate(
  total_year = n(),
  gesthyp_rate = gesthyp_count/total_year
 ) %>%
 filter(Gestational_Hypertension %in% 1,
        !is.na(URBuid)
 ) %>%
 select(BrthYear, URBuid, Gestational_Hypertension, gesthyp_count, gesthyp_rate) %>%
 distinct() %>%
 arrange(URBuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(URBuid, Gestational_Hypertension)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(BrthYear, Gestational_Hypertension)) %>%
 group_by(URBuid) %>%
 mutate(
  gesthyp_delta = (gesthyp_rate - lag(gesthyp_rate))/lag(gesthyp_rate),
  gesthyp_delta = ifelse(BrthYear == 2014, NA, gesthyp_delta),
  gesthyp_deltap = (gesthyp_rate - lag(gesthyp_rate,9))/lag(gesthyp_rate,9)
 ) %>%
 ungroup()

c3 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, Any_Hypertension, URBuid) %>%
 distinct() %>%
 group_by(BrthYear, Any_Hypertension, URBuid) %>%
 mutate(
  hyp_count = n()
 ) %>%
 group_by(BrthYear, URBuid) %>%
 mutate(
  total_year = n(),
  hyp_rate = hyp_count/total_year
 ) %>%
 filter(Any_Hypertension %in% 1,
        !is.na(URBuid)
 ) %>%
 select(BrthYear, URBuid, Any_Hypertension, hyp_count, hyp_rate) %>%
 distinct() %>%
 arrange(URBuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(URBuid, Any_Hypertension)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(BrthYear, Any_Hypertension)) %>%
 group_by(URBuid) %>%
 mutate(
  hyp_delta = (hyp_rate - lag(hyp_rate))/lag(hyp_rate),
  hyp_delta = ifelse(BrthYear == 2014, NA, hyp_delta),
  hyp_deltap = (hyp_rate - lag(hyp_rate,9))/lag(hyp_rate,9)
 ) %>%
 ungroup()

#### Diabetes ----

c4 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, PreExisting_Diabetes, URBuid) %>%
 distinct() %>%
 group_by(BrthYear, PreExisting_Diabetes, URBuid) %>%
 mutate(
  prediab_count = n()
 ) %>%
 group_by(BrthYear, URBuid) %>%
 mutate(
  total_year = n(),
  prediab_rate = prediab_count/total_year
 ) %>%
 filter(PreExisting_Diabetes %in% 1,
        !is.na(URBuid)
 ) %>%
 select(BrthYear, URBuid, PreExisting_Diabetes, prediab_count, prediab_rate) %>%
 distinct() %>%
 arrange(URBuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(URBuid, PreExisting_Diabetes)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(BrthYear, PreExisting_Diabetes)) %>%
 group_by(URBuid) %>%
 mutate(
  prediab_delta = (prediab_rate - lag(prediab_rate))/lag(prediab_rate),
  prediab_delta = ifelse(BrthYear == 2014, NA, prediab_delta),
  prediab_deltap = (prediab_rate - lag(prediab_rate,9))/lag(prediab_rate,9)
 ) %>%
 ungroup()

c5 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, GDM, URBuid) %>%
 distinct() %>%
 group_by(BrthYear, GDM, URBuid) %>%
 mutate(
  gestdiab_count = n()
 ) %>%
 group_by(BrthYear, URBuid) %>%
 mutate(
  total_year = n(),
  gestdiab_rate = gestdiab_count/total_year
 ) %>%
 filter(GDM %in% 1,
        !is.na(URBuid)
 ) %>%
 select(BrthYear, URBuid, GDM, gestdiab_count, gestdiab_rate) %>%
 distinct() %>%
 arrange(URBuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(URBuid, GDM)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(BrthYear, GDM)) %>%
 group_by(URBuid) %>%
 mutate(
  gestdiab_delta = (gestdiab_rate - lag(gestdiab_rate))/lag(gestdiab_rate),
  gestdiab_delta = ifelse(BrthYear == 2014, NA, gestdiab_delta),
  gestdiab_deltap = (gestdiab_rate - lag(gestdiab_rate,9))/lag(gestdiab_rate,9)
 ) %>%
 ungroup()

c6 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, Any_Diabetes, URBuid) %>%
 distinct() %>%
 group_by(BrthYear, Any_Diabetes, URBuid) %>%
 mutate(
  diab_count = n()
 ) %>%
 group_by(BrthYear, URBuid) %>%
 mutate(
  total_year = n(),
  diab_rate = diab_count/total_year
 ) %>%
 filter(Any_Diabetes %in% 1,
        !is.na(URBuid)
 ) %>%
 select(BrthYear, URBuid, Any_Diabetes, diab_count, diab_rate) %>%
 distinct() %>%
 arrange(URBuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(URBuid, Any_Diabetes)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(BrthYear, Any_Diabetes)) %>%
 group_by(URBuid) %>%
 mutate(
  diab_delta = (diab_rate - lag(diab_rate))/lag(diab_rate),
  diab_delta = ifelse(BrthYear == 2014, NA, diab_delta),
  diab_deltap = (diab_rate - lag(diab_rate,9))/lag(diab_rate,9)
 ) %>%
 ungroup()

#### Robson group ----

c7 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs1, RobsnGrp, URBuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs1, URBuid) %>%
 mutate(
  rbs1_count = n()
 ) %>%
 group_by(BrthYear, URBuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs1_rate = rbs1_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs1 %in% 1,
        RobsnGrp %in% 1,
        !is.na(URBuid)
 ) %>%
 select(BrthYear, URBuid, rbs1, rbs1_count, rbs1_rate) %>%
 distinct() %>%
 arrange(URBuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(URBuid, rbs1)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(BrthYear, rbs1)) %>%
 group_by(URBuid) %>%
 mutate(
  rbs1_delta = (rbs1_rate - lag(rbs1_rate))/lag(rbs1_rate),
  rbs1_delta = ifelse(BrthYear == 2014, NA, rbs1_delta),
  rbs1_deltap = (rbs1_rate - lag(rbs1_rate,9))/lag(rbs1_rate,9)
 ) %>%
 ungroup()

c8 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs21, RobsnGrp, URBuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs21, URBuid) %>%
 mutate(
  rbs21_count = n()
 ) %>%
 group_by(BrthYear, URBuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs21_rate = rbs21_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs21 %in% 1,
        RobsnGrp %in% 2.1,
        !is.na(URBuid)
 ) %>%
 select(BrthYear, URBuid, rbs21, rbs21_count, rbs21_rate) %>%
 distinct() %>%
 arrange(URBuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(URBuid, rbs21)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(BrthYear, rbs21)) %>%
 group_by(URBuid) %>%
 mutate(
  rbs21_delta = (rbs21_rate - lag(rbs21_rate))/lag(rbs21_rate),
  rbs21_delta = ifelse(BrthYear == 2014, NA, rbs21_delta),
  rbs21_deltap = (rbs21_rate - lag(rbs21_rate,9))/lag(rbs21_rate,9)
 ) %>%
 ungroup()

c9 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs51, RobsnGrp, URBuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs51, URBuid) %>%
 mutate(
  rbs51_count = n()
 ) %>%
 group_by(BrthYear, URBuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs51_rate = rbs51_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs51 %in% 1,
        RobsnGrp %in% 5.1,
        !is.na(URBuid)
 ) %>%
 select(BrthYear, URBuid, rbs51, rbs51_count, rbs51_rate) %>%
 distinct() %>%
 arrange(URBuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(URBuid, rbs51)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(BrthYear, rbs51)) %>%
 group_by(URBuid) %>%
 mutate(
  rbs51_delta = (rbs51_rate - lag(rbs51_rate))/lag(rbs51_rate),
  rbs51_delta = ifelse(BrthYear == 2014, NA, rbs51_delta),
  rbs51_deltap = (rbs51_rate - lag(rbs51_rate,9))/lag(rbs51_rate,9)
 ) %>%
 ungroup()

#### Postpartum readmission ----

c10 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, ppreadm, URBuid) %>%
 distinct() %>%
 group_by(BrthYear, ppreadm, URBuid) %>%
 mutate(
  ppreadm_count = n()
 ) %>%
 group_by(BrthYear, URBuid) %>%
 mutate(
  total_year = n(),
  ppreadm_rate = ppreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(ppreadm %in% 1,
        !is.na(URBuid)
 ) %>%
 select(BrthYear, URBuid, ppreadm, ppreadm_count, ppreadm_rate) %>%
 distinct() %>%
 arrange(URBuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(URBuid, ppreadm)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(BrthYear, ppreadm)) %>%
 group_by(URBuid) %>%
 mutate(
  ppreadm_delta = (ppreadm_rate - lag(ppreadm_rate))/lag(ppreadm_rate),
  ppreadm_delta = ifelse(BrthYear == 2014, NA, ppreadm_delta),
  ppreadm_deltap = (ppreadm_rate - lag(ppreadm_rate,9))/lag(ppreadm_rate,9)
 ) %>%
 ungroup()

#### Skin to skin ----

c11 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknskn, URBuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, sknskn, URBuid) %>%
 mutate(
  sknskn_count = n()
 ) %>%
 group_by(BrthYear, URBuid, lvb) %>%
 mutate(
  total_year = n(),
  sknskn_rate = sknskn_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskn %in% 1,
        !is.na(URBuid)
 ) %>%
 select(BrthYear, URBuid, sknskn, sknskn_count, sknskn_rate) %>%
 distinct() %>%
 arrange(URBuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(URBuid, sknskn)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(BrthYear, sknskn)) %>%
 group_by(URBuid) %>%
 mutate(
  sknskn_delta = (sknskn_rate - lag(sknskn_rate))/lag(sknskn_rate),
  sknskn_delta = ifelse(BrthYear == 2014, NA, sknskn_delta),
  sknskn_deltap = (sknskn_rate - lag(sknskn_rate,9))/lag(sknskn_rate,9)
 ) %>%
 ungroup()

c12 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknsknvg, URBuid, lvbvg) %>%
 distinct() %>%
 group_by(BrthYear, sknsknvg, URBuid) %>%
 mutate(
  sknsknvg_count = n()
 ) %>%
 group_by(BrthYear, URBuid, lvbvg) %>%
 mutate(
  total_year = n(),
  sknsknvg_rate = sknsknvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknsknvg %in% 1,
        !is.na(URBuid)
 ) %>%
 select(BrthYear, URBuid, sknsknvg, sknsknvg_count, sknsknvg_rate) %>%
 distinct() %>%
 arrange(URBuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(URBuid, sknsknvg)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(BrthYear, sknsknvg)) %>%
 group_by(URBuid) %>%
 mutate(
  sknsknvg_delta = (sknsknvg_rate - lag(sknsknvg_rate))/lag(sknsknvg_rate),
  sknsknvg_delta = ifelse(BrthYear == 2014, NA, sknsknvg_delta),
  sknsknvg_deltap = (sknsknvg_rate - lag(sknsknvg_rate,9))/lag(sknsknvg_rate,9)
 ) %>%
 ungroup()

c13 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknskncs, URBuid, lvbcs) %>%
 distinct() %>%
 group_by(BrthYear, sknskncs, URBuid) %>%
 mutate(
  sknskncs_count = n()
 ) %>%
 group_by(BrthYear, URBuid, lvbcs) %>%
 mutate(
  total_year = n(),
  sknskncs_rate = sknskncs_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskncs %in% 1,
        !is.na(URBuid)
 ) %>%
 select(BrthYear, URBuid, sknskncs, sknskncs_count, sknskncs_rate) %>%
 distinct() %>%
 arrange(URBuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(URBuid, sknskncs)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(BrthYear, sknskncs)) %>%
 group_by(URBuid) %>%
 mutate(
  sknskncs_delta = (sknskncs_rate - lag(sknskncs_rate))/lag(sknskncs_rate),
  sknskncs_delta = ifelse(BrthYear == 2014, NA, sknskncs_delta),
  sknskncs_deltap = (sknskncs_rate - lag(sknskncs_rate,9))/lag(sknskncs_rate,9)
 ) %>%
 ungroup()

#### Neonatal readmission ----

c14 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, neoreadm, URBuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, neoreadm, URBuid) %>%
 mutate(
  neoreadm_count = n()
 ) %>%
 group_by(BrthYear, URBuid, lvb) %>%
 mutate(
  total_year = n(),
  neoreadm_rate = neoreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(neoreadm %in% 1,
        !is.na(URBuid)
 ) %>%
 select(BrthYear, URBuid, neoreadm, neoreadm_count, neoreadm_rate) %>%
 distinct() %>%
 arrange(URBuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(URBuid, neoreadm)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(BrthYear, neoreadm)) %>%
 group_by(URBuid) %>%
 mutate(
  neoreadm_delta = (neoreadm_rate - lag(neoreadm_rate))/lag(neoreadm_rate),
  neoreadm_delta = ifelse(BrthYear == 2014, NA, neoreadm_delta),
  neoreadm_deltap = (neoreadm_rate - lag(neoreadm_rate,9))/lag(neoreadm_rate,9)
 ) %>%
 ungroup()

#### Milk feeding ----

c15 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, excbrst, URBuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, excbrst, URBuid) %>%
 mutate(
  excbrst_count = n()
 ) %>%
 group_by(BrthYear, URBuid, lvb) %>%
 mutate(
  total_year = n(),
  excbrst_rate = excbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(excbrst %in% 1,
        !is.na(URBuid)
 ) %>%
 select(BrthYear, URBuid, excbrst, excbrst_count, excbrst_rate) %>%
 distinct() %>%
 arrange(URBuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(URBuid, excbrst)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(BrthYear, excbrst)) %>%
 group_by(URBuid) %>%
 mutate(
  excbrst_delta = (excbrst_rate - lag(excbrst_rate))/lag(excbrst_rate),
  excbrst_delta = ifelse(BrthYear == 2014, NA, excbrst_delta),
  excbrst_deltap = (excbrst_rate - lag(excbrst_rate,9))/lag(excbrst_rate,9)
 ) %>%
 ungroup()

c16 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, nexcbrst, URBuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, nexcbrst, URBuid) %>%
 mutate(
  nexcbrst_count = n()
 ) %>%
 group_by(BrthYear, URBuid, lvb) %>%
 mutate(
  total_year = n(),
  nexcbrst_rate = nexcbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nexcbrst %in% 1,
        !is.na(URBuid)
 ) %>%
 select(BrthYear, URBuid, nexcbrst, nexcbrst_count, nexcbrst_rate) %>%
 distinct() %>%
 arrange(URBuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(URBuid, nexcbrst)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(BrthYear, nexcbrst)) %>%
 group_by(URBuid) %>%
 mutate(
  nexcbrst_delta = (nexcbrst_rate - lag(nexcbrst_rate))/lag(nexcbrst_rate),
  nexcbrst_delta = ifelse(BrthYear == 2014, NA, nexcbrst_delta),
  nexcbrst_deltap = (nexcbrst_rate - lag(nexcbrst_rate,9))/lag(nexcbrst_rate,9)
 ) %>%
 ungroup()

c17 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, nbrst, URBuid, lvbint) %>%
 distinct() %>%
 group_by(BrthYear, nbrst, URBuid) %>%
 mutate(
  nbrst_count = n()
 ) %>%
 group_by(BrthYear, URBuid, lvbint) %>%
 mutate(
  total_year = n(),
  nbrst_rate = nbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nbrst %in% 1,
        !is.na(URBuid)
 ) %>%
 select(BrthYear, URBuid, nbrst, nbrst_count, nbrst_rate) %>%
 distinct() %>%
 arrange(URBuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(URBuid, nbrst)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(BrthYear, nbrst)) %>%
 group_by(URBuid) %>%
 mutate(
  nbrst_delta = (nbrst_rate - lag(nbrst_rate))/lag(nbrst_rate),
  nbrst_delta = ifelse(BrthYear == 2014, NA, nbrst_delta),
  nbrst_deltap = (nbrst_rate - lag(nbrst_rate,9))/lag(nbrst_rate,9)
 ) %>%
 ungroup()

c18 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, brstinit, URBuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, brstinit, URBuid) %>%
 mutate(
  brstinit_count = n()
 ) %>%
 group_by(BrthYear, URBuid, lvb) %>%
 mutate(
  total_year = n(),
  brstinit_rate = brstinit_count/total_year
 ) %>%
 ungroup() %>%
 filter(brstinit %in% 1,
        !is.na(URBuid)
 ) %>%
 select(BrthYear, URBuid, brstinit, brstinit_count, brstinit_rate) %>%
 distinct() %>%
 arrange(URBuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = (as.numeric(format(as.Date(Sys.Date()),"%Y"))-10):(as.numeric(format(as.Date(Sys.Date()),"%Y"))-1),
                 tidyr::nesting(URBuid, brstinit)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(BrthYear, brstinit)) %>%
 group_by(URBuid) %>%
 mutate(
  brstinit_delta = (brstinit_rate - lag(brstinit_rate))/lag(brstinit_rate),
  brstinit_delta = ifelse(BrthYear == 2014, NA, brstinit_delta),
  brstinit_deltap = (brstinit_rate - lag(brstinit_rate,9))/lag(brstinit_rate,9)
 ) %>%
 ungroup()

curb_stats <- cbind(
 c1 %>% select(-PreExisting_Hypertension),
 c2 %>% select(-Gestational_Hypertension,-BrthYear, -URBuid),
 c3 %>% select(-Any_Hypertension,-BrthYear, -URBuid),
 c4 %>% select(-PreExisting_Diabetes,-BrthYear, -URBuid),
 c5 %>% select(-GDM,-BrthYear, -URBuid),
 c6 %>% select(-Any_Diabetes,-BrthYear, -URBuid),
 c7 %>% select(-rbs1,-BrthYear, -URBuid),
 c8 %>% select(-rbs21,-BrthYear, -URBuid),
 c9 %>% select(-rbs51,-BrthYear, -URBuid),
 c10 %>% select(-ppreadm,-BrthYear, -URBuid),
 c11 %>% select(-sknskn,-BrthYear, -URBuid),
 c12 %>% select(-sknsknvg,-BrthYear, -URBuid),
 c13 %>% select(-sknskncs,-BrthYear, -URBuid),
 c14 %>% select(-neoreadm,-BrthYear, -URBuid),
 c15 %>% select(-excbrst,-BrthYear, -URBuid),
 c16 %>% select(-nexcbrst,-BrthYear, -URBuid),
 c17 %>% select(-nbrst,-BrthYear, -URBuid),
 c18 %>% select(-brstinit,-BrthYear, -URBuid)
)

## Fiscal year ----
### Census division (CD) ----
#### Hypertension ----

c1 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, PreExisting_Hypertension, CDuid) %>%
 distinct() %>%
 group_by(FiscalYear, PreExisting_Hypertension, CDuid) %>%
 mutate(
  prehyp_count = n()
 ) %>%
 group_by(FiscalYear, CDuid) %>%
 mutate(
  total_year = n(),
  prehyp_rate = prehyp_count/total_year
 ) %>%
 filter(PreExisting_Hypertension %in% 1,
        !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, PreExisting_Hypertension, prehyp_count, prehyp_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CDuid, PreExisting_Hypertension)) %>%
 group_by(CDuid) %>%
 mutate(
  prehyp_delta = (prehyp_rate - lag(prehyp_rate))/lag(prehyp_rate),
  prehyp_delta = ifelse(FiscalYear == "2013-2014", NA, prehyp_delta),
  prehyp_deltap = (prehyp_rate - lag(prehyp_rate,10))/lag(prehyp_rate,10)
 ) %>%
 ungroup()

c2 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Gestational_Hypertension, CDuid) %>%
 distinct() %>%
 group_by(FiscalYear, Gestational_Hypertension, CDuid) %>%
 mutate(
  gesthyp_count = n()
 ) %>%
 group_by(FiscalYear, CDuid) %>%
 mutate(
  total_year = n(),
  gesthyp_rate = gesthyp_count/total_year
 ) %>%
 filter(Gestational_Hypertension %in% 1,
        !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, Gestational_Hypertension, gesthyp_count, gesthyp_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CDuid, Gestational_Hypertension)) %>%
 group_by(CDuid) %>%
 mutate(
  gesthyp_delta = (gesthyp_rate - lag(gesthyp_rate))/lag(gesthyp_rate),
  gesthyp_delta = ifelse(FiscalYear == "2013-2014", NA, gesthyp_delta),
  gesthyp_deltap = (gesthyp_rate - lag(gesthyp_rate,10))/lag(gesthyp_rate,10)
 ) %>%
 ungroup()

c3 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Any_Hypertension, CDuid) %>%
 distinct() %>%
 group_by(FiscalYear, Any_Hypertension, CDuid) %>%
 mutate(
  hyp_count = n()
 ) %>%
 group_by(FiscalYear, CDuid) %>%
 mutate(
  total_year = n(),
  hyp_rate = hyp_count/total_year
 ) %>%
 filter(Any_Hypertension %in% 1,
        !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, Any_Hypertension, hyp_count, hyp_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CDuid, Any_Hypertension)) %>%
 group_by(CDuid) %>%
 mutate(
  hyp_delta = (hyp_rate - lag(hyp_rate))/lag(hyp_rate),
  hyp_delta = ifelse(FiscalYear == "2013-2014", NA, hyp_delta),
  hyp_deltap = (hyp_rate - lag(hyp_rate,10))/lag(hyp_rate,10)
 ) %>%
 ungroup()

#### Diabetes ----

c4 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, PreExisting_Diabetes, CDuid) %>%
 distinct() %>%
 group_by(FiscalYear, PreExisting_Diabetes, CDuid) %>%
 mutate(
  prediab_count = n()
 ) %>%
 group_by(FiscalYear, CDuid) %>%
 mutate(
  total_year = n(),
  prediab_rate = prediab_count/total_year
 ) %>%
 filter(PreExisting_Diabetes %in% 1,
        !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, PreExisting_Diabetes, prediab_count, prediab_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CDuid, PreExisting_Diabetes)) %>%
 group_by(CDuid) %>%
 mutate(
  prediab_delta = (prediab_rate - lag(prediab_rate))/lag(prediab_rate),
  prediab_delta = ifelse(FiscalYear == "2013-2014", NA, prediab_delta),
  prediab_deltap = (prediab_rate - lag(prediab_rate,10))/lag(prediab_rate,10)
 ) %>%
 ungroup()

c5 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, GDM, CDuid) %>%
 distinct() %>%
 group_by(FiscalYear, GDM, CDuid) %>%
 mutate(
  gestdiab_count = n()
 ) %>%
 group_by(FiscalYear, CDuid) %>%
 mutate(
  total_year = n(),
  gestdiab_rate = gestdiab_count/total_year
 ) %>%
 filter(GDM %in% 1,
        !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, GDM, gestdiab_count, gestdiab_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CDuid, GDM)) %>%
 group_by(CDuid) %>%
 mutate(
  gestdiab_delta = (gestdiab_rate - lag(gestdiab_rate))/lag(gestdiab_rate),
  gestdiab_delta = ifelse(FiscalYear == "2013-2014", NA, gestdiab_delta),
  gestdiab_deltap = (gestdiab_rate - lag(gestdiab_rate,10))/lag(gestdiab_rate,10)
 ) %>%
 ungroup()

c6 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Any_Diabetes, CDuid) %>%
 distinct() %>%
 group_by(FiscalYear, Any_Diabetes, CDuid) %>%
 mutate(
  diab_count = n()
 ) %>%
 group_by(FiscalYear, CDuid) %>%
 mutate(
  total_year = n(),
  diab_rate = diab_count/total_year
 ) %>%
 filter(Any_Diabetes %in% 1,
        !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, Any_Diabetes, diab_count, diab_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CDuid, Any_Diabetes)) %>%
 group_by(CDuid) %>%
 mutate(
  diab_delta = (diab_rate - lag(diab_rate))/lag(diab_rate),
  diab_delta = ifelse(FiscalYear == "2013-2014", NA, diab_delta),
  diab_deltap = (diab_rate - lag(diab_rate,10))/lag(diab_rate,10)
 ) %>%
 ungroup()

#### Robson group ----

c7 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs1, RobsnGrp, CDuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs1, CDuid) %>%
 mutate(
  rbs1_count = n()
 ) %>%
 group_by(FiscalYear, CDuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs1_rate = rbs1_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs1 %in% 1,
        RobsnGrp %in% 1,
        !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, rbs1, rbs1_count, rbs1_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CDuid, rbs1)) %>%
 group_by(CDuid) %>%
 mutate(
  rbs1_delta = (rbs1_rate - lag(rbs1_rate))/lag(rbs1_rate),
  rbs1_delta = ifelse(FiscalYear == "2013-2014", NA, rbs1_delta),
  rbs1_deltap = (rbs1_rate - lag(rbs1_rate,10))/lag(rbs1_rate,10)
 ) %>%
 ungroup()

c8 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs21, RobsnGrp, CDuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs21, CDuid) %>%
 mutate(
  rbs21_count = n()
 ) %>%
 group_by(FiscalYear, CDuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs21_rate = rbs21_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs21 %in% 1,
        RobsnGrp %in% 2.1,
        !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, rbs21, rbs21_count, rbs21_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CDuid, rbs21)) %>%
 group_by(CDuid) %>%
 mutate(
  rbs21_delta = (rbs21_rate - lag(rbs21_rate))/lag(rbs21_rate),
  rbs21_delta = ifelse(FiscalYear == "2013-2014", NA, rbs21_delta),
  rbs21_deltap = (rbs21_rate - lag(rbs21_rate,10))/lag(rbs21_rate,10)
 ) %>%
 ungroup()

c9 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs51, RobsnGrp, CDuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs51, CDuid) %>%
 mutate(
  rbs51_count = n()
 ) %>%
 group_by(FiscalYear, CDuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs51_rate = rbs51_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs51 %in% 1,
        RobsnGrp %in% 5.1,
        !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, rbs51, rbs51_count, rbs51_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CDuid, rbs51)) %>%
 group_by(CDuid) %>%
 mutate(
  rbs51_delta = (rbs51_rate - lag(rbs51_rate))/lag(rbs51_rate),
  rbs51_delta = ifelse(FiscalYear == "2013-2014", NA, rbs51_delta),
  rbs51_deltap = (rbs51_rate - lag(rbs51_rate,10))/lag(rbs51_rate,10)
 ) %>%
 ungroup()

#### Postpartum readmission ----

c10 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, ppreadm, CDuid) %>%
 distinct() %>%
 group_by(FiscalYear, ppreadm, CDuid) %>%
 mutate(
  ppreadm_count = n()
 ) %>%
 group_by(FiscalYear, CDuid) %>%
 mutate(
  total_year = n(),
  ppreadm_rate = ppreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(ppreadm %in% 1,
        !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, ppreadm, ppreadm_count, ppreadm_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CDuid, ppreadm)) %>%
 group_by(CDuid) %>%
 mutate(
  ppreadm_delta = (ppreadm_rate - lag(ppreadm_rate))/lag(ppreadm_rate),
  ppreadm_delta = ifelse(FiscalYear == "2013-2014", NA, ppreadm_delta),
  ppreadm_deltap = (ppreadm_rate - lag(ppreadm_rate,10))/lag(ppreadm_rate,10)
 ) %>%
 ungroup()

#### Skin to skin ----

c11 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknskn, CDuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, sknskn, CDuid) %>%
 mutate(
  sknskn_count = n()
 ) %>%
 group_by(FiscalYear, CDuid, lvb) %>%
 mutate(
  total_year = n(),
  sknskn_rate = sknskn_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskn %in% 1,
        !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, sknskn, sknskn_count, sknskn_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CDuid, sknskn)) %>%
 group_by(CDuid) %>%
 mutate(
  sknskn_delta = (sknskn_rate - lag(sknskn_rate))/lag(sknskn_rate),
  sknskn_delta = ifelse(FiscalYear == "2013-2014", NA, sknskn_delta),
  sknskn_deltap = (sknskn_rate - lag(sknskn_rate,10))/lag(sknskn_rate,10)
 ) %>%
 ungroup()

c12 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknsknvg, CDuid, lvbvg) %>%
 distinct() %>%
 group_by(FiscalYear, sknsknvg, CDuid) %>%
 mutate(
  sknsknvg_count = n()
 ) %>%
 group_by(FiscalYear, CDuid, lvbvg) %>%
 mutate(
  total_year = n(),
  sknsknvg_rate = sknsknvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknsknvg %in% 1,
        !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, sknsknvg, sknsknvg_count, sknsknvg_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CDuid, sknsknvg)) %>%
 group_by(CDuid) %>%
 mutate(
  sknsknvg_delta = (sknsknvg_rate - lag(sknsknvg_rate))/lag(sknsknvg_rate),
  sknsknvg_delta = ifelse(FiscalYear == "2013-2014", NA, sknsknvg_delta),
  sknsknvg_deltap = (sknsknvg_rate - lag(sknsknvg_rate,10))/lag(sknsknvg_rate,10)
 ) %>%
 ungroup()

c13 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknskncs, CDuid, lvbcs) %>%
 distinct() %>%
 group_by(FiscalYear, sknskncs, CDuid) %>%
 mutate(
  sknskncs_count = n()
 ) %>%
 group_by(FiscalYear, CDuid, lvbcs) %>%
 mutate(
  total_year = n(),
  sknskncs_rate = sknskncs_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskncs %in% 1,
        !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, sknskncs, sknskncs_count, sknskncs_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CDuid, sknskncs)) %>%
 group_by(CDuid) %>%
 mutate(
  sknskncs_delta = (sknskncs_rate - lag(sknskncs_rate))/lag(sknskncs_rate),
  sknskncs_delta = ifelse(FiscalYear == "2013-2014", NA, sknskncs_delta),
  sknskncs_deltap = (sknskncs_rate - lag(sknskncs_rate,10))/lag(sknskncs_rate,10)
 ) %>%
 ungroup()

#### Neonatal readmission ----

c14 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, neoreadm, CDuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, neoreadm, CDuid) %>%
 mutate(
  neoreadm_count = n()
 ) %>%
 group_by(FiscalYear, CDuid, lvb) %>%
 mutate(
  total_year = n(),
  neoreadm_rate = neoreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(neoreadm %in% 1,
        !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, neoreadm, neoreadm_count, neoreadm_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CDuid, neoreadm)) %>%
 group_by(CDuid) %>%
 mutate(
  neoreadm_delta = (neoreadm_rate - lag(neoreadm_rate))/lag(neoreadm_rate),
  neoreadm_delta = ifelse(FiscalYear == "2013-2014", NA, neoreadm_delta),
  neoreadm_deltap = (neoreadm_rate - lag(neoreadm_rate,10))/lag(neoreadm_rate,10)
 ) %>%
 ungroup()

#### Milk feeding ----

c15 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, excbrst, CDuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, excbrst, CDuid) %>%
 mutate(
  excbrst_count = n()
 ) %>%
 group_by(FiscalYear, CDuid, lvb) %>%
 mutate(
  total_year = n(),
  excbrst_rate = excbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(excbrst %in% 1,
        !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, excbrst, excbrst_count, excbrst_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CDuid, excbrst)) %>%
 group_by(CDuid) %>%
 mutate(
  excbrst_delta = (excbrst_rate - lag(excbrst_rate))/lag(excbrst_rate),
  excbrst_delta = ifelse(FiscalYear == "2013-2014", NA, excbrst_delta),
  excbrst_deltap = (excbrst_rate - lag(excbrst_rate,10))/lag(excbrst_rate,10)
 ) %>%
 ungroup()

c16 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, nexcbrst, CDuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, nexcbrst, CDuid) %>%
 mutate(
  nexcbrst_count = n()
 ) %>%
 group_by(FiscalYear, CDuid, lvb) %>%
 mutate(
  total_year = n(),
  nexcbrst_rate = nexcbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nexcbrst %in% 1,
        !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, nexcbrst, nexcbrst_count, nexcbrst_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CDuid, nexcbrst)) %>%
 group_by(CDuid) %>%
 mutate(
  nexcbrst_delta = (nexcbrst_rate - lag(nexcbrst_rate))/lag(nexcbrst_rate),
  nexcbrst_delta = ifelse(FiscalYear == "2013-2014", NA, nexcbrst_delta),
  nexcbrst_deltap = (nexcbrst_rate - lag(nexcbrst_rate,10))/lag(nexcbrst_rate,10)
 ) %>%
 ungroup()

c17 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, nbrst, CDuid, lvbint) %>%
 distinct() %>%
 group_by(FiscalYear, nbrst, CDuid) %>%
 mutate(
  nbrst_count = n()
 ) %>%
 group_by(FiscalYear, CDuid, lvbint) %>%
 mutate(
  total_year = n(),
  nbrst_rate = nbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nbrst %in% 1,
        !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, nbrst, nbrst_count, nbrst_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CDuid, nbrst)) %>%
 group_by(CDuid) %>%
 mutate(
  nbrst_delta = (nbrst_rate - lag(nbrst_rate))/lag(nbrst_rate),
  nbrst_delta = ifelse(FiscalYear == "2013-2014", NA, nbrst_delta),
  nbrst_deltap = (nbrst_rate - lag(nbrst_rate,10))/lag(nbrst_rate,10)
 ) %>%
 ungroup()

c18 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, brstinit, CDuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, brstinit, CDuid) %>%
 mutate(
  brstinit_count = n()
 ) %>%
 group_by(FiscalYear, CDuid, lvb) %>%
 mutate(
  total_year = n(),
  brstinit_rate = brstinit_count/total_year
 ) %>%
 ungroup() %>%
 filter(brstinit %in% 1,
        !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, brstinit, brstinit_count, brstinit_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CDuid, brstinit)) %>%
 group_by(CDuid) %>%
 mutate(
  brstinit_delta = (brstinit_rate - lag(brstinit_rate))/lag(brstinit_rate),
  brstinit_delta = ifelse(FiscalYear == "2013-2014", NA, brstinit_delta),
  brstinit_deltap = (brstinit_rate - lag(brstinit_rate,10))/lag(brstinit_rate,10)
 ) %>%
 ungroup()

fcd_stats <- cbind(
 c1 %>% select(-PreExisting_Hypertension),
 c2 %>% select(-Gestational_Hypertension,-FiscalYear, -CDuid),
 c3 %>% select(-Any_Hypertension,-FiscalYear, -CDuid),
 c4 %>% select(-PreExisting_Diabetes,-FiscalYear, -CDuid),
 c5 %>% select(-GDM,-FiscalYear, -CDuid),
 c6 %>% select(-Any_Diabetes,-FiscalYear, -CDuid),
 c7 %>% select(-rbs1,-FiscalYear, -CDuid),
 c8 %>% select(-rbs21,-FiscalYear, -CDuid),
 c9 %>% select(-rbs51,-FiscalYear, -CDuid),
 c10 %>% select(-ppreadm,-FiscalYear, -CDuid),
 c11 %>% select(-sknskn,-FiscalYear, -CDuid),
 c12 %>% select(-sknsknvg,-FiscalYear, -CDuid),
 c13 %>% select(-sknskncs,-FiscalYear, -CDuid),
 c14 %>% select(-neoreadm,-FiscalYear, -CDuid),
 c15 %>% select(-excbrst,-FiscalYear, -CDuid),
 c16 %>% select(-nexcbrst,-FiscalYear, -CDuid),
 c17 %>% select(-nbrst,-FiscalYear, -CDuid),
 c18 %>% select(-brstinit,-FiscalYear, -CDuid)
)

### Community cluster (CL) ----
#### Hypertension ----

c1 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, PreExisting_Hypertension, CLuid) %>%
 distinct() %>%
 group_by(FiscalYear, PreExisting_Hypertension, CLuid) %>%
 mutate(
  prehyp_count = n()
 ) %>%
 group_by(FiscalYear, CLuid) %>%
 mutate(
  total_year = n(),
  prehyp_rate = prehyp_count/total_year
 ) %>%
 filter(PreExisting_Hypertension %in% 1,
        !is.na(CLuid)
 ) %>%
 select(FiscalYear, CLuid, PreExisting_Hypertension, prehyp_count, prehyp_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CLuid, PreExisting_Hypertension)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(FiscalYear, PreExisting_Hypertension)) %>%
 group_by(CLuid) %>%
 mutate(
  prehyp_delta = (prehyp_rate - lag(prehyp_rate))/lag(prehyp_rate),
  prehyp_delta = ifelse(FiscalYear == "2013-2014", NA, prehyp_delta),
  prehyp_deltap = (prehyp_rate - lag(prehyp_rate,10))/lag(prehyp_rate,10)
 ) %>%
 ungroup()

c2 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Gestational_Hypertension, CLuid) %>%
 distinct() %>%
 group_by(FiscalYear, Gestational_Hypertension, CLuid) %>%
 mutate(
  gesthyp_count = n()
 ) %>%
 group_by(FiscalYear, CLuid) %>%
 mutate(
  total_year = n(),
  gesthyp_rate = gesthyp_count/total_year
 ) %>%
 filter(Gestational_Hypertension %in% 1,
        !is.na(CLuid)
 ) %>%
 select(FiscalYear, CLuid, Gestational_Hypertension, gesthyp_count, gesthyp_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CLuid, Gestational_Hypertension)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(FiscalYear, Gestational_Hypertension)) %>%
 group_by(CLuid) %>%
 mutate(
  gesthyp_delta = (gesthyp_rate - lag(gesthyp_rate))/lag(gesthyp_rate),
  gesthyp_delta = ifelse(FiscalYear == "2013-2014", NA, gesthyp_delta),
  gesthyp_deltap = (gesthyp_rate - lag(gesthyp_rate,10))/lag(gesthyp_rate,10)
 ) %>%
 ungroup()

c3 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Any_Hypertension, CLuid) %>%
 distinct() %>%
 group_by(FiscalYear, Any_Hypertension, CLuid) %>%
 mutate(
  hyp_count = n()
 ) %>%
 group_by(FiscalYear, CLuid) %>%
 mutate(
  total_year = n(),
  hyp_rate = hyp_count/total_year
 ) %>%
 filter(Any_Hypertension %in% 1,
        !is.na(CLuid)
 ) %>%
 select(FiscalYear, CLuid, Any_Hypertension, hyp_count, hyp_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CLuid, Any_Hypertension)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(FiscalYear, Any_Hypertension)) %>%
 group_by(CLuid) %>%
 mutate(
  hyp_delta = (hyp_rate - lag(hyp_rate))/lag(hyp_rate),
  hyp_delta = ifelse(FiscalYear == "2013-2014", NA, hyp_delta),
  hyp_deltap = (hyp_rate - lag(hyp_rate,10))/lag(hyp_rate,10)
 ) %>%
 ungroup()

#### Diabetes ----

c4 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, PreExisting_Diabetes, CLuid) %>%
 distinct() %>%
 group_by(FiscalYear, PreExisting_Diabetes, CLuid) %>%
 mutate(
  prediab_count = n()
 ) %>%
 group_by(FiscalYear, CLuid) %>%
 mutate(
  total_year = n(),
  prediab_rate = prediab_count/total_year
 ) %>%
 filter(PreExisting_Diabetes %in% 1,
        !is.na(CLuid)
 ) %>%
 select(FiscalYear, CLuid, PreExisting_Diabetes, prediab_count, prediab_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CLuid, PreExisting_Diabetes)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(FiscalYear, PreExisting_Diabetes)) %>%
 group_by(CLuid) %>%
 mutate(
  prediab_delta = (prediab_rate - lag(prediab_rate))/lag(prediab_rate),
  prediab_delta = ifelse(FiscalYear == "2013-2014", NA, prediab_delta),
  prediab_deltap = (prediab_rate - lag(prediab_rate,10))/lag(prediab_rate,10)
 ) %>%
 ungroup()

c5 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, GDM, CLuid) %>%
 distinct() %>%
 group_by(FiscalYear, GDM, CLuid) %>%
 mutate(
  gestdiab_count = n()
 ) %>%
 group_by(FiscalYear, CLuid) %>%
 mutate(
  total_year = n(),
  gestdiab_rate = gestdiab_count/total_year
 ) %>%
 filter(GDM %in% 1,
        !is.na(CLuid)
 ) %>%
 select(FiscalYear, CLuid, GDM, gestdiab_count, gestdiab_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CLuid, GDM)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(FiscalYear, GDM)) %>%
 group_by(CLuid) %>%
 mutate(
  gestdiab_delta = (gestdiab_rate - lag(gestdiab_rate))/lag(gestdiab_rate),
  gestdiab_delta = ifelse(FiscalYear == "2013-2014", NA, gestdiab_delta),
  gestdiab_deltap = (gestdiab_rate - lag(gestdiab_rate,10))/lag(gestdiab_rate,10)
 ) %>%
 ungroup()

c6 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Any_Diabetes, CLuid) %>%
 distinct() %>%
 group_by(FiscalYear, Any_Diabetes, CLuid) %>%
 mutate(
  diab_count = n()
 ) %>%
 group_by(FiscalYear, CLuid) %>%
 mutate(
  total_year = n(),
  diab_rate = diab_count/total_year
 ) %>%
 filter(Any_Diabetes %in% 1,
        !is.na(CLuid)
 ) %>%
 select(FiscalYear, CLuid, Any_Diabetes, diab_count, diab_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CLuid, Any_Diabetes)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(FiscalYear, Any_Diabetes)) %>%
 group_by(CLuid) %>%
 mutate(
  diab_delta = (diab_rate - lag(diab_rate))/lag(diab_rate),
  diab_delta = ifelse(FiscalYear == "2013-2014", NA, diab_delta),
  diab_deltap = (diab_rate - lag(diab_rate,10))/lag(diab_rate,10)
 ) %>%
 ungroup()

#### Robson group ----

c7 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs1, RobsnGrp, CLuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs1, CLuid) %>%
 mutate(
  rbs1_count = n()
 ) %>%
 group_by(FiscalYear, CLuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs1_rate = rbs1_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs1 %in% 1,
        RobsnGrp %in% 1,
        !is.na(CLuid)
 ) %>%
 select(FiscalYear, CLuid, rbs1, rbs1_count, rbs1_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CLuid, rbs1)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(FiscalYear, rbs1)) %>%
 group_by(CLuid) %>%
 mutate(
  rbs1_delta = (rbs1_rate - lag(rbs1_rate))/lag(rbs1_rate),
  rbs1_delta = ifelse(FiscalYear == "2013-2014", NA, rbs1_delta),
  rbs1_deltap = (rbs1_rate - lag(rbs1_rate,10))/lag(rbs1_rate,10)
 ) %>%
 ungroup()

c8 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs21, RobsnGrp, CLuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs21, CLuid) %>%
 mutate(
  rbs21_count = n()
 ) %>%
 group_by(FiscalYear, CLuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs21_rate = rbs21_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs21 %in% 1,
        RobsnGrp %in% 2.1,
        !is.na(CLuid)
 ) %>%
 select(FiscalYear, CLuid, rbs21, rbs21_count, rbs21_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CLuid, rbs21)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(FiscalYear, rbs21)) %>%
 group_by(CLuid) %>%
 mutate(
  rbs21_delta = (rbs21_rate - lag(rbs21_rate))/lag(rbs21_rate),
  rbs21_delta = ifelse(FiscalYear == "2013-2014", NA, rbs21_delta),
  rbs21_deltap = (rbs21_rate - lag(rbs21_rate,10))/lag(rbs21_rate,10)
 ) %>%
 ungroup()

c9 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs51, RobsnGrp, CLuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs51, CLuid) %>%
 mutate(
  rbs51_count = n()
 ) %>%
 group_by(FiscalYear, CLuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs51_rate = rbs51_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs51 %in% 1,
        RobsnGrp %in% 5.1,
        !is.na(CLuid)
 ) %>%
 select(FiscalYear, CLuid, rbs51, rbs51_count, rbs51_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CLuid, rbs51)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(FiscalYear, rbs51)) %>%
 group_by(CLuid) %>%
 mutate(
  rbs51_delta = (rbs51_rate - lag(rbs51_rate))/lag(rbs51_rate),
  rbs51_delta = ifelse(FiscalYear == "2013-2014", NA, rbs51_delta),
  rbs51_deltap = (rbs51_rate - lag(rbs51_rate,10))/lag(rbs51_rate,10)
 ) %>%
 ungroup()

#### Postpartum readmission ----

c10 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, ppreadm, CLuid) %>%
 distinct() %>%
 group_by(FiscalYear, ppreadm, CLuid) %>%
 mutate(
  ppreadm_count = n()
 ) %>%
 group_by(FiscalYear, CLuid) %>%
 mutate(
  total_year = n(),
  ppreadm_rate = ppreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(ppreadm %in% 1,
        !is.na(CLuid)
 ) %>%
 select(FiscalYear, CLuid, ppreadm, ppreadm_count, ppreadm_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CLuid, ppreadm)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(FiscalYear, ppreadm)) %>%
 group_by(CLuid) %>%
 mutate(
  ppreadm_delta = (ppreadm_rate - lag(ppreadm_rate))/lag(ppreadm_rate),
  ppreadm_delta = ifelse(FiscalYear == "2013-2014", NA, ppreadm_delta),
  ppreadm_deltap = (ppreadm_rate - lag(ppreadm_rate,10))/lag(ppreadm_rate,10)
 ) %>%
 ungroup()

#### Skin to skin ----

c11 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknskn, CLuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, sknskn, CLuid) %>%
 mutate(
  sknskn_count = n()
 ) %>%
 group_by(FiscalYear, CLuid, lvb) %>%
 mutate(
  total_year = n(),
  sknskn_rate = sknskn_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskn %in% 1,
        !is.na(CLuid)
 ) %>%
 select(FiscalYear, CLuid, sknskn, sknskn_count, sknskn_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CLuid, sknskn)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(FiscalYear, sknskn)) %>%
 group_by(CLuid) %>%
 mutate(
  sknskn_delta = (sknskn_rate - lag(sknskn_rate))/lag(sknskn_rate),
  sknskn_delta = ifelse(FiscalYear == "2013-2014", NA, sknskn_delta),
  sknskn_deltap = (sknskn_rate - lag(sknskn_rate,10))/lag(sknskn_rate,10)
 ) %>%
 ungroup()

c12 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknsknvg, CLuid, lvbvg) %>%
 distinct() %>%
 group_by(FiscalYear, sknsknvg, CLuid) %>%
 mutate(
  sknsknvg_count = n()
 ) %>%
 group_by(FiscalYear, CLuid, lvbvg) %>%
 mutate(
  total_year = n(),
  sknsknvg_rate = sknsknvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknsknvg %in% 1,
        !is.na(CLuid)
 ) %>%
 select(FiscalYear, CLuid, sknsknvg, sknsknvg_count, sknsknvg_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CLuid, sknsknvg)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(FiscalYear, sknsknvg)) %>%
 group_by(CLuid) %>%
 mutate(
  sknsknvg_delta = (sknsknvg_rate - lag(sknsknvg_rate))/lag(sknsknvg_rate),
  sknsknvg_delta = ifelse(FiscalYear == "2013-2014", NA, sknsknvg_delta),
  sknsknvg_deltap = (sknsknvg_rate - lag(sknsknvg_rate,10))/lag(sknsknvg_rate,10)
 ) %>%
 ungroup()

c13 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknskncs, CLuid, lvbcs) %>%
 distinct() %>%
 group_by(FiscalYear, sknskncs, CLuid) %>%
 mutate(
  sknskncs_count = n()
 ) %>%
 group_by(FiscalYear, CLuid, lvbcs) %>%
 mutate(
  total_year = n(),
  sknskncs_rate = sknskncs_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskncs %in% 1,
        !is.na(CLuid)
 ) %>%
 select(FiscalYear, CLuid, sknskncs, sknskncs_count, sknskncs_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CLuid, sknskncs)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(FiscalYear, sknskncs)) %>%
 group_by(CLuid) %>%
 mutate(
  sknskncs_delta = (sknskncs_rate - lag(sknskncs_rate))/lag(sknskncs_rate),
  sknskncs_delta = ifelse(FiscalYear == "2013-2014", NA, sknskncs_delta),
  sknskncs_deltap = (sknskncs_rate - lag(sknskncs_rate,10))/lag(sknskncs_rate,10)
 ) %>%
 ungroup()

#### Neonatal readmission ----

c14 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, neoreadm, CLuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, neoreadm, CLuid) %>%
 mutate(
  neoreadm_count = n()
 ) %>%
 group_by(FiscalYear, CLuid, lvb) %>%
 mutate(
  total_year = n(),
  neoreadm_rate = neoreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(neoreadm %in% 1,
        !is.na(CLuid)
 ) %>%
 select(FiscalYear, CLuid, neoreadm, neoreadm_count, neoreadm_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CLuid, neoreadm)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(FiscalYear, neoreadm)) %>%
 group_by(CLuid) %>%
 mutate(
  neoreadm_delta = (neoreadm_rate - lag(neoreadm_rate))/lag(neoreadm_rate),
  neoreadm_delta = ifelse(FiscalYear == "2013-2014", NA, neoreadm_delta),
  neoreadm_deltap = (neoreadm_rate - lag(neoreadm_rate,10))/lag(neoreadm_rate,10)
 ) %>%
 ungroup()

#### Milk feeding ----

c15 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, excbrst, CLuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, excbrst, CLuid) %>%
 mutate(
  excbrst_count = n()
 ) %>%
 group_by(FiscalYear, CLuid, lvb) %>%
 mutate(
  total_year = n(),
  excbrst_rate = excbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(excbrst %in% 1,
        !is.na(CLuid)
 ) %>%
 select(FiscalYear, CLuid, excbrst, excbrst_count, excbrst_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CLuid, excbrst)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(FiscalYear, excbrst)) %>%
 group_by(CLuid) %>%
 mutate(
  excbrst_delta = (excbrst_rate - lag(excbrst_rate))/lag(excbrst_rate),
  excbrst_delta = ifelse(FiscalYear == "2013-2014", NA, excbrst_delta),
  excbrst_deltap = (excbrst_rate - lag(excbrst_rate,10))/lag(excbrst_rate,10)
 ) %>%
 ungroup()

c16 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, nexcbrst, CLuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, nexcbrst, CLuid) %>%
 mutate(
  nexcbrst_count = n()
 ) %>%
 group_by(FiscalYear, CLuid, lvb) %>%
 mutate(
  total_year = n(),
  nexcbrst_rate = nexcbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nexcbrst %in% 1,
        !is.na(CLuid)
 ) %>%
 select(FiscalYear, CLuid, nexcbrst, nexcbrst_count, nexcbrst_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CLuid, nexcbrst)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(FiscalYear, nexcbrst)) %>%
 group_by(CLuid) %>%
 mutate(
  nexcbrst_delta = (nexcbrst_rate - lag(nexcbrst_rate))/lag(nexcbrst_rate),
  nexcbrst_delta = ifelse(FiscalYear == "2013-2014", NA, nexcbrst_delta),
  nexcbrst_deltap = (nexcbrst_rate - lag(nexcbrst_rate,10))/lag(nexcbrst_rate,10)
 ) %>%
 ungroup()

c17 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, nbrst, CLuid, lvbint) %>%
 distinct() %>%
 group_by(FiscalYear, nbrst, CLuid) %>%
 mutate(
  nbrst_count = n()
 ) %>%
 group_by(FiscalYear, CLuid, lvbint) %>%
 mutate(
  total_year = n(),
  nbrst_rate = nbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nbrst %in% 1,
        !is.na(CLuid)
 ) %>%
 select(FiscalYear, CLuid, nbrst, nbrst_count, nbrst_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CLuid, nbrst)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(FiscalYear, nbrst)) %>%
 group_by(CLuid) %>%
 mutate(
  nbrst_delta = (nbrst_rate - lag(nbrst_rate))/lag(nbrst_rate),
  nbrst_delta = ifelse(FiscalYear == "2013-2014", NA, nbrst_delta),
  nbrst_deltap = (nbrst_rate - lag(nbrst_rate,10))/lag(nbrst_rate,10)
 ) %>%
 ungroup()

c18 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, brstinit, CLuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, brstinit, CLuid) %>%
 mutate(
  brstinit_count = n()
 ) %>%
 group_by(FiscalYear, CLuid, lvb) %>%
 mutate(
  total_year = n(),
  brstinit_rate = brstinit_count/total_year
 ) %>%
 ungroup() %>%
 filter(brstinit %in% 1,
        !is.na(CLuid)
 ) %>%
 select(FiscalYear, CLuid, brstinit, brstinit_count, brstinit_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CLuid, brstinit)) %>%
 tidyr::complete(CLuid = sort(unique(dta$CLuid)),
                 tidyr::nesting(FiscalYear, brstinit)) %>%
 group_by(CLuid) %>%
 mutate(
  brstinit_delta = (brstinit_rate - lag(brstinit_rate))/lag(brstinit_rate),
  brstinit_delta = ifelse(FiscalYear == "2013-2014", NA, brstinit_delta),
  brstinit_deltap = (brstinit_rate - lag(brstinit_rate,10))/lag(brstinit_rate,10)
 ) %>%
 ungroup()

fcl_stats <- cbind(
 c1 %>% select(-PreExisting_Hypertension),
 c2 %>% select(-Gestational_Hypertension,-FiscalYear, -CLuid),
 c3 %>% select(-Any_Hypertension,-FiscalYear, -CLuid),
 c4 %>% select(-PreExisting_Diabetes,-FiscalYear, -CLuid),
 c5 %>% select(-GDM,-FiscalYear, -CLuid),
 c6 %>% select(-Any_Diabetes,-FiscalYear, -CLuid),
 c7 %>% select(-rbs1,-FiscalYear, -CLuid),
 c8 %>% select(-rbs21,-FiscalYear, -CLuid),
 c9 %>% select(-rbs51,-FiscalYear, -CLuid),
 c10 %>% select(-ppreadm,-FiscalYear, -CLuid),
 c11 %>% select(-sknskn,-FiscalYear, -CLuid),
 c12 %>% select(-sknsknvg,-FiscalYear, -CLuid),
 c13 %>% select(-sknskncs,-FiscalYear, -CLuid),
 c14 %>% select(-neoreadm,-FiscalYear, -CLuid),
 c15 %>% select(-excbrst,-FiscalYear, -CLuid),
 c16 %>% select(-nexcbrst,-FiscalYear, -CLuid),
 c17 %>% select(-nbrst,-FiscalYear, -CLuid),
 c18 %>% select(-brstinit,-FiscalYear, -CLuid)
)

### Community health network (CHN) ----
#### Hypertension ----

c1 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, PreExisting_Hypertension, CHNuid) %>%
 distinct() %>%
 group_by(FiscalYear, PreExisting_Hypertension, CHNuid) %>%
 mutate(
  prehyp_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid) %>%
 mutate(
  total_year = n(),
  prehyp_rate = prehyp_count/total_year
 ) %>%
 filter(PreExisting_Hypertension %in% 1,
        !is.na(CHNuid)
 ) %>%
 select(FiscalYear, CHNuid, PreExisting_Hypertension, prehyp_count, prehyp_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CHNuid, PreExisting_Hypertension)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(FiscalYear, PreExisting_Hypertension)) %>%
 group_by(CHNuid) %>%
 mutate(
  prehyp_delta = (prehyp_rate - lag(prehyp_rate))/lag(prehyp_rate),
  prehyp_delta = ifelse(FiscalYear == "2013-2014", NA, prehyp_delta),
  prehyp_deltap = (prehyp_rate - lag(prehyp_rate,10))/lag(prehyp_rate,10)
 ) %>%
 ungroup()

c2 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Gestational_Hypertension, CHNuid) %>%
 distinct() %>%
 group_by(FiscalYear, Gestational_Hypertension, CHNuid) %>%
 mutate(
  gesthyp_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid) %>%
 mutate(
  total_year = n(),
  gesthyp_rate = gesthyp_count/total_year
 ) %>%
 filter(Gestational_Hypertension %in% 1,
        !is.na(CHNuid)
 ) %>%
 select(FiscalYear, CHNuid, Gestational_Hypertension, gesthyp_count, gesthyp_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CHNuid, Gestational_Hypertension)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(FiscalYear, Gestational_Hypertension)) %>%
 group_by(CHNuid) %>%
 mutate(
  gesthyp_delta = (gesthyp_rate - lag(gesthyp_rate))/lag(gesthyp_rate),
  gesthyp_delta = ifelse(FiscalYear == "2013-2014", NA, gesthyp_delta),
  gesthyp_deltap = (gesthyp_rate - lag(gesthyp_rate,10))/lag(gesthyp_rate,10)
 ) %>%
 ungroup()

c3 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Any_Hypertension, CHNuid) %>%
 distinct() %>%
 group_by(FiscalYear, Any_Hypertension, CHNuid) %>%
 mutate(
  hyp_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid) %>%
 mutate(
  total_year = n(),
  hyp_rate = hyp_count/total_year
 ) %>%
 filter(Any_Hypertension %in% 1,
        !is.na(CHNuid)
 ) %>%
 select(FiscalYear, CHNuid, Any_Hypertension, hyp_count, hyp_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CHNuid, Any_Hypertension)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(FiscalYear, Any_Hypertension)) %>%
 group_by(CHNuid) %>%
 mutate(
  hyp_delta = (hyp_rate - lag(hyp_rate))/lag(hyp_rate),
  hyp_delta = ifelse(FiscalYear == "2013-2014", NA, hyp_delta),
  hyp_deltap = (hyp_rate - lag(hyp_rate,10))/lag(hyp_rate,10)
 ) %>%
 ungroup()

#### Diabetes ----

c4 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, PreExisting_Diabetes, CHNuid) %>%
 distinct() %>%
 group_by(FiscalYear, PreExisting_Diabetes, CHNuid) %>%
 mutate(
  prediab_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid) %>%
 mutate(
  total_year = n(),
  prediab_rate = prediab_count/total_year
 ) %>%
 filter(PreExisting_Diabetes %in% 1,
        !is.na(CHNuid)
 ) %>%
 select(FiscalYear, CHNuid, PreExisting_Diabetes, prediab_count, prediab_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CHNuid, PreExisting_Diabetes)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(FiscalYear, PreExisting_Diabetes)) %>%
 group_by(CHNuid) %>%
 mutate(
  prediab_delta = (prediab_rate - lag(prediab_rate))/lag(prediab_rate),
  prediab_delta = ifelse(FiscalYear == "2013-2014", NA, prediab_delta),
  prediab_deltap = (prediab_rate - lag(prediab_rate,10))/lag(prediab_rate,10)
 ) %>%
 ungroup()

c5 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, GDM, CHNuid) %>%
 distinct() %>%
 group_by(FiscalYear, GDM, CHNuid) %>%
 mutate(
  gestdiab_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid) %>%
 mutate(
  total_year = n(),
  gestdiab_rate = gestdiab_count/total_year
 ) %>%
 filter(GDM %in% 1,
        !is.na(CHNuid)
 ) %>%
 select(FiscalYear, CHNuid, GDM, gestdiab_count, gestdiab_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CHNuid, GDM)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(FiscalYear, GDM)) %>%
 group_by(CHNuid) %>%
 mutate(
  gestdiab_delta = (gestdiab_rate - lag(gestdiab_rate))/lag(gestdiab_rate),
  gestdiab_delta = ifelse(FiscalYear == "2013-2014", NA, gestdiab_delta),
  gestdiab_deltap = (gestdiab_rate - lag(gestdiab_rate,10))/lag(gestdiab_rate,10)
 ) %>%
 ungroup()

c6 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Any_Diabetes, CHNuid) %>%
 distinct() %>%
 group_by(FiscalYear, Any_Diabetes, CHNuid) %>%
 mutate(
  diab_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid) %>%
 mutate(
  total_year = n(),
  diab_rate = diab_count/total_year
 ) %>%
 filter(Any_Diabetes %in% 1,
        !is.na(CHNuid)
 ) %>%
 select(FiscalYear, CHNuid, Any_Diabetes, diab_count, diab_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CHNuid, Any_Diabetes)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(FiscalYear, Any_Diabetes)) %>%
 group_by(CHNuid) %>%
 mutate(
  diab_delta = (diab_rate - lag(diab_rate))/lag(diab_rate),
  diab_delta = ifelse(FiscalYear == "2013-2014", NA, diab_delta),
  diab_deltap = (diab_rate - lag(diab_rate,10))/lag(diab_rate,10)
 ) %>%
 ungroup()

#### Robson group ----

c7 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs1, RobsnGrp, CHNuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs1, CHNuid) %>%
 mutate(
  rbs1_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs1_rate = rbs1_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs1 %in% 1,
        RobsnGrp %in% 1,
        !is.na(CHNuid)
 ) %>%
 select(FiscalYear, CHNuid, rbs1, rbs1_count, rbs1_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CHNuid, rbs1)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(FiscalYear, rbs1)) %>%
 group_by(CHNuid) %>%
 mutate(
  rbs1_delta = (rbs1_rate - lag(rbs1_rate))/lag(rbs1_rate),
  rbs1_delta = ifelse(FiscalYear == "2013-2014", NA, rbs1_delta),
  rbs1_deltap = (rbs1_rate - lag(rbs1_rate,10))/lag(rbs1_rate,10)
 ) %>%
 ungroup()

c8 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs21, RobsnGrp, CHNuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs21, CHNuid) %>%
 mutate(
  rbs21_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs21_rate = rbs21_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs21 %in% 1,
        RobsnGrp %in% 2.1,
        !is.na(CHNuid)
 ) %>%
 select(FiscalYear, CHNuid, rbs21, rbs21_count, rbs21_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CHNuid, rbs21)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(FiscalYear, rbs21)) %>%
 group_by(CHNuid) %>%
 mutate(
  rbs21_delta = (rbs21_rate - lag(rbs21_rate))/lag(rbs21_rate),
  rbs21_delta = ifelse(FiscalYear == "2013-2014", NA, rbs21_delta),
  rbs21_deltap = (rbs21_rate - lag(rbs21_rate,10))/lag(rbs21_rate,10)
 ) %>%
 ungroup()

c9 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs51, RobsnGrp, CHNuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs51, CHNuid) %>%
 mutate(
  rbs51_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs51_rate = rbs51_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs51 %in% 1,
        RobsnGrp %in% 5.1,
        !is.na(CHNuid)
 ) %>%
 select(FiscalYear, CHNuid, rbs51, rbs51_count, rbs51_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CHNuid, rbs51)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(FiscalYear, rbs51)) %>%
 group_by(CHNuid) %>%
 mutate(
  rbs51_delta = (rbs51_rate - lag(rbs51_rate))/lag(rbs51_rate),
  rbs51_delta = ifelse(FiscalYear == "2013-2014", NA, rbs51_delta),
  rbs51_deltap = (rbs51_rate - lag(rbs51_rate,10))/lag(rbs51_rate,10)
 ) %>%
 ungroup()

#### Postpartum readmission ----

c10 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, ppreadm, CHNuid) %>%
 distinct() %>%
 group_by(FiscalYear, ppreadm, CHNuid) %>%
 mutate(
  ppreadm_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid) %>%
 mutate(
  total_year = n(),
  ppreadm_rate = ppreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(ppreadm %in% 1,
        !is.na(CHNuid)
 ) %>%
 select(FiscalYear, CHNuid, ppreadm, ppreadm_count, ppreadm_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CHNuid, ppreadm)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(FiscalYear, ppreadm)) %>%
 group_by(CHNuid) %>%
 mutate(
  ppreadm_delta = (ppreadm_rate - lag(ppreadm_rate))/lag(ppreadm_rate),
  ppreadm_delta = ifelse(FiscalYear == "2013-2014", NA, ppreadm_delta),
  ppreadm_deltap = (ppreadm_rate - lag(ppreadm_rate,10))/lag(ppreadm_rate,10)
 ) %>%
 ungroup()

#### Skin to skin ----

c11 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknskn, CHNuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, sknskn, CHNuid) %>%
 mutate(
  sknskn_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid, lvb) %>%
 mutate(
  total_year = n(),
  sknskn_rate = sknskn_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskn %in% 1,
        !is.na(CHNuid)
 ) %>%
 select(FiscalYear, CHNuid, sknskn, sknskn_count, sknskn_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CHNuid, sknskn)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(FiscalYear, sknskn)) %>%
 group_by(CHNuid) %>%
 mutate(
  sknskn_delta = (sknskn_rate - lag(sknskn_rate))/lag(sknskn_rate),
  sknskn_delta = ifelse(FiscalYear == "2013-2014", NA, sknskn_delta),
  sknskn_deltap = (sknskn_rate - lag(sknskn_rate,10))/lag(sknskn_rate,10)
 ) %>%
 ungroup()

c12 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknsknvg, CHNuid, lvbvg) %>%
 distinct() %>%
 group_by(FiscalYear, sknsknvg, CHNuid) %>%
 mutate(
  sknsknvg_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid, lvbvg) %>%
 mutate(
  total_year = n(),
  sknsknvg_rate = sknsknvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknsknvg %in% 1,
        !is.na(CHNuid)
 ) %>%
 select(FiscalYear, CHNuid, sknsknvg, sknsknvg_count, sknsknvg_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CHNuid, sknsknvg)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(FiscalYear, sknsknvg)) %>%
 group_by(CHNuid) %>%
 mutate(
  sknsknvg_delta = (sknsknvg_rate - lag(sknsknvg_rate))/lag(sknsknvg_rate),
  sknsknvg_delta = ifelse(FiscalYear == "2013-2014", NA, sknsknvg_delta),
  sknsknvg_deltap = (sknsknvg_rate - lag(sknsknvg_rate,10))/lag(sknsknvg_rate,10)
 ) %>%
 ungroup()

c13 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknskncs, CHNuid, lvbcs) %>%
 distinct() %>%
 group_by(FiscalYear, sknskncs, CHNuid) %>%
 mutate(
  sknskncs_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid, lvbcs) %>%
 mutate(
  total_year = n(),
  sknskncs_rate = sknskncs_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskncs %in% 1,
        !is.na(CHNuid)
 ) %>%
 select(FiscalYear, CHNuid, sknskncs, sknskncs_count, sknskncs_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CHNuid, sknskncs)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(FiscalYear, sknskncs)) %>%
 group_by(CHNuid) %>%
 mutate(
  sknskncs_delta = (sknskncs_rate - lag(sknskncs_rate))/lag(sknskncs_rate),
  sknskncs_delta = ifelse(FiscalYear == "2013-2014", NA, sknskncs_delta),
  sknskncs_deltap = (sknskncs_rate - lag(sknskncs_rate,10))/lag(sknskncs_rate,10)
 ) %>%
 ungroup()

#### Neonatal readmission ----

c14 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, neoreadm, CHNuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, neoreadm, CHNuid) %>%
 mutate(
  neoreadm_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid, lvb) %>%
 mutate(
  total_year = n(),
  neoreadm_rate = neoreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(neoreadm %in% 1,
        !is.na(CHNuid)
 ) %>%
 select(FiscalYear, CHNuid, neoreadm, neoreadm_count, neoreadm_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CHNuid, neoreadm)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(FiscalYear, neoreadm)) %>%
 group_by(CHNuid) %>%
 mutate(
  neoreadm_delta = (neoreadm_rate - lag(neoreadm_rate))/lag(neoreadm_rate),
  neoreadm_delta = ifelse(FiscalYear == "2013-2014", NA, neoreadm_delta),
  neoreadm_deltap = (neoreadm_rate - lag(neoreadm_rate,10))/lag(neoreadm_rate,10)
 ) %>%
 ungroup()

#### Milk feeding ----

c15 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, excbrst, CHNuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, excbrst, CHNuid) %>%
 mutate(
  excbrst_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid, lvb) %>%
 mutate(
  total_year = n(),
  excbrst_rate = excbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(excbrst %in% 1,
        !is.na(CHNuid)
 ) %>%
 select(FiscalYear, CHNuid, excbrst, excbrst_count, excbrst_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CHNuid, excbrst)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(FiscalYear, excbrst)) %>%
 group_by(CHNuid) %>%
 mutate(
  excbrst_delta = (excbrst_rate - lag(excbrst_rate))/lag(excbrst_rate),
  excbrst_delta = ifelse(FiscalYear == "2013-2014", NA, excbrst_delta),
  excbrst_deltap = (excbrst_rate - lag(excbrst_rate,10))/lag(excbrst_rate,10)
 ) %>%
 ungroup()

c16 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, nexcbrst, CHNuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, nexcbrst, CHNuid) %>%
 mutate(
  nexcbrst_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid, lvb) %>%
 mutate(
  total_year = n(),
  nexcbrst_rate = nexcbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nexcbrst %in% 1,
        !is.na(CHNuid)
 ) %>%
 select(FiscalYear, CHNuid, nexcbrst, nexcbrst_count, nexcbrst_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CHNuid, nexcbrst)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(FiscalYear, nexcbrst)) %>%
 group_by(CHNuid) %>%
 mutate(
  nexcbrst_delta = (nexcbrst_rate - lag(nexcbrst_rate))/lag(nexcbrst_rate),
  nexcbrst_delta = ifelse(FiscalYear == "2013-2014", NA, nexcbrst_delta),
  nexcbrst_deltap = (nexcbrst_rate - lag(nexcbrst_rate,10))/lag(nexcbrst_rate,10)
 ) %>%
 ungroup()

c17 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, nbrst, CHNuid, lvbint) %>%
 distinct() %>%
 group_by(FiscalYear, nbrst, CHNuid) %>%
 mutate(
  nbrst_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid, lvbint) %>%
 mutate(
  total_year = n(),
  nbrst_rate = nbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nbrst %in% 1,
        !is.na(CHNuid)
 ) %>%
 select(FiscalYear, CHNuid, nbrst, nbrst_count, nbrst_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CHNuid, nbrst)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(FiscalYear, nbrst)) %>%
 group_by(CHNuid) %>%
 mutate(
  nbrst_delta = (nbrst_rate - lag(nbrst_rate))/lag(nbrst_rate),
  nbrst_delta = ifelse(FiscalYear == "2013-2014", NA, nbrst_delta),
  nbrst_deltap = (nbrst_rate - lag(nbrst_rate,10))/lag(nbrst_rate,10)
 ) %>%
 ungroup()

c18 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, brstinit, CHNuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, brstinit, CHNuid) %>%
 mutate(
  brstinit_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid, lvb) %>%
 mutate(
  total_year = n(),
  brstinit_rate = brstinit_count/total_year
 ) %>%
 ungroup() %>%
 filter(brstinit %in% 1,
        !is.na(CHNuid)
 ) %>%
 select(FiscalYear, CHNuid, brstinit, brstinit_count, brstinit_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(CHNuid, brstinit)) %>%
 tidyr::complete(CHNuid = sort(unique(dta$CHNuid)),
                 tidyr::nesting(FiscalYear, brstinit)) %>%
 group_by(CHNuid) %>%
 mutate(
  brstinit_delta = (brstinit_rate - lag(brstinit_rate))/lag(brstinit_rate),
  brstinit_delta = ifelse(FiscalYear == "2013-2014", NA, brstinit_delta),
  brstinit_deltap = (brstinit_rate - lag(brstinit_rate,10))/lag(brstinit_rate,10)
 ) %>%
 ungroup()

fchn_stats <- cbind(
 c1 %>% select(-PreExisting_Hypertension),
 c2 %>% select(-Gestational_Hypertension,-FiscalYear, -CHNuid),
 c3 %>% select(-Any_Hypertension,-FiscalYear, -CHNuid),
 c4 %>% select(-PreExisting_Diabetes,-FiscalYear, -CHNuid),
 c5 %>% select(-GDM,-FiscalYear, -CHNuid),
 c6 %>% select(-Any_Diabetes,-FiscalYear, -CHNuid),
 c7 %>% select(-rbs1,-FiscalYear, -CHNuid),
 c8 %>% select(-rbs21,-FiscalYear, -CHNuid),
 c9 %>% select(-rbs51,-FiscalYear, -CHNuid),
 c10 %>% select(-ppreadm,-FiscalYear, -CHNuid),
 c11 %>% select(-sknskn,-FiscalYear, -CHNuid),
 c12 %>% select(-sknsknvg,-FiscalYear, -CHNuid),
 c13 %>% select(-sknskncs,-FiscalYear, -CHNuid),
 c14 %>% select(-neoreadm,-FiscalYear, -CHNuid),
 c15 %>% select(-excbrst,-FiscalYear, -CHNuid),
 c16 %>% select(-nexcbrst,-FiscalYear, -CHNuid),
 c17 %>% select(-nbrst,-FiscalYear, -CHNuid),
 c18 %>% select(-brstinit,-FiscalYear, -CHNuid)
)

### Health authority zone (HR) ----
#### Hypertension ----

c1 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, PreExisting_Hypertension, HRuid) %>%
 distinct() %>%
 group_by(FiscalYear, PreExisting_Hypertension, HRuid) %>%
 mutate(
  prehyp_count = n()
 ) %>%
 group_by(FiscalYear, HRuid) %>%
 mutate(
  total_year = n(),
  prehyp_rate = prehyp_count/total_year
 ) %>%
 filter(PreExisting_Hypertension %in% 1,
        !is.na(HRuid)
 ) %>%
 select(FiscalYear, HRuid, PreExisting_Hypertension, prehyp_count, prehyp_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(HRuid, PreExisting_Hypertension)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(FiscalYear, PreExisting_Hypertension)) %>%
 group_by(HRuid) %>%
 mutate(
  prehyp_delta = (prehyp_rate - lag(prehyp_rate))/lag(prehyp_rate),
  prehyp_delta = ifelse(FiscalYear == "2013-2014", NA, prehyp_delta),
  prehyp_deltap = (prehyp_rate - lag(prehyp_rate,10))/lag(prehyp_rate,10)
 ) %>%
 ungroup()

c2 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Gestational_Hypertension, HRuid) %>%
 distinct() %>%
 group_by(FiscalYear, Gestational_Hypertension, HRuid) %>%
 mutate(
  gesthyp_count = n()
 ) %>%
 group_by(FiscalYear, HRuid) %>%
 mutate(
  total_year = n(),
  gesthyp_rate = gesthyp_count/total_year
 ) %>%
 filter(Gestational_Hypertension %in% 1,
        !is.na(HRuid)
 ) %>%
 select(FiscalYear, HRuid, Gestational_Hypertension, gesthyp_count, gesthyp_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(HRuid, Gestational_Hypertension)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(FiscalYear, Gestational_Hypertension)) %>%
 group_by(HRuid) %>%
 mutate(
  gesthyp_delta = (gesthyp_rate - lag(gesthyp_rate))/lag(gesthyp_rate),
  gesthyp_delta = ifelse(FiscalYear == "2013-2014", NA, gesthyp_delta),
  gesthyp_deltap = (gesthyp_rate - lag(gesthyp_rate,10))/lag(gesthyp_rate,10)
 ) %>%
 ungroup()

c3 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Any_Hypertension, HRuid) %>%
 distinct() %>%
 group_by(FiscalYear, Any_Hypertension, HRuid) %>%
 mutate(
  hyp_count = n()
 ) %>%
 group_by(FiscalYear, HRuid) %>%
 mutate(
  total_year = n(),
  hyp_rate = hyp_count/total_year
 ) %>%
 filter(Any_Hypertension %in% 1,
        !is.na(HRuid)
 ) %>%
 select(FiscalYear, HRuid, Any_Hypertension, hyp_count, hyp_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(HRuid, Any_Hypertension)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(FiscalYear, Any_Hypertension)) %>%
 group_by(HRuid) %>%
 mutate(
  hyp_delta = (hyp_rate - lag(hyp_rate))/lag(hyp_rate),
  hyp_delta = ifelse(FiscalYear == "2013-2014", NA, hyp_delta),
  hyp_deltap = (hyp_rate - lag(hyp_rate,10))/lag(hyp_rate,10)
 ) %>%
 ungroup()

#### Diabetes ----

c4 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, PreExisting_Diabetes, HRuid) %>%
 distinct() %>%
 group_by(FiscalYear, PreExisting_Diabetes, HRuid) %>%
 mutate(
  prediab_count = n()
 ) %>%
 group_by(FiscalYear, HRuid) %>%
 mutate(
  total_year = n(),
  prediab_rate = prediab_count/total_year
 ) %>%
 filter(PreExisting_Diabetes %in% 1,
        !is.na(HRuid)
 ) %>%
 select(FiscalYear, HRuid, PreExisting_Diabetes, prediab_count, prediab_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(HRuid, PreExisting_Diabetes)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(FiscalYear, PreExisting_Diabetes)) %>%
 group_by(HRuid) %>%
 mutate(
  prediab_delta = (prediab_rate - lag(prediab_rate))/lag(prediab_rate),
  prediab_delta = ifelse(FiscalYear == "2013-2014", NA, prediab_delta),
  prediab_deltap = (prediab_rate - lag(prediab_rate,10))/lag(prediab_rate,10)
 ) %>%
 ungroup()

c5 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, GDM, HRuid) %>%
 distinct() %>%
 group_by(FiscalYear, GDM, HRuid) %>%
 mutate(
  gestdiab_count = n()
 ) %>%
 group_by(FiscalYear, HRuid) %>%
 mutate(
  total_year = n(),
  gestdiab_rate = gestdiab_count/total_year
 ) %>%
 filter(GDM %in% 1,
        !is.na(HRuid)
 ) %>%
 select(FiscalYear, HRuid, GDM, gestdiab_count, gestdiab_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(HRuid, GDM)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(FiscalYear, GDM)) %>%
 group_by(HRuid) %>%
 mutate(
  gestdiab_delta = (gestdiab_rate - lag(gestdiab_rate))/lag(gestdiab_rate),
  gestdiab_delta = ifelse(FiscalYear == "2013-2014", NA, gestdiab_delta),
  gestdiab_deltap = (gestdiab_rate - lag(gestdiab_rate,10))/lag(gestdiab_rate,10)
 ) %>%
 ungroup()

c6 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Any_Diabetes, HRuid) %>%
 distinct() %>%
 group_by(FiscalYear, Any_Diabetes, HRuid) %>%
 mutate(
  diab_count = n()
 ) %>%
 group_by(FiscalYear, HRuid) %>%
 mutate(
  total_year = n(),
  diab_rate = diab_count/total_year
 ) %>%
 filter(Any_Diabetes %in% 1,
        !is.na(HRuid)
 ) %>%
 select(FiscalYear, HRuid, Any_Diabetes, diab_count, diab_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(HRuid, Any_Diabetes)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(FiscalYear, Any_Diabetes)) %>%
 group_by(HRuid) %>%
 mutate(
  diab_delta = (diab_rate - lag(diab_rate))/lag(diab_rate),
  diab_delta = ifelse(FiscalYear == "2013-2014", NA, diab_delta),
  diab_deltap = (diab_rate - lag(diab_rate,10))/lag(diab_rate,10)
 ) %>%
 ungroup()

#### Robson group ----

c7 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs1, RobsnGrp, HRuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs1, HRuid) %>%
 mutate(
  rbs1_count = n()
 ) %>%
 group_by(FiscalYear, HRuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs1_rate = rbs1_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs1 %in% 1,
        RobsnGrp %in% 1,
        !is.na(HRuid)
 ) %>%
 select(FiscalYear, HRuid, rbs1, rbs1_count, rbs1_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(HRuid, rbs1)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(FiscalYear, rbs1)) %>%
 group_by(HRuid) %>%
 mutate(
  rbs1_delta = (rbs1_rate - lag(rbs1_rate))/lag(rbs1_rate),
  rbs1_delta = ifelse(FiscalYear == "2013-2014", NA, rbs1_delta),
  rbs1_deltap = (rbs1_rate - lag(rbs1_rate,10))/lag(rbs1_rate,10)
 ) %>%
 ungroup()

c8 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs21, RobsnGrp, HRuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs21, HRuid) %>%
 mutate(
  rbs21_count = n()
 ) %>%
 group_by(FiscalYear, HRuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs21_rate = rbs21_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs21 %in% 1,
        RobsnGrp %in% 2.1,
        !is.na(HRuid)
 ) %>%
 select(FiscalYear, HRuid, rbs21, rbs21_count, rbs21_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(HRuid, rbs21)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(FiscalYear, rbs21)) %>%
 group_by(HRuid) %>%
 mutate(
  rbs21_delta = (rbs21_rate - lag(rbs21_rate))/lag(rbs21_rate),
  rbs21_delta = ifelse(FiscalYear == "2013-2014", NA, rbs21_delta),
  rbs21_deltap = (rbs21_rate - lag(rbs21_rate,10))/lag(rbs21_rate,10)
 ) %>%
 ungroup()

c9 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs51, RobsnGrp, HRuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs51, HRuid) %>%
 mutate(
  rbs51_count = n()
 ) %>%
 group_by(FiscalYear, HRuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs51_rate = rbs51_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs51 %in% 1,
        RobsnGrp %in% 5.1,
        !is.na(HRuid)
 ) %>%
 select(FiscalYear, HRuid, rbs51, rbs51_count, rbs51_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(HRuid, rbs51)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(FiscalYear, rbs51)) %>%
 group_by(HRuid) %>%
 mutate(
  rbs51_delta = (rbs51_rate - lag(rbs51_rate))/lag(rbs51_rate),
  rbs51_delta = ifelse(FiscalYear == "2013-2014", NA, rbs51_delta),
  rbs51_deltap = (rbs51_rate - lag(rbs51_rate,10))/lag(rbs51_rate,10)
 ) %>%
 ungroup()

#### Postpartum readmission ----

c10 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, ppreadm, HRuid) %>%
 distinct() %>%
 group_by(FiscalYear, ppreadm, HRuid) %>%
 mutate(
  ppreadm_count = n()
 ) %>%
 group_by(FiscalYear, HRuid) %>%
 mutate(
  total_year = n(),
  ppreadm_rate = ppreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(ppreadm %in% 1,
        !is.na(HRuid)
 ) %>%
 select(FiscalYear, HRuid, ppreadm, ppreadm_count, ppreadm_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(HRuid, ppreadm)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(FiscalYear, ppreadm)) %>%
 group_by(HRuid) %>%
 mutate(
  ppreadm_delta = (ppreadm_rate - lag(ppreadm_rate))/lag(ppreadm_rate),
  ppreadm_delta = ifelse(FiscalYear == "2013-2014", NA, ppreadm_delta),
  ppreadm_deltap = (ppreadm_rate - lag(ppreadm_rate,10))/lag(ppreadm_rate,10)
 ) %>%
 ungroup()

#### Skin to skin ----

c11 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknskn, HRuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, sknskn, HRuid) %>%
 mutate(
  sknskn_count = n()
 ) %>%
 group_by(FiscalYear, HRuid, lvb) %>%
 mutate(
  total_year = n(),
  sknskn_rate = sknskn_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskn %in% 1,
        !is.na(HRuid)
 ) %>%
 select(FiscalYear, HRuid, sknskn, sknskn_count, sknskn_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(HRuid, sknskn)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(FiscalYear, sknskn)) %>%
 group_by(HRuid) %>%
 mutate(
  sknskn_delta = (sknskn_rate - lag(sknskn_rate))/lag(sknskn_rate),
  sknskn_delta = ifelse(FiscalYear == "2013-2014", NA, sknskn_delta),
  sknskn_deltap = (sknskn_rate - lag(sknskn_rate,10))/lag(sknskn_rate,10)
 ) %>%
 ungroup()

c12 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknsknvg, HRuid, lvbvg) %>%
 distinct() %>%
 group_by(FiscalYear, sknsknvg, HRuid) %>%
 mutate(
  sknsknvg_count = n()
 ) %>%
 group_by(FiscalYear, HRuid, lvbvg) %>%
 mutate(
  total_year = n(),
  sknsknvg_rate = sknsknvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknsknvg %in% 1,
        !is.na(HRuid)
 ) %>%
 select(FiscalYear, HRuid, sknsknvg, sknsknvg_count, sknsknvg_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(HRuid, sknsknvg)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(FiscalYear, sknsknvg)) %>%
 group_by(HRuid) %>%
 mutate(
  sknsknvg_delta = (sknsknvg_rate - lag(sknsknvg_rate))/lag(sknsknvg_rate),
  sknsknvg_delta = ifelse(FiscalYear == "2013-2014", NA, sknsknvg_delta),
  sknsknvg_deltap = (sknsknvg_rate - lag(sknsknvg_rate,10))/lag(sknsknvg_rate,10)
 ) %>%
 ungroup()

c13 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknskncs, HRuid, lvbcs) %>%
 distinct() %>%
 group_by(FiscalYear, sknskncs, HRuid) %>%
 mutate(
  sknskncs_count = n()
 ) %>%
 group_by(FiscalYear, HRuid, lvbcs) %>%
 mutate(
  total_year = n(),
  sknskncs_rate = sknskncs_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskncs %in% 1,
        !is.na(HRuid)
 ) %>%
 select(FiscalYear, HRuid, sknskncs, sknskncs_count, sknskncs_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(HRuid, sknskncs)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(FiscalYear, sknskncs)) %>%
 group_by(HRuid) %>%
 mutate(
  sknskncs_delta = (sknskncs_rate - lag(sknskncs_rate))/lag(sknskncs_rate),
  sknskncs_delta = ifelse(FiscalYear == "2013-2014", NA, sknskncs_delta),
  sknskncs_deltap = (sknskncs_rate - lag(sknskncs_rate,10))/lag(sknskncs_rate,10)
 ) %>%
 ungroup()

#### Neonatal readmission ----

c14 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, neoreadm, HRuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, neoreadm, HRuid) %>%
 mutate(
  neoreadm_count = n()
 ) %>%
 group_by(FiscalYear, HRuid, lvb) %>%
 mutate(
  total_year = n(),
  neoreadm_rate = neoreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(neoreadm %in% 1,
        !is.na(HRuid)
 ) %>%
 select(FiscalYear, HRuid, neoreadm, neoreadm_count, neoreadm_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(HRuid, neoreadm)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(FiscalYear, neoreadm)) %>%
 group_by(HRuid) %>%
 mutate(
  neoreadm_delta = (neoreadm_rate - lag(neoreadm_rate))/lag(neoreadm_rate),
  neoreadm_delta = ifelse(FiscalYear == "2013-2014", NA, neoreadm_delta),
  neoreadm_deltap = (neoreadm_rate - lag(neoreadm_rate,10))/lag(neoreadm_rate,10)
 ) %>%
 ungroup()

#### Milk feeding ----

c15 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, excbrst, HRuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, excbrst, HRuid) %>%
 mutate(
  excbrst_count = n()
 ) %>%
 group_by(FiscalYear, HRuid, lvb) %>%
 mutate(
  total_year = n(),
  excbrst_rate = excbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(excbrst %in% 1,
        !is.na(HRuid)
 ) %>%
 select(FiscalYear, HRuid, excbrst, excbrst_count, excbrst_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(HRuid, excbrst)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(FiscalYear, excbrst)) %>%
 group_by(HRuid) %>%
 mutate(
  excbrst_delta = (excbrst_rate - lag(excbrst_rate))/lag(excbrst_rate),
  excbrst_delta = ifelse(FiscalYear == "2013-2014", NA, excbrst_delta),
  excbrst_deltap = (excbrst_rate - lag(excbrst_rate,10))/lag(excbrst_rate,10)
 ) %>%
 ungroup()

c16 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, nexcbrst, HRuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, nexcbrst, HRuid) %>%
 mutate(
  nexcbrst_count = n()
 ) %>%
 group_by(FiscalYear, HRuid, lvb) %>%
 mutate(
  total_year = n(),
  nexcbrst_rate = nexcbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nexcbrst %in% 1,
        !is.na(HRuid)
 ) %>%
 select(FiscalYear, HRuid, nexcbrst, nexcbrst_count, nexcbrst_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(HRuid, nexcbrst)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(FiscalYear, nexcbrst)) %>%
 group_by(HRuid) %>%
 mutate(
  nexcbrst_delta = (nexcbrst_rate - lag(nexcbrst_rate))/lag(nexcbrst_rate),
  nexcbrst_delta = ifelse(FiscalYear == "2013-2014", NA, nexcbrst_delta),
  nexcbrst_deltap = (nexcbrst_rate - lag(nexcbrst_rate,10))/lag(nexcbrst_rate,10)
 ) %>%
 ungroup()

c17 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, nbrst, HRuid, lvbint) %>%
 distinct() %>%
 group_by(FiscalYear, nbrst, HRuid) %>%
 mutate(
  nbrst_count = n()
 ) %>%
 group_by(FiscalYear, HRuid, lvbint) %>%
 mutate(
  total_year = n(),
  nbrst_rate = nbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nbrst %in% 1,
        !is.na(HRuid)
 ) %>%
 select(FiscalYear, HRuid, nbrst, nbrst_count, nbrst_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(HRuid, nbrst)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(FiscalYear, nbrst)) %>%
 group_by(HRuid) %>%
 mutate(
  nbrst_delta = (nbrst_rate - lag(nbrst_rate))/lag(nbrst_rate),
  nbrst_delta = ifelse(FiscalYear == "2013-2014", NA, nbrst_delta),
  nbrst_deltap = (nbrst_rate - lag(nbrst_rate,10))/lag(nbrst_rate,10)
 ) %>%
 ungroup()

c18 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, brstinit, HRuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, brstinit, HRuid) %>%
 mutate(
  brstinit_count = n()
 ) %>%
 group_by(FiscalYear, HRuid, lvb) %>%
 mutate(
  total_year = n(),
  brstinit_rate = brstinit_count/total_year
 ) %>%
 ungroup() %>%
 filter(brstinit %in% 1,
        !is.na(HRuid)
 ) %>%
 select(FiscalYear, HRuid, brstinit, brstinit_count, brstinit_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(HRuid, brstinit)) %>%
 tidyr::complete(HRuid = sort(unique(dta$HRuid)),
                 tidyr::nesting(FiscalYear, brstinit)) %>%
 group_by(HRuid) %>%
 mutate(
  brstinit_delta = (brstinit_rate - lag(brstinit_rate))/lag(brstinit_rate),
  brstinit_delta = ifelse(FiscalYear == "2013-2014", NA, brstinit_delta),
  brstinit_deltap = (brstinit_rate - lag(brstinit_rate,10))/lag(brstinit_rate,10)
 ) %>%
 ungroup()

fhr_stats <- cbind(
 c1 %>% select(-PreExisting_Hypertension),
 c2 %>% select(-Gestational_Hypertension,-FiscalYear, -HRuid),
 c3 %>% select(-Any_Hypertension,-FiscalYear, -HRuid),
 c4 %>% select(-PreExisting_Diabetes,-FiscalYear, -HRuid),
 c5 %>% select(-GDM,-FiscalYear, -HRuid),
 c6 %>% select(-Any_Diabetes,-FiscalYear, -HRuid),
 c7 %>% select(-rbs1,-FiscalYear, -HRuid),
 c8 %>% select(-rbs21,-FiscalYear, -HRuid),
 c9 %>% select(-rbs51,-FiscalYear, -HRuid),
 c10 %>% select(-ppreadm,-FiscalYear, -HRuid),
 c11 %>% select(-sknskn,-FiscalYear, -HRuid),
 c12 %>% select(-sknsknvg,-FiscalYear, -HRuid),
 c13 %>% select(-sknskncs,-FiscalYear, -HRuid),
 c14 %>% select(-neoreadm,-FiscalYear, -HRuid),
 c15 %>% select(-excbrst,-FiscalYear, -HRuid),
 c16 %>% select(-nexcbrst,-FiscalYear, -HRuid),
 c17 %>% select(-nbrst,-FiscalYear, -HRuid),
 c18 %>% select(-brstinit,-FiscalYear, -HRuid)
)

### Urban x Rural (URB) ----

#### Hypertension ----

c1 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, PreExisting_Hypertension, URBuid) %>%
 distinct() %>%
 group_by(FiscalYear, PreExisting_Hypertension, URBuid) %>%
 mutate(
  prehyp_count = n()
 ) %>%
 group_by(FiscalYear, URBuid) %>%
 mutate(
  total_year = n(),
  prehyp_rate = prehyp_count/total_year
 ) %>%
 filter(PreExisting_Hypertension %in% 1,
        !is.na(URBuid)
 ) %>%
 select(FiscalYear, URBuid, PreExisting_Hypertension, prehyp_count, prehyp_rate) %>%
 distinct() %>%
 arrange(URBuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(URBuid, PreExisting_Hypertension)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(FiscalYear, PreExisting_Hypertension)) %>%
 group_by(URBuid) %>%
 mutate(
  prehyp_delta = (prehyp_rate - lag(prehyp_rate))/lag(prehyp_rate),
  prehyp_delta = ifelse(FiscalYear == "2013-2014", NA, prehyp_delta),
  prehyp_deltap = (prehyp_rate - lag(prehyp_rate,10))/lag(prehyp_rate,10)
 ) %>%
 ungroup()

c2 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Gestational_Hypertension, URBuid) %>%
 distinct() %>%
 group_by(FiscalYear, Gestational_Hypertension, URBuid) %>%
 mutate(
  gesthyp_count = n()
 ) %>%
 group_by(FiscalYear, URBuid) %>%
 mutate(
  total_year = n(),
  gesthyp_rate = gesthyp_count/total_year
 ) %>%
 filter(Gestational_Hypertension %in% 1,
        !is.na(URBuid)
 ) %>%
 select(FiscalYear, URBuid, Gestational_Hypertension, gesthyp_count, gesthyp_rate) %>%
 distinct() %>%
 arrange(URBuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(URBuid, Gestational_Hypertension)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(FiscalYear, Gestational_Hypertension)) %>%
 group_by(URBuid) %>%
 mutate(
  gesthyp_delta = (gesthyp_rate - lag(gesthyp_rate))/lag(gesthyp_rate),
  gesthyp_delta = ifelse(FiscalYear == "2013-2014", NA, gesthyp_delta),
  gesthyp_deltap = (gesthyp_rate - lag(gesthyp_rate,10))/lag(gesthyp_rate,10)
 ) %>%
 ungroup()

c3 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Any_Hypertension, URBuid) %>%
 distinct() %>%
 group_by(FiscalYear, Any_Hypertension, URBuid) %>%
 mutate(
  hyp_count = n()
 ) %>%
 group_by(FiscalYear, URBuid) %>%
 mutate(
  total_year = n(),
  hyp_rate = hyp_count/total_year
 ) %>%
 filter(Any_Hypertension %in% 1,
        !is.na(URBuid)
 ) %>%
 select(FiscalYear, URBuid, Any_Hypertension, hyp_count, hyp_rate) %>%
 distinct() %>%
 arrange(URBuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(URBuid, Any_Hypertension)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(FiscalYear, Any_Hypertension)) %>%
 group_by(URBuid) %>%
 mutate(
  hyp_delta = (hyp_rate - lag(hyp_rate))/lag(hyp_rate),
  hyp_delta = ifelse(FiscalYear == "2013-2014", NA, hyp_delta),
  hyp_deltap = (hyp_rate - lag(hyp_rate,10))/lag(hyp_rate,10)
 ) %>%
 ungroup()

#### Diabetes ----

c4 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, PreExisting_Diabetes, URBuid) %>%
 distinct() %>%
 group_by(FiscalYear, PreExisting_Diabetes, URBuid) %>%
 mutate(
  prediab_count = n()
 ) %>%
 group_by(FiscalYear, URBuid) %>%
 mutate(
  total_year = n(),
  prediab_rate = prediab_count/total_year
 ) %>%
 filter(PreExisting_Diabetes %in% 1,
        !is.na(URBuid)
 ) %>%
 select(FiscalYear, URBuid, PreExisting_Diabetes, prediab_count, prediab_rate) %>%
 distinct() %>%
 arrange(URBuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(URBuid, PreExisting_Diabetes)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(FiscalYear, PreExisting_Diabetes)) %>%
 group_by(URBuid) %>%
 mutate(
  prediab_delta = (prediab_rate - lag(prediab_rate))/lag(prediab_rate),
  prediab_delta = ifelse(FiscalYear == "2013-2014", NA, prediab_delta),
  prediab_deltap = (prediab_rate - lag(prediab_rate,10))/lag(prediab_rate,10)
 ) %>%
 ungroup()

c5 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, GDM, URBuid) %>%
 distinct() %>%
 group_by(FiscalYear, GDM, URBuid) %>%
 mutate(
  gestdiab_count = n()
 ) %>%
 group_by(FiscalYear, URBuid) %>%
 mutate(
  total_year = n(),
  gestdiab_rate = gestdiab_count/total_year
 ) %>%
 filter(GDM %in% 1,
        !is.na(URBuid)
 ) %>%
 select(FiscalYear, URBuid, GDM, gestdiab_count, gestdiab_rate) %>%
 distinct() %>%
 arrange(URBuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(URBuid, GDM)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(FiscalYear, GDM)) %>%
 group_by(URBuid) %>%
 mutate(
  gestdiab_delta = (gestdiab_rate - lag(gestdiab_rate))/lag(gestdiab_rate),
  gestdiab_delta = ifelse(FiscalYear == "2013-2014", NA, gestdiab_delta),
  gestdiab_deltap = (gestdiab_rate - lag(gestdiab_rate,10))/lag(gestdiab_rate,10)
 ) %>%
 ungroup()

c6 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Any_Diabetes, URBuid) %>%
 distinct() %>%
 group_by(FiscalYear, Any_Diabetes, URBuid) %>%
 mutate(
  diab_count = n()
 ) %>%
 group_by(FiscalYear, URBuid) %>%
 mutate(
  total_year = n(),
  diab_rate = diab_count/total_year
 ) %>%
 filter(Any_Diabetes %in% 1,
        !is.na(URBuid)
 ) %>%
 select(FiscalYear, URBuid, Any_Diabetes, diab_count, diab_rate) %>%
 distinct() %>%
 arrange(URBuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(URBuid, Any_Diabetes)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(FiscalYear, Any_Diabetes)) %>%
 group_by(URBuid) %>%
 mutate(
  diab_delta = (diab_rate - lag(diab_rate))/lag(diab_rate),
  diab_delta = ifelse(FiscalYear == "2013-2014", NA, diab_delta),
  diab_deltap = (diab_rate - lag(diab_rate,10))/lag(diab_rate,10)
 ) %>%
 ungroup()

#### Robson group ----

c7 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs1, RobsnGrp, URBuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs1, URBuid) %>%
 mutate(
  rbs1_count = n()
 ) %>%
 group_by(FiscalYear, URBuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs1_rate = rbs1_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs1 %in% 1,
        RobsnGrp %in% 1,
        !is.na(URBuid)
 ) %>%
 select(FiscalYear, URBuid, rbs1, rbs1_count, rbs1_rate) %>%
 distinct() %>%
 arrange(URBuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(URBuid, rbs1)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(FiscalYear, rbs1)) %>%
 group_by(URBuid) %>%
 mutate(
  rbs1_delta = (rbs1_rate - lag(rbs1_rate))/lag(rbs1_rate),
  rbs1_delta = ifelse(FiscalYear == "2013-2014", NA, rbs1_delta),
  rbs1_deltap = (rbs1_rate - lag(rbs1_rate,10))/lag(rbs1_rate,10)
 ) %>%
 ungroup()

c8 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs21, RobsnGrp, URBuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs21, URBuid) %>%
 mutate(
  rbs21_count = n()
 ) %>%
 group_by(FiscalYear, URBuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs21_rate = rbs21_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs21 %in% 1,
        RobsnGrp %in% 2.1,
        !is.na(URBuid)
 ) %>%
 select(FiscalYear, URBuid, rbs21, rbs21_count, rbs21_rate) %>%
 distinct() %>%
 arrange(URBuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(URBuid, rbs21)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(FiscalYear, rbs21)) %>%
 group_by(URBuid) %>%
 mutate(
  rbs21_delta = (rbs21_rate - lag(rbs21_rate))/lag(rbs21_rate),
  rbs21_delta = ifelse(FiscalYear == "2013-2014", NA, rbs21_delta),
  rbs21_deltap = (rbs21_rate - lag(rbs21_rate,10))/lag(rbs21_rate,10)
 ) %>%
 ungroup()

c9 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs51, RobsnGrp, URBuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs51, URBuid) %>%
 mutate(
  rbs51_count = n()
 ) %>%
 group_by(FiscalYear, URBuid, RobsnGrp) %>%
 mutate(
  total_year = n(),
  rbs51_rate = rbs51_count/total_year
 ) %>%
 ungroup() %>%
 filter(rbs51 %in% 1,
        RobsnGrp %in% 5.1,
        !is.na(URBuid)
 ) %>%
 select(FiscalYear, URBuid, rbs51, rbs51_count, rbs51_rate) %>%
 distinct() %>%
 arrange(URBuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(URBuid, rbs51)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(FiscalYear, rbs51)) %>%
 group_by(URBuid) %>%
 mutate(
  rbs51_delta = (rbs51_rate - lag(rbs51_rate))/lag(rbs51_rate),
  rbs51_delta = ifelse(FiscalYear == "2013-2014", NA, rbs51_delta),
  rbs51_deltap = (rbs51_rate - lag(rbs51_rate,10))/lag(rbs51_rate,10)
 ) %>%
 ungroup()

#### Postpartum readmission ----

c10 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, ppreadm, URBuid) %>%
 distinct() %>%
 group_by(FiscalYear, ppreadm, URBuid) %>%
 mutate(
  ppreadm_count = n()
 ) %>%
 group_by(FiscalYear, URBuid) %>%
 mutate(
  total_year = n(),
  ppreadm_rate = ppreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(ppreadm %in% 1,
        !is.na(URBuid)
 ) %>%
 select(FiscalYear, URBuid, ppreadm, ppreadm_count, ppreadm_rate) %>%
 distinct() %>%
 arrange(URBuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(URBuid, ppreadm)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(FiscalYear, ppreadm)) %>%
 group_by(URBuid) %>%
 mutate(
  ppreadm_delta = (ppreadm_rate - lag(ppreadm_rate))/lag(ppreadm_rate),
  ppreadm_delta = ifelse(FiscalYear == "2013-2014", NA, ppreadm_delta),
  ppreadm_deltap = (ppreadm_rate - lag(ppreadm_rate,10))/lag(ppreadm_rate,10)
 ) %>%
 ungroup()

#### Skin to skin ----

c11 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknskn, URBuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, sknskn, URBuid) %>%
 mutate(
  sknskn_count = n()
 ) %>%
 group_by(FiscalYear, URBuid, lvb) %>%
 mutate(
  total_year = n(),
  sknskn_rate = sknskn_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskn %in% 1,
        !is.na(URBuid)
 ) %>%
 select(FiscalYear, URBuid, sknskn, sknskn_count, sknskn_rate) %>%
 distinct() %>%
 arrange(URBuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(URBuid, sknskn)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(FiscalYear, sknskn)) %>%
 group_by(URBuid) %>%
 mutate(
  sknskn_delta = (sknskn_rate - lag(sknskn_rate))/lag(sknskn_rate),
  sknskn_delta = ifelse(FiscalYear == "2013-2014", NA, sknskn_delta),
  sknskn_deltap = (sknskn_rate - lag(sknskn_rate,10))/lag(sknskn_rate,10)
 ) %>%
 ungroup()

c12 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknsknvg, URBuid, lvbvg) %>%
 distinct() %>%
 group_by(FiscalYear, sknsknvg, URBuid) %>%
 mutate(
  sknsknvg_count = n()
 ) %>%
 group_by(FiscalYear, URBuid, lvbvg) %>%
 mutate(
  total_year = n(),
  sknsknvg_rate = sknsknvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknsknvg %in% 1,
        !is.na(URBuid)
 ) %>%
 select(FiscalYear, URBuid, sknsknvg, sknsknvg_count, sknsknvg_rate) %>%
 distinct() %>%
 arrange(URBuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(URBuid, sknsknvg)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(FiscalYear, sknsknvg)) %>%
 group_by(URBuid) %>%
 mutate(
  sknsknvg_delta = (sknsknvg_rate - lag(sknsknvg_rate))/lag(sknsknvg_rate),
  sknsknvg_delta = ifelse(FiscalYear == "2013-2014", NA, sknsknvg_delta),
  sknsknvg_deltap = (sknsknvg_rate - lag(sknsknvg_rate,10))/lag(sknsknvg_rate,10)
 ) %>%
 ungroup()

c13 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknskncs, URBuid, lvbcs) %>%
 distinct() %>%
 group_by(FiscalYear, sknskncs, URBuid) %>%
 mutate(
  sknskncs_count = n()
 ) %>%
 group_by(FiscalYear, URBuid, lvbcs) %>%
 mutate(
  total_year = n(),
  sknskncs_rate = sknskncs_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskncs %in% 1,
        !is.na(URBuid)
 ) %>%
 select(FiscalYear, URBuid, sknskncs, sknskncs_count, sknskncs_rate) %>%
 distinct() %>%
 arrange(URBuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(URBuid, sknskncs)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(FiscalYear, sknskncs)) %>%
 group_by(URBuid) %>%
 mutate(
  sknskncs_delta = (sknskncs_rate - lag(sknskncs_rate))/lag(sknskncs_rate),
  sknskncs_delta = ifelse(FiscalYear == "2013-2014", NA, sknskncs_delta),
  sknskncs_deltap = (sknskncs_rate - lag(sknskncs_rate,10))/lag(sknskncs_rate,10)
 ) %>%
 ungroup()

#### Neonatal readmission ----

c14 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, neoreadm, URBuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, neoreadm, URBuid) %>%
 mutate(
  neoreadm_count = n()
 ) %>%
 group_by(FiscalYear, URBuid, lvb) %>%
 mutate(
  total_year = n(),
  neoreadm_rate = neoreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(neoreadm %in% 1,
        !is.na(URBuid)
 ) %>%
 select(FiscalYear, URBuid, neoreadm, neoreadm_count, neoreadm_rate) %>%
 distinct() %>%
 arrange(URBuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(URBuid, neoreadm)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(FiscalYear, neoreadm)) %>%
 group_by(URBuid) %>%
 mutate(
  neoreadm_delta = (neoreadm_rate - lag(neoreadm_rate))/lag(neoreadm_rate),
  neoreadm_delta = ifelse(FiscalYear == "2013-2014", NA, neoreadm_delta),
  neoreadm_deltap = (neoreadm_rate - lag(neoreadm_rate,10))/lag(neoreadm_rate,10)
 ) %>%
 ungroup()

#### Milk feeding ----

c15 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, excbrst, URBuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, excbrst, URBuid) %>%
 mutate(
  excbrst_count = n()
 ) %>%
 group_by(FiscalYear, URBuid, lvb) %>%
 mutate(
  total_year = n(),
  excbrst_rate = excbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(excbrst %in% 1,
        !is.na(URBuid)
 ) %>%
 select(FiscalYear, URBuid, excbrst, excbrst_count, excbrst_rate) %>%
 distinct() %>%
 arrange(URBuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(URBuid, excbrst)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(FiscalYear, excbrst)) %>%
 group_by(URBuid) %>%
 mutate(
  excbrst_delta = (excbrst_rate - lag(excbrst_rate))/lag(excbrst_rate),
  excbrst_delta = ifelse(FiscalYear == "2013-2014", NA, excbrst_delta),
  excbrst_deltap = (excbrst_rate - lag(excbrst_rate,10))/lag(excbrst_rate,10)
 ) %>%
 ungroup()

c16 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, nexcbrst, URBuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, nexcbrst, URBuid) %>%
 mutate(
  nexcbrst_count = n()
 ) %>%
 group_by(FiscalYear, URBuid, lvb) %>%
 mutate(
  total_year = n(),
  nexcbrst_rate = nexcbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nexcbrst %in% 1,
        !is.na(URBuid)
 ) %>%
 select(FiscalYear, URBuid, nexcbrst, nexcbrst_count, nexcbrst_rate) %>%
 distinct() %>%
 arrange(URBuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(URBuid, nexcbrst)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(FiscalYear, nexcbrst)) %>%
 group_by(URBuid) %>%
 mutate(
  nexcbrst_delta = (nexcbrst_rate - lag(nexcbrst_rate))/lag(nexcbrst_rate),
  nexcbrst_delta = ifelse(FiscalYear == "2013-2014", NA, nexcbrst_delta),
  nexcbrst_deltap = (nexcbrst_rate - lag(nexcbrst_rate,10))/lag(nexcbrst_rate,10)
 ) %>%
 ungroup()

c17 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, nbrst, URBuid, lvbint) %>%
 distinct() %>%
 group_by(FiscalYear, nbrst, URBuid) %>%
 mutate(
  nbrst_count = n()
 ) %>%
 group_by(FiscalYear, URBuid, lvbint) %>%
 mutate(
  total_year = n(),
  nbrst_rate = nbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nbrst %in% 1,
        !is.na(URBuid)
 ) %>%
 select(FiscalYear, URBuid, nbrst, nbrst_count, nbrst_rate) %>%
 distinct() %>%
 arrange(URBuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(URBuid, nbrst)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(FiscalYear, nbrst)) %>%
 group_by(URBuid) %>%
 mutate(
  nbrst_delta = (nbrst_rate - lag(nbrst_rate))/lag(nbrst_rate),
  nbrst_delta = ifelse(FiscalYear == "2013-2014", NA, nbrst_delta),
  nbrst_deltap = (nbrst_rate - lag(nbrst_rate,10))/lag(nbrst_rate,10)
 ) %>%
 ungroup()

c18 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, brstinit, URBuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, brstinit, URBuid) %>%
 mutate(
  brstinit_count = n()
 ) %>%
 group_by(FiscalYear, URBuid, lvb) %>%
 mutate(
  total_year = n(),
  brstinit_rate = brstinit_count/total_year
 ) %>%
 ungroup() %>%
 filter(brstinit %in% 1,
        !is.na(URBuid)
 ) %>%
 select(FiscalYear, URBuid, brstinit, brstinit_count, brstinit_rate) %>%
 distinct() %>%
 arrange(URBuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(URBuid, brstinit)) %>%
 tidyr::complete(URBuid = sort(unique(dta$URBuid)),
                 tidyr::nesting(FiscalYear, brstinit)) %>%
 group_by(URBuid) %>%
 mutate(
  brstinit_delta = (brstinit_rate - lag(brstinit_rate))/lag(brstinit_rate),
  brstinit_delta = ifelse(FiscalYear == "2013-2014", NA, brstinit_delta),
  brstinit_deltap = (brstinit_rate - lag(brstinit_rate,10))/lag(brstinit_rate,10)
 ) %>%
 ungroup()

furb_stats <- cbind(
 c1 %>% select(-PreExisting_Hypertension),
 c2 %>% select(-Gestational_Hypertension,-FiscalYear, -URBuid),
 c3 %>% select(-Any_Hypertension,-FiscalYear, -URBuid),
 c4 %>% select(-PreExisting_Diabetes,-FiscalYear, -URBuid),
 c5 %>% select(-GDM,-FiscalYear, -URBuid),
 c6 %>% select(-Any_Diabetes,-FiscalYear, -URBuid),
 c7 %>% select(-rbs1,-FiscalYear, -URBuid),
 c8 %>% select(-rbs21,-FiscalYear, -URBuid),
 c9 %>% select(-rbs51,-FiscalYear, -URBuid),
 c10 %>% select(-ppreadm,-FiscalYear, -URBuid),
 c11 %>% select(-sknskn,-FiscalYear, -URBuid),
 c12 %>% select(-sknsknvg,-FiscalYear, -URBuid),
 c13 %>% select(-sknskncs,-FiscalYear, -URBuid),
 c14 %>% select(-neoreadm,-FiscalYear, -URBuid),
 c15 %>% select(-excbrst,-FiscalYear, -URBuid),
 c16 %>% select(-nexcbrst,-FiscalYear, -URBuid),
 c17 %>% select(-nbrst,-FiscalYear, -URBuid),
 c18 %>% select(-brstinit,-FiscalYear, -URBuid)
)

## Add shapefile to stats ----
### Census district (CD) ----

# ccd_stats <- merge(
#  ccd_stats,
#  cd_shp,
#  by.x = "CDuid",
#  by.y = "GeoUID"
# ) %>%
#  sf::st_sf() %>%
#  sf::st_make_valid() %>%
#  rmapshaper::ms_simplify() %>%
#  st_geometry
#
# fcd_stats <- merge(
#  fcd_stats,
#  cd_shp,
#  by.x = "CDuid",
#  by.y = "GeoUID"
# )
#
# ### Community cluster (CL) ----
#
# ccl_stats <- merge(
#  ccl_stats,
#  cl_shp,
#  by.x = "CLuid",
#  by.y = "GeoUID"
# )
#
# fcl_stats <- merge(
#  fcl_stats,
#  cl_shp,
#  by.x = "CLuid",
#  by.y = "GeoUID"
# )
#
# ### Community health network (CHN) ----
#
# cchn_stats <- merge(
#  cchn_stats,
#  chn_shp,
#  by.x = "CHNuid",
#  by.y = "GeoUID"
# )
#
# fchn_stats <- merge(
#  fchn_stats,
#  chn_shp,
#  by.x = "CHNuid",
#  by.y = "GeoUID"
# )
#
# ### Health authority zone (HR) ----
#
# chr_stats <- merge(
#  chr_stats,
#  hr_shp,
#  by.x = "HRuid",
#  by.y = "GeoUID"
# )
#
# fhr_stats <- merge(
#  fhr_stats,
#  hr_shp,
#  by.x = "HRuid",
#  by.y = "GeoUID"
# )
#
# ### Urban x Rural (URB) ----
#
# curb_stats <- merge(
#  curb_stats,
#  urb_shp,
#  by.x = "URBuid",
#  by.y = "GeoUID"
# )
#
# fcurb_stats <- merge(
#  fcurb_stats,
#  urb_shp,
#  by.x = "URBuid",
#  by.y = "GeoUID"
# )

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

arrow::write_parquet(
 ctab_stats,
 "./data/ctab_stats.parquet")

arrow::write_parquet(
 ftab_stats,
 "./data/ftab_stats.parquet")

arrow::write_parquet(
 ctab_dist,
 "./data/ctab_dist.parquet")

arrow::write_parquet(
 ccd_stats,
 "./data/ccd_stats.parquet")

arrow::write_parquet(
 ccl_stats,
 "./data/ccl_stats.parquet")

arrow::write_parquet(
 cchn_stats,
 "./data/cchn_stats.parquet")

arrow::write_parquet(
 chr_stats,
 "./data/chr_stats.parquet")

arrow::write_parquet(
 curb_stats,
 "./data/curb_stats.parquet")

arrow::write_parquet(
 fcd_stats,
 "./data/fcd_stats.parquet")

arrow::write_parquet(
 fcl_stats,
 "./data/fcl_stats.parquet")

arrow::write_parquet(
 fchn_stats,
 "./data/fchn_stats.parquet")

arrow::write_parquet(
 fhr_stats,
 "./data/fhr_stats.parquet")

arrow::write_parquet(
 furb_stats,
 "./data/furb_stats.parquet")


# Color palettes ----
# https://colors.muz.li/
## Dark blue - #000054: c("#125d72","#1a70a3","#94e8ff","#c9f3ff","#ffffff")
## Light blue - #196f03: c("#5d92a2","#85c2e8","#c9f2ff","#e4f8ff","#ffffff")
## Dark green - #44ad99: c("#307972","#44ad99","#b2fff8","#d8fffb","#ffffff")
## Light green - #75d6c4: c("#52968f","#75d6c4","#c5fffa","#e2fffc","#ffffff")
## Red - #d9715f: c("#99504e","#db7370","#ffc2c1","#ffe1e0","#ffffff")
## Yellow - #f2c577: c("#a99554","#f2c478","#fff0bf","#fff7df","#ffffff")
## Dark grey - #a1a1bb
## Light grey - #f2ebe2
## Black - #2f2f3f
