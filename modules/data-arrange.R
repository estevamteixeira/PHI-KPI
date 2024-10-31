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
 "DLINTBFD",# Intend to Breastfeed
 ## Anemia ----
 "MANEM",# Indicator: Anemia
 "R014_01500",# Anemia in pregnancy Anemia in pregnancy - Hgb < 100 g/l recorded on chart as BEFORE delivery
 "MO990",# Anemia comp pregnancy/childbirth/puerperium
 "MD50_D53",# Nutritional anaemias
 "MD55_D59",# Haemolytic anaemias
 "MD60_D64",# Aplastic and other anemias
 ## 3rd degree laceration ----
 "MO702",# Third degree perineal laceration during delivery
 "MO70201",# Third degree perineal laceration del/delivery
 "MO70204",# Third degree perineal laceration del/postpartum
 "MO70211",# Third degree perineal laceration delivery type 3a/delivery
 "MO70214",# Third degree perineal laceration delivery type 3a/postpartum
 "MO70221",# Third degree perineal laceration delivery type 3b/delivery
 "MO70231",# Third degree perineal laceration delivery type 3c/delivery
 "MO70234",# Third degree perineal laceration delivery type 3c/postpartum
 "MO70281",# Third degree perineal laceration delivery other type/delivery
 "MO70291",# Third degree perineal laceration delivery type unspecified/delivery
 ## 4th degree laceration ----
 "MO703",# Fourth degree perineal laceration during delivery
 "MO70301",# Fourth degree perineal laceration del/delivery
 "MO70304",# Fourth degree perineal laceration del/postpartum
 ## Episiotomy ----
 "DMEPISIO",# Episiotomy (most serious)
 ## Blood product ----
 "RBC",# Red Blood Cell Transfusion
 "Any_Blood_Product",# Any of 13 specified or unspecified components
 ## Postpartum haemorrhage ----
 "Postpartum_Haemorrhage",# Postpartum Haemorrhage (diagnoses, interventions)
 ## Procedural intervention ----
 "R029",# Procedures for postpartum hemhorrage
 "R029_00100",# Uterine compression suture (eg.B-Lynch or Cho)
 "R029_00200",# Tying (ligation) of uterine arteries
 "R029_00300",# Embolization of uterine arteries
 "R029_00400",# Uterine tamponade (use of Bakri balloon or uterine packing, not to be confused with vaginal packing
 ## Uterine rupture ----
 "Uterine_Rupture",# Ruptured Uterus: Chart Review
 ## Hysterectomy ----
 "M_5MD60RC",# Cesarean hysterectomy with forceps
 "M_5MD60RD",# Cesarean hysterectomy with vacuum
 "M_5MD60KE",# Cesarean hysterectomy
 "M_5MD60CB",# Cesarean hysterectomy with vacuum and forceps
 "M_1RM89",# Excise tot uterus
 ## Mode of Delivery ----
 "MODEDEL",# Mode of Delivery
 ## IV iron therapy BEFORE delivery ----
 "R003_02100",# IV iron therapy recorded on chart as before delivery
 ## Gestational age ----
 "GA_BEST",# Best overall estimate of gestational age
 ## Corticosteroids ----
 "M_5AC20ALJ2",# Antepartum med tx combo route corticosteroid
 "I_1GD35BAJ2",# Pharm tx laryn cart EPO &system corticosteroid
 "DLNumStrds",# Number of admissions in pregnancy where steroids administered
 "DLSteroids",# Type of steroids administered
 ## Delivery hospital ----
 "DLHosp",# Delivery Hospital
 ## NICU ----
 "SCNAdm",# Number times Admitted to NICU - total
 ## Respiratory distress ----
 "R059",# Respiratory distress syndrome
 "R059_00200",# Respiratory distress syndrome |  IRDS, mild
 "R059_00300",# Respiratory distress syndrome |  IRDS, moderate
 "R059_00400",# Respiratory distress syndrome |  IRDS, severe
 "R059_00500",# Respiratory distress syndrome |  IRDS ,severity not stated
 "R059_00600",# Respiratory distress syndrome |  Transient Tachypnea of the newborn
 ## Delayed cord clamping ----
 "DEL_CORD_CLAMP",# Delayed cord clamping
 ## ICU admission ----
 "ICU_ADMISSION",# ICU_ADMISSION
 "DLNUMFET",# Number of Foetuses
 "POSATDEL",# Position at Delivery
 "DLPARA",# Number of Pregnancies, Excluding the Present, with >= 500g Birth
 "LABOUR",# Initiation of Labour
 "DLPRVCS",
 ## Steroids ----
 "R068_00200",# Maternal systemic steroid therapy |  Dexamethasone 24 to <=48 hours before delivery
 "R068_00300",# Maternal systemic steroid therapy |  Dexamethasone > 48 hours but <= 7 days before delivery
 "R068_00700",# Maternal systemic steroid therapy |  Betamethasone (Celestone)24 to <=48 hours before delivery
 "R068_00800",# Maternal systemic steroid therapy |  Betamethasone (Celestone)> 48 hours but <= 7 days before delivery
 "R068_01200",# Maternal systemic steroid therapy |  Unknown steroid 24 to <= 48 hours before delivery
 "R068_01300",# Maternal systemic steroid therapy |  Unknown steroid > 48 hours but <= 7 days before delivery
 ## Primary Indication for C-Section ----
 "INDICCS1",# Primary Indication for C-Section
 "CSstatus",# Type of C/S
 ## Severe Maternal Morbidity ----
 "JOGC_AnySMM",
 "JOGC_ICU"
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
   #between(BTBrthDT, as.POSIXct("2024-04-01 00:00:00.0000", tz="UTC"), as.POSIXct("2025-03-31 23:59:59.0000", tz="UTC")) ~ "2024-2025",
   TRUE ~ NA_character_
 ), levels = c(
  "2013-2014","2014-2015","2015-2016","2016-2017","2017-2018","2018-2019",
  "2019-2020","2020-2021","2021-2022","2022-2023","2023-2024"
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

##-------------------------
## Calendar year stats ----
##-------------------------

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
  prehyp_deltap = case_when(
   !is.na(lag(prehyp_rate, 9)) ~ (prehyp_rate - lag(prehyp_rate, 9)) / lag(prehyp_rate, 9),
   !is.na(lag(prehyp_rate, 8)) ~ (prehyp_rate - lag(prehyp_rate, 8)) / lag(prehyp_rate, 8),
   !is.na(lag(prehyp_rate, 7)) ~ (prehyp_rate - lag(prehyp_rate, 7)) / lag(prehyp_rate, 7),
   !is.na(lag(prehyp_rate, 6)) ~ (prehyp_rate - lag(prehyp_rate, 6)) / lag(prehyp_rate, 6),
   !is.na(lag(prehyp_rate, 5)) ~ (prehyp_rate - lag(prehyp_rate, 5)) / lag(prehyp_rate, 5),
   !is.na(lag(prehyp_rate, 4)) ~ (prehyp_rate - lag(prehyp_rate, 4)) / lag(prehyp_rate, 4),
   !is.na(lag(prehyp_rate, 3)) ~ (prehyp_rate - lag(prehyp_rate, 3)) / lag(prehyp_rate, 3),
   !is.na(lag(prehyp_rate, 2)) ~ (prehyp_rate - lag(prehyp_rate, 2)) / lag(prehyp_rate, 2),
   !is.na(lag(prehyp_rate, 1)) ~ (prehyp_rate - lag(prehyp_rate, 1)) / lag(prehyp_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

c2 <- dta %>%
 group_by(BrthYear, Gestational_Hypertension) %>%
 mutate(
  gesthyp_count = n()
 ) %>%
 group_by(BrthYear) %>%
 mutate(
  total_year = n(),
  gesthyp_rate = gesthyp_count/total_year,
  gesthyp_prev = 1000*gesthyp_rate
 ) %>%
 ungroup() %>%
 filter(Gestational_Hypertension %in% 1) %>%
 select(BrthYear, Gestational_Hypertension, gesthyp_count,gesthyp_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  gesthyp_delta = (gesthyp_rate - lag(gesthyp_rate))/lag(gesthyp_rate),
  gesthyp_deltap = case_when(
   !is.na(lag(gesthyp_rate, 9)) ~ (gesthyp_rate - lag(gesthyp_rate, 9)) / lag(gesthyp_rate, 9),
   !is.na(lag(gesthyp_rate, 8)) ~ (gesthyp_rate - lag(gesthyp_rate, 8)) / lag(gesthyp_rate, 8),
   !is.na(lag(gesthyp_rate, 7)) ~ (gesthyp_rate - lag(gesthyp_rate, 7)) / lag(gesthyp_rate, 7),
   !is.na(lag(gesthyp_rate, 6)) ~ (gesthyp_rate - lag(gesthyp_rate, 6)) / lag(gesthyp_rate, 6),
   !is.na(lag(gesthyp_rate, 5)) ~ (gesthyp_rate - lag(gesthyp_rate, 5)) / lag(gesthyp_rate, 5),
   !is.na(lag(gesthyp_rate, 4)) ~ (gesthyp_rate - lag(gesthyp_rate, 4)) / lag(gesthyp_rate, 4),
   !is.na(lag(gesthyp_rate, 3)) ~ (gesthyp_rate - lag(gesthyp_rate, 3)) / lag(gesthyp_rate, 3),
   !is.na(lag(gesthyp_rate, 2)) ~ (gesthyp_rate - lag(gesthyp_rate, 2)) / lag(gesthyp_rate, 2),
   !is.na(lag(gesthyp_rate, 1)) ~ (gesthyp_rate - lag(gesthyp_rate, 1)) / lag(gesthyp_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
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
  hyp_deltap = case_when(
   !is.na(lag(hyp_rate, 9)) ~ (hyp_rate - lag(hyp_rate, 9)) / lag(hyp_rate, 9),
   !is.na(lag(hyp_rate, 8)) ~ (hyp_rate - lag(hyp_rate, 8)) / lag(hyp_rate, 8),
   !is.na(lag(hyp_rate, 7)) ~ (hyp_rate - lag(hyp_rate, 7)) / lag(hyp_rate, 7),
   !is.na(lag(hyp_rate, 6)) ~ (hyp_rate - lag(hyp_rate, 6)) / lag(hyp_rate, 6),
   !is.na(lag(hyp_rate, 5)) ~ (hyp_rate - lag(hyp_rate, 5)) / lag(hyp_rate, 5),
   !is.na(lag(hyp_rate, 4)) ~ (hyp_rate - lag(hyp_rate, 4)) / lag(hyp_rate, 4),
   !is.na(lag(hyp_rate, 3)) ~ (hyp_rate - lag(hyp_rate, 3)) / lag(hyp_rate, 3),
   !is.na(lag(hyp_rate, 2)) ~ (hyp_rate - lag(hyp_rate, 2)) / lag(hyp_rate, 2),
   !is.na(lag(hyp_rate, 1)) ~ (hyp_rate - lag(hyp_rate, 1)) / lag(hyp_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Anaemia ----

c4 <- dta %>%
 select(BrthYear, MANEM, R014_01500, MO990, MD50_D53, MD55_D59, MD60_D64) %>%
 mutate(anemia = case_when(
  MANEM > 0 | R014_01500 > 0 | MO990 > 0 | MD50_D53 > 0 |
   MD55_D59 > 0 | MD60_D64 > 0 ~ 1,
  TRUE ~ NA_integer_
 )) %>%
 group_by(BrthYear, anemia) %>%
 mutate(
  anemia_count = n()
 ) %>%
 group_by(BrthYear) %>%
 mutate(
  total_year = n(),
  anemia_rate = anemia_count/total_year
 ) %>%
 ungroup() %>%
 filter(anemia %in% 1) %>%
 select(BrthYear, anemia, anemia_count, anemia_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  anemia_delta = (anemia_rate - lag(anemia_rate))/lag(anemia_rate),
  anemia_deltap = case_when(
   !is.na(lag(anemia_rate, 9)) ~ (anemia_rate - lag(anemia_rate, 9)) / lag(anemia_rate, 9),
   !is.na(lag(anemia_rate, 8)) ~ (anemia_rate - lag(anemia_rate, 8)) / lag(anemia_rate, 8),
   !is.na(lag(anemia_rate, 7)) ~ (anemia_rate - lag(anemia_rate, 7)) / lag(anemia_rate, 7),
   !is.na(lag(anemia_rate, 6)) ~ (anemia_rate - lag(anemia_rate, 6)) / lag(anemia_rate, 6),
   !is.na(lag(anemia_rate, 5)) ~ (anemia_rate - lag(anemia_rate, 5)) / lag(anemia_rate, 5),
   !is.na(lag(anemia_rate, 4)) ~ (anemia_rate - lag(anemia_rate, 4)) / lag(anemia_rate, 4),
   !is.na(lag(anemia_rate, 3)) ~ (anemia_rate - lag(anemia_rate, 3)) / lag(anemia_rate, 3),
   !is.na(lag(anemia_rate, 2)) ~ (anemia_rate - lag(anemia_rate, 2)) / lag(anemia_rate, 2),
   !is.na(lag(anemia_rate, 1)) ~ (anemia_rate - lag(anemia_rate, 1)) / lag(anemia_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Diabetes ----

c5 <- dta %>%
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
  prediab_deltap = case_when(
   !is.na(lag(prediab_rate, 9)) ~ (prediab_rate - lag(prediab_rate, 9)) / lag(prediab_rate, 9),
   !is.na(lag(prediab_rate, 8)) ~ (prediab_rate - lag(prediab_rate, 8)) / lag(prediab_rate, 8),
   !is.na(lag(prediab_rate, 7)) ~ (prediab_rate - lag(prediab_rate, 7)) / lag(prediab_rate, 7),
   !is.na(lag(prediab_rate, 6)) ~ (prediab_rate - lag(prediab_rate, 6)) / lag(prediab_rate, 6),
   !is.na(lag(prediab_rate, 5)) ~ (prediab_rate - lag(prediab_rate, 5)) / lag(prediab_rate, 5),
   !is.na(lag(prediab_rate, 4)) ~ (prediab_rate - lag(prediab_rate, 4)) / lag(prediab_rate, 4),
   !is.na(lag(prediab_rate, 3)) ~ (prediab_rate - lag(prediab_rate, 3)) / lag(prediab_rate, 3),
   !is.na(lag(prediab_rate, 2)) ~ (prediab_rate - lag(prediab_rate, 2)) / lag(prediab_rate, 2),
   !is.na(lag(prediab_rate, 1)) ~ (prediab_rate - lag(prediab_rate, 1)) / lag(prediab_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

c6 <- dta %>%
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
  gestdiab_deltap = case_when(
   !is.na(lag(gestdiab_rate, 9)) ~ (gestdiab_rate - lag(gestdiab_rate, 9)) / lag(gestdiab_rate, 9),
   !is.na(lag(gestdiab_rate, 8)) ~ (gestdiab_rate - lag(gestdiab_rate, 8)) / lag(gestdiab_rate, 8),
   !is.na(lag(gestdiab_rate, 7)) ~ (gestdiab_rate - lag(gestdiab_rate, 7)) / lag(gestdiab_rate, 7),
   !is.na(lag(gestdiab_rate, 6)) ~ (gestdiab_rate - lag(gestdiab_rate, 6)) / lag(gestdiab_rate, 6),
   !is.na(lag(gestdiab_rate, 5)) ~ (gestdiab_rate - lag(gestdiab_rate, 5)) / lag(gestdiab_rate, 5),
   !is.na(lag(gestdiab_rate, 4)) ~ (gestdiab_rate - lag(gestdiab_rate, 4)) / lag(gestdiab_rate, 4),
   !is.na(lag(gestdiab_rate, 3)) ~ (gestdiab_rate - lag(gestdiab_rate, 3)) / lag(gestdiab_rate, 3),
   !is.na(lag(gestdiab_rate, 2)) ~ (gestdiab_rate - lag(gestdiab_rate, 2)) / lag(gestdiab_rate, 2),
   !is.na(lag(gestdiab_rate, 1)) ~ (gestdiab_rate - lag(gestdiab_rate, 1)) / lag(gestdiab_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

c7 <- dta %>%
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
  diab_deltap = case_when(
   !is.na(lag(diab_rate, 9)) ~ (diab_rate - lag(diab_rate, 9)) / lag(diab_rate, 9),
   !is.na(lag(diab_rate, 8)) ~ (diab_rate - lag(diab_rate, 8)) / lag(diab_rate, 8),
   !is.na(lag(diab_rate, 7)) ~ (diab_rate - lag(diab_rate, 7)) / lag(diab_rate, 7),
   !is.na(lag(diab_rate, 6)) ~ (diab_rate - lag(diab_rate, 6)) / lag(diab_rate, 6),
   !is.na(lag(diab_rate, 5)) ~ (diab_rate - lag(diab_rate, 5)) / lag(diab_rate, 5),
   !is.na(lag(diab_rate, 4)) ~ (diab_rate - lag(diab_rate, 4)) / lag(diab_rate, 4),
   !is.na(lag(diab_rate, 3)) ~ (diab_rate - lag(diab_rate, 3)) / lag(diab_rate, 3),
   !is.na(lag(diab_rate, 2)) ~ (diab_rate - lag(diab_rate, 2)) / lag(diab_rate, 2),
   !is.na(lag(diab_rate, 1)) ~ (diab_rate - lag(diab_rate, 1)) / lag(diab_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Severe Perineal Trauma with Spontaneous Vaginal Birth ----

c8 <- dta %>%
 select(BrthYear,
        ## 3rd degree laceration
        MO702,# Third degree perineal laceration during delivery
        MO70201,# Third degree perineal laceration del/delivery
        MO70204,# Third degree perineal laceration del/postpartum
        MO70211,# Third degree perineal laceration delivery type 3a/delivery
        MO70214,# Third degree perineal laceration delivery type 3a/postpartum
        MO70221,# Third degree perineal laceration delivery type 3b/delivery
        MO70231,# Third degree perineal laceration delivery type 3c/delivery
        MO70234,# Third degree perineal laceration delivery type 3c/postpartum
        MO70281,# Third degree perineal laceration delivery other type/delivery
        MO70291,# Third degree perineal laceration delivery type unspecified/delivery
        ## 4th degree laceration
        MO703,# Fourth degree perineal laceration during delivery
        MO70301,# Fourth degree perineal laceration del/delivery
        MO70304,# Fourth degree perineal laceration del/postpart
        DMMETHOD) %>%
 mutate(
  trd = case_when(
   MO702 > 0 | MO70201 > 0 | MO70204 > 0 |
    MO70211 > 0 | MO70214 > 0 | MO70221 > 0 |
    MO70231 > 0 | MO70234 > 0 | MO70281 > 0 | MO70291 > 0 ~ 1,
   TRUE ~ NA_integer_
  ),
  frth = case_when(
   MO703 > 0 | MO70301 > 0 | MO70304 > 0 ~ 1,
   TRUE ~ NA_integer_
  ),
  sptvg = case_when(
   toupper(DMMETHOD) %in% c("SPT", "BIV") ~ 1,
   TRUE ~ NA_integer_
  ),
  trdfrth = case_when(
   trd > 0 | frth > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, trdfrth, sptvg) %>%
 mutate(
  spt_count = n()
 ) %>%
 group_by(BrthYear, sptvg) %>%
 mutate(
  total_year = n(),
  spt_rate = spt_count/total_year
 ) %>%
 ungroup() %>%
 filter(trdfrth %in% 1, sptvg %in% 1) %>%
 select(BrthYear,sptvg,spt_count,spt_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  spt_delta = (spt_rate - lag(spt_rate))/lag(spt_rate),
  spt_deltap = case_when(
   !is.na(lag(spt_rate, 9)) ~ (spt_rate - lag(spt_rate, 9)) / lag(spt_rate, 9),
   !is.na(lag(spt_rate, 8)) ~ (spt_rate - lag(spt_rate, 8)) / lag(spt_rate, 8),
   !is.na(lag(spt_rate, 7)) ~ (spt_rate - lag(spt_rate, 7)) / lag(spt_rate, 7),
   !is.na(lag(spt_rate, 6)) ~ (spt_rate - lag(spt_rate, 6)) / lag(spt_rate, 6),
   !is.na(lag(spt_rate, 5)) ~ (spt_rate - lag(spt_rate, 5)) / lag(spt_rate, 5),
   !is.na(lag(spt_rate, 4)) ~ (spt_rate - lag(spt_rate, 4)) / lag(spt_rate, 4),
   !is.na(lag(spt_rate, 3)) ~ (spt_rate - lag(spt_rate, 3)) / lag(spt_rate, 3),
   !is.na(lag(spt_rate, 2)) ~ (spt_rate - lag(spt_rate, 2)) / lag(spt_rate, 2),
   !is.na(lag(spt_rate, 1)) ~ (spt_rate - lag(spt_rate, 1)) / lag(spt_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Mediolateral Episiotomy with Operative Vaginal Birth ----
#### ND = Not Done
#### MD = Midline
#### ML = Mediolateral

c9 <- dta %>%
 select(BrthYear,
        DMMETHOD,
        DMEPISIO) %>%
 mutate(
  med = case_when(
   DMEPISIO %in% "ML" ~ 1,
   TRUE ~ NA_integer_
  ),
  assvg = case_when(
   toupper(DMMETHOD) %in% c("VAC", "LOW", "MID") ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, med, assvg) %>%
 mutate(
  med_count = n()
 ) %>%
 group_by(BrthYear, assvg) %>%
 mutate(
  total_year = n(),
  med_rate = med_count/total_year
 ) %>%
 ungroup() %>%
 filter(med %in% 1, assvg %in% 1) %>%
 select(BrthYear,med,med_count,med_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  med_delta = (med_rate - lag(med_rate))/lag(med_rate),
  med_deltap = case_when(
   !is.na(lag(med_rate, 9)) ~ (med_rate - lag(med_rate, 9)) / lag(med_rate, 9),
   !is.na(lag(med_rate, 8)) ~ (med_rate - lag(med_rate, 8)) / lag(med_rate, 8),
   !is.na(lag(med_rate, 7)) ~ (med_rate - lag(med_rate, 7)) / lag(med_rate, 7),
   !is.na(lag(med_rate, 6)) ~ (med_rate - lag(med_rate, 6)) / lag(med_rate, 6),
   !is.na(lag(med_rate, 5)) ~ (med_rate - lag(med_rate, 5)) / lag(med_rate, 5),
   !is.na(lag(med_rate, 4)) ~ (med_rate - lag(med_rate, 4)) / lag(med_rate, 4),
   !is.na(lag(med_rate, 3)) ~ (med_rate - lag(med_rate, 3)) / lag(med_rate, 3),
   !is.na(lag(med_rate, 2)) ~ (med_rate - lag(med_rate, 2)) / lag(med_rate, 2),
   !is.na(lag(med_rate, 1)) ~ (med_rate - lag(med_rate, 1)) / lag(med_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Severe perineal trauma with Operative Vaginal Birth and mediolateral episiotomy ----

c10 <- dta %>%
 select(BrthYear,
        ## 3rd degree laceration
        MO702,# Third degree perineal laceration during delivery
        MO70201,# Third degree perineal laceration del/delivery
        MO70204,# Third degree perineal laceration del/postpartum
        MO70211,# Third degree perineal laceration delivery type 3a/delivery
        MO70214,# Third degree perineal laceration delivery type 3a/postpartum
        MO70221,# Third degree perineal laceration delivery type 3b/delivery
        MO70231,# Third degree perineal laceration delivery type 3c/delivery
        MO70234,# Third degree perineal laceration delivery type 3c/postpartum
        MO70281,# Third degree perineal laceration delivery other type/delivery
        MO70291,# Third degree perineal laceration delivery type unspecified/delivery
        ## 4th degree laceration
        MO703,# Fourth degree perineal laceration during delivery
        MO70301,# Fourth degree perineal laceration del/delivery
        MO70304,# Fourth degree perineal laceration del/postpart
        DMMETHOD,
        DMEPISIO) %>%
 mutate(
  trd = case_when(
   MO702 > 0 | MO70201 > 0 | MO70204 > 0 |
    MO70211 > 0 | MO70214 > 0 | MO70221 > 0 |
    MO70231 > 0 | MO70234 > 0 | MO70281 > 0 | MO70291 > 0 ~ 1,
   TRUE ~ NA_integer_
  ),
  frth = case_when(
   MO703 > 0 | MO70301 > 0 | MO70304 > 0 ~ 1,
   TRUE ~ NA_integer_
  ),
  assvg = case_when(
   toupper(DMMETHOD) %in% c("VAC", "LOW", "MID") ~ 1,
   TRUE ~ NA_integer_
  ),
  trdfrth = case_when(
   trd > 0 | frth > 0 ~ 1,
   TRUE ~ NA_integer_
  ),
  med = case_when(
   DMEPISIO %in% "ML" ~ 1,
   TRUE ~ NA_integer_
  ),
  sptassvgmed = case_when(
   trdfrth > 0 & med > 0 & assvg > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, trdfrth, assvg, med) %>%
 mutate(
  sptassvgmed_count = n()
 ) %>%
 group_by(BrthYear, assvg) %>%
 mutate(
  total_year = n(),
  sptassvgmed_rate = sptassvgmed_count/total_year
 ) %>%
 ungroup() %>%
 filter(sptassvgmed %in% 1) %>%
 select(BrthYear,sptassvgmed,sptassvgmed_count,sptassvgmed_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  sptassvgmed_delta = (sptassvgmed_rate - lag(sptassvgmed_rate))/lag(sptassvgmed_rate),
  sptassvgmed_deltap = case_when(
   !is.na(lag(sptassvgmed_rate, 9)) ~ (sptassvgmed_rate - lag(sptassvgmed_rate, 9)) / lag(sptassvgmed_rate, 9),
   !is.na(lag(sptassvgmed_rate, 8)) ~ (sptassvgmed_rate - lag(sptassvgmed_rate, 8)) / lag(sptassvgmed_rate, 8),
   !is.na(lag(sptassvgmed_rate, 7)) ~ (sptassvgmed_rate - lag(sptassvgmed_rate, 7)) / lag(sptassvgmed_rate, 7),
   !is.na(lag(sptassvgmed_rate, 6)) ~ (sptassvgmed_rate - lag(sptassvgmed_rate, 6)) / lag(sptassvgmed_rate, 6),
   !is.na(lag(sptassvgmed_rate, 5)) ~ (sptassvgmed_rate - lag(sptassvgmed_rate, 5)) / lag(sptassvgmed_rate, 5),
   !is.na(lag(sptassvgmed_rate, 4)) ~ (sptassvgmed_rate - lag(sptassvgmed_rate, 4)) / lag(sptassvgmed_rate, 4),
   !is.na(lag(sptassvgmed_rate, 3)) ~ (sptassvgmed_rate - lag(sptassvgmed_rate, 3)) / lag(sptassvgmed_rate, 3),
   !is.na(lag(sptassvgmed_rate, 2)) ~ (sptassvgmed_rate - lag(sptassvgmed_rate, 2)) / lag(sptassvgmed_rate, 2),
   !is.na(lag(sptassvgmed_rate, 1)) ~ (sptassvgmed_rate - lag(sptassvgmed_rate, 1)) / lag(sptassvgmed_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Severe perineal trauma with Operative Vaginal Birth without mediolateral episiotomy ----

c11 <- dta %>%
 select(BrthYear,
        ## 3rd degree laceration
        MO702,# Third degree perineal laceration during delivery
        MO70201,# Third degree perineal laceration del/delivery
        MO70204,# Third degree perineal laceration del/postpartum
        MO70211,# Third degree perineal laceration delivery type 3a/delivery
        MO70214,# Third degree perineal laceration delivery type 3a/postpartum
        MO70221,# Third degree perineal laceration delivery type 3b/delivery
        MO70231,# Third degree perineal laceration delivery type 3c/delivery
        MO70234,# Third degree perineal laceration delivery type 3c/postpartum
        MO70281,# Third degree perineal laceration delivery other type/delivery
        MO70291,# Third degree perineal laceration delivery type unspecified/delivery
        ## 4th degree laceration
        MO703,# Fourth degree perineal laceration during delivery
        MO70301,# Fourth degree perineal laceration del/delivery
        MO70304,# Fourth degree perineal laceration del/postpart
        DMMETHOD,
        DMEPISIO) %>%
 mutate(
  trd = case_when(
   MO702 > 0 | MO70201 > 0 | MO70204 > 0 |
    MO70211 > 0 | MO70214 > 0 | MO70221 > 0 |
    MO70231 > 0 | MO70234 > 0 | MO70281 > 0 | MO70291 > 0 ~ 1,
   TRUE ~ NA_integer_
  ),
  frth = case_when(
   MO703 > 0 | MO70301 > 0 | MO70304 > 0 ~ 1,
   TRUE ~ NA_integer_
  ),
  assvg = case_when(
   toupper(DMMETHOD) %in% c("VAC", "LOW", "MID") ~ 1,
   TRUE ~ NA_integer_
  ),
  trdfrth = case_when(
   trd > 0 | frth > 0 ~ 1,
   TRUE ~ NA_integer_
  ),
  med = case_when(
   !DMEPISIO %in% "ML" ~ 1,
   TRUE ~ NA_integer_
  ),
  sptassvgnmed = case_when(
   trdfrth > 0 & med > 0 & assvg > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, trdfrth, assvg, med) %>%
 mutate(
  sptassvgnmed_count = n()
 ) %>%
 group_by(BrthYear, assvg) %>%
 mutate(
  total_year = n(),
  sptassvgnmed_rate = sptassvgnmed_count/total_year
 ) %>%
 ungroup() %>%
 filter(sptassvgnmed %in% 1) %>%
 select(BrthYear,sptassvgnmed,sptassvgnmed_count,sptassvgnmed_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  sptassvgnmed_delta = (sptassvgnmed_rate - lag(sptassvgnmed_rate))/lag(sptassvgnmed_rate),
  sptassvgnmed_deltap = case_when(
   !is.na(lag(sptassvgnmed_rate, 9)) ~ (sptassvgnmed_rate - lag(sptassvgnmed_rate, 9)) / lag(sptassvgnmed_rate, 9),
   !is.na(lag(sptassvgnmed_rate, 8)) ~ (sptassvgnmed_rate - lag(sptassvgnmed_rate, 8)) / lag(sptassvgnmed_rate, 8),
   !is.na(lag(sptassvgnmed_rate, 7)) ~ (sptassvgnmed_rate - lag(sptassvgnmed_rate, 7)) / lag(sptassvgnmed_rate, 7),
   !is.na(lag(sptassvgnmed_rate, 6)) ~ (sptassvgnmed_rate - lag(sptassvgnmed_rate, 6)) / lag(sptassvgnmed_rate, 6),
   !is.na(lag(sptassvgnmed_rate, 5)) ~ (sptassvgnmed_rate - lag(sptassvgnmed_rate, 5)) / lag(sptassvgnmed_rate, 5),
   !is.na(lag(sptassvgnmed_rate, 4)) ~ (sptassvgnmed_rate - lag(sptassvgnmed_rate, 4)) / lag(sptassvgnmed_rate, 4),
   !is.na(lag(sptassvgnmed_rate, 3)) ~ (sptassvgnmed_rate - lag(sptassvgnmed_rate, 3)) / lag(sptassvgnmed_rate, 3),
   !is.na(lag(sptassvgnmed_rate, 2)) ~ (sptassvgnmed_rate - lag(sptassvgnmed_rate, 2)) / lag(sptassvgnmed_rate, 2),
   !is.na(lag(sptassvgnmed_rate, 1)) ~ (sptassvgnmed_rate - lag(sptassvgnmed_rate, 1)) / lag(sptassvgnmed_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Robson group ----

c12 <- dta %>%
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
  rbs1_deltap = case_when(
   !is.na(lag(rbs1_rate, 9)) ~ (rbs1_rate - lag(rbs1_rate, 9)) / lag(rbs1_rate, 9),
   !is.na(lag(rbs1_rate, 8)) ~ (rbs1_rate - lag(rbs1_rate, 8)) / lag(rbs1_rate, 8),
   !is.na(lag(rbs1_rate, 7)) ~ (rbs1_rate - lag(rbs1_rate, 7)) / lag(rbs1_rate, 7),
   !is.na(lag(rbs1_rate, 6)) ~ (rbs1_rate - lag(rbs1_rate, 6)) / lag(rbs1_rate, 6),
   !is.na(lag(rbs1_rate, 5)) ~ (rbs1_rate - lag(rbs1_rate, 5)) / lag(rbs1_rate, 5),
   !is.na(lag(rbs1_rate, 4)) ~ (rbs1_rate - lag(rbs1_rate, 4)) / lag(rbs1_rate, 4),
   !is.na(lag(rbs1_rate, 3)) ~ (rbs1_rate - lag(rbs1_rate, 3)) / lag(rbs1_rate, 3),
   !is.na(lag(rbs1_rate, 2)) ~ (rbs1_rate - lag(rbs1_rate, 2)) / lag(rbs1_rate, 2),
   !is.na(lag(rbs1_rate, 1)) ~ (rbs1_rate - lag(rbs1_rate, 1)) / lag(rbs1_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

c13 <- dta %>%
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
  rbs21_deltap = case_when(
   !is.na(lag(rbs21_rate, 9)) ~ (rbs21_rate - lag(rbs21_rate, 9)) / lag(rbs21_rate, 9),
   !is.na(lag(rbs21_rate, 8)) ~ (rbs21_rate - lag(rbs21_rate, 8)) / lag(rbs21_rate, 8),
   !is.na(lag(rbs21_rate, 7)) ~ (rbs21_rate - lag(rbs21_rate, 7)) / lag(rbs21_rate, 7),
   !is.na(lag(rbs21_rate, 6)) ~ (rbs21_rate - lag(rbs21_rate, 6)) / lag(rbs21_rate, 6),
   !is.na(lag(rbs21_rate, 5)) ~ (rbs21_rate - lag(rbs21_rate, 5)) / lag(rbs21_rate, 5),
   !is.na(lag(rbs21_rate, 4)) ~ (rbs21_rate - lag(rbs21_rate, 4)) / lag(rbs21_rate, 4),
   !is.na(lag(rbs21_rate, 3)) ~ (rbs21_rate - lag(rbs21_rate, 3)) / lag(rbs21_rate, 3),
   !is.na(lag(rbs21_rate, 2)) ~ (rbs21_rate - lag(rbs21_rate, 2)) / lag(rbs21_rate, 2),
   !is.na(lag(rbs21_rate, 1)) ~ (rbs21_rate - lag(rbs21_rate, 1)) / lag(rbs21_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

c14 <- dta %>%
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
  rbs51_deltap = case_when(
   !is.na(lag(rbs51_rate, 9)) ~ (rbs51_rate - lag(rbs51_rate, 9)) / lag(rbs51_rate, 9),
   !is.na(lag(rbs51_rate, 8)) ~ (rbs51_rate - lag(rbs51_rate, 8)) / lag(rbs51_rate, 8),
   !is.na(lag(rbs51_rate, 7)) ~ (rbs51_rate - lag(rbs51_rate, 7)) / lag(rbs51_rate, 7),
   !is.na(lag(rbs51_rate, 6)) ~ (rbs51_rate - lag(rbs51_rate, 6)) / lag(rbs51_rate, 6),
   !is.na(lag(rbs51_rate, 5)) ~ (rbs51_rate - lag(rbs51_rate, 5)) / lag(rbs51_rate, 5),
   !is.na(lag(rbs51_rate, 4)) ~ (rbs51_rate - lag(rbs51_rate, 4)) / lag(rbs51_rate, 4),
   !is.na(lag(rbs51_rate, 3)) ~ (rbs51_rate - lag(rbs51_rate, 3)) / lag(rbs51_rate, 3),
   !is.na(lag(rbs51_rate, 2)) ~ (rbs51_rate - lag(rbs51_rate, 2)) / lag(rbs51_rate, 2),
   !is.na(lag(rbs51_rate, 1)) ~ (rbs51_rate - lag(rbs51_rate, 1)) / lag(rbs51_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Postpartum hemorrhage treated with a blood transfusion ----

c15 <- dta %>%
 select(BrthYear, Any_Blood_Product, Postpartum_Haemorrhage) %>%
 mutate(
  pphbl = case_when(
   Any_Blood_Product > 0 & Postpartum_Haemorrhage > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, Any_Blood_Product, Postpartum_Haemorrhage) %>%
 mutate(
  pphbl_count = n()
 ) %>%
 group_by(BrthYear) %>%
 mutate(
  total_year = n(),
  pphbl_rate = pphbl_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Blood_Product %in% 1, Postpartum_Haemorrhage %in% 1) %>%
 select(BrthYear,pphbl, pphbl_count, pphbl_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  pphbl_delta = (pphbl_rate - lag(pphbl_rate))/lag(pphbl_rate),
  pphbl_deltap = case_when(
   !is.na(lag(pphbl_rate, 9)) ~ (pphbl_rate - lag(pphbl_rate, 9)) / lag(pphbl_rate, 9),
   !is.na(lag(pphbl_rate, 8)) ~ (pphbl_rate - lag(pphbl_rate, 8)) / lag(pphbl_rate, 8),
   !is.na(lag(pphbl_rate, 7)) ~ (pphbl_rate - lag(pphbl_rate, 7)) / lag(pphbl_rate, 7),
   !is.na(lag(pphbl_rate, 6)) ~ (pphbl_rate - lag(pphbl_rate, 6)) / lag(pphbl_rate, 6),
   !is.na(lag(pphbl_rate, 5)) ~ (pphbl_rate - lag(pphbl_rate, 5)) / lag(pphbl_rate, 5),
   !is.na(lag(pphbl_rate, 4)) ~ (pphbl_rate - lag(pphbl_rate, 4)) / lag(pphbl_rate, 4),
   !is.na(lag(pphbl_rate, 3)) ~ (pphbl_rate - lag(pphbl_rate, 3)) / lag(pphbl_rate, 3),
   !is.na(lag(pphbl_rate, 2)) ~ (pphbl_rate - lag(pphbl_rate, 2)) / lag(pphbl_rate, 2),
   !is.na(lag(pphbl_rate, 1)) ~ (pphbl_rate - lag(pphbl_rate, 1)) / lag(pphbl_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Postpartum hemorrhage resulting in procedural intervention ----

c16 <- dta %>%
 select(BrthYear, Postpartum_Haemorrhage,
        #### Procedural intervention
        R029,# Procedures for postpartum hemorrhage
        R029_00100,# Uterine compression suture (eg.B-Lynch or Cho)
        R029_00200,# Tying (ligation) of uterine arteries
        R029_00300,# Embolization of uterine arteries
        R029_00400,# Uterine tamponade (use of Bakri balloon or uterine packing, not to be confused with vaginal packing
        #### Hysterectomy
        M_5MD60RC,# Cesarean hysterectomy with forceps
        M_5MD60RD,# Cesarean hysterectomy with vacuum
        M_5MD60KE,# Cesarean hysterectomy
        M_5MD60CB,# Cesarean hysterectomy with vacuum and forceps
        M_1RM89,# Excise tot uterus
        MODEDEL) %>%
 mutate(
  # Procedural intervention
  prcint = case_when(
   R029 > 0 | R029_00100 > 0 |
    R029_00200 > 0 | R029_00300 > 0 |
    R029_00400 > 0 ~ 1,
   TRUE ~ NA_integer_
  ),
  # Hysterectomy
  hyst = case_when(
   M_5MD60RC > 0 | M_5MD60RD > 0 |
    M_5MD60KE > 0 | M_5MD60CB > 0 |
    M_1RM89 > 0 | toupper(MODEDEL) %in% "CSH" ~ 1,
   TRUE ~ NA_integer_
  ),
  pphpi = case_when(
   prcint > 0 | hyst > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, pphpi, Postpartum_Haemorrhage) %>%
 mutate(
  pphpi_count = n()
 ) %>%
 group_by(BrthYear, Postpartum_Haemorrhage) %>%
 mutate(
  total_year = n(),
  pphpi_rate = pphpi_count/total_year
 ) %>%
 ungroup() %>%
 filter(pphpi %in% 1, Postpartum_Haemorrhage %in% 1) %>%
 select(BrthYear,pphpi, pphpi_count, pphpi_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pphpi),
                 fill = list(
                  pphpi_count = 0,
                  pphpi_rate = NA
                 )) %>%
 mutate(
  pphpi_delta = (pphpi_rate - lag(pphpi_rate))/lag(pphpi_rate),
  pphpi_deltap = case_when(
   !is.na(lag(pphpi_rate, 9)) ~ (pphpi_rate - lag(pphpi_rate, 9)) / lag(pphpi_rate, 9),
   !is.na(lag(pphpi_rate, 8)) ~ (pphpi_rate - lag(pphpi_rate, 8)) / lag(pphpi_rate, 8),
   !is.na(lag(pphpi_rate, 7)) ~ (pphpi_rate - lag(pphpi_rate, 7)) / lag(pphpi_rate, 7),
   !is.na(lag(pphpi_rate, 6)) ~ (pphpi_rate - lag(pphpi_rate, 6)) / lag(pphpi_rate, 6),
   !is.na(lag(pphpi_rate, 5)) ~ (pphpi_rate - lag(pphpi_rate, 5)) / lag(pphpi_rate, 5),
   !is.na(lag(pphpi_rate, 4)) ~ (pphpi_rate - lag(pphpi_rate, 4)) / lag(pphpi_rate, 4),
   !is.na(lag(pphpi_rate, 3)) ~ (pphpi_rate - lag(pphpi_rate, 3)) / lag(pphpi_rate, 3),
   !is.na(lag(pphpi_rate, 2)) ~ (pphpi_rate - lag(pphpi_rate, 2)) / lag(pphpi_rate, 2),
   !is.na(lag(pphpi_rate, 1)) ~ (pphpi_rate - lag(pphpi_rate, 1)) / lag(pphpi_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Postpartum readmission rate ----

c17 <- dta %>%
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
  ppreadm_deltap = case_when(
   !is.na(lag(ppreadm_rate, 9)) ~ (ppreadm_rate - lag(ppreadm_rate, 9)) / lag(ppreadm_rate, 9),
   !is.na(lag(ppreadm_rate, 8)) ~ (ppreadm_rate - lag(ppreadm_rate, 8)) / lag(ppreadm_rate, 8),
   !is.na(lag(ppreadm_rate, 7)) ~ (ppreadm_rate - lag(ppreadm_rate, 7)) / lag(ppreadm_rate, 7),
   !is.na(lag(ppreadm_rate, 6)) ~ (ppreadm_rate - lag(ppreadm_rate, 6)) / lag(ppreadm_rate, 6),
   !is.na(lag(ppreadm_rate, 5)) ~ (ppreadm_rate - lag(ppreadm_rate, 5)) / lag(ppreadm_rate, 5),
   !is.na(lag(ppreadm_rate, 4)) ~ (ppreadm_rate - lag(ppreadm_rate, 4)) / lag(ppreadm_rate, 4),
   !is.na(lag(ppreadm_rate, 3)) ~ (ppreadm_rate - lag(ppreadm_rate, 3)) / lag(ppreadm_rate, 3),
   !is.na(lag(ppreadm_rate, 2)) ~ (ppreadm_rate - lag(ppreadm_rate, 2)) / lag(ppreadm_rate, 2),
   !is.na(lag(ppreadm_rate, 1)) ~ (ppreadm_rate - lag(ppreadm_rate, 1)) / lag(ppreadm_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Postpartum patients who received medical anticoagulation prophylaxis when indicated ----

#c18

### Skin to skin ----

c19 <- dta %>%
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
 filter(sknskn %in% 1, lvb %in% 1) %>%
 select(BrthYear, sknskn, sknskn_count,sknskn_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  sknskn_delta = (sknskn_rate - lag(sknskn_rate))/lag(sknskn_rate),
  sknskn_deltap = case_when(
   !is.na(lag(sknskn_rate, 9)) ~ (sknskn_rate - lag(sknskn_rate, 9)) / lag(sknskn_rate, 9),
   !is.na(lag(sknskn_rate, 8)) ~ (sknskn_rate - lag(sknskn_rate, 8)) / lag(sknskn_rate, 8),
   !is.na(lag(sknskn_rate, 7)) ~ (sknskn_rate - lag(sknskn_rate, 7)) / lag(sknskn_rate, 7),
   !is.na(lag(sknskn_rate, 6)) ~ (sknskn_rate - lag(sknskn_rate, 6)) / lag(sknskn_rate, 6),
   !is.na(lag(sknskn_rate, 5)) ~ (sknskn_rate - lag(sknskn_rate, 5)) / lag(sknskn_rate, 5),
   !is.na(lag(sknskn_rate, 4)) ~ (sknskn_rate - lag(sknskn_rate, 4)) / lag(sknskn_rate, 4),
   !is.na(lag(sknskn_rate, 3)) ~ (sknskn_rate - lag(sknskn_rate, 3)) / lag(sknskn_rate, 3),
   !is.na(lag(sknskn_rate, 2)) ~ (sknskn_rate - lag(sknskn_rate, 2)) / lag(sknskn_rate, 2),
   !is.na(lag(sknskn_rate, 1)) ~ (sknskn_rate - lag(sknskn_rate, 1)) / lag(sknskn_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

c20 <- dta %>%
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
 filter(sknsknvg %in% 1, lvb %in% 1) %>%
 select(BrthYear, sknsknvg, sknsknvg_count,sknsknvg_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  sknsknvg_delta = (sknsknvg_rate - lag(sknsknvg_rate))/lag(sknsknvg_rate),
  sknsknvg_deltap = case_when(
   !is.na(lag(sknsknvg_rate, 9)) ~ (sknsknvg_rate - lag(sknsknvg_rate, 9)) / lag(sknsknvg_rate, 9),
   !is.na(lag(sknsknvg_rate, 8)) ~ (sknsknvg_rate - lag(sknsknvg_rate, 8)) / lag(sknsknvg_rate, 8),
   !is.na(lag(sknsknvg_rate, 7)) ~ (sknsknvg_rate - lag(sknsknvg_rate, 7)) / lag(sknsknvg_rate, 7),
   !is.na(lag(sknsknvg_rate, 6)) ~ (sknsknvg_rate - lag(sknsknvg_rate, 6)) / lag(sknsknvg_rate, 6),
   !is.na(lag(sknsknvg_rate, 5)) ~ (sknsknvg_rate - lag(sknsknvg_rate, 5)) / lag(sknsknvg_rate, 5),
   !is.na(lag(sknsknvg_rate, 4)) ~ (sknsknvg_rate - lag(sknsknvg_rate, 4)) / lag(sknsknvg_rate, 4),
   !is.na(lag(sknsknvg_rate, 3)) ~ (sknsknvg_rate - lag(sknsknvg_rate, 3)) / lag(sknsknvg_rate, 3),
   !is.na(lag(sknsknvg_rate, 2)) ~ (sknsknvg_rate - lag(sknsknvg_rate, 2)) / lag(sknsknvg_rate, 2),
   !is.na(lag(sknsknvg_rate, 1)) ~ (sknsknvg_rate - lag(sknsknvg_rate, 1)) / lag(sknsknvg_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

c21 <- dta %>%
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
 filter(sknskncs %in% 1, lvb %in% 1) %>%
 select(BrthYear, sknskncs, sknskncs_count,sknskncs_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  sknskncs_delta = (sknskncs_rate - lag(sknskncs_rate))/lag(sknskncs_rate),
  sknskncs_deltap = case_when(
   !is.na(lag(sknskncs_rate, 9)) ~ (sknskncs_rate - lag(sknskncs_rate, 9)) / lag(sknskncs_rate, 9),
   !is.na(lag(sknskncs_rate, 8)) ~ (sknskncs_rate - lag(sknskncs_rate, 8)) / lag(sknskncs_rate, 8),
   !is.na(lag(sknskncs_rate, 7)) ~ (sknskncs_rate - lag(sknskncs_rate, 7)) / lag(sknskncs_rate, 7),
   !is.na(lag(sknskncs_rate, 6)) ~ (sknskncs_rate - lag(sknskncs_rate, 6)) / lag(sknskncs_rate, 6),
   !is.na(lag(sknskncs_rate, 5)) ~ (sknskncs_rate - lag(sknskncs_rate, 5)) / lag(sknskncs_rate, 5),
   !is.na(lag(sknskncs_rate, 4)) ~ (sknskncs_rate - lag(sknskncs_rate, 4)) / lag(sknskncs_rate, 4),
   !is.na(lag(sknskncs_rate, 3)) ~ (sknskncs_rate - lag(sknskncs_rate, 3)) / lag(sknskncs_rate, 3),
   !is.na(lag(sknskncs_rate, 2)) ~ (sknskncs_rate - lag(sknskncs_rate, 2)) / lag(sknskncs_rate, 2),
   !is.na(lag(sknskncs_rate, 1)) ~ (sknskncs_rate - lag(sknskncs_rate, 1)) / lag(sknskncs_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Postpartum blood products amongst those who received antenatal iron therapy ----

c22 <- dta %>%
 mutate(
  pphiv = case_when(
   Any_Blood_Product > 0 & R003_02100 > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, Any_Blood_Product, R003_02100) %>%
 mutate(
  pphiv_count = n()
 ) %>%
 group_by(BrthYear, Any_Blood_Product) %>%
 mutate(
  total_year = n(),
  pphiv_rate = pphiv_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Blood_Product %in% 1, R003_02100 %in% 1) %>%
 select(BrthYear, pphiv, pphiv_count, pphiv_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pphiv),
                 fill = list(
                  pphiv_count = 0,
                  pphiv_rate = NA
                 )) %>%
 mutate(
  pphiv_delta = (pphiv_rate - lag(pphiv_rate))/lag(pphiv_rate),
  pphiv_deltap = case_when(
   !is.na(lag(pphiv_rate, 9)) ~ (pphiv_rate - lag(pphiv_rate, 9)) / lag(pphiv_rate, 9),
   !is.na(lag(pphiv_rate, 8)) ~ (pphiv_rate - lag(pphiv_rate, 8)) / lag(pphiv_rate, 8),
   !is.na(lag(pphiv_rate, 7)) ~ (pphiv_rate - lag(pphiv_rate, 7)) / lag(pphiv_rate, 7),
   !is.na(lag(pphiv_rate, 6)) ~ (pphiv_rate - lag(pphiv_rate, 6)) / lag(pphiv_rate, 6),
   !is.na(lag(pphiv_rate, 5)) ~ (pphiv_rate - lag(pphiv_rate, 5)) / lag(pphiv_rate, 5),
   !is.na(lag(pphiv_rate, 4)) ~ (pphiv_rate - lag(pphiv_rate, 4)) / lag(pphiv_rate, 4),
   !is.na(lag(pphiv_rate, 3)) ~ (pphiv_rate - lag(pphiv_rate, 3)) / lag(pphiv_rate, 3),
   !is.na(lag(pphiv_rate, 2)) ~ (pphiv_rate - lag(pphiv_rate, 2)) / lag(pphiv_rate, 2),
   !is.na(lag(pphiv_rate, 1)) ~ (pphiv_rate - lag(pphiv_rate, 1)) / lag(pphiv_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Neonatal readmission rate ----

c23 <- dta %>%
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
 filter(neoreadm %in% 1, lvb %in% 1) %>%
 select(BrthYear, neoreadm, neoreadm_count,neoreadm_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  neoreadm_delta = (neoreadm_rate - lag(neoreadm_rate))/lag(neoreadm_rate),
  neoreadm_deltap = case_when(
   !is.na(lag(neoreadm_rate, 9)) ~ (neoreadm_rate - lag(neoreadm_rate, 9)) / lag(neoreadm_rate, 9),
   !is.na(lag(neoreadm_rate, 8)) ~ (neoreadm_rate - lag(neoreadm_rate, 8)) / lag(neoreadm_rate, 8),
   !is.na(lag(neoreadm_rate, 7)) ~ (neoreadm_rate - lag(neoreadm_rate, 7)) / lag(neoreadm_rate, 7),
   !is.na(lag(neoreadm_rate, 6)) ~ (neoreadm_rate - lag(neoreadm_rate, 6)) / lag(neoreadm_rate, 6),
   !is.na(lag(neoreadm_rate, 5)) ~ (neoreadm_rate - lag(neoreadm_rate, 5)) / lag(neoreadm_rate, 5),
   !is.na(lag(neoreadm_rate, 4)) ~ (neoreadm_rate - lag(neoreadm_rate, 4)) / lag(neoreadm_rate, 4),
   !is.na(lag(neoreadm_rate, 3)) ~ (neoreadm_rate - lag(neoreadm_rate, 3)) / lag(neoreadm_rate, 3),
   !is.na(lag(neoreadm_rate, 2)) ~ (neoreadm_rate - lag(neoreadm_rate, 2)) / lag(neoreadm_rate, 2),
   !is.na(lag(neoreadm_rate, 1)) ~ (neoreadm_rate - lag(neoreadm_rate, 1)) / lag(neoreadm_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Preterm infants who received complete course of steroids > 24h and < 7 days prior to birth ----

c24 <- dta %>%
 mutate(
pretster = case_when(
 (GA_BEST < 35 & GA_BEST >=24) &
  (R068_00200 > 0 | R068_00300 > 0 | R068_00700 > 0 |
    R068_00800 > 0 | R068_01200 > 0 | R068_01300 > 0 ) ~ 1,
 TRUE ~ NA_integer_
),
den = case_when(
 (GA_BEST < 35 & GA_BEST >=24) ~ 1,
 TRUE ~ NA_integer_
)
 ) %>%
 group_by(BrthYear, pretster) %>%
 mutate(
  pretster_count = n()
 ) %>%
 group_by(BrthYear, den) %>%
 mutate(
  total_year = n(),
  pretster_rate = pretster_count/total_year
 ) %>%
 ungroup() %>%
 filter(pretster %in% 1) %>%
 select(BrthYear, pretster, pretster_count,pretster_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pretster),
                 fill = list(
                  pretster_count = 0,
                  pretster_rate = NA
                 )) %>%
 mutate(
  pretster_delta = (pretster_rate - lag(pretster_rate))/lag(pretster_rate),
  pretster_deltap = case_when(
   !is.na(lag(pretster_rate, 9)) ~ (pretster_rate - lag(pretster_rate, 9)) / lag(pretster_rate, 9),
   !is.na(lag(pretster_rate, 8)) ~ (pretster_rate - lag(pretster_rate, 8)) / lag(pretster_rate, 8),
   !is.na(lag(pretster_rate, 7)) ~ (pretster_rate - lag(pretster_rate, 7)) / lag(pretster_rate, 7),
   !is.na(lag(pretster_rate, 6)) ~ (pretster_rate - lag(pretster_rate, 6)) / lag(pretster_rate, 6),
   !is.na(lag(pretster_rate, 5)) ~ (pretster_rate - lag(pretster_rate, 5)) / lag(pretster_rate, 5),
   !is.na(lag(pretster_rate, 4)) ~ (pretster_rate - lag(pretster_rate, 4)) / lag(pretster_rate, 4),
   !is.na(lag(pretster_rate, 3)) ~ (pretster_rate - lag(pretster_rate, 3)) / lag(pretster_rate, 3),
   !is.na(lag(pretster_rate, 2)) ~ (pretster_rate - lag(pretster_rate, 2)) / lag(pretster_rate, 2),
   !is.na(lag(pretster_rate, 1)) ~ (pretster_rate - lag(pretster_rate, 1)) / lag(pretster_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Preterm babies born in facility without NICU ----

c25 <- dta %>%
 mutate(
  pretnnicu = case_when(
   (GA_BEST < 34) &
    (!DLHosp %in% c(86,87,-12)) ~ 1,
   TRUE ~ NA_integer_
  ),
  den = case_when(
   (GA_BEST < 34) &
    (!DLHosp %in% c(-12)) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, pretnnicu) %>%
 mutate(
  pretnnicu_count = n()
 ) %>%
 group_by(BrthYear, den) %>%
 mutate(
  total_year = n(),
  pretnnicu_rate = pretnnicu_count/total_year
 ) %>%
 ungroup() %>%
 filter(pretnnicu %in% 1) %>%
 select(BrthYear, pretnnicu, pretnnicu_count,pretnnicu_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pretnnicu),
                 fill = list(
                  pretnnicu_count = 0,
                  pretnnicu_rate = NA
                 )) %>%
 mutate(
  pretnnicu_delta = (pretnnicu_rate - lag(pretnnicu_rate))/lag(pretnnicu_rate),
  pretnnicu_deltap = case_when(
   !is.na(lag(pretnnicu_rate, 9)) ~ (pretnnicu_rate - lag(pretnnicu_rate, 9)) / lag(pretnnicu_rate, 9),
   !is.na(lag(pretnnicu_rate, 8)) ~ (pretnnicu_rate - lag(pretnnicu_rate, 8)) / lag(pretnnicu_rate, 8),
   !is.na(lag(pretnnicu_rate, 7)) ~ (pretnnicu_rate - lag(pretnnicu_rate, 7)) / lag(pretnnicu_rate, 7),
   !is.na(lag(pretnnicu_rate, 6)) ~ (pretnnicu_rate - lag(pretnnicu_rate, 6)) / lag(pretnnicu_rate, 6),
   !is.na(lag(pretnnicu_rate, 5)) ~ (pretnnicu_rate - lag(pretnnicu_rate, 5)) / lag(pretnnicu_rate, 5),
   !is.na(lag(pretnnicu_rate, 4)) ~ (pretnnicu_rate - lag(pretnnicu_rate, 4)) / lag(pretnnicu_rate, 4),
   !is.na(lag(pretnnicu_rate, 3)) ~ (pretnnicu_rate - lag(pretnnicu_rate, 3)) / lag(pretnnicu_rate, 3),
   !is.na(lag(pretnnicu_rate, 2)) ~ (pretnnicu_rate - lag(pretnnicu_rate, 2)) / lag(pretnnicu_rate, 2),
   !is.na(lag(pretnnicu_rate, 1)) ~ (pretnnicu_rate - lag(pretnnicu_rate, 1)) / lag(pretnnicu_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Newborn respiratory distress associated with low-risk repeat cesarean at term gestation (≥ 37 weeks < 39 weeks) ----

c26 <- dta %>%
 mutate(
  newlwcs = case_when(
   (GA_BEST < 39 & GA_BEST >= 37) &
    (DLPRVCS > 0) &
    (toupper(DMMETHOD) %in% "CST") &
    (!toupper(BTOUTCOM) %in% "FTD") &
    (R059_00200 > 0 | R059_00300 > 0 | R059_00400 > 0 | R059_00500 > 0 | R059_00600 > 0) ~ 1,
   TRUE ~ NA_integer_
  ),
  den = case_when(
   (GA_BEST < 39 & GA_BEST >= 37) &
    (DLPRVCS > 0) &
    (toupper(DMMETHOD) %in% "CST") &
    (!toupper(BTOUTCOM) %in% "FTD") ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, newlwcs) %>%
 mutate(
  newlwcs_count = n()
 ) %>%
 group_by(BrthYear, den) %>%
 mutate(
  total_year = n(),
  newlwcs_rate = newlwcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(newlwcs %in% 1) %>%
 select(BrthYear, newlwcs, newlwcs_count,newlwcs_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(newlwcs),
                 fill = list(
                  newlwcs_count = 0,
                  newlwcs_rate = NA
                 )) %>%
 mutate(
  newlwcs_delta = (newlwcs_rate - lag(newlwcs_rate))/lag(newlwcs_rate),
  newlwcs_deltap = case_when(
   !is.na(lag(newlwcs_rate, 9)) ~ (newlwcs_rate - lag(newlwcs_rate, 9)) / lag(newlwcs_rate, 9),
   !is.na(lag(newlwcs_rate, 8)) ~ (newlwcs_rate - lag(newlwcs_rate, 8)) / lag(newlwcs_rate, 8),
   !is.na(lag(newlwcs_rate, 7)) ~ (newlwcs_rate - lag(newlwcs_rate, 7)) / lag(newlwcs_rate, 7),
   !is.na(lag(newlwcs_rate, 6)) ~ (newlwcs_rate - lag(newlwcs_rate, 6)) / lag(newlwcs_rate, 6),
   !is.na(lag(newlwcs_rate, 5)) ~ (newlwcs_rate - lag(newlwcs_rate, 5)) / lag(newlwcs_rate, 5),
   !is.na(lag(newlwcs_rate, 4)) ~ (newlwcs_rate - lag(newlwcs_rate, 4)) / lag(newlwcs_rate, 4),
   !is.na(lag(newlwcs_rate, 3)) ~ (newlwcs_rate - lag(newlwcs_rate, 3)) / lag(newlwcs_rate, 3),
   !is.na(lag(newlwcs_rate, 2)) ~ (newlwcs_rate - lag(newlwcs_rate, 2)) / lag(newlwcs_rate, 2),
   !is.na(lag(newlwcs_rate, 1)) ~ (newlwcs_rate - lag(newlwcs_rate, 1)) / lag(newlwcs_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Delayed Cord Clamping  ----

c27 <- dta %>%
 mutate(
  delcord = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (DEL_CORD_CLAMP %in% 3) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, delcord) %>%
 mutate(
  delcord_count = n()
 ) %>%
 group_by(BrthYear, lvb) %>%
 mutate(
  total_year = n(),
  delcord_rate = delcord_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcord %in% 1, lvb %in% 1) %>%
 select(BrthYear, delcord, delcord_count,delcord_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcord),
                 fill = list(
                  delcord_count = 0,
                  delcord_rate = NA
                 )) %>%
 mutate(
  delcord_delta = (delcord_rate - lag(delcord_rate))/lag(delcord_rate),
  delcord_deltap = case_when(
   !is.na(lag(delcord_rate, 9)) ~ (delcord_rate - lag(delcord_rate, 9)) / lag(delcord_rate, 9),
   !is.na(lag(delcord_rate, 8)) ~ (delcord_rate - lag(delcord_rate, 8)) / lag(delcord_rate, 8),
   !is.na(lag(delcord_rate, 7)) ~ (delcord_rate - lag(delcord_rate, 7)) / lag(delcord_rate, 7),
   !is.na(lag(delcord_rate, 6)) ~ (delcord_rate - lag(delcord_rate, 6)) / lag(delcord_rate, 6),
   !is.na(lag(delcord_rate, 5)) ~ (delcord_rate - lag(delcord_rate, 5)) / lag(delcord_rate, 5),
   !is.na(lag(delcord_rate, 4)) ~ (delcord_rate - lag(delcord_rate, 4)) / lag(delcord_rate, 4),
   !is.na(lag(delcord_rate, 3)) ~ (delcord_rate - lag(delcord_rate, 3)) / lag(delcord_rate, 3),
   !is.na(lag(delcord_rate, 2)) ~ (delcord_rate - lag(delcord_rate, 2)) / lag(delcord_rate, 2),
   !is.na(lag(delcord_rate, 1)) ~ (delcord_rate - lag(delcord_rate, 1)) / lag(delcord_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Delayed Cord Clamping amongst Term Newborns ----

c28 <- dta %>%
 mutate(
  delcordterm = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (DEL_CORD_CLAMP %in% 3) &
    (GA_BEST >= 37) ~ 1,
   TRUE ~ NA_integer_
  ),
  den = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (GA_BEST >= 37) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, delcordterm) %>%
 mutate(
  delcordterm_count = n()
 ) %>%
 group_by(BrthYear, den) %>%
 mutate(
  total_year = n(),
  delcordterm_rate = delcordterm_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordterm %in% 1) %>%
 select(BrthYear, delcordterm, delcordterm_count,delcordterm_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordterm),
                 fill = list(
                  delcordterm_count = 0,
                  delcordterm_rate = NA
                 )) %>%
 mutate(
  delcordterm_delta = (delcordterm_rate - lag(delcordterm_rate))/lag(delcordterm_rate),
  delcordterm_deltap = case_when(
   !is.na(lag(delcordterm_rate, 9)) ~ (delcordterm_rate - lag(delcordterm_rate, 9)) / lag(delcordterm_rate, 9),
   !is.na(lag(delcordterm_rate, 8)) ~ (delcordterm_rate - lag(delcordterm_rate, 8)) / lag(delcordterm_rate, 8),
   !is.na(lag(delcordterm_rate, 7)) ~ (delcordterm_rate - lag(delcordterm_rate, 7)) / lag(delcordterm_rate, 7),
   !is.na(lag(delcordterm_rate, 6)) ~ (delcordterm_rate - lag(delcordterm_rate, 6)) / lag(delcordterm_rate, 6),
   !is.na(lag(delcordterm_rate, 5)) ~ (delcordterm_rate - lag(delcordterm_rate, 5)) / lag(delcordterm_rate, 5),
   !is.na(lag(delcordterm_rate, 4)) ~ (delcordterm_rate - lag(delcordterm_rate, 4)) / lag(delcordterm_rate, 4),
   !is.na(lag(delcordterm_rate, 3)) ~ (delcordterm_rate - lag(delcordterm_rate, 3)) / lag(delcordterm_rate, 3),
   !is.na(lag(delcordterm_rate, 2)) ~ (delcordterm_rate - lag(delcordterm_rate, 2)) / lag(delcordterm_rate, 2),
   !is.na(lag(delcordterm_rate, 1)) ~ (delcordterm_rate - lag(delcordterm_rate, 1)) / lag(delcordterm_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Delayed Cord Clamping amongst Term Newborns following spontaneous vaginal birth ----

c29 <- dta %>%
 mutate(
  delcordtermvg = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (DEL_CORD_CLAMP %in% 3) &
    (GA_BEST >= 37) &
    (toupper(DMMETHOD) %in% c("SPT","BIV")) ~ 1,
   TRUE ~ NA_integer_
  ),
  den = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (GA_BEST >= 37) &
    (toupper(DMMETHOD) %in% c("SPT","BIV")) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, delcordtermvg) %>%
 mutate(
  delcordtermvg_count = n()
 ) %>%
 group_by(BrthYear, den) %>%
 mutate(
  total_year = n(),
  delcordtermvg_rate = delcordtermvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordtermvg %in% 1) %>%
 select(BrthYear, delcordtermvg, delcordtermvg_count,delcordtermvg_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordtermvg),
                 fill = list(
                  delcordtermvg_count = 0,
                  delcordtermvg_rate = NA
                 )) %>%
 mutate(
  delcordtermvg_delta = (delcordtermvg_rate - lag(delcordtermvg_rate))/lag(delcordtermvg_rate),
  delcordtermvg_deltap = case_when(
   !is.na(lag(delcordtermvg_rate, 9)) ~ (delcordtermvg_rate - lag(delcordtermvg_rate, 9)) / lag(delcordtermvg_rate, 9),
   !is.na(lag(delcordtermvg_rate, 8)) ~ (delcordtermvg_rate - lag(delcordtermvg_rate, 8)) / lag(delcordtermvg_rate, 8),
   !is.na(lag(delcordtermvg_rate, 7)) ~ (delcordtermvg_rate - lag(delcordtermvg_rate, 7)) / lag(delcordtermvg_rate, 7),
   !is.na(lag(delcordtermvg_rate, 6)) ~ (delcordtermvg_rate - lag(delcordtermvg_rate, 6)) / lag(delcordtermvg_rate, 6),
   !is.na(lag(delcordtermvg_rate, 5)) ~ (delcordtermvg_rate - lag(delcordtermvg_rate, 5)) / lag(delcordtermvg_rate, 5),
   !is.na(lag(delcordtermvg_rate, 4)) ~ (delcordtermvg_rate - lag(delcordtermvg_rate, 4)) / lag(delcordtermvg_rate, 4),
   !is.na(lag(delcordtermvg_rate, 3)) ~ (delcordtermvg_rate - lag(delcordtermvg_rate, 3)) / lag(delcordtermvg_rate, 3),
   !is.na(lag(delcordtermvg_rate, 2)) ~ (delcordtermvg_rate - lag(delcordtermvg_rate, 2)) / lag(delcordtermvg_rate, 2),
   !is.na(lag(delcordtermvg_rate, 1)) ~ (delcordtermvg_rate - lag(delcordtermvg_rate, 1)) / lag(delcordtermvg_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Delayed Cord Clamping amongst Term Newborns following caesarean birth ----

c30 <- dta %>%
 mutate(
  delcordtermcs = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (DEL_CORD_CLAMP %in% 3) &
    (GA_BEST >= 37) &
    (toupper(DMMETHOD) %in% c("CST")) ~ 1,
   TRUE ~ NA_integer_
  ),
  den = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (GA_BEST >= 37) &
    (toupper(DMMETHOD) %in% c("CST")) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, delcordtermcs) %>%
 mutate(
  delcordtermcs_count = n()
 ) %>%
 group_by(BrthYear, den) %>%
 mutate(
  total_year = n(),
  delcordtermcs_rate = delcordtermcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordtermcs %in% 1) %>%
 select(BrthYear, delcordtermcs, delcordtermcs_count,delcordtermcs_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordtermcs),
                 fill = list(
                  delcordtermcs_count = 0,
                  delcordtermcs_rate = NA
                 )) %>%
 mutate(
  delcordtermcs_delta = (delcordtermcs_rate - lag(delcordtermcs_rate))/lag(delcordtermcs_rate),
  delcordtermcs_deltap = case_when(
   !is.na(lag(delcordtermcs_rate, 9)) ~ (delcordtermcs_rate - lag(delcordtermcs_rate, 9)) / lag(delcordtermcs_rate, 9),
   !is.na(lag(delcordtermcs_rate, 8)) ~ (delcordtermcs_rate - lag(delcordtermcs_rate, 8)) / lag(delcordtermcs_rate, 8),
   !is.na(lag(delcordtermcs_rate, 7)) ~ (delcordtermcs_rate - lag(delcordtermcs_rate, 7)) / lag(delcordtermcs_rate, 7),
   !is.na(lag(delcordtermcs_rate, 6)) ~ (delcordtermcs_rate - lag(delcordtermcs_rate, 6)) / lag(delcordtermcs_rate, 6),
   !is.na(lag(delcordtermcs_rate, 5)) ~ (delcordtermcs_rate - lag(delcordtermcs_rate, 5)) / lag(delcordtermcs_rate, 5),
   !is.na(lag(delcordtermcs_rate, 4)) ~ (delcordtermcs_rate - lag(delcordtermcs_rate, 4)) / lag(delcordtermcs_rate, 4),
   !is.na(lag(delcordtermcs_rate, 3)) ~ (delcordtermcs_rate - lag(delcordtermcs_rate, 3)) / lag(delcordtermcs_rate, 3),
   !is.na(lag(delcordtermcs_rate, 2)) ~ (delcordtermcs_rate - lag(delcordtermcs_rate, 2)) / lag(delcordtermcs_rate, 2),
   !is.na(lag(delcordtermcs_rate, 1)) ~ (delcordtermcs_rate - lag(delcordtermcs_rate, 1)) / lag(delcordtermcs_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Delayed Cord Clamping amongst preterm Newborns ----

c31 <- dta %>%
 mutate(
  delcordpreterm = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (DEL_CORD_CLAMP %in% 3) &
    (GA_BEST < 37) ~ 1,
   TRUE ~ NA_integer_
  ),
  den = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (GA_BEST < 37) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, delcordpreterm) %>%
 mutate(
  delcordpreterm_count = n()
 ) %>%
 group_by(BrthYear, den) %>%
 mutate(
  total_year = n(),
  delcordpreterm_rate = delcordpreterm_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpreterm %in% 1) %>%
 select(BrthYear, delcordpreterm, delcordpreterm_count,delcordpreterm_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordpreterm),
                 fill = list(
                  delcordpreterm_count = 0,
                  delcordpreterm_rate = NA
                 )) %>%
 mutate(
  delcordpreterm_delta = (delcordpreterm_rate - lag(delcordpreterm_rate))/lag(delcordpreterm_rate),
  delcordpreterm_deltap = case_when(
   !is.na(lag(delcordpreterm_rate, 9)) ~ (delcordpreterm_rate - lag(delcordpreterm_rate, 9)) / lag(delcordpreterm_rate, 9),
   !is.na(lag(delcordpreterm_rate, 8)) ~ (delcordpreterm_rate - lag(delcordpreterm_rate, 8)) / lag(delcordpreterm_rate, 8),
   !is.na(lag(delcordpreterm_rate, 7)) ~ (delcordpreterm_rate - lag(delcordpreterm_rate, 7)) / lag(delcordpreterm_rate, 7),
   !is.na(lag(delcordpreterm_rate, 6)) ~ (delcordpreterm_rate - lag(delcordpreterm_rate, 6)) / lag(delcordpreterm_rate, 6),
   !is.na(lag(delcordpreterm_rate, 5)) ~ (delcordpreterm_rate - lag(delcordpreterm_rate, 5)) / lag(delcordpreterm_rate, 5),
   !is.na(lag(delcordpreterm_rate, 4)) ~ (delcordpreterm_rate - lag(delcordpreterm_rate, 4)) / lag(delcordpreterm_rate, 4),
   !is.na(lag(delcordpreterm_rate, 3)) ~ (delcordpreterm_rate - lag(delcordpreterm_rate, 3)) / lag(delcordpreterm_rate, 3),
   !is.na(lag(delcordpreterm_rate, 2)) ~ (delcordpreterm_rate - lag(delcordpreterm_rate, 2)) / lag(delcordpreterm_rate, 2),
   !is.na(lag(delcordpreterm_rate, 1)) ~ (delcordpreterm_rate - lag(delcordpreterm_rate, 1)) / lag(delcordpreterm_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Delayed Cord Clamping amongst preterm Newborns following spontaneous vaginal birth ----

c32 <- dta %>%
 mutate(
  delcordpretermvg = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (DEL_CORD_CLAMP %in% 3) &
    (GA_BEST < 37) &
    (toupper(DMMETHOD) %in% c("SPT","BIV")) ~ 1,
   TRUE ~ NA_integer_
  ),
  den = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (GA_BEST < 37) &
    (toupper(DMMETHOD) %in% c("SPT","BIV")) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, delcordpretermvg) %>%
 mutate(
  delcordpretermvg_count = n()
 ) %>%
 group_by(BrthYear, den) %>%
 mutate(
  total_year = n(),
  delcordpretermvg_rate = delcordpretermvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpretermvg %in% 1) %>%
 select(BrthYear, delcordpretermvg, delcordpretermvg_count,delcordpretermvg_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordpretermvg),
                 fill = list(
                  delcordpretermvg_count = 0,
                  delcordpretermvg_rate = NA
                 )) %>%
 mutate(
  delcordpretermvg_delta = (delcordpretermvg_rate - lag(delcordpretermvg_rate))/lag(delcordpretermvg_rate),
  delcordpretermvg_deltap = case_when(
   !is.na(lag(delcordpretermvg_rate, 9)) ~ (delcordpretermvg_rate - lag(delcordpretermvg_rate, 9)) / lag(delcordpretermvg_rate, 9),
   !is.na(lag(delcordpretermvg_rate, 8)) ~ (delcordpretermvg_rate - lag(delcordpretermvg_rate, 8)) / lag(delcordpretermvg_rate, 8),
   !is.na(lag(delcordpretermvg_rate, 7)) ~ (delcordpretermvg_rate - lag(delcordpretermvg_rate, 7)) / lag(delcordpretermvg_rate, 7),
   !is.na(lag(delcordpretermvg_rate, 6)) ~ (delcordpretermvg_rate - lag(delcordpretermvg_rate, 6)) / lag(delcordpretermvg_rate, 6),
   !is.na(lag(delcordpretermvg_rate, 5)) ~ (delcordpretermvg_rate - lag(delcordpretermvg_rate, 5)) / lag(delcordpretermvg_rate, 5),
   !is.na(lag(delcordpretermvg_rate, 4)) ~ (delcordpretermvg_rate - lag(delcordpretermvg_rate, 4)) / lag(delcordpretermvg_rate, 4),
   !is.na(lag(delcordpretermvg_rate, 3)) ~ (delcordpretermvg_rate - lag(delcordpretermvg_rate, 3)) / lag(delcordpretermvg_rate, 3),
   !is.na(lag(delcordpretermvg_rate, 2)) ~ (delcordpretermvg_rate - lag(delcordpretermvg_rate, 2)) / lag(delcordpretermvg_rate, 2),
   !is.na(lag(delcordpretermvg_rate, 1)) ~ (delcordpretermvg_rate - lag(delcordpretermvg_rate, 1)) / lag(delcordpretermvg_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Delayed Cord Clamping amongst preterm Newborns following caesarean birth ----

c33 <- dta %>%
 mutate(
  delcordpretermcs = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (DEL_CORD_CLAMP %in% 3) &
    (GA_BEST < 37) &
    (toupper(DMMETHOD) %in% c("CST")) ~ 1,
   TRUE ~ NA_integer_
  ),
  den = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (GA_BEST < 37) &
    (toupper(DMMETHOD) %in% c("CST")) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, delcordpretermcs) %>%
 mutate(
  delcordpretermcs_count = n()
 ) %>%
 group_by(BrthYear, den) %>%
 mutate(
  total_year = n(),
  delcordpretermcs_rate = delcordpretermcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpretermcs %in% 1) %>%
 select(BrthYear, delcordpretermcs, delcordpretermcs_count,delcordpretermcs_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordpretermcs),
                 fill = list(
                  delcordpretermcs_count = 0,
                  delcordpretermcs_rate = NA
                 )) %>%
 mutate(
  delcordpretermcs_delta = (delcordpretermcs_rate - lag(delcordpretermcs_rate))/lag(delcordpretermcs_rate),
  delcordpretermcs_deltap = case_when(
   !is.na(lag(delcordpretermcs_rate, 9)) ~ (delcordpretermcs_rate - lag(delcordpretermcs_rate, 9)) / lag(delcordpretermcs_rate, 9),
   !is.na(lag(delcordpretermcs_rate, 8)) ~ (delcordpretermcs_rate - lag(delcordpretermcs_rate, 8)) / lag(delcordpretermcs_rate, 8),
   !is.na(lag(delcordpretermcs_rate, 7)) ~ (delcordpretermcs_rate - lag(delcordpretermcs_rate, 7)) / lag(delcordpretermcs_rate, 7),
   !is.na(lag(delcordpretermcs_rate, 6)) ~ (delcordpretermcs_rate - lag(delcordpretermcs_rate, 6)) / lag(delcordpretermcs_rate, 6),
   !is.na(lag(delcordpretermcs_rate, 5)) ~ (delcordpretermcs_rate - lag(delcordpretermcs_rate, 5)) / lag(delcordpretermcs_rate, 5),
   !is.na(lag(delcordpretermcs_rate, 4)) ~ (delcordpretermcs_rate - lag(delcordpretermcs_rate, 4)) / lag(delcordpretermcs_rate, 4),
   !is.na(lag(delcordpretermcs_rate, 3)) ~ (delcordpretermcs_rate - lag(delcordpretermcs_rate, 3)) / lag(delcordpretermcs_rate, 3),
   !is.na(lag(delcordpretermcs_rate, 2)) ~ (delcordpretermcs_rate - lag(delcordpretermcs_rate, 2)) / lag(delcordpretermcs_rate, 2),
   !is.na(lag(delcordpretermcs_rate, 1)) ~ (delcordpretermcs_rate - lag(delcordpretermcs_rate, 1)) / lag(delcordpretermcs_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Exclusive Human milk feeding ----

c34 <- dta %>%
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
 filter(excbrst %in% 1, lvb %in% 1) %>%
 select(BrthYear, excbrst, excbrst_count,excbrst_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  excbrst_delta = (excbrst_rate - lag(excbrst_rate))/lag(excbrst_rate),
  excbrst_deltap = case_when(
   !is.na(lag(excbrst_rate, 9)) ~ (excbrst_rate - lag(excbrst_rate, 9)) / lag(excbrst_rate, 9),
   !is.na(lag(excbrst_rate, 8)) ~ (excbrst_rate - lag(excbrst_rate, 8)) / lag(excbrst_rate, 8),
   !is.na(lag(excbrst_rate, 7)) ~ (excbrst_rate - lag(excbrst_rate, 7)) / lag(excbrst_rate, 7),
   !is.na(lag(excbrst_rate, 6)) ~ (excbrst_rate - lag(excbrst_rate, 6)) / lag(excbrst_rate, 6),
   !is.na(lag(excbrst_rate, 5)) ~ (excbrst_rate - lag(excbrst_rate, 5)) / lag(excbrst_rate, 5),
   !is.na(lag(excbrst_rate, 4)) ~ (excbrst_rate - lag(excbrst_rate, 4)) / lag(excbrst_rate, 4),
   !is.na(lag(excbrst_rate, 3)) ~ (excbrst_rate - lag(excbrst_rate, 3)) / lag(excbrst_rate, 3),
   !is.na(lag(excbrst_rate, 2)) ~ (excbrst_rate - lag(excbrst_rate, 2)) / lag(excbrst_rate, 2),
   !is.na(lag(excbrst_rate, 1)) ~ (excbrst_rate - lag(excbrst_rate, 1)) / lag(excbrst_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Non-exclusive (any) human milk feeding ----

c35 <- dta %>%
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
 filter(nexcbrst %in% 1, lvb %in% 1) %>%
 select(BrthYear, nexcbrst, nexcbrst_count,nexcbrst_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  nexcbrst_delta = (nexcbrst_rate - lag(nexcbrst_rate))/lag(nexcbrst_rate),
  nexcbrst_deltap = case_when(
   !is.na(lag(nexcbrst_rate, 9)) ~ (nexcbrst_rate - lag(nexcbrst_rate, 9)) / lag(nexcbrst_rate, 9),
   !is.na(lag(nexcbrst_rate, 8)) ~ (nexcbrst_rate - lag(nexcbrst_rate, 8)) / lag(nexcbrst_rate, 8),
   !is.na(lag(nexcbrst_rate, 7)) ~ (nexcbrst_rate - lag(nexcbrst_rate, 7)) / lag(nexcbrst_rate, 7),
   !is.na(lag(nexcbrst_rate, 6)) ~ (nexcbrst_rate - lag(nexcbrst_rate, 6)) / lag(nexcbrst_rate, 6),
   !is.na(lag(nexcbrst_rate, 5)) ~ (nexcbrst_rate - lag(nexcbrst_rate, 5)) / lag(nexcbrst_rate, 5),
   !is.na(lag(nexcbrst_rate, 4)) ~ (nexcbrst_rate - lag(nexcbrst_rate, 4)) / lag(nexcbrst_rate, 4),
   !is.na(lag(nexcbrst_rate, 3)) ~ (nexcbrst_rate - lag(nexcbrst_rate, 3)) / lag(nexcbrst_rate, 3),
   !is.na(lag(nexcbrst_rate, 2)) ~ (nexcbrst_rate - lag(nexcbrst_rate, 2)) / lag(nexcbrst_rate, 2),
   !is.na(lag(nexcbrst_rate, 1)) ~ (nexcbrst_rate - lag(nexcbrst_rate, 1)) / lag(nexcbrst_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### No breast/chest feeding among those who intended to breast/chest feed ----

c36 <- dta %>%
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
 filter(nbrst %in% 1, lvb %in% 1) %>%
 select(BrthYear, nbrst, nbrst_count,nbrst_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  nbrst_delta = (nbrst_rate - lag(nbrst_rate))/lag(nbrst_rate),
  nbrst_deltap = case_when(
   !is.na(lag(nbrst_rate, 9)) ~ (nbrst_rate - lag(nbrst_rate, 9)) / lag(nbrst_rate, 9),
   !is.na(lag(nbrst_rate, 8)) ~ (nbrst_rate - lag(nbrst_rate, 8)) / lag(nbrst_rate, 8),
   !is.na(lag(nbrst_rate, 7)) ~ (nbrst_rate - lag(nbrst_rate, 7)) / lag(nbrst_rate, 7),
   !is.na(lag(nbrst_rate, 6)) ~ (nbrst_rate - lag(nbrst_rate, 6)) / lag(nbrst_rate, 6),
   !is.na(lag(nbrst_rate, 5)) ~ (nbrst_rate - lag(nbrst_rate, 5)) / lag(nbrst_rate, 5),
   !is.na(lag(nbrst_rate, 4)) ~ (nbrst_rate - lag(nbrst_rate, 4)) / lag(nbrst_rate, 4),
   !is.na(lag(nbrst_rate, 3)) ~ (nbrst_rate - lag(nbrst_rate, 3)) / lag(nbrst_rate, 3),
   !is.na(lag(nbrst_rate, 2)) ~ (nbrst_rate - lag(nbrst_rate, 2)) / lag(nbrst_rate, 2),
   !is.na(lag(nbrst_rate, 1)) ~ (nbrst_rate - lag(nbrst_rate, 1)) / lag(nbrst_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### The breast/chest feeding initiation rate ----

c37 <- dta %>%
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
 filter(brstinit %in% 1, lvb %in% 1) %>%
 select(BrthYear, brstinit, brstinit_count,brstinit_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  brstinit_delta = (brstinit_rate - lag(brstinit_rate))/lag(brstinit_rate),
  brstinit_deltap = case_when(
   !is.na(lag(brstinit_rate, 9)) ~ (brstinit_rate - lag(brstinit_rate, 9)) / lag(brstinit_rate, 9),
   !is.na(lag(brstinit_rate, 8)) ~ (brstinit_rate - lag(brstinit_rate, 8)) / lag(brstinit_rate, 8),
   !is.na(lag(brstinit_rate, 7)) ~ (brstinit_rate - lag(brstinit_rate, 7)) / lag(brstinit_rate, 7),
   !is.na(lag(brstinit_rate, 6)) ~ (brstinit_rate - lag(brstinit_rate, 6)) / lag(brstinit_rate, 6),
   !is.na(lag(brstinit_rate, 5)) ~ (brstinit_rate - lag(brstinit_rate, 5)) / lag(brstinit_rate, 5),
   !is.na(lag(brstinit_rate, 4)) ~ (brstinit_rate - lag(brstinit_rate, 4)) / lag(brstinit_rate, 4),
   !is.na(lag(brstinit_rate, 3)) ~ (brstinit_rate - lag(brstinit_rate, 3)) / lag(brstinit_rate, 3),
   !is.na(lag(brstinit_rate, 2)) ~ (brstinit_rate - lag(brstinit_rate, 2)) / lag(brstinit_rate, 2),
   !is.na(lag(brstinit_rate, 1)) ~ (brstinit_rate - lag(brstinit_rate, 1)) / lag(brstinit_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### ICU Admission during Pregnancy or Postpartum ----

c38 <- dta %>%
 mutate(
  icu = case_when(
   JOGC_ICU > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, JOGC_ICU) %>%
 mutate(
  icu_count = n()
 ) %>%
 group_by(BrthYear) %>%
 mutate(
  total_year = n(),
  icu_rate = icu_count/total_year
 ) %>%
 ungroup() %>%
 filter(JOGC_ICU %in% 1) %>%
 select(BrthYear, icu, icu_count,icu_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  icu_delta = (icu_rate - lag(icu_rate))/lag(icu_rate),
  icu_deltap = case_when(
   !is.na(lag(icu_rate, 9)) ~ (icu_rate - lag(icu_rate, 9)) / lag(icu_rate, 9),
   !is.na(lag(icu_rate, 8)) ~ (icu_rate - lag(icu_rate, 8)) / lag(icu_rate, 8),
   !is.na(lag(icu_rate, 7)) ~ (icu_rate - lag(icu_rate, 7)) / lag(icu_rate, 7),
   !is.na(lag(icu_rate, 6)) ~ (icu_rate - lag(icu_rate, 6)) / lag(icu_rate, 6),
   !is.na(lag(icu_rate, 5)) ~ (icu_rate - lag(icu_rate, 5)) / lag(icu_rate, 5),
   !is.na(lag(icu_rate, 4)) ~ (icu_rate - lag(icu_rate, 4)) / lag(icu_rate, 4),
   !is.na(lag(icu_rate, 3)) ~ (icu_rate - lag(icu_rate, 3)) / lag(icu_rate, 3),
   !is.na(lag(icu_rate, 2)) ~ (icu_rate - lag(icu_rate, 2)) / lag(icu_rate, 2),
   !is.na(lag(icu_rate, 1)) ~ (icu_rate - lag(icu_rate, 1)) / lag(icu_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Rate of Severe Morbidity in Pregnancy or Postpartum ----

c39 <- dta %>%
 mutate(
  smm = case_when(
   JOGC_AnySMM > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, JOGC_AnySMM) %>%
 mutate(
  smm_count = n()
 ) %>%
 group_by(BrthYear, lvb) %>%
 mutate(
  total_year = n(),
  smm_rate = smm_count/total_year
 ) %>%
 ungroup() %>%
 filter(JOGC_AnySMM %in% 1, lvb %in% 1) %>%
 select(BrthYear, smm, smm_count,smm_rate) %>%
 distinct() %>%
 arrange(BrthYear) %>%
 mutate(
  smm_delta = (smm_rate - lag(smm_rate))/lag(smm_rate),
  smm_deltap = case_when(
   !is.na(lag(smm_rate, 9)) ~ (smm_rate - lag(smm_rate, 9)) / lag(smm_rate, 9),
   !is.na(lag(smm_rate, 8)) ~ (smm_rate - lag(smm_rate, 8)) / lag(smm_rate, 8),
   !is.na(lag(smm_rate, 7)) ~ (smm_rate - lag(smm_rate, 7)) / lag(smm_rate, 7),
   !is.na(lag(smm_rate, 6)) ~ (smm_rate - lag(smm_rate, 6)) / lag(smm_rate, 6),
   !is.na(lag(smm_rate, 5)) ~ (smm_rate - lag(smm_rate, 5)) / lag(smm_rate, 5),
   !is.na(lag(smm_rate, 4)) ~ (smm_rate - lag(smm_rate, 4)) / lag(smm_rate, 4),
   !is.na(lag(smm_rate, 3)) ~ (smm_rate - lag(smm_rate, 3)) / lag(smm_rate, 3),
   !is.na(lag(smm_rate, 2)) ~ (smm_rate - lag(smm_rate, 2)) / lag(smm_rate, 2),
   !is.na(lag(smm_rate, 1)) ~ (smm_rate - lag(smm_rate, 1)) / lag(smm_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

cyearly_stats <- cbind(
 c1 %>% select(-PreExisting_Hypertension),
 c2 %>% select(-Gestational_Hypertension,-BrthYear),
 c3 %>% select(-Any_Hypertension,-BrthYear),
 c4 %>% select(-anemia,-BrthYear),
 c5 %>% select(-PreExisting_Diabetes,-BrthYear),
 c6 %>% select(-GDM,-BrthYear),
 c7 %>% select(-Any_Diabetes,-BrthYear),
 c8 %>% select(-sptvg,-BrthYear),
 c9 %>% select(-med,-BrthYear),
 c10 %>% select(-sptassvgmed,-BrthYear),
 c11 %>% select(-sptassvgnmed,-BrthYear),
 c12 %>% select(-RobsnGrp,-BrthYear),
 c13 %>% select(-RobsnGrp,-BrthYear),
 c14 %>% select(-RobsnGrp,-BrthYear),
 c15 %>% select(-pphbl,-BrthYear),
 c16 %>% select(-pphpi,-BrthYear),
 c17 %>% select(-ppreadm,-BrthYear),
 #c18 %>% select(-brstinit,-BrthYear),
 c19 %>% select(-sknskn,-BrthYear),
 c20 %>% select(-sknsknvg,-BrthYear),
 c21 %>% select(-sknskncs,-BrthYear),
 c22 %>% select(-pphiv,-BrthYear),
 c23 %>% select(-neoreadm,-BrthYear),
 c24 %>% select(-pretster,-BrthYear),
 c25 %>% select(-pretnnicu,-BrthYear),
 c26 %>% select(-newlwcs,-BrthYear),
 c27 %>% select(-delcord,-BrthYear),
 c28 %>% select(-delcordterm,-BrthYear),
 c29 %>% select(-delcordtermvg,-BrthYear),
 c30 %>% select(-delcordtermcs,-BrthYear),
 c31 %>% select(-delcordpreterm,-BrthYear),
 c32 %>% select(-delcordpretermvg,-BrthYear),
 c33 %>% select(-delcordpretermcs,-BrthYear),
 c34 %>% select(-excbrst,-BrthYear),
 c35 %>% select(-nexcbrst,-BrthYear),
 c36 %>% select(-nbrst,-BrthYear),
 c37 %>% select(-brstinit,-BrthYear),
 c38 %>% select(-icu,-BrthYear),
 c39 %>% select(-smm,-BrthYear)
)

##-----------------------
## Fiscal year stats ----
##-----------------------

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
  prehyp_deltap = case_when(
   !is.na(lag(prehyp_rate, 9)) ~ (prehyp_rate - lag(prehyp_rate, 9)) / lag(prehyp_rate, 9),
   !is.na(lag(prehyp_rate, 8)) ~ (prehyp_rate - lag(prehyp_rate, 8)) / lag(prehyp_rate, 8),
   !is.na(lag(prehyp_rate, 7)) ~ (prehyp_rate - lag(prehyp_rate, 7)) / lag(prehyp_rate, 7),
   !is.na(lag(prehyp_rate, 6)) ~ (prehyp_rate - lag(prehyp_rate, 6)) / lag(prehyp_rate, 6),
   !is.na(lag(prehyp_rate, 5)) ~ (prehyp_rate - lag(prehyp_rate, 5)) / lag(prehyp_rate, 5),
   !is.na(lag(prehyp_rate, 4)) ~ (prehyp_rate - lag(prehyp_rate, 4)) / lag(prehyp_rate, 4),
   !is.na(lag(prehyp_rate, 3)) ~ (prehyp_rate - lag(prehyp_rate, 3)) / lag(prehyp_rate, 3),
   !is.na(lag(prehyp_rate, 2)) ~ (prehyp_rate - lag(prehyp_rate, 2)) / lag(prehyp_rate, 2),
   !is.na(lag(prehyp_rate, 1)) ~ (prehyp_rate - lag(prehyp_rate, 1)) / lag(prehyp_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

c2 <- dta %>%
 group_by(FiscalYear, Gestational_Hypertension) %>%
 mutate(
  gesthyp_count = n()
 ) %>%
 group_by(FiscalYear) %>%
 mutate(
  total_year = n(),
  gesthyp_rate = gesthyp_count/total_year,
  gesthyp_prev = 1000*gesthyp_rate
 ) %>%
 ungroup() %>%
 filter(Gestational_Hypertension %in% 1) %>%
 select(FiscalYear, Gestational_Hypertension, gesthyp_count,gesthyp_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  gesthyp_delta = (gesthyp_rate - lag(gesthyp_rate))/lag(gesthyp_rate),
  gesthyp_deltap = case_when(
   !is.na(lag(gesthyp_rate, 9)) ~ (gesthyp_rate - lag(gesthyp_rate, 9)) / lag(gesthyp_rate, 9),
   !is.na(lag(gesthyp_rate, 8)) ~ (gesthyp_rate - lag(gesthyp_rate, 8)) / lag(gesthyp_rate, 8),
   !is.na(lag(gesthyp_rate, 7)) ~ (gesthyp_rate - lag(gesthyp_rate, 7)) / lag(gesthyp_rate, 7),
   !is.na(lag(gesthyp_rate, 6)) ~ (gesthyp_rate - lag(gesthyp_rate, 6)) / lag(gesthyp_rate, 6),
   !is.na(lag(gesthyp_rate, 5)) ~ (gesthyp_rate - lag(gesthyp_rate, 5)) / lag(gesthyp_rate, 5),
   !is.na(lag(gesthyp_rate, 4)) ~ (gesthyp_rate - lag(gesthyp_rate, 4)) / lag(gesthyp_rate, 4),
   !is.na(lag(gesthyp_rate, 3)) ~ (gesthyp_rate - lag(gesthyp_rate, 3)) / lag(gesthyp_rate, 3),
   !is.na(lag(gesthyp_rate, 2)) ~ (gesthyp_rate - lag(gesthyp_rate, 2)) / lag(gesthyp_rate, 2),
   !is.na(lag(gesthyp_rate, 1)) ~ (gesthyp_rate - lag(gesthyp_rate, 1)) / lag(gesthyp_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
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
  hyp_deltap = case_when(
   !is.na(lag(hyp_rate, 9)) ~ (hyp_rate - lag(hyp_rate, 9)) / lag(hyp_rate, 9),
   !is.na(lag(hyp_rate, 8)) ~ (hyp_rate - lag(hyp_rate, 8)) / lag(hyp_rate, 8),
   !is.na(lag(hyp_rate, 7)) ~ (hyp_rate - lag(hyp_rate, 7)) / lag(hyp_rate, 7),
   !is.na(lag(hyp_rate, 6)) ~ (hyp_rate - lag(hyp_rate, 6)) / lag(hyp_rate, 6),
   !is.na(lag(hyp_rate, 5)) ~ (hyp_rate - lag(hyp_rate, 5)) / lag(hyp_rate, 5),
   !is.na(lag(hyp_rate, 4)) ~ (hyp_rate - lag(hyp_rate, 4)) / lag(hyp_rate, 4),
   !is.na(lag(hyp_rate, 3)) ~ (hyp_rate - lag(hyp_rate, 3)) / lag(hyp_rate, 3),
   !is.na(lag(hyp_rate, 2)) ~ (hyp_rate - lag(hyp_rate, 2)) / lag(hyp_rate, 2),
   !is.na(lag(hyp_rate, 1)) ~ (hyp_rate - lag(hyp_rate, 1)) / lag(hyp_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Anaemia ----

c4 <- dta %>%
 select(FiscalYear, MANEM, R014_01500, MO990, MD50_D53, MD55_D59, MD60_D64) %>%
 mutate(anemia = case_when(
  MANEM > 0 | R014_01500 > 0 | MO990 > 0 | MD50_D53 > 0 |
   MD55_D59 > 0 | MD60_D64 > 0 ~ 1,
  TRUE ~ NA_integer_
 )) %>%
 group_by(FiscalYear, anemia) %>%
 mutate(
  anemia_count = n()
 ) %>%
 group_by(FiscalYear) %>%
 mutate(
  total_year = n(),
  anemia_rate = anemia_count/total_year
 ) %>%
 ungroup() %>%
 filter(anemia %in% 1) %>%
 select(FiscalYear, anemia, anemia_count, anemia_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  anemia_delta = (anemia_rate - lag(anemia_rate))/lag(anemia_rate),
  anemia_deltap = case_when(
   !is.na(lag(anemia_rate, 9)) ~ (anemia_rate - lag(anemia_rate, 9)) / lag(anemia_rate, 9),
   !is.na(lag(anemia_rate, 8)) ~ (anemia_rate - lag(anemia_rate, 8)) / lag(anemia_rate, 8),
   !is.na(lag(anemia_rate, 7)) ~ (anemia_rate - lag(anemia_rate, 7)) / lag(anemia_rate, 7),
   !is.na(lag(anemia_rate, 6)) ~ (anemia_rate - lag(anemia_rate, 6)) / lag(anemia_rate, 6),
   !is.na(lag(anemia_rate, 5)) ~ (anemia_rate - lag(anemia_rate, 5)) / lag(anemia_rate, 5),
   !is.na(lag(anemia_rate, 4)) ~ (anemia_rate - lag(anemia_rate, 4)) / lag(anemia_rate, 4),
   !is.na(lag(anemia_rate, 3)) ~ (anemia_rate - lag(anemia_rate, 3)) / lag(anemia_rate, 3),
   !is.na(lag(anemia_rate, 2)) ~ (anemia_rate - lag(anemia_rate, 2)) / lag(anemia_rate, 2),
   !is.na(lag(anemia_rate, 1)) ~ (anemia_rate - lag(anemia_rate, 1)) / lag(anemia_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Diabetes ----

c5 <- dta %>%
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
  prediab_deltap = case_when(
   !is.na(lag(prediab_rate, 9)) ~ (prediab_rate - lag(prediab_rate, 9)) / lag(prediab_rate, 9),
   !is.na(lag(prediab_rate, 8)) ~ (prediab_rate - lag(prediab_rate, 8)) / lag(prediab_rate, 8),
   !is.na(lag(prediab_rate, 7)) ~ (prediab_rate - lag(prediab_rate, 7)) / lag(prediab_rate, 7),
   !is.na(lag(prediab_rate, 6)) ~ (prediab_rate - lag(prediab_rate, 6)) / lag(prediab_rate, 6),
   !is.na(lag(prediab_rate, 5)) ~ (prediab_rate - lag(prediab_rate, 5)) / lag(prediab_rate, 5),
   !is.na(lag(prediab_rate, 4)) ~ (prediab_rate - lag(prediab_rate, 4)) / lag(prediab_rate, 4),
   !is.na(lag(prediab_rate, 3)) ~ (prediab_rate - lag(prediab_rate, 3)) / lag(prediab_rate, 3),
   !is.na(lag(prediab_rate, 2)) ~ (prediab_rate - lag(prediab_rate, 2)) / lag(prediab_rate, 2),
   !is.na(lag(prediab_rate, 1)) ~ (prediab_rate - lag(prediab_rate, 1)) / lag(prediab_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

c6 <- dta %>%
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
  gestdiab_deltap = case_when(
   !is.na(lag(gestdiab_rate, 9)) ~ (gestdiab_rate - lag(gestdiab_rate, 9)) / lag(gestdiab_rate, 9),
   !is.na(lag(gestdiab_rate, 8)) ~ (gestdiab_rate - lag(gestdiab_rate, 8)) / lag(gestdiab_rate, 8),
   !is.na(lag(gestdiab_rate, 7)) ~ (gestdiab_rate - lag(gestdiab_rate, 7)) / lag(gestdiab_rate, 7),
   !is.na(lag(gestdiab_rate, 6)) ~ (gestdiab_rate - lag(gestdiab_rate, 6)) / lag(gestdiab_rate, 6),
   !is.na(lag(gestdiab_rate, 5)) ~ (gestdiab_rate - lag(gestdiab_rate, 5)) / lag(gestdiab_rate, 5),
   !is.na(lag(gestdiab_rate, 4)) ~ (gestdiab_rate - lag(gestdiab_rate, 4)) / lag(gestdiab_rate, 4),
   !is.na(lag(gestdiab_rate, 3)) ~ (gestdiab_rate - lag(gestdiab_rate, 3)) / lag(gestdiab_rate, 3),
   !is.na(lag(gestdiab_rate, 2)) ~ (gestdiab_rate - lag(gestdiab_rate, 2)) / lag(gestdiab_rate, 2),
   !is.na(lag(gestdiab_rate, 1)) ~ (gestdiab_rate - lag(gestdiab_rate, 1)) / lag(gestdiab_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

c7 <- dta %>%
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
  diab_deltap = case_when(
   !is.na(lag(diab_rate, 9)) ~ (diab_rate - lag(diab_rate, 9)) / lag(diab_rate, 9),
   !is.na(lag(diab_rate, 8)) ~ (diab_rate - lag(diab_rate, 8)) / lag(diab_rate, 8),
   !is.na(lag(diab_rate, 7)) ~ (diab_rate - lag(diab_rate, 7)) / lag(diab_rate, 7),
   !is.na(lag(diab_rate, 6)) ~ (diab_rate - lag(diab_rate, 6)) / lag(diab_rate, 6),
   !is.na(lag(diab_rate, 5)) ~ (diab_rate - lag(diab_rate, 5)) / lag(diab_rate, 5),
   !is.na(lag(diab_rate, 4)) ~ (diab_rate - lag(diab_rate, 4)) / lag(diab_rate, 4),
   !is.na(lag(diab_rate, 3)) ~ (diab_rate - lag(diab_rate, 3)) / lag(diab_rate, 3),
   !is.na(lag(diab_rate, 2)) ~ (diab_rate - lag(diab_rate, 2)) / lag(diab_rate, 2),
   !is.na(lag(diab_rate, 1)) ~ (diab_rate - lag(diab_rate, 1)) / lag(diab_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Severe Perineal Trauma with Spontaneous Vaginal Birth ----

c8 <- dta %>%
 select(FiscalYear,
        ## 3rd degree laceration
        MO702,# Third degree perineal laceration during delivery
        MO70201,# Third degree perineal laceration del/delivery
        MO70204,# Third degree perineal laceration del/postpartum
        MO70211,# Third degree perineal laceration delivery type 3a/delivery
        MO70214,# Third degree perineal laceration delivery type 3a/postpartum
        MO70221,# Third degree perineal laceration delivery type 3b/delivery
        MO70231,# Third degree perineal laceration delivery type 3c/delivery
        MO70234,# Third degree perineal laceration delivery type 3c/postpartum
        MO70281,# Third degree perineal laceration delivery other type/delivery
        MO70291,# Third degree perineal laceration delivery type unspecified/delivery
        ## 4th degree laceration
        MO703,# Fourth degree perineal laceration during delivery
        MO70301,# Fourth degree perineal laceration del/delivery
        MO70304,# Fourth degree perineal laceration del/postpart
        DMMETHOD) %>%
 mutate(
  trd = case_when(
   MO702 > 0 | MO70201 > 0 | MO70204 > 0 |
    MO70211 > 0 | MO70214 > 0 | MO70221 > 0 |
    MO70231 > 0 | MO70234 > 0 | MO70281 > 0 | MO70291 > 0 ~ 1,
   TRUE ~ NA_integer_
  ),
  frth = case_when(
   MO703 > 0 | MO70301 > 0 | MO70304 > 0 ~ 1,
   TRUE ~ NA_integer_
  ),
  sptvg = case_when(
   toupper(DMMETHOD) %in% c("SPT", "BIV") ~ 1,
   TRUE ~ NA_integer_
  ),
  trdfrth = case_when(
   trd > 0 | frth > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, trdfrth, sptvg) %>%
 mutate(
  spt_count = n()
 ) %>%
 group_by(FiscalYear, sptvg) %>%
 mutate(
  total_year = n(),
  spt_rate = spt_count/total_year
 ) %>%
 ungroup() %>%
 filter(trdfrth %in% 1, sptvg %in% 1) %>%
 select(FiscalYear,sptvg,spt_count,spt_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  spt_delta = (spt_rate - lag(spt_rate))/lag(spt_rate),
  spt_deltap = case_when(
   !is.na(lag(spt_rate, 9)) ~ (spt_rate - lag(spt_rate, 9)) / lag(spt_rate, 9),
   !is.na(lag(spt_rate, 8)) ~ (spt_rate - lag(spt_rate, 8)) / lag(spt_rate, 8),
   !is.na(lag(spt_rate, 7)) ~ (spt_rate - lag(spt_rate, 7)) / lag(spt_rate, 7),
   !is.na(lag(spt_rate, 6)) ~ (spt_rate - lag(spt_rate, 6)) / lag(spt_rate, 6),
   !is.na(lag(spt_rate, 5)) ~ (spt_rate - lag(spt_rate, 5)) / lag(spt_rate, 5),
   !is.na(lag(spt_rate, 4)) ~ (spt_rate - lag(spt_rate, 4)) / lag(spt_rate, 4),
   !is.na(lag(spt_rate, 3)) ~ (spt_rate - lag(spt_rate, 3)) / lag(spt_rate, 3),
   !is.na(lag(spt_rate, 2)) ~ (spt_rate - lag(spt_rate, 2)) / lag(spt_rate, 2),
   !is.na(lag(spt_rate, 1)) ~ (spt_rate - lag(spt_rate, 1)) / lag(spt_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Mediolateral Episiotomy with Operative Vaginal Birth ----
#### ND = Not Done
#### MD = Midline
#### ML = Mediolateral

c9 <- dta %>%
 select(FiscalYear,
        DMMETHOD,
        DMEPISIO) %>%
 mutate(
  med = case_when(
   DMEPISIO %in% "ML" ~ 1,
   TRUE ~ NA_integer_
  ),
  assvg = case_when(
   toupper(DMMETHOD) %in% c("VAC", "LOW", "MID") ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, med, assvg) %>%
 mutate(
  med_count = n()
 ) %>%
 group_by(FiscalYear, assvg) %>%
 mutate(
  total_year = n(),
  med_rate = med_count/total_year
 ) %>%
 ungroup() %>%
 filter(med %in% 1, assvg %in% 1) %>%
 select(FiscalYear,med,med_count,med_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  med_delta = (med_rate - lag(med_rate))/lag(med_rate),
  med_deltap = case_when(
   !is.na(lag(med_rate, 9)) ~ (med_rate - lag(med_rate, 9)) / lag(med_rate, 9),
   !is.na(lag(med_rate, 8)) ~ (med_rate - lag(med_rate, 8)) / lag(med_rate, 8),
   !is.na(lag(med_rate, 7)) ~ (med_rate - lag(med_rate, 7)) / lag(med_rate, 7),
   !is.na(lag(med_rate, 6)) ~ (med_rate - lag(med_rate, 6)) / lag(med_rate, 6),
   !is.na(lag(med_rate, 5)) ~ (med_rate - lag(med_rate, 5)) / lag(med_rate, 5),
   !is.na(lag(med_rate, 4)) ~ (med_rate - lag(med_rate, 4)) / lag(med_rate, 4),
   !is.na(lag(med_rate, 3)) ~ (med_rate - lag(med_rate, 3)) / lag(med_rate, 3),
   !is.na(lag(med_rate, 2)) ~ (med_rate - lag(med_rate, 2)) / lag(med_rate, 2),
   !is.na(lag(med_rate, 1)) ~ (med_rate - lag(med_rate, 1)) / lag(med_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Severe perineal trauma with Operative Vaginal Birth and mediolateral episiotomy ----

c10 <- dta %>%
 select(FiscalYear,
        ## 3rd degree laceration
        MO702,# Third degree perineal laceration during delivery
        MO70201,# Third degree perineal laceration del/delivery
        MO70204,# Third degree perineal laceration del/postpartum
        MO70211,# Third degree perineal laceration delivery type 3a/delivery
        MO70214,# Third degree perineal laceration delivery type 3a/postpartum
        MO70221,# Third degree perineal laceration delivery type 3b/delivery
        MO70231,# Third degree perineal laceration delivery type 3c/delivery
        MO70234,# Third degree perineal laceration delivery type 3c/postpartum
        MO70281,# Third degree perineal laceration delivery other type/delivery
        MO70291,# Third degree perineal laceration delivery type unspecified/delivery
        ## 4th degree laceration
        MO703,# Fourth degree perineal laceration during delivery
        MO70301,# Fourth degree perineal laceration del/delivery
        MO70304,# Fourth degree perineal laceration del/postpart
        DMMETHOD,
        DMEPISIO) %>%
 mutate(
  trd = case_when(
   MO702 > 0 | MO70201 > 0 | MO70204 > 0 |
    MO70211 > 0 | MO70214 > 0 | MO70221 > 0 |
    MO70231 > 0 | MO70234 > 0 | MO70281 > 0 | MO70291 > 0 ~ 1,
   TRUE ~ NA_integer_
  ),
  frth = case_when(
   MO703 > 0 | MO70301 > 0 | MO70304 > 0 ~ 1,
   TRUE ~ NA_integer_
  ),
  assvg = case_when(
   toupper(DMMETHOD) %in% c("VAC", "LOW", "MID") ~ 1,
   TRUE ~ NA_integer_
  ),
  trdfrth = case_when(
   trd > 0 | frth > 0 ~ 1,
   TRUE ~ NA_integer_
  ),
  med = case_when(
   DMEPISIO %in% "ML" ~ 1,
   TRUE ~ NA_integer_
  ),
  sptassvgmed = case_when(
   trdfrth > 0 & med > 0 & assvg > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, trdfrth, assvg, med) %>%
 mutate(
  sptassvgmed_count = n()
 ) %>%
 group_by(FiscalYear, assvg) %>%
 mutate(
  total_year = n(),
  sptassvgmed_rate = sptassvgmed_count/total_year
 ) %>%
 ungroup() %>%
 filter(sptassvgmed %in% 1) %>%
 select(FiscalYear,sptassvgmed,sptassvgmed_count,sptassvgmed_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  sptassvgmed_delta = (sptassvgmed_rate - lag(sptassvgmed_rate))/lag(sptassvgmed_rate),
  sptassvgmed_deltap = case_when(
   !is.na(lag(sptassvgmed_rate, 9)) ~ (sptassvgmed_rate - lag(sptassvgmed_rate, 9)) / lag(sptassvgmed_rate, 9),
   !is.na(lag(sptassvgmed_rate, 8)) ~ (sptassvgmed_rate - lag(sptassvgmed_rate, 8)) / lag(sptassvgmed_rate, 8),
   !is.na(lag(sptassvgmed_rate, 7)) ~ (sptassvgmed_rate - lag(sptassvgmed_rate, 7)) / lag(sptassvgmed_rate, 7),
   !is.na(lag(sptassvgmed_rate, 6)) ~ (sptassvgmed_rate - lag(sptassvgmed_rate, 6)) / lag(sptassvgmed_rate, 6),
   !is.na(lag(sptassvgmed_rate, 5)) ~ (sptassvgmed_rate - lag(sptassvgmed_rate, 5)) / lag(sptassvgmed_rate, 5),
   !is.na(lag(sptassvgmed_rate, 4)) ~ (sptassvgmed_rate - lag(sptassvgmed_rate, 4)) / lag(sptassvgmed_rate, 4),
   !is.na(lag(sptassvgmed_rate, 3)) ~ (sptassvgmed_rate - lag(sptassvgmed_rate, 3)) / lag(sptassvgmed_rate, 3),
   !is.na(lag(sptassvgmed_rate, 2)) ~ (sptassvgmed_rate - lag(sptassvgmed_rate, 2)) / lag(sptassvgmed_rate, 2),
   !is.na(lag(sptassvgmed_rate, 1)) ~ (sptassvgmed_rate - lag(sptassvgmed_rate, 1)) / lag(sptassvgmed_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Severe perineal trauma with Operative Vaginal Birth without mediolateral episiotomy ----

c11 <- dta %>%
 select(FiscalYear,
        ## 3rd degree laceration
        MO702,# Third degree perineal laceration during delivery
        MO70201,# Third degree perineal laceration del/delivery
        MO70204,# Third degree perineal laceration del/postpartum
        MO70211,# Third degree perineal laceration delivery type 3a/delivery
        MO70214,# Third degree perineal laceration delivery type 3a/postpartum
        MO70221,# Third degree perineal laceration delivery type 3b/delivery
        MO70231,# Third degree perineal laceration delivery type 3c/delivery
        MO70234,# Third degree perineal laceration delivery type 3c/postpartum
        MO70281,# Third degree perineal laceration delivery other type/delivery
        MO70291,# Third degree perineal laceration delivery type unspecified/delivery
        ## 4th degree laceration
        MO703,# Fourth degree perineal laceration during delivery
        MO70301,# Fourth degree perineal laceration del/delivery
        MO70304,# Fourth degree perineal laceration del/postpart
        DMMETHOD,
        DMEPISIO) %>%
 mutate(
  trd = case_when(
   MO702 > 0 | MO70201 > 0 | MO70204 > 0 |
    MO70211 > 0 | MO70214 > 0 | MO70221 > 0 |
    MO70231 > 0 | MO70234 > 0 | MO70281 > 0 | MO70291 > 0 ~ 1,
   TRUE ~ NA_integer_
  ),
  frth = case_when(
   MO703 > 0 | MO70301 > 0 | MO70304 > 0 ~ 1,
   TRUE ~ NA_integer_
  ),
  assvg = case_when(
   toupper(DMMETHOD) %in% c("VAC", "LOW", "MID") ~ 1,
   TRUE ~ NA_integer_
  ),
  trdfrth = case_when(
   trd > 0 | frth > 0 ~ 1,
   TRUE ~ NA_integer_
  ),
  med = case_when(
   !DMEPISIO %in% "ML" ~ 1,
   TRUE ~ NA_integer_
  ),
  sptassvgnmed = case_when(
   trdfrth > 0 & med > 0 & assvg > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, trdfrth, assvg, med) %>%
 mutate(
  sptassvgnmed_count = n()
 ) %>%
 group_by(FiscalYear, assvg) %>%
 mutate(
  total_year = n(),
  sptassvgnmed_rate = sptassvgnmed_count/total_year
 ) %>%
 ungroup() %>%
 filter(sptassvgnmed %in% 1) %>%
 select(FiscalYear,sptassvgnmed,sptassvgnmed_count,sptassvgnmed_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  sptassvgnmed_delta = (sptassvgnmed_rate - lag(sptassvgnmed_rate))/lag(sptassvgnmed_rate),
  sptassvgnmed_deltap = case_when(
   !is.na(lag(sptassvgnmed_rate, 9)) ~ (sptassvgnmed_rate - lag(sptassvgnmed_rate, 9)) / lag(sptassvgnmed_rate, 9),
   !is.na(lag(sptassvgnmed_rate, 8)) ~ (sptassvgnmed_rate - lag(sptassvgnmed_rate, 8)) / lag(sptassvgnmed_rate, 8),
   !is.na(lag(sptassvgnmed_rate, 7)) ~ (sptassvgnmed_rate - lag(sptassvgnmed_rate, 7)) / lag(sptassvgnmed_rate, 7),
   !is.na(lag(sptassvgnmed_rate, 6)) ~ (sptassvgnmed_rate - lag(sptassvgnmed_rate, 6)) / lag(sptassvgnmed_rate, 6),
   !is.na(lag(sptassvgnmed_rate, 5)) ~ (sptassvgnmed_rate - lag(sptassvgnmed_rate, 5)) / lag(sptassvgnmed_rate, 5),
   !is.na(lag(sptassvgnmed_rate, 4)) ~ (sptassvgnmed_rate - lag(sptassvgnmed_rate, 4)) / lag(sptassvgnmed_rate, 4),
   !is.na(lag(sptassvgnmed_rate, 3)) ~ (sptassvgnmed_rate - lag(sptassvgnmed_rate, 3)) / lag(sptassvgnmed_rate, 3),
   !is.na(lag(sptassvgnmed_rate, 2)) ~ (sptassvgnmed_rate - lag(sptassvgnmed_rate, 2)) / lag(sptassvgnmed_rate, 2),
   !is.na(lag(sptassvgnmed_rate, 1)) ~ (sptassvgnmed_rate - lag(sptassvgnmed_rate, 1)) / lag(sptassvgnmed_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Robson group ----

c12 <- dta %>%
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
  rbs1_deltap = case_when(
   !is.na(lag(rbs1_rate, 9)) ~ (rbs1_rate - lag(rbs1_rate, 9)) / lag(rbs1_rate, 9),
   !is.na(lag(rbs1_rate, 8)) ~ (rbs1_rate - lag(rbs1_rate, 8)) / lag(rbs1_rate, 8),
   !is.na(lag(rbs1_rate, 7)) ~ (rbs1_rate - lag(rbs1_rate, 7)) / lag(rbs1_rate, 7),
   !is.na(lag(rbs1_rate, 6)) ~ (rbs1_rate - lag(rbs1_rate, 6)) / lag(rbs1_rate, 6),
   !is.na(lag(rbs1_rate, 5)) ~ (rbs1_rate - lag(rbs1_rate, 5)) / lag(rbs1_rate, 5),
   !is.na(lag(rbs1_rate, 4)) ~ (rbs1_rate - lag(rbs1_rate, 4)) / lag(rbs1_rate, 4),
   !is.na(lag(rbs1_rate, 3)) ~ (rbs1_rate - lag(rbs1_rate, 3)) / lag(rbs1_rate, 3),
   !is.na(lag(rbs1_rate, 2)) ~ (rbs1_rate - lag(rbs1_rate, 2)) / lag(rbs1_rate, 2),
   !is.na(lag(rbs1_rate, 1)) ~ (rbs1_rate - lag(rbs1_rate, 1)) / lag(rbs1_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

c13 <- dta %>%
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
  rbs21_deltap = case_when(
   !is.na(lag(rbs21_rate, 9)) ~ (rbs21_rate - lag(rbs21_rate, 9)) / lag(rbs21_rate, 9),
   !is.na(lag(rbs21_rate, 8)) ~ (rbs21_rate - lag(rbs21_rate, 8)) / lag(rbs21_rate, 8),
   !is.na(lag(rbs21_rate, 7)) ~ (rbs21_rate - lag(rbs21_rate, 7)) / lag(rbs21_rate, 7),
   !is.na(lag(rbs21_rate, 6)) ~ (rbs21_rate - lag(rbs21_rate, 6)) / lag(rbs21_rate, 6),
   !is.na(lag(rbs21_rate, 5)) ~ (rbs21_rate - lag(rbs21_rate, 5)) / lag(rbs21_rate, 5),
   !is.na(lag(rbs21_rate, 4)) ~ (rbs21_rate - lag(rbs21_rate, 4)) / lag(rbs21_rate, 4),
   !is.na(lag(rbs21_rate, 3)) ~ (rbs21_rate - lag(rbs21_rate, 3)) / lag(rbs21_rate, 3),
   !is.na(lag(rbs21_rate, 2)) ~ (rbs21_rate - lag(rbs21_rate, 2)) / lag(rbs21_rate, 2),
   !is.na(lag(rbs21_rate, 1)) ~ (rbs21_rate - lag(rbs21_rate, 1)) / lag(rbs21_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

c14 <- dta %>%
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
  rbs51_deltap = case_when(
   !is.na(lag(rbs51_rate, 9)) ~ (rbs51_rate - lag(rbs51_rate, 9)) / lag(rbs51_rate, 9),
   !is.na(lag(rbs51_rate, 8)) ~ (rbs51_rate - lag(rbs51_rate, 8)) / lag(rbs51_rate, 8),
   !is.na(lag(rbs51_rate, 7)) ~ (rbs51_rate - lag(rbs51_rate, 7)) / lag(rbs51_rate, 7),
   !is.na(lag(rbs51_rate, 6)) ~ (rbs51_rate - lag(rbs51_rate, 6)) / lag(rbs51_rate, 6),
   !is.na(lag(rbs51_rate, 5)) ~ (rbs51_rate - lag(rbs51_rate, 5)) / lag(rbs51_rate, 5),
   !is.na(lag(rbs51_rate, 4)) ~ (rbs51_rate - lag(rbs51_rate, 4)) / lag(rbs51_rate, 4),
   !is.na(lag(rbs51_rate, 3)) ~ (rbs51_rate - lag(rbs51_rate, 3)) / lag(rbs51_rate, 3),
   !is.na(lag(rbs51_rate, 2)) ~ (rbs51_rate - lag(rbs51_rate, 2)) / lag(rbs51_rate, 2),
   !is.na(lag(rbs51_rate, 1)) ~ (rbs51_rate - lag(rbs51_rate, 1)) / lag(rbs51_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Postpartum hemorrhage treated with a blood transfusion ----

c15 <- dta %>%
 select(FiscalYear, Any_Blood_Product, Postpartum_Haemorrhage) %>%
 mutate(
  pphbl = case_when(
   Any_Blood_Product > 0 & Postpartum_Haemorrhage > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, Any_Blood_Product, Postpartum_Haemorrhage) %>%
 mutate(
  pphbl_count = n()
 ) %>%
 group_by(FiscalYear) %>%
 mutate(
  total_year = n(),
  pphbl_rate = pphbl_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Blood_Product %in% 1, Postpartum_Haemorrhage %in% 1) %>%
 select(FiscalYear,pphbl, pphbl_count, pphbl_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  pphbl_delta = (pphbl_rate - lag(pphbl_rate))/lag(pphbl_rate),
  pphbl_deltap = case_when(
   !is.na(lag(pphbl_rate, 9)) ~ (pphbl_rate - lag(pphbl_rate, 9)) / lag(pphbl_rate, 9),
   !is.na(lag(pphbl_rate, 8)) ~ (pphbl_rate - lag(pphbl_rate, 8)) / lag(pphbl_rate, 8),
   !is.na(lag(pphbl_rate, 7)) ~ (pphbl_rate - lag(pphbl_rate, 7)) / lag(pphbl_rate, 7),
   !is.na(lag(pphbl_rate, 6)) ~ (pphbl_rate - lag(pphbl_rate, 6)) / lag(pphbl_rate, 6),
   !is.na(lag(pphbl_rate, 5)) ~ (pphbl_rate - lag(pphbl_rate, 5)) / lag(pphbl_rate, 5),
   !is.na(lag(pphbl_rate, 4)) ~ (pphbl_rate - lag(pphbl_rate, 4)) / lag(pphbl_rate, 4),
   !is.na(lag(pphbl_rate, 3)) ~ (pphbl_rate - lag(pphbl_rate, 3)) / lag(pphbl_rate, 3),
   !is.na(lag(pphbl_rate, 2)) ~ (pphbl_rate - lag(pphbl_rate, 2)) / lag(pphbl_rate, 2),
   !is.na(lag(pphbl_rate, 1)) ~ (pphbl_rate - lag(pphbl_rate, 1)) / lag(pphbl_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Postpartum hemorrhage resulting in procedural intervention ----

c16 <- dta %>%
 select(FiscalYear, Postpartum_Haemorrhage,
        #### Procedural intervention
        R029,# Procedures for postpartum hemorrhage
        R029_00100,# Uterine compression suture (eg.B-Lynch or Cho)
        R029_00200,# Tying (ligation) of uterine arteries
        R029_00300,# Embolization of uterine arteries
        R029_00400,# Uterine tamponade (use of Bakri balloon or uterine packing, not to be confused with vaginal packing
        #### Hysterectomy
        M_5MD60RC,# Cesarean hysterectomy with forceps
        M_5MD60RD,# Cesarean hysterectomy with vacuum
        M_5MD60KE,# Cesarean hysterectomy
        M_5MD60CB,# Cesarean hysterectomy with vacuum and forceps
        M_1RM89,# Excise tot uterus
        MODEDEL) %>%
 mutate(
  # Procedural intervention
  prcint = case_when(
   R029 > 0 | R029_00100 > 0 |
    R029_00200 > 0 | R029_00300 > 0 |
    R029_00400 > 0 ~ 1,
   TRUE ~ NA_integer_
  ),
  # Hysterectomy
  hyst = case_when(
   M_5MD60RC > 0 | M_5MD60RD > 0 |
    M_5MD60KE > 0 | M_5MD60CB > 0 |
    M_1RM89 > 0 | toupper(MODEDEL) %in% "CSH" ~ 1,
   TRUE ~ NA_integer_
  ),
  pphpi = case_when(
   prcint > 0 | hyst > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, pphpi, Postpartum_Haemorrhage) %>%
 mutate(
  pphpi_count = n()
 ) %>%
 group_by(FiscalYear, Postpartum_Haemorrhage) %>%
 mutate(
  total_year = n(),
  pphpi_rate = pphpi_count/total_year
 ) %>%
 ungroup() %>%
 filter(pphpi %in% 1, Postpartum_Haemorrhage %in% 1) %>%
 select(FiscalYear,pphpi, pphpi_count, pphpi_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 tidyr::complete(FiscalYear = levels(dta$FiscalYear),
                 tidyr::nesting(pphpi),
                 fill = list(
                  pphpi_count = 0,
                  pphpi_rate = NA
                 )) %>%
 mutate(
  pphpi_delta = (pphpi_rate - lag(pphpi_rate))/lag(pphpi_rate),
  pphpi_deltap = case_when(
   !is.na(lag(pphpi_rate, 9)) ~ (pphpi_rate - lag(pphpi_rate, 9)) / lag(pphpi_rate, 9),
   !is.na(lag(pphpi_rate, 8)) ~ (pphpi_rate - lag(pphpi_rate, 8)) / lag(pphpi_rate, 8),
   !is.na(lag(pphpi_rate, 7)) ~ (pphpi_rate - lag(pphpi_rate, 7)) / lag(pphpi_rate, 7),
   !is.na(lag(pphpi_rate, 6)) ~ (pphpi_rate - lag(pphpi_rate, 6)) / lag(pphpi_rate, 6),
   !is.na(lag(pphpi_rate, 5)) ~ (pphpi_rate - lag(pphpi_rate, 5)) / lag(pphpi_rate, 5),
   !is.na(lag(pphpi_rate, 4)) ~ (pphpi_rate - lag(pphpi_rate, 4)) / lag(pphpi_rate, 4),
   !is.na(lag(pphpi_rate, 3)) ~ (pphpi_rate - lag(pphpi_rate, 3)) / lag(pphpi_rate, 3),
   !is.na(lag(pphpi_rate, 2)) ~ (pphpi_rate - lag(pphpi_rate, 2)) / lag(pphpi_rate, 2),
   !is.na(lag(pphpi_rate, 1)) ~ (pphpi_rate - lag(pphpi_rate, 1)) / lag(pphpi_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Postpartum readmission rate ----

c17 <- dta %>%
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
  ppreadm_deltap = case_when(
   !is.na(lag(ppreadm_rate, 9)) ~ (ppreadm_rate - lag(ppreadm_rate, 9)) / lag(ppreadm_rate, 9),
   !is.na(lag(ppreadm_rate, 8)) ~ (ppreadm_rate - lag(ppreadm_rate, 8)) / lag(ppreadm_rate, 8),
   !is.na(lag(ppreadm_rate, 7)) ~ (ppreadm_rate - lag(ppreadm_rate, 7)) / lag(ppreadm_rate, 7),
   !is.na(lag(ppreadm_rate, 6)) ~ (ppreadm_rate - lag(ppreadm_rate, 6)) / lag(ppreadm_rate, 6),
   !is.na(lag(ppreadm_rate, 5)) ~ (ppreadm_rate - lag(ppreadm_rate, 5)) / lag(ppreadm_rate, 5),
   !is.na(lag(ppreadm_rate, 4)) ~ (ppreadm_rate - lag(ppreadm_rate, 4)) / lag(ppreadm_rate, 4),
   !is.na(lag(ppreadm_rate, 3)) ~ (ppreadm_rate - lag(ppreadm_rate, 3)) / lag(ppreadm_rate, 3),
   !is.na(lag(ppreadm_rate, 2)) ~ (ppreadm_rate - lag(ppreadm_rate, 2)) / lag(ppreadm_rate, 2),
   !is.na(lag(ppreadm_rate, 1)) ~ (ppreadm_rate - lag(ppreadm_rate, 1)) / lag(ppreadm_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Postpartum patients who received medical anticoagulation prophylaxis when indicated ----

c18

### Skin to skin ----

c19 <- dta %>%
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
 filter(sknskn %in% 1, lvb %in% 1) %>%
 select(FiscalYear, sknskn, sknskn_count,sknskn_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  sknskn_delta = (sknskn_rate - lag(sknskn_rate))/lag(sknskn_rate),
  sknskn_deltap = case_when(
   !is.na(lag(sknskn_rate, 9)) ~ (sknskn_rate - lag(sknskn_rate, 9)) / lag(sknskn_rate, 9),
   !is.na(lag(sknskn_rate, 8)) ~ (sknskn_rate - lag(sknskn_rate, 8)) / lag(sknskn_rate, 8),
   !is.na(lag(sknskn_rate, 7)) ~ (sknskn_rate - lag(sknskn_rate, 7)) / lag(sknskn_rate, 7),
   !is.na(lag(sknskn_rate, 6)) ~ (sknskn_rate - lag(sknskn_rate, 6)) / lag(sknskn_rate, 6),
   !is.na(lag(sknskn_rate, 5)) ~ (sknskn_rate - lag(sknskn_rate, 5)) / lag(sknskn_rate, 5),
   !is.na(lag(sknskn_rate, 4)) ~ (sknskn_rate - lag(sknskn_rate, 4)) / lag(sknskn_rate, 4),
   !is.na(lag(sknskn_rate, 3)) ~ (sknskn_rate - lag(sknskn_rate, 3)) / lag(sknskn_rate, 3),
   !is.na(lag(sknskn_rate, 2)) ~ (sknskn_rate - lag(sknskn_rate, 2)) / lag(sknskn_rate, 2),
   !is.na(lag(sknskn_rate, 1)) ~ (sknskn_rate - lag(sknskn_rate, 1)) / lag(sknskn_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

c20 <- dta %>%
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
 filter(sknsknvg %in% 1, lvb %in% 1) %>%
 select(FiscalYear, sknsknvg, sknsknvg_count,sknsknvg_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  sknsknvg_delta = (sknsknvg_rate - lag(sknsknvg_rate))/lag(sknsknvg_rate),
  sknsknvg_deltap = case_when(
   !is.na(lag(sknsknvg_rate, 9)) ~ (sknsknvg_rate - lag(sknsknvg_rate, 9)) / lag(sknsknvg_rate, 9),
   !is.na(lag(sknsknvg_rate, 8)) ~ (sknsknvg_rate - lag(sknsknvg_rate, 8)) / lag(sknsknvg_rate, 8),
   !is.na(lag(sknsknvg_rate, 7)) ~ (sknsknvg_rate - lag(sknsknvg_rate, 7)) / lag(sknsknvg_rate, 7),
   !is.na(lag(sknsknvg_rate, 6)) ~ (sknsknvg_rate - lag(sknsknvg_rate, 6)) / lag(sknsknvg_rate, 6),
   !is.na(lag(sknsknvg_rate, 5)) ~ (sknsknvg_rate - lag(sknsknvg_rate, 5)) / lag(sknsknvg_rate, 5),
   !is.na(lag(sknsknvg_rate, 4)) ~ (sknsknvg_rate - lag(sknsknvg_rate, 4)) / lag(sknsknvg_rate, 4),
   !is.na(lag(sknsknvg_rate, 3)) ~ (sknsknvg_rate - lag(sknsknvg_rate, 3)) / lag(sknsknvg_rate, 3),
   !is.na(lag(sknsknvg_rate, 2)) ~ (sknsknvg_rate - lag(sknsknvg_rate, 2)) / lag(sknsknvg_rate, 2),
   !is.na(lag(sknsknvg_rate, 1)) ~ (sknsknvg_rate - lag(sknsknvg_rate, 1)) / lag(sknsknvg_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

c21 <- dta %>%
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
 filter(sknskncs %in% 1, lvb %in% 1) %>%
 select(FiscalYear, sknskncs, sknskncs_count,sknskncs_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  sknskncs_delta = (sknskncs_rate - lag(sknskncs_rate))/lag(sknskncs_rate),
  sknskncs_deltap = case_when(
   !is.na(lag(sknskncs_rate, 9)) ~ (sknskncs_rate - lag(sknskncs_rate, 9)) / lag(sknskncs_rate, 9),
   !is.na(lag(sknskncs_rate, 8)) ~ (sknskncs_rate - lag(sknskncs_rate, 8)) / lag(sknskncs_rate, 8),
   !is.na(lag(sknskncs_rate, 7)) ~ (sknskncs_rate - lag(sknskncs_rate, 7)) / lag(sknskncs_rate, 7),
   !is.na(lag(sknskncs_rate, 6)) ~ (sknskncs_rate - lag(sknskncs_rate, 6)) / lag(sknskncs_rate, 6),
   !is.na(lag(sknskncs_rate, 5)) ~ (sknskncs_rate - lag(sknskncs_rate, 5)) / lag(sknskncs_rate, 5),
   !is.na(lag(sknskncs_rate, 4)) ~ (sknskncs_rate - lag(sknskncs_rate, 4)) / lag(sknskncs_rate, 4),
   !is.na(lag(sknskncs_rate, 3)) ~ (sknskncs_rate - lag(sknskncs_rate, 3)) / lag(sknskncs_rate, 3),
   !is.na(lag(sknskncs_rate, 2)) ~ (sknskncs_rate - lag(sknskncs_rate, 2)) / lag(sknskncs_rate, 2),
   !is.na(lag(sknskncs_rate, 1)) ~ (sknskncs_rate - lag(sknskncs_rate, 1)) / lag(sknskncs_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Postpartum blood products amongst those who received antenatal iron therapy ----

c22 <- dta %>%
 mutate(
  pphiv = case_when(
   Any_Blood_Product > 0 & R003_02100 > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, Any_Blood_Product, R003_02100) %>%
 mutate(
  pphiv_count = n()
 ) %>%
 group_by(FiscalYear, Any_Blood_Product) %>%
 mutate(
  total_year = n(),
  pphiv_rate = pphiv_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Blood_Product %in% 1, R003_02100 %in% 1) %>%
 select(FiscalYear, pphiv, pphiv_count, pphiv_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 tidyr::complete(FiscalYear = levels(dta$FiscalYear),
                 tidyr::nesting(pphiv),
                 fill = list(
                  pphiv_count = 0,
                  pphiv_rate = NA
                 )) %>%
 mutate(
  pphiv_delta = (pphiv_rate - lag(pphiv_rate))/lag(pphiv_rate),
  pphiv_deltap = case_when(
   !is.na(lag(pphiv_rate, 9)) ~ (pphiv_rate - lag(pphiv_rate, 9)) / lag(pphiv_rate, 9),
   !is.na(lag(pphiv_rate, 8)) ~ (pphiv_rate - lag(pphiv_rate, 8)) / lag(pphiv_rate, 8),
   !is.na(lag(pphiv_rate, 7)) ~ (pphiv_rate - lag(pphiv_rate, 7)) / lag(pphiv_rate, 7),
   !is.na(lag(pphiv_rate, 6)) ~ (pphiv_rate - lag(pphiv_rate, 6)) / lag(pphiv_rate, 6),
   !is.na(lag(pphiv_rate, 5)) ~ (pphiv_rate - lag(pphiv_rate, 5)) / lag(pphiv_rate, 5),
   !is.na(lag(pphiv_rate, 4)) ~ (pphiv_rate - lag(pphiv_rate, 4)) / lag(pphiv_rate, 4),
   !is.na(lag(pphiv_rate, 3)) ~ (pphiv_rate - lag(pphiv_rate, 3)) / lag(pphiv_rate, 3),
   !is.na(lag(pphiv_rate, 2)) ~ (pphiv_rate - lag(pphiv_rate, 2)) / lag(pphiv_rate, 2),
   !is.na(lag(pphiv_rate, 1)) ~ (pphiv_rate - lag(pphiv_rate, 1)) / lag(pphiv_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Neonatal readmission rate ----

c23 <- dta %>%
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
 filter(neoreadm %in% 1, lvb %in% 1) %>%
 select(FiscalYear, neoreadm, neoreadm_count,neoreadm_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  neoreadm_delta = (neoreadm_rate - lag(neoreadm_rate))/lag(neoreadm_rate),
  neoreadm_deltap = case_when(
   !is.na(lag(neoreadm_rate, 9)) ~ (neoreadm_rate - lag(neoreadm_rate, 9)) / lag(neoreadm_rate, 9),
   !is.na(lag(neoreadm_rate, 8)) ~ (neoreadm_rate - lag(neoreadm_rate, 8)) / lag(neoreadm_rate, 8),
   !is.na(lag(neoreadm_rate, 7)) ~ (neoreadm_rate - lag(neoreadm_rate, 7)) / lag(neoreadm_rate, 7),
   !is.na(lag(neoreadm_rate, 6)) ~ (neoreadm_rate - lag(neoreadm_rate, 6)) / lag(neoreadm_rate, 6),
   !is.na(lag(neoreadm_rate, 5)) ~ (neoreadm_rate - lag(neoreadm_rate, 5)) / lag(neoreadm_rate, 5),
   !is.na(lag(neoreadm_rate, 4)) ~ (neoreadm_rate - lag(neoreadm_rate, 4)) / lag(neoreadm_rate, 4),
   !is.na(lag(neoreadm_rate, 3)) ~ (neoreadm_rate - lag(neoreadm_rate, 3)) / lag(neoreadm_rate, 3),
   !is.na(lag(neoreadm_rate, 2)) ~ (neoreadm_rate - lag(neoreadm_rate, 2)) / lag(neoreadm_rate, 2),
   !is.na(lag(neoreadm_rate, 1)) ~ (neoreadm_rate - lag(neoreadm_rate, 1)) / lag(neoreadm_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Preterm infants who received complete course of steroids > 24h and < 7 days prior to birth ----

c24 <- dta %>%
 mutate(
  pretster = case_when(
   (GA_BEST < 35 & GA_BEST >=24) &
    (R068_00200 > 0 | R068_00300 > 0 | R068_00700 > 0 |
      R068_00800 > 0 | R068_01200 > 0 | R068_01300 > 0 ) ~ 1,
   TRUE ~ NA_integer_
  ),
  den = case_when(
   (GA_BEST < 35 & GA_BEST >=24) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, pretster) %>%
 mutate(
  pretster_count = n()
 ) %>%
 group_by(FiscalYear, den) %>%
 mutate(
  total_year = n(),
  pretster_rate = pretster_count/total_year
 ) %>%
 ungroup() %>%
 filter(pretster %in% 1) %>%
 select(FiscalYear, pretster, pretster_count,pretster_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 tidyr::complete(FiscalYear = levels(dta$FiscalYear),
                 tidyr::nesting(pretster),
                 fill = list(
                  pretster_count = 0,
                  pretster_rate = NA
                 )) %>%
 mutate(
  pretster_delta = (pretster_rate - lag(pretster_rate))/lag(pretster_rate),
  pretster_deltap = case_when(
   !is.na(lag(pretster_rate, 9)) ~ (pretster_rate - lag(pretster_rate, 9)) / lag(pretster_rate, 9),
   !is.na(lag(pretster_rate, 8)) ~ (pretster_rate - lag(pretster_rate, 8)) / lag(pretster_rate, 8),
   !is.na(lag(pretster_rate, 7)) ~ (pretster_rate - lag(pretster_rate, 7)) / lag(pretster_rate, 7),
   !is.na(lag(pretster_rate, 6)) ~ (pretster_rate - lag(pretster_rate, 6)) / lag(pretster_rate, 6),
   !is.na(lag(pretster_rate, 5)) ~ (pretster_rate - lag(pretster_rate, 5)) / lag(pretster_rate, 5),
   !is.na(lag(pretster_rate, 4)) ~ (pretster_rate - lag(pretster_rate, 4)) / lag(pretster_rate, 4),
   !is.na(lag(pretster_rate, 3)) ~ (pretster_rate - lag(pretster_rate, 3)) / lag(pretster_rate, 3),
   !is.na(lag(pretster_rate, 2)) ~ (pretster_rate - lag(pretster_rate, 2)) / lag(pretster_rate, 2),
   !is.na(lag(pretster_rate, 1)) ~ (pretster_rate - lag(pretster_rate, 1)) / lag(pretster_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Preterm babies born in facility without NICU ----

c25 <- dta %>%
 mutate(
  pretnnicu = case_when(
   (GA_BEST < 34) &
    (!DLHosp %in% c(86,87,-12)) ~ 1,
   TRUE ~ NA_integer_
  ),
  den = case_when(
   (GA_BEST < 34) &
    (!DLHosp %in% c(-12)) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, pretnnicu) %>%
 mutate(
  pretnnicu_count = n()
 ) %>%
 group_by(FiscalYear, den) %>%
 mutate(
  total_year = n(),
  pretnnicu_rate = pretnnicu_count/total_year
 ) %>%
 ungroup() %>%
 filter(pretnnicu %in% 1) %>%
 select(FiscalYear, pretnnicu, pretnnicu_count,pretnnicu_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 tidyr::complete(FiscalYear = levels(dta$FiscalYear),
                 tidyr::nesting(pretnnicu),
                 fill = list(
                  pretnnicu_count = 0,
                  pretnnicu_rate = NA
                 )) %>%
 mutate(
  pretnnicu_delta = (pretnnicu_rate - lag(pretnnicu_rate))/lag(pretnnicu_rate),
  pretnnicu_deltap = case_when(
   !is.na(lag(pretnnicu_rate, 9)) ~ (pretnnicu_rate - lag(pretnnicu_rate, 9)) / lag(pretnnicu_rate, 9),
   !is.na(lag(pretnnicu_rate, 8)) ~ (pretnnicu_rate - lag(pretnnicu_rate, 8)) / lag(pretnnicu_rate, 8),
   !is.na(lag(pretnnicu_rate, 7)) ~ (pretnnicu_rate - lag(pretnnicu_rate, 7)) / lag(pretnnicu_rate, 7),
   !is.na(lag(pretnnicu_rate, 6)) ~ (pretnnicu_rate - lag(pretnnicu_rate, 6)) / lag(pretnnicu_rate, 6),
   !is.na(lag(pretnnicu_rate, 5)) ~ (pretnnicu_rate - lag(pretnnicu_rate, 5)) / lag(pretnnicu_rate, 5),
   !is.na(lag(pretnnicu_rate, 4)) ~ (pretnnicu_rate - lag(pretnnicu_rate, 4)) / lag(pretnnicu_rate, 4),
   !is.na(lag(pretnnicu_rate, 3)) ~ (pretnnicu_rate - lag(pretnnicu_rate, 3)) / lag(pretnnicu_rate, 3),
   !is.na(lag(pretnnicu_rate, 2)) ~ (pretnnicu_rate - lag(pretnnicu_rate, 2)) / lag(pretnnicu_rate, 2),
   !is.na(lag(pretnnicu_rate, 1)) ~ (pretnnicu_rate - lag(pretnnicu_rate, 1)) / lag(pretnnicu_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Newborn respiratory distress associated with low-risk repeat cesarean at term gestation (≥ 37 weeks < 39 weeks) ----

c26 <- dta %>%
 mutate(
  newlwcs = case_when(
   (GA_BEST < 39 & GA_BEST >= 37) &
    (DLPRVCS > 0) &
    (toupper(DMMETHOD) %in% "CST") &
    (!toupper(BTOUTCOM) %in% "FTD") &
    (R059_00200 > 0 | R059_00300 > 0 | R059_00400 > 0 | R059_00500 > 0 | R059_00600 > 0) ~ 1,
   TRUE ~ NA_integer_
  ),
  den = case_when(
   (GA_BEST < 39 & GA_BEST >= 37) &
    (DLPRVCS > 0) &
    (toupper(DMMETHOD) %in% "CST") &
    (!toupper(BTOUTCOM) %in% "FTD") ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, newlwcs) %>%
 mutate(
  newlwcs_count = n()
 ) %>%
 group_by(FiscalYear, den) %>%
 mutate(
  total_year = n(),
  newlwcs_rate = newlwcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(newlwcs %in% 1) %>%
 select(FiscalYear, newlwcs, newlwcs_count,newlwcs_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 tidyr::complete(FiscalYear = levels(dta$FiscalYear),
                 tidyr::nesting(newlwcs),
                 fill = list(
                  newlwcs_count = 0,
                  newlwcs_rate = NA
                 )) %>%
 mutate(
  newlwcs_delta = (newlwcs_rate - lag(newlwcs_rate))/lag(newlwcs_rate),
  newlwcs_deltap = case_when(
   !is.na(lag(newlwcs_rate, 9)) ~ (newlwcs_rate - lag(newlwcs_rate, 9)) / lag(newlwcs_rate, 9),
   !is.na(lag(newlwcs_rate, 8)) ~ (newlwcs_rate - lag(newlwcs_rate, 8)) / lag(newlwcs_rate, 8),
   !is.na(lag(newlwcs_rate, 7)) ~ (newlwcs_rate - lag(newlwcs_rate, 7)) / lag(newlwcs_rate, 7),
   !is.na(lag(newlwcs_rate, 6)) ~ (newlwcs_rate - lag(newlwcs_rate, 6)) / lag(newlwcs_rate, 6),
   !is.na(lag(newlwcs_rate, 5)) ~ (newlwcs_rate - lag(newlwcs_rate, 5)) / lag(newlwcs_rate, 5),
   !is.na(lag(newlwcs_rate, 4)) ~ (newlwcs_rate - lag(newlwcs_rate, 4)) / lag(newlwcs_rate, 4),
   !is.na(lag(newlwcs_rate, 3)) ~ (newlwcs_rate - lag(newlwcs_rate, 3)) / lag(newlwcs_rate, 3),
   !is.na(lag(newlwcs_rate, 2)) ~ (newlwcs_rate - lag(newlwcs_rate, 2)) / lag(newlwcs_rate, 2),
   !is.na(lag(newlwcs_rate, 1)) ~ (newlwcs_rate - lag(newlwcs_rate, 1)) / lag(newlwcs_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Delayed Cord Clamping  ----

c27 <- dta %>%
 mutate(
  delcord = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (DEL_CORD_CLAMP %in% 3) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, delcord) %>%
 mutate(
  delcord_count = n()
 ) %>%
 group_by(FiscalYear, lvb) %>%
 mutate(
  total_year = n(),
  delcord_rate = delcord_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcord %in% 1, lvb %in% 1) %>%
 select(FiscalYear, delcord, delcord_count,delcord_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 tidyr::complete(FiscalYear = levels(dta$FiscalYear),
                 tidyr::nesting(delcord),
                 fill = list(
                  delcord_count = 0,
                  delcord_rate = NA
                 )) %>%
 mutate(
  delcord_delta = (delcord_rate - lag(delcord_rate))/lag(delcord_rate),
  delcord_deltap = case_when(
   !is.na(lag(delcord_rate, 9)) ~ (delcord_rate - lag(delcord_rate, 9)) / lag(delcord_rate, 9),
   !is.na(lag(delcord_rate, 8)) ~ (delcord_rate - lag(delcord_rate, 8)) / lag(delcord_rate, 8),
   !is.na(lag(delcord_rate, 7)) ~ (delcord_rate - lag(delcord_rate, 7)) / lag(delcord_rate, 7),
   !is.na(lag(delcord_rate, 6)) ~ (delcord_rate - lag(delcord_rate, 6)) / lag(delcord_rate, 6),
   !is.na(lag(delcord_rate, 5)) ~ (delcord_rate - lag(delcord_rate, 5)) / lag(delcord_rate, 5),
   !is.na(lag(delcord_rate, 4)) ~ (delcord_rate - lag(delcord_rate, 4)) / lag(delcord_rate, 4),
   !is.na(lag(delcord_rate, 3)) ~ (delcord_rate - lag(delcord_rate, 3)) / lag(delcord_rate, 3),
   !is.na(lag(delcord_rate, 2)) ~ (delcord_rate - lag(delcord_rate, 2)) / lag(delcord_rate, 2),
   !is.na(lag(delcord_rate, 1)) ~ (delcord_rate - lag(delcord_rate, 1)) / lag(delcord_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Delayed Cord Clamping amongst Term Newborns ----

c28 <- dta %>%
 mutate(
  delcordterm = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (DEL_CORD_CLAMP %in% 3) &
    (GA_BEST >= 37) ~ 1,
   TRUE ~ NA_integer_
  ),
  den = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (GA_BEST >= 37) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, delcordterm) %>%
 mutate(
  delcordterm_count = n()
 ) %>%
 group_by(FiscalYear, den) %>%
 mutate(
  total_year = n(),
  delcordterm_rate = delcordterm_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordterm %in% 1) %>%
 select(FiscalYear, delcordterm, delcordterm_count,delcordterm_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 tidyr::complete(FiscalYear = levels(dta$FiscalYear),
                 tidyr::nesting(delcordterm),
                 fill = list(
                  delcordterm_count = 0,
                  delcordterm_rate = NA
                 )) %>%
 mutate(
  delcordterm_delta = (delcordterm_rate - lag(delcordterm_rate))/lag(delcordterm_rate),
  delcordterm_deltap = case_when(
   !is.na(lag(delcordterm_rate, 9)) ~ (delcordterm_rate - lag(delcordterm_rate, 9)) / lag(delcordterm_rate, 9),
   !is.na(lag(delcordterm_rate, 8)) ~ (delcordterm_rate - lag(delcordterm_rate, 8)) / lag(delcordterm_rate, 8),
   !is.na(lag(delcordterm_rate, 7)) ~ (delcordterm_rate - lag(delcordterm_rate, 7)) / lag(delcordterm_rate, 7),
   !is.na(lag(delcordterm_rate, 6)) ~ (delcordterm_rate - lag(delcordterm_rate, 6)) / lag(delcordterm_rate, 6),
   !is.na(lag(delcordterm_rate, 5)) ~ (delcordterm_rate - lag(delcordterm_rate, 5)) / lag(delcordterm_rate, 5),
   !is.na(lag(delcordterm_rate, 4)) ~ (delcordterm_rate - lag(delcordterm_rate, 4)) / lag(delcordterm_rate, 4),
   !is.na(lag(delcordterm_rate, 3)) ~ (delcordterm_rate - lag(delcordterm_rate, 3)) / lag(delcordterm_rate, 3),
   !is.na(lag(delcordterm_rate, 2)) ~ (delcordterm_rate - lag(delcordterm_rate, 2)) / lag(delcordterm_rate, 2),
   !is.na(lag(delcordterm_rate, 1)) ~ (delcordterm_rate - lag(delcordterm_rate, 1)) / lag(delcordterm_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Delayed Cord Clamping amongst Term Newborns following spontaneous vaginal birth ----

c29 <- dta %>%
 mutate(
  delcordtermvg = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (DEL_CORD_CLAMP %in% 3) &
    (GA_BEST >= 37) &
    (toupper(DMMETHOD) %in% c("SPT","BIV")) ~ 1,
   TRUE ~ NA_integer_
  ),
  den = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (GA_BEST >= 37) &
    (toupper(DMMETHOD) %in% c("SPT","BIV")) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, delcordtermvg) %>%
 mutate(
  delcordtermvg_count = n()
 ) %>%
 group_by(FiscalYear, den) %>%
 mutate(
  total_year = n(),
  delcordtermvg_rate = delcordtermvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordtermvg %in% 1) %>%
 select(FiscalYear, delcordtermvg, delcordtermvg_count,delcordtermvg_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 tidyr::complete(FiscalYear = levels(dta$FiscalYear),
                 tidyr::nesting(delcordtermvg),
                 fill = list(
                  delcordtermvg_count = 0,
                  delcordtermvg_rate = NA
                 )) %>%
 mutate(
  delcordtermvg_delta = (delcordtermvg_rate - lag(delcordtermvg_rate))/lag(delcordtermvg_rate),
  delcordtermvg_deltap = case_when(
   !is.na(lag(delcordtermvg_rate, 9)) ~ (delcordtermvg_rate - lag(delcordtermvg_rate, 9)) / lag(delcordtermvg_rate, 9),
   !is.na(lag(delcordtermvg_rate, 8)) ~ (delcordtermvg_rate - lag(delcordtermvg_rate, 8)) / lag(delcordtermvg_rate, 8),
   !is.na(lag(delcordtermvg_rate, 7)) ~ (delcordtermvg_rate - lag(delcordtermvg_rate, 7)) / lag(delcordtermvg_rate, 7),
   !is.na(lag(delcordtermvg_rate, 6)) ~ (delcordtermvg_rate - lag(delcordtermvg_rate, 6)) / lag(delcordtermvg_rate, 6),
   !is.na(lag(delcordtermvg_rate, 5)) ~ (delcordtermvg_rate - lag(delcordtermvg_rate, 5)) / lag(delcordtermvg_rate, 5),
   !is.na(lag(delcordtermvg_rate, 4)) ~ (delcordtermvg_rate - lag(delcordtermvg_rate, 4)) / lag(delcordtermvg_rate, 4),
   !is.na(lag(delcordtermvg_rate, 3)) ~ (delcordtermvg_rate - lag(delcordtermvg_rate, 3)) / lag(delcordtermvg_rate, 3),
   !is.na(lag(delcordtermvg_rate, 2)) ~ (delcordtermvg_rate - lag(delcordtermvg_rate, 2)) / lag(delcordtermvg_rate, 2),
   !is.na(lag(delcordtermvg_rate, 1)) ~ (delcordtermvg_rate - lag(delcordtermvg_rate, 1)) / lag(delcordtermvg_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Delayed Cord Clamping amongst Term Newborns following caesarean birth ----

c30 <- dta %>%
 mutate(
  delcordtermcs = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (DEL_CORD_CLAMP %in% 3) &
    (GA_BEST >= 37) &
    (toupper(DMMETHOD) %in% c("CST")) ~ 1,
   TRUE ~ NA_integer_
  ),
  den = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (GA_BEST >= 37) &
    (toupper(DMMETHOD) %in% c("CST")) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, delcordtermcs) %>%
 mutate(
  delcordtermcs_count = n()
 ) %>%
 group_by(FiscalYear, den) %>%
 mutate(
  total_year = n(),
  delcordtermcs_rate = delcordtermcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordtermcs %in% 1) %>%
 select(FiscalYear, delcordtermcs, delcordtermcs_count,delcordtermcs_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 tidyr::complete(FiscalYear = levels(dta$FiscalYear),
                 tidyr::nesting(delcordtermcs),
                 fill = list(
                  delcordtermcs_count = 0,
                  delcordtermcs_rate = NA
                 )) %>%
 mutate(
  delcordtermcs_delta = (delcordtermcs_rate - lag(delcordtermcs_rate))/lag(delcordtermcs_rate),
  delcordtermcs_deltap = case_when(
   !is.na(lag(delcordtermcs_rate, 9)) ~ (delcordtermcs_rate - lag(delcordtermcs_rate, 9)) / lag(delcordtermcs_rate, 9),
   !is.na(lag(delcordtermcs_rate, 8)) ~ (delcordtermcs_rate - lag(delcordtermcs_rate, 8)) / lag(delcordtermcs_rate, 8),
   !is.na(lag(delcordtermcs_rate, 7)) ~ (delcordtermcs_rate - lag(delcordtermcs_rate, 7)) / lag(delcordtermcs_rate, 7),
   !is.na(lag(delcordtermcs_rate, 6)) ~ (delcordtermcs_rate - lag(delcordtermcs_rate, 6)) / lag(delcordtermcs_rate, 6),
   !is.na(lag(delcordtermcs_rate, 5)) ~ (delcordtermcs_rate - lag(delcordtermcs_rate, 5)) / lag(delcordtermcs_rate, 5),
   !is.na(lag(delcordtermcs_rate, 4)) ~ (delcordtermcs_rate - lag(delcordtermcs_rate, 4)) / lag(delcordtermcs_rate, 4),
   !is.na(lag(delcordtermcs_rate, 3)) ~ (delcordtermcs_rate - lag(delcordtermcs_rate, 3)) / lag(delcordtermcs_rate, 3),
   !is.na(lag(delcordtermcs_rate, 2)) ~ (delcordtermcs_rate - lag(delcordtermcs_rate, 2)) / lag(delcordtermcs_rate, 2),
   !is.na(lag(delcordtermcs_rate, 1)) ~ (delcordtermcs_rate - lag(delcordtermcs_rate, 1)) / lag(delcordtermcs_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Delayed Cord Clamping amongst preterm Newborns ----

c31 <- dta %>%
 mutate(
  delcordpreterm = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (DEL_CORD_CLAMP %in% 3) &
    (GA_BEST < 37) ~ 1,
   TRUE ~ NA_integer_
  ),
  den = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (GA_BEST < 37) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, delcordpreterm) %>%
 mutate(
  delcordpreterm_count = n()
 ) %>%
 group_by(FiscalYear, den) %>%
 mutate(
  total_year = n(),
  delcordpreterm_rate = delcordpreterm_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpreterm %in% 1) %>%
 select(FiscalYear, delcordpreterm, delcordpreterm_count,delcordpreterm_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 tidyr::complete(FiscalYear = levels(dta$FiscalYear),
                 tidyr::nesting(delcordpreterm),
                 fill = list(
                  delcordpreterm_count = 0,
                  delcordpreterm_rate = NA
                 )) %>%
 mutate(
  delcordpreterm_delta = (delcordpreterm_rate - lag(delcordpreterm_rate))/lag(delcordpreterm_rate),
  delcordpreterm_deltap = case_when(
   !is.na(lag(delcordpreterm_rate, 9)) ~ (delcordpreterm_rate - lag(delcordpreterm_rate, 9)) / lag(delcordpreterm_rate, 9),
   !is.na(lag(delcordpreterm_rate, 8)) ~ (delcordpreterm_rate - lag(delcordpreterm_rate, 8)) / lag(delcordpreterm_rate, 8),
   !is.na(lag(delcordpreterm_rate, 7)) ~ (delcordpreterm_rate - lag(delcordpreterm_rate, 7)) / lag(delcordpreterm_rate, 7),
   !is.na(lag(delcordpreterm_rate, 6)) ~ (delcordpreterm_rate - lag(delcordpreterm_rate, 6)) / lag(delcordpreterm_rate, 6),
   !is.na(lag(delcordpreterm_rate, 5)) ~ (delcordpreterm_rate - lag(delcordpreterm_rate, 5)) / lag(delcordpreterm_rate, 5),
   !is.na(lag(delcordpreterm_rate, 4)) ~ (delcordpreterm_rate - lag(delcordpreterm_rate, 4)) / lag(delcordpreterm_rate, 4),
   !is.na(lag(delcordpreterm_rate, 3)) ~ (delcordpreterm_rate - lag(delcordpreterm_rate, 3)) / lag(delcordpreterm_rate, 3),
   !is.na(lag(delcordpreterm_rate, 2)) ~ (delcordpreterm_rate - lag(delcordpreterm_rate, 2)) / lag(delcordpreterm_rate, 2),
   !is.na(lag(delcordpreterm_rate, 1)) ~ (delcordpreterm_rate - lag(delcordpreterm_rate, 1)) / lag(delcordpreterm_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Delayed Cord Clamping amongst preterm Newborns following spontaneous vaginal birth ----

c32 <- dta %>%
 mutate(
  delcordpretermvg = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (DEL_CORD_CLAMP %in% 3) &
    (GA_BEST < 37) &
    (toupper(DMMETHOD) %in% c("SPT","BIV")) ~ 1,
   TRUE ~ NA_integer_
  ),
  den = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (GA_BEST < 37) &
    (toupper(DMMETHOD) %in% c("SPT","BIV")) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, delcordpretermvg) %>%
 mutate(
  delcordpretermvg_count = n()
 ) %>%
 group_by(FiscalYear, den) %>%
 mutate(
  total_year = n(),
  delcordpretermvg_rate = delcordpretermvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpretermvg %in% 1) %>%
 select(FiscalYear, delcordpretermvg, delcordpretermvg_count,delcordpretermvg_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 tidyr::complete(FiscalYear = levels(dta$FiscalYear),
                 tidyr::nesting(delcordpretermvg),
                 fill = list(
                  delcordpretermvg_count = 0,
                  delcordpretermvg_rate = NA
                 )) %>%
 mutate(
  delcordpretermvg_delta = (delcordpretermvg_rate - lag(delcordpretermvg_rate))/lag(delcordpretermvg_rate),
  delcordpretermvg_deltap = case_when(
   !is.na(lag(delcordpretermvg_rate, 9)) ~ (delcordpretermvg_rate - lag(delcordpretermvg_rate, 9)) / lag(delcordpretermvg_rate, 9),
   !is.na(lag(delcordpretermvg_rate, 8)) ~ (delcordpretermvg_rate - lag(delcordpretermvg_rate, 8)) / lag(delcordpretermvg_rate, 8),
   !is.na(lag(delcordpretermvg_rate, 7)) ~ (delcordpretermvg_rate - lag(delcordpretermvg_rate, 7)) / lag(delcordpretermvg_rate, 7),
   !is.na(lag(delcordpretermvg_rate, 6)) ~ (delcordpretermvg_rate - lag(delcordpretermvg_rate, 6)) / lag(delcordpretermvg_rate, 6),
   !is.na(lag(delcordpretermvg_rate, 5)) ~ (delcordpretermvg_rate - lag(delcordpretermvg_rate, 5)) / lag(delcordpretermvg_rate, 5),
   !is.na(lag(delcordpretermvg_rate, 4)) ~ (delcordpretermvg_rate - lag(delcordpretermvg_rate, 4)) / lag(delcordpretermvg_rate, 4),
   !is.na(lag(delcordpretermvg_rate, 3)) ~ (delcordpretermvg_rate - lag(delcordpretermvg_rate, 3)) / lag(delcordpretermvg_rate, 3),
   !is.na(lag(delcordpretermvg_rate, 2)) ~ (delcordpretermvg_rate - lag(delcordpretermvg_rate, 2)) / lag(delcordpretermvg_rate, 2),
   !is.na(lag(delcordpretermvg_rate, 1)) ~ (delcordpretermvg_rate - lag(delcordpretermvg_rate, 1)) / lag(delcordpretermvg_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Delayed Cord Clamping amongst preterm Newborns following caesarean birth ----

c33 <- dta %>%
 mutate(
  delcordpretermcs = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (DEL_CORD_CLAMP %in% 3) &
    (GA_BEST < 37) &
    (toupper(DMMETHOD) %in% c("CST")) ~ 1,
   TRUE ~ NA_integer_
  ),
  den = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (GA_BEST < 37) &
    (toupper(DMMETHOD) %in% c("CST")) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, delcordpretermcs) %>%
 mutate(
  delcordpretermcs_count = n()
 ) %>%
 group_by(FiscalYear, den) %>%
 mutate(
  total_year = n(),
  delcordpretermcs_rate = delcordpretermcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpretermcs %in% 1) %>%
 select(FiscalYear, delcordpretermcs, delcordpretermcs_count,delcordpretermcs_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 tidyr::complete(FiscalYear = levels(dta$FiscalYear),
                 tidyr::nesting(delcordpretermcs),
                 fill = list(
                  delcordpretermcs_count = 0,
                  delcordpretermcs_rate = NA
                 )) %>%
 mutate(
  delcordpretermcs_delta = (delcordpretermcs_rate - lag(delcordpretermcs_rate))/lag(delcordpretermcs_rate),
  delcordpretermcs_deltap = case_when(
   !is.na(lag(delcordpretermcs_rate, 9)) ~ (delcordpretermcs_rate - lag(delcordpretermcs_rate, 9)) / lag(delcordpretermcs_rate, 9),
   !is.na(lag(delcordpretermcs_rate, 8)) ~ (delcordpretermcs_rate - lag(delcordpretermcs_rate, 8)) / lag(delcordpretermcs_rate, 8),
   !is.na(lag(delcordpretermcs_rate, 7)) ~ (delcordpretermcs_rate - lag(delcordpretermcs_rate, 7)) / lag(delcordpretermcs_rate, 7),
   !is.na(lag(delcordpretermcs_rate, 6)) ~ (delcordpretermcs_rate - lag(delcordpretermcs_rate, 6)) / lag(delcordpretermcs_rate, 6),
   !is.na(lag(delcordpretermcs_rate, 5)) ~ (delcordpretermcs_rate - lag(delcordpretermcs_rate, 5)) / lag(delcordpretermcs_rate, 5),
   !is.na(lag(delcordpretermcs_rate, 4)) ~ (delcordpretermcs_rate - lag(delcordpretermcs_rate, 4)) / lag(delcordpretermcs_rate, 4),
   !is.na(lag(delcordpretermcs_rate, 3)) ~ (delcordpretermcs_rate - lag(delcordpretermcs_rate, 3)) / lag(delcordpretermcs_rate, 3),
   !is.na(lag(delcordpretermcs_rate, 2)) ~ (delcordpretermcs_rate - lag(delcordpretermcs_rate, 2)) / lag(delcordpretermcs_rate, 2),
   !is.na(lag(delcordpretermcs_rate, 1)) ~ (delcordpretermcs_rate - lag(delcordpretermcs_rate, 1)) / lag(delcordpretermcs_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Exclusive Human milk feeding ----

c34 <- dta %>%
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
 filter(excbrst %in% 1, lvb %in% 1) %>%
 select(FiscalYear, excbrst, excbrst_count,excbrst_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  excbrst_delta = (excbrst_rate - lag(excbrst_rate))/lag(excbrst_rate),
  excbrst_deltap = case_when(
   !is.na(lag(excbrst_rate, 9)) ~ (excbrst_rate - lag(excbrst_rate, 9)) / lag(excbrst_rate, 9),
   !is.na(lag(excbrst_rate, 8)) ~ (excbrst_rate - lag(excbrst_rate, 8)) / lag(excbrst_rate, 8),
   !is.na(lag(excbrst_rate, 7)) ~ (excbrst_rate - lag(excbrst_rate, 7)) / lag(excbrst_rate, 7),
   !is.na(lag(excbrst_rate, 6)) ~ (excbrst_rate - lag(excbrst_rate, 6)) / lag(excbrst_rate, 6),
   !is.na(lag(excbrst_rate, 5)) ~ (excbrst_rate - lag(excbrst_rate, 5)) / lag(excbrst_rate, 5),
   !is.na(lag(excbrst_rate, 4)) ~ (excbrst_rate - lag(excbrst_rate, 4)) / lag(excbrst_rate, 4),
   !is.na(lag(excbrst_rate, 3)) ~ (excbrst_rate - lag(excbrst_rate, 3)) / lag(excbrst_rate, 3),
   !is.na(lag(excbrst_rate, 2)) ~ (excbrst_rate - lag(excbrst_rate, 2)) / lag(excbrst_rate, 2),
   !is.na(lag(excbrst_rate, 1)) ~ (excbrst_rate - lag(excbrst_rate, 1)) / lag(excbrst_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Non-exclusive (any) human milk feeding ----

c35 <- dta %>%
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
 filter(nexcbrst %in% 1, lvb %in% 1) %>%
 select(FiscalYear, nexcbrst, nexcbrst_count,nexcbrst_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  nexcbrst_delta = (nexcbrst_rate - lag(nexcbrst_rate))/lag(nexcbrst_rate),
  nexcbrst_deltap = case_when(
   !is.na(lag(nexcbrst_rate, 9)) ~ (nexcbrst_rate - lag(nexcbrst_rate, 9)) / lag(nexcbrst_rate, 9),
   !is.na(lag(nexcbrst_rate, 8)) ~ (nexcbrst_rate - lag(nexcbrst_rate, 8)) / lag(nexcbrst_rate, 8),
   !is.na(lag(nexcbrst_rate, 7)) ~ (nexcbrst_rate - lag(nexcbrst_rate, 7)) / lag(nexcbrst_rate, 7),
   !is.na(lag(nexcbrst_rate, 6)) ~ (nexcbrst_rate - lag(nexcbrst_rate, 6)) / lag(nexcbrst_rate, 6),
   !is.na(lag(nexcbrst_rate, 5)) ~ (nexcbrst_rate - lag(nexcbrst_rate, 5)) / lag(nexcbrst_rate, 5),
   !is.na(lag(nexcbrst_rate, 4)) ~ (nexcbrst_rate - lag(nexcbrst_rate, 4)) / lag(nexcbrst_rate, 4),
   !is.na(lag(nexcbrst_rate, 3)) ~ (nexcbrst_rate - lag(nexcbrst_rate, 3)) / lag(nexcbrst_rate, 3),
   !is.na(lag(nexcbrst_rate, 2)) ~ (nexcbrst_rate - lag(nexcbrst_rate, 2)) / lag(nexcbrst_rate, 2),
   !is.na(lag(nexcbrst_rate, 1)) ~ (nexcbrst_rate - lag(nexcbrst_rate, 1)) / lag(nexcbrst_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### No breast/chest feeding among those who intended to breast/chest feed ----

c36 <- dta %>%
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
 filter(nbrst %in% 1, lvb %in% 1) %>%
 select(FiscalYear, nbrst, nbrst_count,nbrst_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  nbrst_delta = (nbrst_rate - lag(nbrst_rate))/lag(nbrst_rate),
  nbrst_deltap = case_when(
   !is.na(lag(nbrst_rate, 9)) ~ (nbrst_rate - lag(nbrst_rate, 9)) / lag(nbrst_rate, 9),
   !is.na(lag(nbrst_rate, 8)) ~ (nbrst_rate - lag(nbrst_rate, 8)) / lag(nbrst_rate, 8),
   !is.na(lag(nbrst_rate, 7)) ~ (nbrst_rate - lag(nbrst_rate, 7)) / lag(nbrst_rate, 7),
   !is.na(lag(nbrst_rate, 6)) ~ (nbrst_rate - lag(nbrst_rate, 6)) / lag(nbrst_rate, 6),
   !is.na(lag(nbrst_rate, 5)) ~ (nbrst_rate - lag(nbrst_rate, 5)) / lag(nbrst_rate, 5),
   !is.na(lag(nbrst_rate, 4)) ~ (nbrst_rate - lag(nbrst_rate, 4)) / lag(nbrst_rate, 4),
   !is.na(lag(nbrst_rate, 3)) ~ (nbrst_rate - lag(nbrst_rate, 3)) / lag(nbrst_rate, 3),
   !is.na(lag(nbrst_rate, 2)) ~ (nbrst_rate - lag(nbrst_rate, 2)) / lag(nbrst_rate, 2),
   !is.na(lag(nbrst_rate, 1)) ~ (nbrst_rate - lag(nbrst_rate, 1)) / lag(nbrst_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### The breast/chest feeding initiation rate ----

c37 <- dta %>%
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
 filter(brstinit %in% 1, lvb %in% 1) %>%
 select(FiscalYear, brstinit, brstinit_count,brstinit_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  brstinit_delta = (brstinit_rate - lag(brstinit_rate))/lag(brstinit_rate),
  brstinit_deltap = case_when(
   !is.na(lag(brstinit_rate, 9)) ~ (brstinit_rate - lag(brstinit_rate, 9)) / lag(brstinit_rate, 9),
   !is.na(lag(brstinit_rate, 8)) ~ (brstinit_rate - lag(brstinit_rate, 8)) / lag(brstinit_rate, 8),
   !is.na(lag(brstinit_rate, 7)) ~ (brstinit_rate - lag(brstinit_rate, 7)) / lag(brstinit_rate, 7),
   !is.na(lag(brstinit_rate, 6)) ~ (brstinit_rate - lag(brstinit_rate, 6)) / lag(brstinit_rate, 6),
   !is.na(lag(brstinit_rate, 5)) ~ (brstinit_rate - lag(brstinit_rate, 5)) / lag(brstinit_rate, 5),
   !is.na(lag(brstinit_rate, 4)) ~ (brstinit_rate - lag(brstinit_rate, 4)) / lag(brstinit_rate, 4),
   !is.na(lag(brstinit_rate, 3)) ~ (brstinit_rate - lag(brstinit_rate, 3)) / lag(brstinit_rate, 3),
   !is.na(lag(brstinit_rate, 2)) ~ (brstinit_rate - lag(brstinit_rate, 2)) / lag(brstinit_rate, 2),
   !is.na(lag(brstinit_rate, 1)) ~ (brstinit_rate - lag(brstinit_rate, 1)) / lag(brstinit_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### ICU Admission during Pregnancy or Postpartum ----

c38 <- dta %>%
 mutate(
  icu = case_when(
   JOGC_ICU > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, JOGC_ICU) %>%
 mutate(
  icu_count = n()
 ) %>%
 group_by(FiscalYear) %>%
 mutate(
  total_year = n(),
  icu_rate = icu_count/total_year
 ) %>%
 ungroup() %>%
 filter(JOGC_ICU %in% 1) %>%
 select(FiscalYear, icu, icu_count,icu_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  icu_delta = (icu_rate - lag(icu_rate))/lag(icu_rate),
  icu_deltap = case_when(
   !is.na(lag(icu_rate, 9)) ~ (icu_rate - lag(icu_rate, 9)) / lag(icu_rate, 9),
   !is.na(lag(icu_rate, 8)) ~ (icu_rate - lag(icu_rate, 8)) / lag(icu_rate, 8),
   !is.na(lag(icu_rate, 7)) ~ (icu_rate - lag(icu_rate, 7)) / lag(icu_rate, 7),
   !is.na(lag(icu_rate, 6)) ~ (icu_rate - lag(icu_rate, 6)) / lag(icu_rate, 6),
   !is.na(lag(icu_rate, 5)) ~ (icu_rate - lag(icu_rate, 5)) / lag(icu_rate, 5),
   !is.na(lag(icu_rate, 4)) ~ (icu_rate - lag(icu_rate, 4)) / lag(icu_rate, 4),
   !is.na(lag(icu_rate, 3)) ~ (icu_rate - lag(icu_rate, 3)) / lag(icu_rate, 3),
   !is.na(lag(icu_rate, 2)) ~ (icu_rate - lag(icu_rate, 2)) / lag(icu_rate, 2),
   !is.na(lag(icu_rate, 1)) ~ (icu_rate - lag(icu_rate, 1)) / lag(icu_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

### Rate of Severe Morbidity in Pregnancy or Postpartum ----

c39 <- dta %>%
 mutate(
  smm = case_when(
   JOGC_AnySMM > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, JOGC_AnySMM) %>%
 mutate(
  smm_count = n()
 ) %>%
 group_by(FiscalYear, lvb) %>%
 mutate(
  total_year = n(),
  smm_rate = smm_count/total_year
 ) %>%
 ungroup() %>%
 filter(JOGC_AnySMM %in% 1, lvb %in% 1) %>%
 select(FiscalYear, smm, smm_count,smm_rate) %>%
 distinct() %>%
 arrange(FiscalYear) %>%
 mutate(
  smm_delta = (smm_rate - lag(smm_rate))/lag(smm_rate),
  smm_deltap = case_when(
   !is.na(lag(smm_rate, 9)) ~ (smm_rate - lag(smm_rate, 9)) / lag(smm_rate, 9),
   !is.na(lag(smm_rate, 8)) ~ (smm_rate - lag(smm_rate, 8)) / lag(smm_rate, 8),
   !is.na(lag(smm_rate, 7)) ~ (smm_rate - lag(smm_rate, 7)) / lag(smm_rate, 7),
   !is.na(lag(smm_rate, 6)) ~ (smm_rate - lag(smm_rate, 6)) / lag(smm_rate, 6),
   !is.na(lag(smm_rate, 5)) ~ (smm_rate - lag(smm_rate, 5)) / lag(smm_rate, 5),
   !is.na(lag(smm_rate, 4)) ~ (smm_rate - lag(smm_rate, 4)) / lag(smm_rate, 4),
   !is.na(lag(smm_rate, 3)) ~ (smm_rate - lag(smm_rate, 3)) / lag(smm_rate, 3),
   !is.na(lag(smm_rate, 2)) ~ (smm_rate - lag(smm_rate, 2)) / lag(smm_rate, 2),
   !is.na(lag(smm_rate, 1)) ~ (smm_rate - lag(smm_rate, 1)) / lag(smm_rate, 1),
   TRUE ~ NA_real_ # if all lag values are NA
  )
 )

fyearly_stats <- cbind(
 c1 %>% select(-PreExisting_Hypertension),
 c2 %>% select(-Gestational_Hypertension,-FiscalYear),
 c3 %>% select(-Any_Hypertension,-FiscalYear),
 c4 %>% select(-anemia,-FiscalYear),
 c5 %>% select(-PreExisting_Diabetes,-FiscalYear),
 c6 %>% select(-GDM,-FiscalYear),
 c7 %>% select(-Any_Diabetes,-FiscalYear),
 c8 %>% select(-sptvg,-FiscalYear),
 c9 %>% select(-med,-FiscalYear),
 c10 %>% select(-sptassvgmed,-FiscalYear),
 c11 %>% select(-sptassvgnmed,-FiscalYear),
 c12 %>% select(-RobsnGrp,-FiscalYear),
 c13 %>% select(-RobsnGrp,-FiscalYear),
 c14 %>% select(-RobsnGrp,-FiscalYear),
 c15 %>% select(-pphbl,-FiscalYear),
 c16 %>% select(-pphpi,-FiscalYear),
 c17 %>% select(-ppreadm,-FiscalYear),
 #c18 %>% select(-brstinit,-FiscalYear),
 c19 %>% select(-sknskn,-FiscalYear),
 c20 %>% select(-sknsknvg,-FiscalYear),
 c21 %>% select(-sknskncs,-FiscalYear),
 c22 %>% select(-pphiv,-FiscalYear),
 c23 %>% select(-neoreadm,-FiscalYear),
 c24 %>% select(-pretster,-FiscalYear),
 c25 %>% select(-pretnnicu,-FiscalYear),
 c26 %>% select(-newlwcs,-FiscalYear),
 c27 %>% select(-delcord,-FiscalYear),
 c28 %>% select(-delcordterm,-FiscalYear),
 c29 %>% select(-delcordtermvg,-FiscalYear),
 c30 %>% select(-delcordtermcs,-FiscalYear),
 c31 %>% select(-delcordpreterm,-FiscalYear),
 c32 %>% select(-delcordpretermvg,-FiscalYear),
 c33 %>% select(-delcordpretermcs,-FiscalYear),
 c34 %>% select(-excbrst,-FiscalYear),
 c35 %>% select(-nexcbrst,-FiscalYear),
 c36 %>% select(-nbrst,-FiscalYear),
 c37 %>% select(-brstinit,-FiscalYear),
 c38 %>% select(-icu,-FiscalYear),
 c39 %>% select(-smm,-FiscalYear)
)

# Table stats ----

source("./modules/data-arrange-tbl.R")

# Map stats ----

source("./modules/data-arrange-map.R")

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
 ftab_dist,
 "./data/ftab_dist.parquet")

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
