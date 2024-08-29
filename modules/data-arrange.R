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
   between(BTBrthDT, as.POSIXct("2008-04-01 00:00:00.0000", tz="UTC"), as.POSIXct("2009-03-31 23:59:59.0000", tz="UTC")) ~ "2008-2009",
   between(BTBrthDT, as.POSIXct("2009-04-01 00:00:00.0000", tz="UTC"), as.POSIXct("2010-03-31 23:59:59.0000", tz="UTC")) ~ "2009-2010",
   between(BTBrthDT, as.POSIXct("2010-04-01 00:00:00.0000", tz="UTC"), as.POSIXct("2011-03-31 23:59:59.0000", tz="UTC")) ~ "2010-2011",
   between(BTBrthDT, as.POSIXct("2011-04-01 00:00:00.0000", tz="UTC"), as.POSIXct("2012-03-31 23:59:59.0000", tz="UTC")) ~ "2011-2012",
   between(BTBrthDT, as.POSIXct("2012-04-01 00:00:00.0000", tz="UTC"), as.POSIXct("2013-03-31 23:59:59.0000", tz="UTC")) ~ "2012-2013",
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
  "2008-2009","2009-2010","2010-2011","2011-2012","2012-2013","2013-2014",
  "2014-2015","2015-2016","2016-2017","2017-2018","2018-2019","2019-2020",
  "2020-2021","2021-2022","2022-2023","2023-2024","2024-2025"
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
