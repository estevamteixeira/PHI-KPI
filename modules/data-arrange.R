# Script to prepare data for the
# Perinatal Health Indicators and Key Performance Indicators ----
## Time frame: 10 years if possible
## Geography: provincial, compared with national data if available

# Loading required packages ----

library(haven) # read SAS data
library(dplyr) # data manipulation
library(arrow) # integration to Apache arrow: in-memory data structure
library(sf) # Support for simple features, a standardized way to encode spatial vector data

# Periods for display ----

t0 <- 2014
tn <- 2023

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
 ## Mother's Residence DHA ----
 "DLResDHA",
 ## NICU ----
 "SCNAdm",# Number times Admitted to NICU - total
 "NECONHSP", # Contact Hospital
 "NEADMFRM", # Location Immediately Prior to Transfer/Re-admission
 "NETOHOSP", # Immediate Destination for Infant on Discharge from Transfer/Re-admission
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
 "DLPRVCS", # Previous Caesarean
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
 "JOGC_ICU",
 "CountyNo", # Mother's County #
 "DLCOUNTY", # Mother's County of Residence
 "DLTOHOSP", # Immediate Destination of Mother on Discharge from Delivery Admission
 "BTTOHOSP", # Immediate Destination for Infant on Discharge from Birth Admission
 "CONHOSP" # Contact Hospital
 )

# Load dataset ----
dta <- haven::read_sas("H:/RCP/RCP_Data/TeixeiEC/Monster/Monster.sas7bdat"
                           #, n_max = 2
                           ,col_select = myvec
) %>%
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
 ## 1
 prehyp_num = case_when(
  PreExisting_Hypertension %in% 1 ~ 1,
  TRUE ~ 0
 ),
 ## 2
 gesthyp_num = case_when(
  Gestational_Hypertension %in% 1 ~ 1,
  TRUE ~ 0
 ),
 ## 3
 hyp_num = case_when(
  Any_Hypertension %in% 1 ~ 1,
  TRUE ~ 0
 ),
 ## 4
 anemia_num = case_when(
  MANEM > 0 | R014_01500 > 0 | MO990 > 0 | MD50_D53 > 0 |
   MD55_D59 > 0 | MD60_D64 > 0 ~ 1,
  TRUE ~ 0
 ),
 ## 5
 prediab_num = case_when(
  PreExisting_Diabetes %in% 1 ~ 1,
  TRUE ~ 0
 ),
 ## 6
 gestdiab_num = case_when(
  GDM %in% 1 ~ 1,
  TRUE ~ 0
 ),
 ## 7
 diab_num = case_when(
  Any_Diabetes %in% 1 ~ 1,
  TRUE ~ 0
 ),
 ## 8
 trd = case_when(
  MO702 > 0 | MO70201 > 0 | MO70204 > 0 |
   MO70211 > 0 | MO70214 > 0 | MO70221 > 0 |
   MO70231 > 0 | MO70234 > 0 | MO70281 > 0 | MO70291 > 0 ~ 1,
  TRUE ~ 0
 ),
 frth = case_when(
  MO703 > 0 | MO70301 > 0 | MO70304 > 0 ~ 1,
  TRUE ~ 0
 ),
 sptvg = case_when(
  toupper(DMMETHOD) %in% c("SPT", "BIV") ~ 1,
  TRUE ~ 0
 ),
 spt_num = case_when(
  (trd > 0 | frth > 0) & toupper(DMMETHOD) %in% c("SPT", "BIV") ~ 1,
  TRUE ~ 0
 ),
 ## 9
 med_num = case_when(
  (DMEPISIO %in% "ML") & toupper(DMMETHOD) %in% c("VAC", "LOW", "MID") ~ 1,
  TRUE ~ 0
 ),
 assvg = case_when(
  toupper(DMMETHOD) %in% c("VAC", "LOW", "MID") ~ 1,
  TRUE ~ 0
  ),
 ## 10
 sptassvgmed_num = case_when(
  (trd > 0 | frth > 0) & (DMEPISIO %in% "ML") & toupper(DMMETHOD) %in% c("VAC", "LOW", "MID") ~ 1,
  TRUE ~ 0
 ),
 ## 11
 sptassvgnmed_num = case_when(
  (trd > 0 | frth > 0) & (!DMEPISIO %in% "ML") & toupper(DMMETHOD) %in% c("VAC", "LOW", "MID") ~ 1,
  TRUE ~ 0
 ),
 ## 12
 rbs1_num = case_when(
  RobsnGrp %in% 1 & tolower(DMMETHOD) %in% "cst" ~ 1,
  TRUE ~ 0
 ),
 rbs1_den = case_when(
  RobsnGrp %in% 1 ~ 1,
  TRUE ~ 0
 ),
 ## 13
 rbs21_num = case_when(
  RobsnGrp %in% 2.1 & tolower(DMMETHOD) %in% "cst" ~ 1,
  TRUE ~ 0
 ),
 rbs21_den = case_when(
  RobsnGrp %in% 2.1 ~ 1,
  TRUE ~ 0
 ),
 ## 14
 rbs51_num = case_when(
  RobsnGrp %in% 5.1 & tolower(DMMETHOD) %in% "cst" ~ 1,
  TRUE ~ 0
 ),
 rbs51_den = case_when(
  RobsnGrp %in% 5.1 ~ 1,
  TRUE ~ 0
 ),
 ## 15
 pphbl_num = case_when(
  Any_Blood_Product > 0 & Postpartum_Haemorrhage > 0 ~ 1,
  TRUE ~ 0
 ),
 ## 16
 # Procedural intervention
 prcint = case_when(
  R029 > 0 | R029_00100 > 0 |
   R029_00200 > 0 | R029_00300 > 0 |
   R029_00400 > 0 ~ 1,
  TRUE ~ 0
 ),
 # Hysterectomy
 hyst = case_when(
  M_5MD60RC > 0 | M_5MD60RD > 0 |
   M_5MD60KE > 0 | M_5MD60CB > 0 |
   M_1RM89 > 0 | toupper(MODEDEL) %in% "CSH" ~ 1,
  TRUE ~ 0
 ),
 pphpi_num = case_when(
  (prcint > 0 | hyst > 0) & Postpartum_Haemorrhage > 0 ~ 1,
  TRUE ~ 0
 ),
 pphpi_den = case_when(
  Postpartum_Haemorrhage > 0 ~ 1,
  TRUE ~ 0
 ),
 ## 17
 ppreadm_num = case_when(
  NumPPAdm > 0 ~ 1,
  TRUE ~ 0
 ),
 ## 19
 sknskn_num = case_when(
  tolower(BRSTCON1) %in% "y" ~ 1,
  TRUE ~ 0
 ),
 ## 20
 sknsknvg_num = case_when(
  tolower(BRSTCON1) %in% "y" & tolower(DMMETHOD) %in% c("biv","spt","vac","low","mid") ~ 1,
  TRUE ~ 0
 ),
 ## 21
 sknskncs_num = case_when(
  tolower(BRSTCON1) %in% "y" & tolower(DMMETHOD) %in% c("cst") ~ 1,
  TRUE ~ 0
 ),
 ## 22
 pphiv_num = case_when(
  Any_Blood_Product > 0 & R003_02100 > 0 ~ 1,
  TRUE ~ 0
 ),
 pphiv_den = case_when(
  Any_Blood_Product > 0 ~ 1,
  TRUE ~ 0
 ),
 ## 23
 neoreadm_num = case_when(
  NeoAdmit > 0 ~ 1,
  TRUE ~ 0
 ),
 ## 24
 pretster_num = case_when(
  (GA_BEST < 35 & GA_BEST >= 24) &
   (R068_00200 > 0 | R068_00300 > 0 | R068_00700 > 0 |
     R068_00800 > 0 | R068_01200 > 0 | R068_01300 > 0 ) ~ 1,
  TRUE ~ 0
 ),
 pretster_den = case_when(
  (GA_BEST < 35 & GA_BEST >= 24) ~ 1,
  TRUE ~ 0
 ),
 ## 25
 pretnnicu_num = case_when(
  (GA_BEST < 34) &
   (DLHosp %in% c(11,14,18,30,43,56,67)) ~ 1,
  TRUE ~ 0
 ),
 pretnnicu_den = case_when(
  (GA_BEST < 34) &
   (DLHosp %in% c(-5,-2,-1,11,14,18,30,43,56,67,86,87) ) ~ 1,
  TRUE ~ 0
 ),
 ## 26
 newlwcs_num = case_when(
  (GA_BEST < 39 & GA_BEST >= 37) &
   (DLPRVCS > 0) &
   (toupper(DMMETHOD) %in% "CST") &
   (!toupper(BTOUTCOM) %in% "FTD") &
   (R059_00200 > 0 | R059_00300 > 0 | R059_00400 > 0 | R059_00500 > 0 | R059_00600 > 0) &
   (tolower(CSstatus) %in% c("n4","n4n4","rc","rcrc")) ~ 1,
  TRUE ~ 0
 ),
 newlwcs_den = case_when(
  (GA_BEST < 39 & GA_BEST >= 37) &
   (DLPRVCS > 0) &
   (toupper(DMMETHOD) %in% "CST") &
   (!toupper(BTOUTCOM) %in% "FTD") &
   (tolower(CSstatus) %in% c("n4","n4n4","rc","rcrc")) ~ 1,
  TRUE ~ 0
 ),
 ## 27
 delcord_num = case_when(
  (!toupper(BTOUTCOM) %in% "FTD") &
   (DEL_CORD_CLAMP %in% 3) ~ 1,
  TRUE ~ 0
 ),
 ## 28
 delcordterm_num = case_when(
  (!toupper(BTOUTCOM) %in% "FTD") &
   (DEL_CORD_CLAMP %in% 3) &
   (GA_BEST >= 37) ~ 1,
  TRUE ~ 0
 ),
 delcordterm_den = case_when(
  (!toupper(BTOUTCOM) %in% "FTD") &
   (GA_BEST >= 37) ~ 1,
  TRUE ~ 0
 ),
 ## 29
 delcordtermvg_num = case_when(
  (!toupper(BTOUTCOM) %in% "FTD") &
   (DEL_CORD_CLAMP %in% 3) &
   (GA_BEST >= 37) &
   (toupper(DMMETHOD) %in% c("SPT","BIV")) ~ 1,
  TRUE ~ 0
 ),
 delcordtermvg_den = case_when(
  (!toupper(BTOUTCOM) %in% "FTD") &
   (GA_BEST >= 37) &
   (toupper(DMMETHOD) %in% c("SPT","BIV")) ~ 1,
  TRUE ~ 0
 ),
 ## 30
 delcordtermcs_num = case_when(
  (!toupper(BTOUTCOM) %in% "FTD") &
   (DEL_CORD_CLAMP %in% 3) &
   (GA_BEST >= 37) &
   (toupper(DMMETHOD) %in% c("CST")) ~ 1,
  TRUE ~ 0
 ),
 delcordtermcs_den = case_when(
  (!toupper(BTOUTCOM) %in% "FTD") &
   (GA_BEST >= 37) &
   (toupper(DMMETHOD) %in% c("CST")) ~ 1,
  TRUE ~ 0
 ),
 ## 31
 delcordpreterm_num = case_when(
  (!toupper(BTOUTCOM) %in% "FTD") &
   (DEL_CORD_CLAMP %in% 3) &
   (GA_BEST < 37) ~ 1,
  TRUE ~ 0
 ),
 delcordpreterm_den = case_when(
  (!toupper(BTOUTCOM) %in% "FTD") &
   (GA_BEST < 37) ~ 1,
  TRUE ~ 0
 ),
 ## 32
 delcordpretermvg_num = case_when(
  (!toupper(BTOUTCOM) %in% "FTD") &
   (DEL_CORD_CLAMP %in% 3) &
   (GA_BEST < 37) &
   (toupper(DMMETHOD) %in% c("SPT","BIV")) ~ 1,
  TRUE ~ 0
 ),
 delcordpretermvg_den = case_when(
  (!toupper(BTOUTCOM) %in% "FTD") &
   (GA_BEST < 37) &
   (toupper(DMMETHOD) %in% c("SPT","BIV")) ~ 1,
  TRUE ~ 0
 ),
 ## 33
 delcordpretermcs_num = case_when(
  (!toupper(BTOUTCOM) %in% "FTD") &
   (DEL_CORD_CLAMP %in% 3) &
   (GA_BEST < 37) &
   (toupper(DMMETHOD) %in% c("CST")) ~ 1,
  TRUE ~ 0
 ),
 delcordpretermcs_den = case_when(
  (!toupper(BTOUTCOM) %in% "FTD") &
   (GA_BEST < 37) &
   (toupper(DMMETHOD) %in% c("CST")) ~ 1,
  TRUE ~ 0
 ),
 ## 34
 excbrst_num = case_when(
  tolower(BRSTFDIS) %in% "e" ~ 1,
  TRUE ~ 0
 ),
 ## 35
 nexcbrst_num = case_when(
  tolower(BRSTFDIS) %in% "s" ~ 1,
  TRUE ~ 0
 ),
 ## 36
 nbrst_num = case_when(
  tolower(BRSTFDIS) %in% "n" & tolower(DLINTBFD) %in% "y" ~ 1,
  TRUE ~ 0
 ),
 ## 37
 brstinit_num = case_when(
  tolower(BRSTFDIS) %in% c("s","e") ~ 1,
  TRUE ~ 0
 ),
 ## 38
 icu_num = case_when(
  JOGC_ICU > 0 ~ 1,
  TRUE ~ 0
 ),
 ## 39
 smm_num = case_when(
  JOGC_AnySMM > 0 ~ 1,
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
 ) %>% # filter data by birth year
 filter(
  between(BrthYear,
                t0,#as.numeric(format(as.Date(Sys.Date()),"%Y"))-10,
                tn#as.numeric(format(as.Date(Sys.Date()),"%Y"))-1
 ),
 !DLHosp %in% c(-32,-12)
 ) %>%
 mutate(across(where(is.factor), droplevels))

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

## Hospitals ----

hosp_shp <- sf::read_sf("./data/NSC_hosp.shp")

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
  # URBuid = ifelse(CSDuid %in% urb | (as.integer(DAuid) %% 1000 != 999), 1, ifelse(!CSDuid %in% urb & (as.integer(CSDuid) %% 1000 != 999), 0, NA_character_)),
  # URBname = ifelse(URBuid %in% 1,"Urban",ifelse(URBuid %in% 0, "Rural", NA_character_)),
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
cstats <- function(data, id_col, period_col, num_col, den_col = NULL,
                   count_col, rate_col, total_col, delta_col, delta_colp,
                   ref_year = t0, valid_period) {
 library(dplyr)
 library(tidyr)

 result <- data %>%
  select(!!sym(id_col), !!sym(period_col), !!sym(num_col), all_of(den_col)) %>%
  distinct()

  # If `den_col` is provided, use it; otherwise, count all cases per period
 if (!is.null(den_col)) {
  result <- result %>% filter(!!sym(den_col) == 1)
 }

 result <- result %>%
  # Count occurrences of the condition
  group_by(!!sym(period_col), !!sym(num_col)) %>%
  mutate(!!sym(count_col) := n()) %>%
  group_by(!!sym(period_col)) %>%
  mutate(!!total_col := n(),
         !!sym(rate_col) := !!sym(count_col) / !!sym(total_col)) %>%

  # Filter for relevant condition cases
  filter(!!sym(num_col) == 1) %>%

  # Select relevant columns
  select(!!sym(period_col), !!sym(num_col), !!sym(count_col), !!sym(total_col), !!sym(rate_col)) %>%
  arrange(!!sym(period_col)) %>%
  ungroup() %>%
  distinct() %>%

  complete(!!sym(period_col) := valid_period,
           tidyr::nesting(!!sym(num_col))
           )

t0 <- result %>% filter(!is.na(!!sym(rate_col))) %>% pull(!!sym(period_col)) %>% row_number() %>% min()
tn <- result %>% filter(!is.na(!!sym(rate_col))) %>% pull(!!sym(period_col)) %>% row_number() %>% max()
lag_period <- (tn-t0)

 result <- result %>%
  # Calculating deltas
  mutate(
   !!sym(delta_col) := (!!sym(rate_col) - lag(!!sym(rate_col))) / lag(!!sym(rate_col)),
   !!sym(delta_col) := ifelse(!!sym(period_col) == ref_year, NA, !!sym(delta_col)),
   !!sym(delta_colp) := (!!sym(rate_col) - lag(!!sym(rate_col), lag_period)) / lag(!!sym(rate_col), lag_period)
  )

 return(result)
}

##-------------------------
## Calendar year stats ----
##-------------------------

period <- sort(unique(t0:tn))

c1 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "prehyp_num", count_col = "prehyp_count", rate_col = "prehyp_rate", total_col = "prehyp_total", delta_col = "prehyp_delta", delta_colp = "prehyp_deltap", valid_period = period)
c2 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "gesthyp_num", count_col = "gesthyp_count", rate_col = "gesthyp_rate", total_col = "gesthyp_total", delta_col = "gesthyp_delta", delta_colp = "gesthyp_deltap", valid_period = period)
c3 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "hyp_num", count_col = "hyp_count", rate_col = "hyp_rate", total_col = "hyp_total", delta_col = "hyp_delta", delta_colp = "hyp_deltap", valid_period = period)
c4 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "anemia_num", count_col = "anemia_count", rate_col = "anemia_rate", total_col = "anemia_total", delta_col = "anemia_delta", delta_colp = "anemia_deltap", valid_period = period)
c5 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "prediab_num", count_col = "prediab_count", rate_col = "prediab_rate", total_col = "prediab_total", delta_col = "prediab_delta", delta_colp = "prediab_deltap", valid_period = period)
c6 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "gestdiab_num", count_col = "gestdiab_count", rate_col = "gestdiab_rate", total_col = "gestdiab_total", delta_col = "gestdiab_delta", delta_colp = "gestdiab_deltap", valid_period = period)
c7 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "diab_num", count_col = "diab_count", rate_col = "diab_rate", total_col = "diab_total", delta_col = "diab_delta", delta_colp = "diab_deltap", valid_period = period)
c8 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "spt_num", count_col = "spt_count", rate_col = "spt_rate", total_col = "spt_total", delta_col = "spt_delta", delta_colp = "spt_deltap", valid_period = period)
c9 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "med_num", den_col = "assvg", count_col = "med_count", rate_col = "med_rate", total_col = "med_total", delta_col = "med_delta", delta_colp = "med_deltap", valid_period = period)
c10 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "sptassvgmed_num", den_col = "assvg", count_col = "sptassvgmed_count", rate_col = "sptassvgmed_rate", total_col = "sptassvgmed_total", delta_col = "sptassvgmed_delta", delta_colp = "sptassvgmed_deltap", valid_period = period)
c11 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "sptassvgnmed_num", den_col = "assvg", count_col = "sptassvgnmed_count", rate_col = "sptassvgnmed_rate", total_col = "sptassvgnmed_total", delta_col = "sptassvgnmed_delta", delta_colp = "sptassvgnmed_deltap", valid_period = period)
c12 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "rbs1_num", den_col = "rbs1_den", count_col = "rbs1_count", rate_col = "rbs1_rate", total_col = "rbs1_total", delta_col = "rbs1_delta", delta_colp = "rbs1_deltap", valid_period = period)
c13 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "rbs21_num", den_col = "rbs21_den", count_col = "rbs21_count", rate_col = "rbs21_rate", total_col = "rbs21_total", delta_col = "rbs21_delta", delta_colp = "rbs21_deltap", valid_period = period)
c14 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "rbs51_num", den_col = "rbs51_den", count_col = "rbs51_count", rate_col = "rbs51_rate", total_col = "rbs51_total", delta_col = "rbs51_delta", delta_colp = "rbs51_deltap", valid_period = period)
c15 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "pphbl_num", count_col = "pphbl_count", rate_col = "pphbl_rate", total_col = "pphbl_total", delta_col = "pphbl_delta", delta_colp = "pphbl_deltap", valid_period = period)
c16 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "pphpi_num", den_col = "pphpi_den", count_col = "pphpi_count", rate_col = "pphpi_rate", total_col = "pphpi_total", delta_col = "pphpi_delta", delta_colp = "pphpi_deltap", valid_period = period)
c17 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "ppreadm_num", count_col = "ppreadm_count", rate_col = "ppreadm_rate", total_col = "ppreadm_total", delta_col = "ppreadm_delta", delta_colp = "ppreadm_deltap", valid_period = period)
#c18 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "hyp_num", count_col = "hyp_count", rate_col = "hyp_rate", total_col = "hyp_total", delta_col = "hyp_delta", delta_colp = "hyp_deltap", valid_period = period)
c19 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "sknskn_num", den_col = "lvb", count_col = "sknskn_count", rate_col = "sknskn_rate", total_col = "sknskn_total", delta_col = "sknskn_delta", delta_colp = "sknskn_deltap", valid_period = period)
c20 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "sknsknvg_num", den_col = "lvbvg", count_col = "sknsknvg_count", rate_col = "sknsknvg_rate", total_col = "sknsknvg_total", delta_col = "sknsknvg_delta", delta_colp = "sknsknvg_deltap", valid_period = period)
c21 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "sknskncs_num", den_col = "lvbcs", count_col = "sknskncs_count", rate_col = "sknskncs_rate", total_col = "sknskncs_total", delta_col = "sknskncs_delta", delta_colp = "sknskncs_deltap", valid_period = period)
c22 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "pphiv_num", den_col = "pphiv_den", count_col = "pphiv_count", rate_col = "pphiv_rate", total_col = "pphiv_total", delta_col = "pphiv_delta", delta_colp = "pphiv_deltap", valid_period = period)
c23 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "neoreadm_num", den_col = "lvb", count_col = "neoreadm_count", rate_col = "neoreadm_rate", total_col = "neoreadm_total", delta_col = "neoreadm_delta", delta_colp = "neoreadm_deltap", valid_period = period)
c24 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "pretster_num", den_col = "pretster_den", count_col = "pretster_count", rate_col = "pretster_rate", total_col = "pretster_total", delta_col = "pretster_delta", delta_colp = "pretster_deltap", valid_period = period)
c25 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "pretnnicu_num", den_col = "pretnnicu_den", count_col = "pretnnicu_count", rate_col = "pretnnicu_rate", total_col = "pretnnicu_total", delta_col = "pretnnicu_delta", delta_colp = "pretnnicu_deltap", valid_period = period)
c26 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "newlwcs_num", den_col = "newlwcs_den", count_col = "newlwcs_count", rate_col = "newlwcs_rate", total_col = "newlwcs_total", delta_col = "newlwcs_delta", delta_colp = "newlwcs_deltap", valid_period = period)
c27 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "delcord_num", den_col = "lvb", count_col = "delcord_count", rate_col = "delcord_rate", total_col = "delcord_total", delta_col = "delcord_delta", delta_colp = "delcord_deltap", valid_period = period)
c28 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "delcordterm_num", den_col = "delcordterm_den", count_col = "delcordterm_count", rate_col = "delcordterm_rate", total_col = "delcordterm_total", delta_col = "delcordterm_delta", delta_colp = "delcordterm_deltap", valid_period = period)
c29 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "delcordtermvg_num", den_col = "delcordtermvg_den", count_col = "delcordtermvg_count", rate_col = "delcordtermvg_rate", total_col = "delcordtermvg_total", delta_col = "delcordtermvg_delta", delta_colp = "delcordtermvg_deltap", valid_period = period)
c30 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "delcordtermcs_num", den_col = "delcordtermcs_den", count_col = "delcordtermcs_count", rate_col = "delcordtermcs_rate", total_col = "delcordtermcs_total", delta_col = "delcordtermcs_delta", delta_colp = "delcordtermcs_deltap", valid_period = period)
c31 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "delcordpreterm_num", den_col = "delcordpreterm_den", count_col = "delcordpreterm_count", rate_col = "delcordpreterm_rate", total_col = "delcordpreterm_total", delta_col = "delcordpreterm_delta", delta_colp = "delcordpreterm_deltap", valid_period = period)
c32 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "delcordpretermvg_num", den_col = "delcordpretermvg_den", count_col = "delcordpretermvg_count", rate_col = "delcordpretermvg_rate", total_col = "delcordpretermvg_total", delta_col = "delcordpretermvg_delta", delta_colp = "delcordpretermvg_deltap", valid_period = period)
c33 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "delcordpretermcs_num", den_col = "delcordpretermcs_den", count_col = "delcordpretermcs_count", rate_col = "delcordpretermcs_rate", total_col = "delcordpretermcs_total", delta_col = "delcordpretermcs_delta", delta_colp = "delcordpretermcs_deltap", valid_period = period)
c34 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "excbrst_num", den_col = "lvb", count_col = "excbrst_count", rate_col = "excbrst_rate", total_col = "excbrst_total", delta_col = "excbrst_delta", delta_colp = "excbrst_deltap", valid_period = period)
c35 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "nexcbrst_num", den_col = "lvb", count_col = "nexcbrst_count", rate_col = "nexcbrst_rate", total_col = "nexcbrst_total", delta_col = "nexcbrst_delta", delta_colp = "nexcbrst_deltap", valid_period = period)
c36 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "nbrst_num", den_col = "lvbint", count_col = "nbrst_count", rate_col = "nbrst_rate", total_col = "nbrst_total", delta_col = "nbrst_delta", delta_colp = "nbrst_deltap", valid_period = period)
c37 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "brstinit_num", den_col = "lvb", count_col = "brstinit_count", rate_col = "brstinit_rate", total_col = "brstinit_total", delta_col = "brstinit_delta", delta_colp = "brstinit_deltap", valid_period = period)
c38 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "icu_num", count_col = "icu_count", rate_col = "icu_rate", total_col = "icu_total", delta_col = "icu_delta", delta_colp = "icu_deltap", valid_period = period)
c39 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "smm_num", count_col = "smm_count", rate_col = "smm_rate", total_col = "smm_total", delta_col = "smm_delta", delta_colp = "smm_deltap", valid_period = period)

cyearly_stats <- cbind(
 c1 %>% select(-ends_with("_num")),
 c2 %>% select(-ends_with("_num"),-BrthYear),
 c3 %>% select(-ends_with("_num"),-BrthYear),
 c4 %>% select(-ends_with("_num"),-BrthYear),
 c5 %>% select(-ends_with("_num"),-BrthYear),
 c6 %>% select(-ends_with("_num"),-BrthYear),
 c7 %>% select(-ends_with("_num"),-BrthYear),
 c8 %>% select(-ends_with("_num"),-BrthYear),
 c9 %>% select(-ends_with("_num"),-BrthYear),
 c10 %>% select(-ends_with("_num"),-BrthYear),
 c11 %>% select(-ends_with("_num"),-BrthYear),
 c12 %>% select(-ends_with("_num"),-BrthYear),
 c13 %>% select(-ends_with("_num"),-BrthYear),
 c14 %>% select(-ends_with("_num"),-BrthYear),
 c15 %>% select(-ends_with("_num"),-BrthYear),
 c16 %>% select(-ends_with("_num"),-BrthYear),
 c17 %>% select(-ends_with("_num"),-BrthYear),
 #c18 %>% select(-ends_with("_num"),-BrthYear),
 c19 %>% select(-ends_with("_num"),-BrthYear),
 c20 %>% select(-ends_with("_num"),-BrthYear),
 c21 %>% select(-ends_with("_num"),-BrthYear),
 c22 %>% select(-ends_with("_num"),-BrthYear),
 c23 %>% select(-ends_with("_num"),-BrthYear),
 c24 %>% select(-ends_with("_num"),-BrthYear),
 c25 %>% select(-ends_with("_num"),-BrthYear),
 c26 %>% select(-ends_with("_num"),-BrthYear),
 c27 %>% select(-ends_with("_num"),-BrthYear),
 c28 %>% select(-ends_with("_num"),-BrthYear),
 c29 %>% select(-ends_with("_num"),-BrthYear),
 c30 %>% select(-ends_with("_num"),-BrthYear),
 c31 %>% select(-ends_with("_num"),-BrthYear),
 c32 %>% select(-ends_with("_num"),-BrthYear),
 c33 %>% select(-ends_with("_num"),-BrthYear),
 c34 %>% select(-ends_with("_num"),-BrthYear),
 c35 %>% select(-ends_with("_num"),-BrthYear),
 c36 %>% select(-ends_with("_num"),-BrthYear),
 c37 %>% select(-ends_with("_num"),-BrthYear),
 c38 %>% select(-ends_with("_num"),-BrthYear),
 c39 %>% select(-ends_with("_num"),-BrthYear)
)

##-----------------------
## Fiscal year stats ----
##-----------------------

period <- unique(levels(dta$FiscalYear))

c1 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "prehyp_num", count_col = "prehyp_count", rate_col = "prehyp_rate", total_col = "prehyp_total", delta_col = "prehyp_delta", delta_colp = "prehyp_deltap", valid_period = period)
c2 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "gesthyp_num", count_col = "gesthyp_count", rate_col = "gesthyp_rate", total_col = "gesthyp_total", delta_col = "gesthyp_delta", delta_colp = "gesthyp_deltap", valid_period = period)
c3 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "hyp_num", count_col = "hyp_count", rate_col = "hyp_rate", total_col = "hyp_total", delta_col = "hyp_delta", delta_colp = "hyp_deltap", valid_period = period)
c4 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "anemia_num", count_col = "anemia_count", rate_col = "anemia_rate", total_col = "anemia_total", delta_col = "anemia_delta", delta_colp = "anemia_deltap", valid_period = period)
c5 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "prediab_num", count_col = "prediab_count", rate_col = "prediab_rate", total_col = "prediab_total", delta_col = "prediab_delta", delta_colp = "prediab_deltap", valid_period = period)
c6 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "gestdiab_num", count_col = "gestdiab_count", rate_col = "gestdiab_rate", total_col = "gestdiab_total", delta_col = "gestdiab_delta", delta_colp = "gestdiab_deltap", valid_period = period)
c7 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "diab_num", count_col = "diab_count", rate_col = "diab_rate", total_col = "diab_total", delta_col = "diab_delta", delta_colp = "diab_deltap", valid_period = period)
c8 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "spt_num", count_col = "spt_count", rate_col = "spt_rate", total_col = "spt_total", delta_col = "spt_delta", delta_colp = "spt_deltap", valid_period = period)
c9 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "med_num", den_col = "assvg", count_col = "med_count", rate_col = "med_rate", total_col = "med_total", delta_col = "med_delta", delta_colp = "med_deltap", valid_period = period)
c10 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "sptassvgmed_num", den_col = "assvg", count_col = "sptassvgmed_count", rate_col = "sptassvgmed_rate", total_col = "sptassvgmed_total", delta_col = "sptassvgmed_delta", delta_colp = "sptassvgmed_deltap", valid_period = period)
c11 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "sptassvgnmed_num", den_col = "assvg", count_col = "sptassvgnmed_count", rate_col = "sptassvgnmed_rate", total_col = "sptassvgnmed_total", delta_col = "sptassvgnmed_delta", delta_colp = "sptassvgnmed_deltap", valid_period = period)
c12 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "rbs1_num", den_col = "rbs1_den", count_col = "rbs1_count", rate_col = "rbs1_rate", total_col = "rbs1_total", delta_col = "rbs1_delta", delta_colp = "rbs1_deltap", valid_period = period)
c13 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "rbs21_num", den_col = "rbs21_den", count_col = "rbs21_count", rate_col = "rbs21_rate", total_col = "rbs21_total", delta_col = "rbs21_delta", delta_colp = "rbs21_deltap", valid_period = period)
c14 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "rbs51_num", den_col = "rbs51_den", count_col = "rbs51_count", rate_col = "rbs51_rate", total_col = "rbs51_total", delta_col = "rbs51_delta", delta_colp = "rbs51_deltap", valid_period = period)
c15 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "pphbl_num", count_col = "pphbl_count", rate_col = "pphbl_rate", total_col = "pphbl_total", delta_col = "pphbl_delta", delta_colp = "pphbl_deltap", valid_period = period)
c16 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "pphpi_num", den_col = "pphpi_den", count_col = "pphpi_count", rate_col = "pphpi_rate", total_col = "pphpi_total", delta_col = "pphpi_delta", delta_colp = "pphpi_deltap", valid_period = period)
c17 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "ppreadm_num", count_col = "ppreadm_count", rate_col = "ppreadm_rate", total_col = "ppreadm_total", delta_col = "ppreadm_delta", delta_colp = "ppreadm_deltap", valid_period = period)
#c18 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "hyp_num", count_col = "hyp_count", rate_col = "hyp_rate", total_col = "hyp_total", delta_col = "hyp_delta", delta_colp = "hyp_deltap", valid_period = period)
c19 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "sknskn_num", den_col = "lvb", count_col = "sknskn_count", rate_col = "sknskn_rate", total_col = "sknskn_total", delta_col = "sknskn_delta", delta_colp = "sknskn_deltap", valid_period = period)
c20 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "sknsknvg_num", den_col = "lvbvg", count_col = "sknsknvg_count", rate_col = "sknsknvg_rate", total_col = "sknsknvg_total", delta_col = "sknsknvg_delta", delta_colp = "sknsknvg_deltap", valid_period = period)
c21 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "sknskncs_num", den_col = "lvbcs", count_col = "sknskncs_count", rate_col = "sknskncs_rate", total_col = "sknskncs_total", delta_col = "sknskncs_delta", delta_colp = "sknskncs_deltap", valid_period = period)
c22 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "pphiv_num", den_col = "pphiv_den", count_col = "pphiv_count", rate_col = "pphiv_rate", total_col = "pphiv_total", delta_col = "pphiv_delta", delta_colp = "pphiv_deltap", valid_period = period)
c23 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "neoreadm_num", den_col = "lvb", count_col = "neoreadm_count", rate_col = "neoreadm_rate", total_col = "neoreadm_total", delta_col = "neoreadm_delta", delta_colp = "neoreadm_deltap", valid_period = period)
c24 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "pretster_num", den_col = "pretster_den", count_col = "pretster_count", rate_col = "pretster_rate", total_col = "pretster_total", delta_col = "pretster_delta", delta_colp = "pretster_deltap", valid_period = period)
c25 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "pretnnicu_num", den_col = "pretnnicu_den", count_col = "pretnnicu_count", rate_col = "pretnnicu_rate", total_col = "pretnnicu_total", delta_col = "pretnnicu_delta", delta_colp = "pretnnicu_deltap", valid_period = period)
c26 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "newlwcs_num", den_col = "newlwcs_den", count_col = "newlwcs_count", rate_col = "newlwcs_rate", total_col = "newlwcs_total", delta_col = "newlwcs_delta", delta_colp = "newlwcs_deltap", valid_period = period)
c27 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "delcord_num", den_col = "lvb", count_col = "delcord_count", rate_col = "delcord_rate", total_col = "delcord_total", delta_col = "delcord_delta", delta_colp = "delcord_deltap", valid_period = period)
c28 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "delcordterm_num", den_col = "delcordterm_den", count_col = "delcordterm_count", rate_col = "delcordterm_rate", total_col = "delcordterm_total", delta_col = "delcordterm_delta", delta_colp = "delcordterm_deltap", valid_period = period)
c29 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "delcordtermvg_num", den_col = "delcordtermvg_den", count_col = "delcordtermvg_count", rate_col = "delcordtermvg_rate", total_col = "delcordtermvg_total", delta_col = "delcordtermvg_delta", delta_colp = "delcordtermvg_deltap", valid_period = period)
c30 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "delcordtermcs_num", den_col = "delcordtermcs_den", count_col = "delcordtermcs_count", rate_col = "delcordtermcs_rate", total_col = "delcordtermcs_total", delta_col = "delcordtermcs_delta", delta_colp = "delcordtermcs_deltap", valid_period = period)
c31 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "delcordpreterm_num", den_col = "delcordpreterm_den", count_col = "delcordpreterm_count", rate_col = "delcordpreterm_rate", total_col = "delcordpreterm_total", delta_col = "delcordpreterm_delta", delta_colp = "delcordpreterm_deltap", valid_period = period)
c32 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "delcordpretermvg_num", den_col = "delcordpretermvg_den", count_col = "delcordpretermvg_count", rate_col = "delcordpretermvg_rate", total_col = "delcordpretermvg_total", delta_col = "delcordpretermvg_delta", delta_colp = "delcordpretermvg_deltap", valid_period = period)
c33 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "delcordpretermcs_num", den_col = "delcordpretermcs_den", count_col = "delcordpretermcs_count", rate_col = "delcordpretermcs_rate", total_col = "delcordpretermcs_total", delta_col = "delcordpretermcs_delta", delta_colp = "delcordpretermcs_deltap", valid_period = period)
c34 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "excbrst_num", den_col = "lvb", count_col = "excbrst_count", rate_col = "excbrst_rate", total_col = "excbrst_total", delta_col = "excbrst_delta", delta_colp = "excbrst_deltap", valid_period = period)
c35 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "nexcbrst_num", den_col = "lvb", count_col = "nexcbrst_count", rate_col = "nexcbrst_rate", total_col = "nexcbrst_total", delta_col = "nexcbrst_delta", delta_colp = "nexcbrst_deltap", valid_period = period)
c36 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "nbrst_num", den_col = "lvbint", count_col = "nbrst_count", rate_col = "nbrst_rate", total_col = "nbrst_total", delta_col = "nbrst_delta", delta_colp = "nbrst_deltap", valid_period = period)
c37 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "brstinit_num", den_col = "lvb", count_col = "brstinit_count", rate_col = "brstinit_rate", total_col = "brstinit_total", delta_col = "brstinit_delta", delta_colp = "brstinit_deltap", valid_period = period)
c38 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "icu_num", count_col = "icu_count", rate_col = "icu_rate", total_col = "icu_total", delta_col = "icu_delta", delta_colp = "icu_deltap", valid_period = period)
c39 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "smm_num", count_col = "smm_count", rate_col = "smm_rate", total_col = "smm_total", delta_col = "smm_delta", delta_colp = "smm_deltap", valid_period = period)

fyearly_stats <- cbind(
 c1 %>% select(-ends_with("_num")),
 c2 %>% select(-ends_with("_num"),-FiscalYear),
 c3 %>% select(-ends_with("_num"),-FiscalYear),
 c4 %>% select(-ends_with("_num"),-FiscalYear),
 c5 %>% select(-ends_with("_num"),-FiscalYear),
 c6 %>% select(-ends_with("_num"),-FiscalYear),
 c7 %>% select(-ends_with("_num"),-FiscalYear),
 c8 %>% select(-ends_with("_num"),-FiscalYear),
 c9 %>% select(-ends_with("_num"),-FiscalYear),
 c10 %>% select(-ends_with("_num"),-FiscalYear),
 c11 %>% select(-ends_with("_num"),-FiscalYear),
 c12 %>% select(-ends_with("_num"),-FiscalYear),
 c13 %>% select(-ends_with("_num"),-FiscalYear),
 c14 %>% select(-ends_with("_num"),-FiscalYear),
 c15 %>% select(-ends_with("_num"),-FiscalYear),
 c16 %>% select(-ends_with("_num"),-FiscalYear),
 c17 %>% select(-ends_with("_num"),-FiscalYear),
 #c18 %>% select(-ends_with("_num"),-FiscalYear),
 c19 %>% select(-ends_with("_num"),-FiscalYear),
 c20 %>% select(-ends_with("_num"),-FiscalYear),
 c21 %>% select(-ends_with("_num"),-FiscalYear),
 c22 %>% select(-ends_with("_num"),-FiscalYear),
 c23 %>% select(-ends_with("_num"),-FiscalYear),
 c24 %>% select(-ends_with("_num"),-FiscalYear),
 c25 %>% select(-ends_with("_num"),-FiscalYear),
 c26 %>% select(-ends_with("_num"),-FiscalYear),
 c27 %>% select(-ends_with("_num"),-FiscalYear),
 c28 %>% select(-ends_with("_num"),-FiscalYear),
 c29 %>% select(-ends_with("_num"),-FiscalYear),
 c30 %>% select(-ends_with("_num"),-FiscalYear),
 c31 %>% select(-ends_with("_num"),-FiscalYear),
 c32 %>% select(-ends_with("_num"),-FiscalYear),
 c33 %>% select(-ends_with("_num"),-FiscalYear),
 c34 %>% select(-ends_with("_num"),-FiscalYear),
 c35 %>% select(-ends_with("_num"),-FiscalYear),
 c36 %>% select(-ends_with("_num"),-FiscalYear),
 c37 %>% select(-ends_with("_num"),-FiscalYear),
 c38 %>% select(-ends_with("_num"),-FiscalYear),
 c39 %>% select(-ends_with("_num"),-FiscalYear)
) %>%
 mutate(FiscalYear = factor(FiscalYear))

# Table stats ----

source("./modules/data-arrange-tbl.R")

# Trend stats ----

source("./modules/data-arrange-trend.R")

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
 fyearly_stats %>% mutate(FiscalYear = factor(FiscalYear)),
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
 ccd_stats %>%
  mutate(across(ends_with("id"), as.character)),
 "./data/ccd_stats.parquet")

arrow::write_parquet(
 ccl_stats %>%
  mutate(across(ends_with("id"), as.character)),
 "./data/ccl_stats.parquet")

arrow::write_parquet(
 cchn_stats %>%
  mutate(across(ends_with("id"), as.character)),
 "./data/cchn_stats.parquet")

arrow::write_parquet(
 chr_stats %>%
  mutate(across(ends_with("id"), as.character)),
 "./data/chr_stats.parquet")

arrow::write_parquet(
 fcd_stats %>%
  mutate(across(ends_with("id"), as.character),
         FiscalYear = factor(FiscalYear)),
 "./data/fcd_stats.parquet")

arrow::write_parquet(
 fcl_stats %>%
  mutate(across(ends_with("id"), as.character),
         FiscalYear = factor(FiscalYear)),
 "./data/fcl_stats.parquet")

arrow::write_parquet(
 fchn_stats %>%
  mutate(across(ends_with("id"), as.character),
         FiscalYear = factor(FiscalYear)),
 "./data/fchn_stats.parquet")

arrow::write_parquet(
 fhr_stats %>%
  mutate(across(ends_with("id"), as.character),
         FiscalYear = factor(FiscalYear)),
 "./data/fhr_stats.parquet")

arrow::write_parquet(
 chosp_stats,
 "./data/chosp_stats.parquet")

arrow::write_parquet(
 fhosp_stats %>% mutate(FiscalYear = factor(FiscalYear)),
 "./data/fhosp_stats.parquet")

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
