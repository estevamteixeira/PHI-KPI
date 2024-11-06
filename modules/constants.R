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

ccd_stats <- arrow::read_parquet("./data/ccd_stats.parquet", as_data_frame = FALSE)

## Community Clusters (CL) ----

cl_shp <- sf::read_sf("./data/NSC_cl.shp") %>%
 rename(c(GeoUID = clusterid,
          name = cluster))

ccl_stats <- arrow::read_parquet("./data/ccl_stats.parquet", as_data_frame = FALSE)

## Community Health Networks (CHN) ----

chn_shp <- sf::read_sf("./data/NSC_chn.shp") %>%
 rename(c(GeoUID = network_id,
          name = network)) %>%
 mutate(name = gsub(" Comm H. Network","",name))

cchn_stats <- arrow::read_parquet("./data/cchn_stats.parquet", as_data_frame = FALSE)

## Health Authority Zones (HR) ----

hr_shp <- sf::read_sf("./data/NSC_hr.shp") %>%
 select(ZoneID, Name, geometry) %>%
 rename(c(GeoUID = ZoneID,
          name = Name))

chr_stats <- arrow::read_parquet("./data/chr_stats.parquet", as_data_frame = FALSE)

## Urban x Rural (urb) ----

# urb_shp <- sf::read_sf("./data/NSC_urban.shp") %>%
#  rename(GeoUID = FID) %>%
#  mutate(GeoUID = 1)
# sf::st_make_valid() %>%
# rmapshaper::ms_simplify() %>%
# sf::st_make_valid()

# curb_stats <- arrow::read_parquet("./data/curb_stats.parquet", as_data_frame = FALSE)

# Import dashboard data ----

cyearly_stats <- arrow::read_parquet("./data/cyearly_stats.parquet", as_data_frame = FALSE)
fyearly_stats <- arrow::read_parquet("./data/fyearly_stats.parquet", as_data_frame = FALSE)
ctab_stats <- arrow::read_parquet("./data/ctab_stats.parquet", as_data_frame = FALSE)
ftab_stats <- arrow::read_parquet("./data/ftab_stats.parquet", as_data_frame = FALSE)
ctab_dist <- arrow::read_parquet("./data/ctab_dist.parquet", as_data_frame = FALSE)
ftab_dist <- arrow::read_parquet("./data/ftab_dist.parquet", as_data_frame = FALSE)

# Data time range ----
app_time_range <- paste(min(cyearly_stats %>% select(BrthYear) %>% collect(), na.rm = TRUE),
                        "-",
                        max(cyearly_stats %>% select(BrthYear) %>% collect(), na.rm = TRUE))

# Labels for SelectInput() ----
## Indicator ----

metrics_lbl <- list(
 ### Hypertension
 ### Diabetes
 stats::setNames(
  cyearly_stats %>% select(contains("hyp_rate"),
                           contains("diab_rate")) %>% collect() %>% names(),
  c("Pre-existing Hypertension",
    "Gestational Hypertension",
    "Any Hypertension (Pre-existing/Gestational)",
    "Pre-existing Diabetes",
    "Gestational Diabetes",
    "Any Diabetes (Pre-existing/Gestational)")),
 ### Anaemia in pregnancy
 stats::setNames(
  cyearly_stats %>% select(contains("anemia_rate")) %>% collect() %>% names(),
  c("Anaemia")),
 ### Severe Perineal Trauma with Spontaneous Vaginal Birth
 ### Mediolateral Episiotomy with Operative Vaginal Birth
 ### Severe Perineal Trauma with Operative Vaginal Birth and mediolateral episiotomy
 ### Severe Perineal Trauma with Operative Vaginal Birth without mediolateral episiotomy
 stats::setNames(
  cyearly_stats %>% select(contains("spt_rate"),
                           contains("med_rate"),
                           contains("sptassvgmed_rate"),
                           contains("sptassvgnmed_rate")) %>% collect() %>% names(),
  c("Severe Perineal Trauma with Spontaneous Vaginal Birth",
    "Mediolateral Episiotomy with Operative Vaginal Birth",
    "Severe Perineal Trauma with Operative Vaginal Birth and Mediolateral Episiotomy",
    "Severe Perineal Trauma with Operative Vaginal Birth without Mediolateral Episiotomy")),
 ### Robson
 stats::setNames(
  cyearly_stats %>% select(contains("rbs") & ends_with("rate")) %>% collect() %>% names(),
  c("Robson Group 1",
    "Robson Group 2a",
    "Robson Group 5a")),
 ### Postpartum hemorrhage treated with a blood transfusion
 ### Postpartum hemorrhage resulting in procedural intervention
 stats::setNames(
  cyearly_stats %>% select(contains("pphbl_rate"),
                           contains("pphpi_rate")) %>% collect() %>% names(),
  c("Postpartum Hemorrhage Treated with a Blood Transfusion",
    "Postpartum Hemorrhage Resulting in Procedural Intervention")),
 ### Postpartum readmission
 ### Postpartum patients who received medical anticoagulation prophylaxis when indicated
 ### Skin to skin
 stats::setNames(
  cyearly_stats %>% select(contains("ppre") & ends_with("rate"),
                           contains("skn") & ends_with("rate")) %>% collect() %>% names(),
  c("Postpartum Readmission",
    "Skin to Skin",
    "Skin to Skin: Vaginal Birth",
    "Skin to Skin: C-section")),
 ### Postpartum blood products amongst those who received antenatal iron therapy
 stats::setNames(
  cyearly_stats %>% select(contains("pphiv_rate")) %>% collect() %>% names(),
  c("Postpartum blood products amongst those who received antenatal iron therapy")),
 ### Neonatal readmission
 stats::setNames(
  cyearly_stats %>% select(contains("neo") & ends_with("rate")) %>% collect() %>% names(),
  c("Neonatal Readmission")),
 ### Preterm infants who received complete course of steroids >24h and < 7 days prior to birth
 ### Preterm babies born in facility without NICU
 stats::setNames(
  cyearly_stats %>% select(contains("pretster") & ends_with("rate"),
                           contains("pretnnicu") & ends_with("rate")) %>% collect() %>% names(),
  c("Preterm infants who received steroids prior to birth",
    "Preterm infants born in facility without NICU")),
 ### Newborn respiratory distress associated with low-risk repeat cesarean at term gestation (≥ 37 weeks < 39 weeks)
 ### Delayed Cord Clamping
 ### Delayed Cord Clamping amongst Term Newborns
 ### Delayed Cord Clamping amongst Term Newborns following spontaneous vaginal birth
 ### Delayed Cord Clamping amongst Term Newborns following cesarean birth
 ### Delayed Cord Clamping amongst Preterm Newborns
 ### Delayed Cord Clamping amongst Preterm Newborns following spontaneous vaginal birth
 ### Delayed Cord Clamping amongst Preterm Newborns following cesarean birth
 stats::setNames(
  cyearly_stats %>% select(contains("newlwcs_rate"),
                           contains("delcord_rate"),
                           contains("delcordterm_rate"),
                           contains("delcordtermvg_rate"),
                           contains("delcordtermcs_rate"),
                           contains("delcordpreterm_rate"),
                           contains("delcordpretermvg_rate"),
                           contains("delcordpretermcs_rate")) %>% collect() %>% names(),
  c("Newborn respiratory distress associated with low-risk repeat Caesarean at term gestation",
    "Delayed Cord Clamping",
    "Delayed Cord Clamping amongst Term Newborns",
    "Delayed Cord Clamping amongst Term Newborns after Spontaneous Vaginal Birth",
    "Delayed Cord Clamping amongst Term Newborns after Caesarean Birth",
    "Delayed Cord Clamping amongst Preterm Newborns",
    "Delayed Cord Clamping amongst Preterm Newborns after Spontaneous Vaginal Birth",
    "Delayed Cord Clamping amongst Preterm Newborns after Caesarean Birth")),
 ### Breastfeeding
 stats::setNames(
  cyearly_stats %>% select(contains("brst") & ends_with("rate")) %>% collect() %>% names(),
  c("Exclusive Breastfeeding",
    "Non-Exclusive Breastfeeding",
    "No Breastfeeding",
    "Breastfeeding Initiation"))
 )

# List name ----
## The outermost names will be used as the group labels

names(metrics_lbl) <- c("Antenatal",
                        "Intrapartum",
                        "Postpartum",
                        "Newborn",
                        "Infant Feeding")

# Tooltip text ----

ttip <- list(
 prehyp_rate = paste0(
  # "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of pregnant patients diagnosed with pre-existing hypertension.</li>",
  # "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Patients diagnosed with pre-existing hypertension during pregnancy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> All postpartum patients.</li></ul>"
 ),
 gesthyp_rate = paste0(
  # "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of pregnant patients diagnosed with gestational hypertension.</li>",
  # "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Patients diagnosed with gestational hypertension during pregnancy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> All postpartum patients.</li></ul>"
 ),
 hyp_rate = paste0(
  # "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of pregnant patients diagnosed with hypertension (pre-existing, gestational, and other or unspecified) during pregnancy.</li>",
  # "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Patients diagnosed with hypertension (pre-existing, gestational, and other or unspecified) during pregnancy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> All postpartum patients.</li></ul>"
 ),
 prediab_rate = paste0(
  # "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of pregnant patients diagnosed with pre-existing diabetes during pregnancy.</li>",
  # "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Patients diagnosed with pre-existing diabetes during pregnancy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> All postpartum patients.</li></ul>"
 ),
 gestdiab_rate = paste0(
  # "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of pregnant patients diagnosed with gestational diabetes during pregnancy.</li>",
  # "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Patients diagnosed with gestational diabetes during pregnancy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> All postpartum patients.</li></ul>"
 ),
 diab_rate = paste0(
  # "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of pregnant patients diagnosed with diabetes (pre-existing, gestational, and other or unspecified) during pregnancy.</li>",
  # "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Patients diagnosed with diabetes (pre-existing, gestational, and other or unspecified) during pregnancy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> All postpartum patients.</li></ul>"
 ),
 rbs1_rate = paste0(
  # "<ul> <li style='text-align:left;'> <b>Definition:</b> The proportion of postpartum primiparous patients (without other specified risk factors) who had spontaneous onset of labor and a Caesarean birth.</li>",
  # "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Postpartum patients 37+ weeks, singleton, cephalic presentation, nulliparous, spontaneous labor who had a Caesarean section.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> All postpartum patients 37+ weeks, singleton, cephalic presentation, nulliparous, spontaneous labor.</li></ul>"
 ),
 rbs21_rate = paste0(
  # "<ul> <li style='text-align:left;'> <b>Definition:</b> The proportion of postpartum primiparous patients whose labor was induced and who had a Caesarean.</li>",
  # "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Postpartum patients 37+ weeks, singleton, cephalic presentation, nulliparous, induced labor who had a Caesarean section.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> All postpartum patients 37+ weeks, singleton, cephalic presentation, nulliparous, induced labor.</li></ul>"
 ),
 rbs51_rate = paste0(
  # "<ul> <li style='text-align:left;'> <b>Definition:</b> The proportion of postpartum multiparous patients who had a previous Caesarean delivery and experienced spontaneous onset of labor, with cesarean as the mode of delivery.</li>",
  # "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Postpartum patients 37+ weeks, singleton, cephalic presentation, multiparous, previous cesarean, spontaneous labor who have Caesarean.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Postpartum patients 37+ weeks, singleton, cephalic presentation, multiparous, previous cesarean, spontaneous labor.</li></ul>"
 ),
 ppreadm_rate = paste0(
  # "<ul> <li style='text-align:left;'> <b>Definition:</b> The proportion of patients who were readmitted to the hospital within the first 6 weeks postpartum.</li>",
  # "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Patients readmitted to the hospital within the first 6 weeks postpartum, stratified by mode of delivery: vaginal, operative vaginal, and Caesarean.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> All postpartum patients.</li></ul>"
 ),
 sknskn_rate = paste0(
  # "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of babies placed skin to skin with the birthing parent within five minutes of birth (excluding cases of Caesarean section with general anesthesia) for at least one hour or until the completion of the first feed.</li>",
  # "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Newborns coded with 'initial mother-baby contact' lasting 60+ minutes.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> All liveborn babies.</li></ul>"
 ),
 sknsknvg_rate = paste0(
  # "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of babies placed skin-to-skin with the birthing parent within five minutes of the recorded time of vaginal birth for at least one hour, or until completion of the first feed (as recorded in NSAPD as 'baby to breast').</li>",
  # "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Newborns for whom 'initial mother-baby contact' is coded for a duration 60+ minutes following vaginal birth.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> All live babies born vaginally.</li></ul>"
 ),
 sknskncs_rate = paste0(
  # "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of babies placed skin-to-skin with the birthing parent within five minutes of the recorded time of Caesarean birth (exception: Caesarean-section with general anesthesia) for at least one hour, or until completion of the first feed (as recorded in NSAPD as 'baby to breast').</li>",
  # "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Newborns for whom 'initial mother-baby contact' is coded following cesarean birth for a duration 60+ minutes.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> All live babies born via Caesarean.</li></ul>"
 ),
 neoreadm_rate = paste0(
  # "<ul> <li style='text-align:left;'> <b>Definition:</b> The proportion of newborns who were readmitted to the hospital within 42 days after birth, following discharge from the birth admission.</li>",
  # "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of newborns readmitted to the hospital within 42 days after birth, following discharge from the birth admission.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> All live newborns discharged from the hospital within 42 days after birth.</li></ul>"
 ),
 excbrst_rate = paste0(
  # "<ul> <li style='text-align:left;'> <b>Definition:</b> The proportion of infants born at a specified facility within a given time period who received human milk exclusively during the hospital stay (from birth to discharge), including those supplemented with expressed or donor human milk or who received medically indicated supplementation with human milk substitute.</li>",
  # "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Infants who received human milk exclusively during the hospital stay (from birth to discharge), including those supplemented with expressed or donor human milk or who received medically indicated supplementation with human milk substitute.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> All liveborn infants born at the specified facility within the given time frame.</li></ul>"
 ),
 nexcbrst_rate = paste0(
  # "<ul> <li style='text-align:left;'> <b>Definition:</b> The proportion of infants born at a specified facility within a given time period who received human milk (including expressed or donor milk) along with water, water-based drinks, fruit juice, ritual fluids, or any other liquid, including human milk substitutes or solids.</li>",
  # "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Infants who received human milk (including expressed or donor milk) and water, water-based drinks, fruit juice, ritual fluids, or any other liquid, including human milk substitutes or solids.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> All liveborn infants born at the specified facility within the given time frame.</li></ul>"
 ),
 nbrst_rate = paste0(
  # "<ul> <li style='text-align:left;'> <b>Definition:</b> The proportion of postpartum patients who delivered at least one liveborn infant, intended to breast/chestfeed (as recorded in the PNR or health record), but whose infant received no human milk. For multiple births, NSAPD refers to the last born.</li>",
  # "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Postpartum patients with at least one liveborn infant and a documented intention to breast/chestfeed, whose infant received no human milk.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> All postpartum patients with at least one liveborn infant and a documented intention to breast/chestfeed.</li></ul>"
 ),
 brstinit_rate = paste0(
  # "<ul> <li style='text-align:left;'> <b>Definition:</b> The proportion of postpartum patients who delivered at least one liveborn infant, where the infant received any human milk from birth to discharge.</li>",
  # "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Postpartum patients with at least one liveborn infant whose infant received any human milk from birth to discharge.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> All postpartum patients with at least one liveborn infant.</li></ul>"
 )
)

## Geographies for maps ----

geo_lbl <- c(
 "cd", "cl", "chn", "hr"#, "urb"
)

# The outermost names will be used as label in the dash

names(geo_lbl) <- c(
 "Census Divisions", "Community Clusters", "Community Health Networks", "Health Authority Zones"#, "Urban x Rural"
)

