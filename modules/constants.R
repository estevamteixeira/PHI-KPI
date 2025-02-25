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
fcd_stats <- arrow::read_parquet("./data/fcd_stats.parquet", as_data_frame = FALSE)

## Community Clusters (CL) ----

cl_shp <- sf::read_sf("./data/NSC_cl.shp") %>%
 rename(c(GeoUID = clusterid,
          name = cluster))

ccl_stats <- arrow::read_parquet("./data/ccl_stats.parquet", as_data_frame = FALSE)
fcl_stats <- arrow::read_parquet("./data/fcl_stats.parquet", as_data_frame = FALSE)

## Community Health Networks (CHN) ----

chn_shp <- sf::read_sf("./data/NSC_chn.shp") %>%
 rename(c(GeoUID = network_id,
          name = network)) %>%
 mutate(name = gsub(" Comm H. Network","",name))

cchn_stats <- arrow::read_parquet("./data/cchn_stats.parquet", as_data_frame = FALSE)
fchn_stats <- arrow::read_parquet("./data/fchn_stats.parquet", as_data_frame = FALSE)

## Health Authority Zones (HR) ----

hr_shp <- sf::read_sf("./data/NSC_hr.shp") %>%
 select(ZoneID, Name, geometry) %>%
 rename(c(GeoUID = ZoneID,
          name = Name))

chr_stats <- arrow::read_parquet("./data/chr_stats.parquet", as_data_frame = FALSE)
fhr_stats <- arrow::read_parquet("./data/fhr_stats.parquet", as_data_frame = FALSE)

## Urban x Rural (urb) ----

# urb_shp <- sf::read_sf("./data/NSC_urban.shp") %>%
#  rename(GeoUID = FID) %>%
#  mutate(GeoUID = 1)
# sf::st_make_valid() %>%
# rmapshaper::ms_simplify() %>%
# sf::st_make_valid()

# curb_stats <- arrow::read_parquet("./data/curb_stats.parquet", as_data_frame = FALSE)

## Hospital ----

hosp_shp <- sf::read_sf("./data/NSC_hosp.shp") %>%
 select(GeoUID, FACILITY, TYPE, geometry)

# Import dashboard data ----

cyearly_stats <- arrow::read_parquet("./data/cyearly_stats.parquet", as_data_frame = FALSE)
fyearly_stats <- arrow::read_parquet("./data/fyearly_stats.parquet", as_data_frame = FALSE)
ctab_stats <- arrow::read_parquet("./data/ctab_stats.parquet", as_data_frame = FALSE)
ftab_stats <- arrow::read_parquet("./data/ftab_stats.parquet", as_data_frame = FALSE)
ctab_dist <- arrow::read_parquet("./data/ctab_dist.parquet", as_data_frame = FALSE)
ftab_dist <- arrow::read_parquet("./data/ftab_dist.parquet", as_data_frame = FALSE)
chosp_stats <- arrow::read_parquet("./data/chosp_stats.parquet", as_data_frame = FALSE)
fhosp_stats <- arrow::read_parquet("./data/fhosp_stats.parquet", as_data_frame = FALSE)

# Data time range ----
app_time_range <- paste(min(cyearly_stats %>% select(BrthYear) %>% collect(), na.rm = TRUE),
                        "-",
                        max(cyearly_stats %>% select(BrthYear) %>% collect(), na.rm = TRUE))

# Labels for SelectInput() ----
## Indicator ----

metrics_lbl <- list(
 ### Hypertension
 ### Diabetes
 ### Anaemia in pregnancy
 stats::setNames(
  cyearly_stats %>% select(contains("hyp_rate"),
                           contains("anemia_rate"),
                           contains("diab_rate")) %>% collect() %>% names(),
  c("Pre-existing Hypertension",
    "Gestational Hypertension",
    "Any Hypertension (Pre-existing/Gestational)",
    "Anaemia",
    "Pre-existing Diabetes",
    "Gestational Diabetes",
    "Any Diabetes (Pre-existing/Gestational)")),
 ### Severe Perineal Trauma with Spontaneous Vaginal Birth
 ### Mediolateral Episiotomy with Operative Vaginal Birth
 ### Severe Perineal Trauma with Operative Vaginal Birth and mediolateral episiotomy
 ### Severe Perineal Trauma with Operative Vaginal Birth without mediolateral episiotomy
 ### Robson
 stats::setNames(
  cyearly_stats %>% select(contains("spt_rate"),
                           contains("med_rate"),
                           contains("sptassvgmed_rate"),
                           contains("sptassvgnmed_rate"),
                           contains("rbs") & ends_with("rate")) %>% collect() %>% names(),
  c("Severe Perineal Trauma with Spontaneous Vaginal Birth",
    "Mediolateral Episiotomy with Operative Vaginal Birth",
    "Severe Perineal Trauma with Operative Vaginal Birth and Mediolateral Episiotomy",
    "Severe Perineal Trauma with Operative Vaginal Birth without Mediolateral Episiotomy",
    "Robson Group 1",
    "Robson Group 2a",
    "Robson Group 5a")),
 ### Postpartum hemorrhage treated with a blood transfusion
 ### Postpartum hemorrhage resulting in procedural intervention
 ### Postpartum readmission
 ### Postpartum Number of patients who received medical anticoagulation prophylaxis when indicated
 ### Skin to skin
 ### Postpartum blood products amongst those who received antenatal iron therapy
 stats::setNames(
  cyearly_stats %>% select(contains("pphbl_rate"),
                           contains("pphpi_rate"),
                           contains("ppre") & ends_with("rate"),
                           contains("skn") & ends_with("rate"),
                           contains("pphiv_rate")) %>% collect() %>% names(),
  c("Postpartum Hemorrhage Treated with a Blood Transfusion",
    "Postpartum Hemorrhage Resulting in Procedural Intervention",
    "Postpartum Readmission",
    "Skin to Skin",
    "Skin to Skin: Vaginal Birth",
    "Skin to Skin: C-section",
    "Postpartum blood products amongst those who received antenatal iron therapy")),
 ### Neonatal readmission
 ### Preterm infants who received complete course of steroids >24h and < 7 days prior to birth
 ### Preterm babies born in facility without NICU
 ### Newborn respiratory distress associated with low-risk repeat Caesarean at term gestation (37+ weeks < 39 weeks)
 ### Delayed Cord Clamping
 ### Delayed Cord Clamping amongst Term Number of newborns
 ### Delayed Cord Clamping amongst Term Number of newborns following spontaneous vaginal birth
 ### Delayed Cord Clamping amongst Term Number of newborns following Caesarean birth
 ### Delayed Cord Clamping amongst Preterm Number of newborns
 ### Delayed Cord Clamping amongst Preterm Number of newborns following spontaneous vaginal birth
 ### Delayed Cord Clamping amongst Preterm Number of newborns following Caesarean birth
 stats::setNames(
  cyearly_stats %>% select(contains("neo") & ends_with("rate"),
                           contains("pretster") & ends_with("rate"),
                           contains("pretnnicu") & ends_with("rate"),
                           contains("newlwcs_rate"),
                           contains("delcord_rate"),
                           contains("delcordterm_rate"),
                           contains("delcordtermvg_rate"),
                           contains("delcordtermcs_rate"),
                           contains("delcordpreterm_rate"),
                           contains("delcordpretermvg_rate"),
                           contains("delcordpretermcs_rate")) %>% collect() %>% names(),
  c("Neonatal Readmission",
    "Preterm infants who received steroids prior to birth",
    "Preterm infants born in facility without NICU",
    "Newborn respiratory distress associated with low-risk repeat Caesarean at term gestation",
    "Delayed Cord Clamping",
    "Delayed Cord Clamping amongst Term Number of newborns",
    "Delayed Cord Clamping amongst Term Number of newborns after Spontaneous Vaginal Birth",
    "Delayed Cord Clamping amongst Term Number of newborns after Caesarean Birth",
    "Delayed Cord Clamping amongst Preterm Number of newborns",
    "Delayed Cord Clamping amongst Preterm Number of newborns after Spontaneous Vaginal Birth",
    "Delayed Cord Clamping amongst Preterm Number of newborns after Caesarean Birth")),
 ### Breastfeeding
 stats::setNames(
  cyearly_stats %>% select(contains("brst") & ends_with("rate")) %>% collect() %>% names(),
  c("Exclusive Breastfeeding",
    "Non-Exclusive Breastfeeding",
    "No Breastfeeding",
    "Breastfeeding Initiation")),
 ### ICU Admission during Pregnancy or Postpartum
 ### Rate of Severe Morbidity in Pregnancy or Postpartum
 stats::setNames(
  cyearly_stats %>% select(matches("^icu_rate$"),
                           contains("smm_rate")) %>% collect() %>% names(),
  c("ICU Admission during Pregnancy or Postpartum",
    "Severe Maternal Morbidity in Pregnancy or Postpartum"))
 )

# List name ----
## The outermost names will be used as the group labels

names(metrics_lbl) <- c("Antenatal",
                        "Intrapartum",
                        "Postpartum",
                        "Newborn",
                        "Infant Feeding",
                        "Other")

# Tooltip text ----

ttip <- list(
 prehyp_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of pregnant patients diagnosed with pre-existing hypertension.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of patients diagnosed with pre-existing hypertension during pregnancy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of postpartum patients.</li></ul>"
 ),
 gesthyp_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of pregnant patients diagnosed with gestational hypertension.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of patients diagnosed with gestational hypertension during pregnancy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of postpartum patients.</li></ul>"
 ),
 hyp_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of pregnant patients diagnosed with hypertension (pre-existing, gestational, and other or unspecified) during pregnancy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of patients diagnosed with hypertension (pre-existing, gestational, and other or unspecified) during pregnancy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of postpartum patients.</li></ul>"
 ),
 anemia_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of patients with Hgb &#8804 100 g/L during pregnancy, recorded in prenatal records or reported in blood results before delivery.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of patients with Hgb &#8804 100 g/L during pregnancy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of postpartum patients.</li></ul>"
 )
 ,
 prediab_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of pregnant patients diagnosed with pre-existing diabetes during pregnancy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of patients diagnosed with pre-existing diabetes during pregnancy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of postpartum patients.</li></ul>"
 ),
 gestdiab_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of pregnant patients diagnosed with gestational diabetes during pregnancy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of patients diagnosed with gestational diabetes during pregnancy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of postpartum patients.</li></ul>"
 ),
 diab_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of pregnant patients diagnosed with diabetes (pre-existing, gestational, and other or unspecified) during pregnancy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of patients diagnosed with diabetes (pre-existing, gestational, and other or unspecified) during pregnancy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of postpartum patients.</li></ul>"
 ),
 spt_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of postpartum patients who experienced 3<sup>rd</sup> or 4<sup>th</sup> degree perineal lacerations during spontaneous vaginal birth.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of postpartum patients with documented 3<sup>rd</sup> or 4<sup>th</sup> degree perineal lacerations following spontaneous vaginal birth.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of postpartum patients.</li></ul>"
 ),
 med_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of postpartum patients who underwent mediolateral episiotomy during operative vaginal birth.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of postpartum patients with documented mediolateral episiotomy following vaginal birth assisted by forceps or vacuum.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of postpartum patients with operative vaginal birth.</li></ul>"
 ),
 sptassvgmed_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of postpartum patients with 3<sup>rd</sup> and 4<sup>th</sup> degree lacerations during operative vaginal birth with mediolateral episiotomy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of postpartum patients with documented 3<sup>rd</sup> and 4<sup>th</sup> degree lacerations following vaginal birth assisted by forceps or vacuum, who also had a mediolateral episiotomy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of postpartum patients with operative vaginal birth.</li></ul>"
 ),
 sptassvgnmed_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of postpartum patients with 3<sup>rd</sup> and 4<sup>th</sup> degree lacerations during operative vaginal birth without mediolateral episiotomy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of postpartum patients with documented 3<sup>rd</sup> and 4<sup>th</sup> degree lacerations following vaginal birth assisted by forceps or vacuum, who did not have a mediolateral episiotomy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of postpartum patients with operative vaginal birth.</li></ul>"
 ),
 rbs1_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of postpartum primiparous patients (without other specified risk factors) who had spontaneous onset of labor and a Caesarean birth.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of postpartum patients 37+ weeks, singleton, cephalic presentation, nulliparous, spontaneous labor who had a Caesarean section.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of postpartum patients 37+ weeks, singleton, cephalic presentation, nulliparous, spontaneous labor.</li></ul>"
 ),
 rbs21_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of postpartum primiparous patients whose labor was induced and who had a Caesarean.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of postpartum patients 37+ weeks, singleton, cephalic presentation, nulliparous, induced labor who had a Caesarean section.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of postpartum patients 37+ weeks, singleton, cephalic presentation, nulliparous, induced labor.</li></ul>"
 ),
 rbs51_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of postpartum multiparous patients who had a previous Caesarean delivery and experienced spontaneous onset of labor, with Caesarean as the mode of delivery.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of postpartum patients 37+ weeks, singleton, cephalic presentation, multiparous, previous Caesarean, spontaneous labor who have Caesarean.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of postpartum patients 37+ weeks, singleton, cephalic presentation, multiparous, previous Caesarean, spontaneous labor.</li></ul>"
 ),
 pphbl_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of postpartum patients who experienced postpartum hemorrhage (500+ mL with vaginal birth or 1000+ mL with Caesarean, as defined by CIHI) and received blood products (excluding WinRho).</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of postpartum patients with documented postpartum hemorrhage (500+ mL with vaginal birth or 1000+ mL with Caesarean) who received blood products.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of postpartum patients.</li></ul>"
 ),
 pphpi_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of postpartum patients with postpartum hemorrhage (500+ mL with vaginal birth or 1000+ mL with Caesarean, as defined by CIHI) who were treated with procedural interventions including uterine compression suture, ligation or embolization of uterine arteries, uterine tamponade, or hysterectomy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of postpartum patients with documented postpartum hemorrhage (500+ mL with vaginal birth or 1000+ mL with Caesarean) who were treated with uterine compression sutures, ligation or embolization of uterine arteries, uterine tamponade, or hysterectomy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of postpartum patients with documented postpartum hemorrhage.</li></ul>"
 ),
 ppreadm_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of patients who were readmitted to the hospital within the first 6 weeks postpartum.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of patients readmitted to the hospital within the first 6 weeks postpartum, stratified by mode of delivery: vaginal, operative vaginal, and Caesarean.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of postpartum patients.</li></ul>"
 ),
 sknskn_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of babies placed skin to skin with the birthing parent within five minutes of birth (excluding cases of Caesarean section with general anesthesia) for at least one hour or until the completion of the first feed.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of newborns coded with 'initial mother-baby contact' lasting 60+ minutes.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of liveborn babies.</li></ul>"
 ),
 sknsknvg_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of babies placed skin-to-skin with the birthing parent within five minutes of the recorded time of vaginal birth for at least one hour, or until completion of the first feed (as recorded in NSAPD as 'baby to breast').</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of newborns for whom 'initial mother-baby contact' is coded for a duration 60+ minutes following vaginal birth.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of live babies born vaginally.</li></ul>"
 ),
 sknskncs_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of babies placed skin-to-skin with the birthing parent within five minutes of the recorded time of Caesarean birth (exception: Caesarean-section with general anesthesia) for at least one hour, or until completion of the first feed.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of newborns for whom 'initial mother-baby contact' is coded following Caesarean birth for a duration 60+ minutes.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of live babies born via Caesarean.</li></ul>"
 ),
 pphiv_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of patients who received postpartum blood products and also received IV Iron therapy during pregnancy.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of patients who received IV Iron therapy during pregnancy and blood products postpartum.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of patients who received postpartum blood products.</li></ul>"
 ),
 neoreadm_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of newborns who were readmitted to any hospital within 42 days after birth, following discharge from the birth admission.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of newborns readmitted to any hospital within 42 days after birth, following discharge from the birth admission.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of live newborns discharged from any hospital within 42 days after birth.</li></ul>"
 ),
 pretster_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of infants born in Nova Scotia at < 35 weeks gestation whose mothers received 2 doses of corticosteroids 24 hours to 7 days before birth.</li>",
  "<br>",
  "<li style='text-align:left;'><b>Numerator:</b> Number of infants born at 24 to < 35 weeks gestation whose mothers received 2 doses of corticosteroids 24 hours to 7 days before birth.</li>",
  "<br>",
  "<li style='text-align:left;'><b>Denominator:</b> Number of newborns born at 24 to 34 weeks gestation.</li></ul>"
  ),
 pretnnicu_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of newborns < 34 weeks gestation born in a Nova Scotia facility without a Level 3 NICU (excludes IWK and CBRH).</li>",
  "<br>",
  "<li style='text-align:left;'><b>Numerator:</b> Number of newborns < 34 weeks gestation born in a Nova Scotia facility providing Level 1 or 2 care or in a facility without an active perinatal service.</li>",
  "<br>",
  "<li style='text-align:left;'><b>Denominator:</b> Number of newborns born in Nova Scotia at < 34 weeks gestation.</li></ul>"
 ),
 newlwcs_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of respiratory distressed infants born via elective repeat Caesarean section (ERCS) with no other indication at 37+ to < 39 weeks gestation.</li>",
  "<br>",
  "<li style='text-align:left;'><b>Numerator:</b> Number of live infants born by elective or maternal-choice repeat Caesarean at 37+ to < 39 weeks gestation, coded with respiratory distress.</li>",
  "<br>",
  "<li style='text-align:left;'><b>Denominator:</b> Number of live infants born by elective or maternal-choice repeat Caesarean at 37+ to < 39 weeks gestation.</li></ul>"
 ),
 delcord_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of liveborn newborns with documented umbilical cord clamping 1-3 minutes following birth (All modes).</li>",
  "<br>",
  "<li style='text-align:left;'><b>Numerator:</b> Number of live newborns with documented umbilical cord clamping 1-3 minutes following birth (All modes).</li>",
  "<br>",
  "<li style='text-align:left;'><b>Denominator:</b> Number of liveborn newborns.</li></ul>"
 ),
 delcordterm_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of live newborns at term (37+ weeks) gestation with documented umbilical cord clamping 1-3 minutes following birth (All modes).</li>",
  "<br>",
  "<li style='text-align:left;'><b>Numerator:</b> Number of live newborns at term (37+ weeks) gestation with documented umbilical cord clamping 1-3 minutes following birth (All modes).</li>",
  "<br>",
  "<li style='text-align:left;'><b>Denominator:</b> Number of live newborns at term (37+ weeks) gestation.</li></ul>"
 ),
 delcordtermvg_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of live newborns at term (37+ weeks) gestation with documented umbilical cord clamping 1-3 minutes following spontaneous vaginal birth.</li>",
  "<br>",
  "<li style='text-align:left;'><b>Numerator:</b> Number of live newborns at term (37+ weeks) gestation with documented umbilical cord clamping 1-3 minutes following spontaneous vaginal birth (All modes).</li>",
  "<br>",
  "<li style='text-align:left;'><b>Denominator:</b> Number of live newborns at term (37+ weeks) gestation following spontaneous vaginal birth.</li></ul>"
 ),
 delcordtermcs_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of live newborns at term (37+ weeks) gestation with documented umbilical cord clamping 1-3 minutes following Caesarean birth.</li>",
  "<br>",
  "<li style='text-align:left;'><b>Numerator:</b> Number of live newborns at term (37+ weeks) gestation with documented umbilical cord clamping 1-3 minutes following Caesarean birth.</li>",
  "<br>",
  "<li style='text-align:left;'><b>Denominator:</b> Number of live newborns at term (37+ weeks) gestation following Caesarean birth.</li></ul>"
 ),
 delcordpreterm_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of live preterm newborns (< 37 weeks gestation) with documented umbilical cord clamping 1-3 minutes following birth (All modes).</li>",
  "<br>",
  "<li style='text-align:left;'><b>Numerator:</b> Number of live preterm newborns (< 37 weeks gestation) with documented umbilical cord clamping 1-3 minutes following birth (All modes).</li>",
  "<br>",
  "<li style='text-align:left;'><b>Denominator:</b> Number of live preterm newborns (< 37 weeks gestation).</li></ul>"
 ),
 delcordpretermvg_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of live preterm newborns (< 37 weeks) with umbilical cord clamping 1-3 minutes following spontaneous vaginal birth.</li>",
  "<br>",
  "<li style='text-align:left;'><b>Numerator:</b> Number of infants live preterm newborns (< 37 weeks) with umbilical cord clamping 1-3 minutes following spontaneous vaginal birth.</li>",
  "<br>",
  "<li style='text-align:left;'><b>Denominator:</b> Number of live preterm newborns (< 37 weeks) following spontaneous vaginal birth.</li></ul>"
 ),
 delcordpretermcs_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of live preterm newborns (< 37 weeks) with umbilical cord clamping 1-3 minutes following Caesarean birth.</li>",
  "<br>",
  "<li style='text-align:left;'><b>Numerator:</b> Live preterm newborns (< 37 weeks) with umbilical cord clamping 1-3 minutes following Caesarean birth.</li>",
  "<br>",
  "<li style='text-align:left;'><b>Denominator:</b> Number of live preterm newborns (< 37 weeks) following Caesarean birth.</li></ul>"
 ),
 excbrst_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of infants born at a specified facility within a given time period who received human milk exclusively during the hospital stay (from birth to discharge), including those supplemented with expressed or donor human milk or who received medically indicated supplementation with human milk substitute.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of infants who received human milk exclusively during the hospital stay (from birth to discharge), including those supplemented with expressed or donor human milk or who received medically indicated supplementation with human milk substitute.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of liveborn infants born at the specified facility within the given time frame.</li></ul>"
 ),
 nexcbrst_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of infants born at a specified facility within a given time period who received human milk (including expressed or donor milk) along with water, water-based drinks, fruit juice, ritual fluids, or any other liquid, including human milk substitutes or solids.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of infants who received human milk (including expressed or donor milk) and water, water-based drinks, fruit juice, ritual fluids, or any other liquid, including human milk substitutes or solids.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of liveborn infants born at the specified facility within the given time frame.</li></ul>"
 ),
 nbrst_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of postpartum patients who delivered at least one liveborn infant, intended to breast/chestfeed (as recorded in the PNR or health record), but whose infant received no human milk. For multiple births, NSAPD refers to the last born.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of postpartum patients with at least one liveborn infant and a documented intention to breast/chestfeed, whose infant received no human milk.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of postpartum patients with at least one liveborn infant and a documented intention to breast/chestfeed.</li></ul>"
 ),
 brstinit_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of postpartum patients who delivered at least one liveborn infant, where the infant received any human milk from birth to discharge.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of postpartum patients with at least one liveborn infant whose infant received any human milk from birth to discharge.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of postpartum patients with at least one liveborn infant.</li></ul>"
 ),
 icu_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of patients who were admitted or transferred to ICU during pregnancy or up to 6 weeks following birth of their infant.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Numerator:</b> Number of patients who were admitted or transferred to ICU during pregnancy or up to 6 weeks following birth of their infant.</li>",
  "<br>",
  "<li style='text-align:left;'> <b>Denominator:</b> Number of postpartum patients.</li></ul>"
 ),
 smm_rate = paste0(
  "<ul> <li style='text-align:left;'> <b>Definition:</b> Proportion of disease-specific, intervention-specific, or organ dysfunction-based morbidities following birth.<br> Specific morbidities include: SPE, HELLP, eclampsia, severe haemorrhage, surgical complications, hysterectomy, sepsis, embolism, shock, DIC, assisted ventilation, cardiac conditions, acute renal failure, severe uterine rupture, and cerebrovascular accidents.</li>",
  "<br>",
  "<li style='text-align:left;'><b>Numerator:</b> Number of disease-specific, intervention-specific, or organ dysfunction-based morbidities associated with delivery.<br> Specific morbidities include: SPE, HELLP, eclampsia, severe haemorrhage, surgical complications, hysterectomy, sepsis, embolism, shock, DIC, assisted ventilation, cardiac conditions, acute renal failure, severe uterine rupture, and cerebrovascular accidents.</li>",
  "<br>",
  "<li style='text-align:left;'><b>Denominator:</b> Number of postpartum patients.</li></ul>"
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

## Footer ----

rcp_legal <- tags$span(
 HTML("&copy; "),  # This generates the copyright symbol
 format(Sys.Date(), "%Y"),  # Displays the current year dynamically
 tags$a(
  href = "https://rcp.nshealth.ca/",
  target = "_blank",
  rel = "nofollow noreferrer",
  "Reproductive Care Program of Nova Scotia"
 ),
 # tags$img(
 #  src = "logo/rcp-logo-transparent.svg",
 #  alt = "RCP logo",
 #  style = "height:20px; margin-left: 10px;"
 # ),
 style = "float:right !important; padding-right: 30px; font-size: 0.75rem;text-decoration: none;"
 )

