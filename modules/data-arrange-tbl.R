#-----------------
# Table stats ----
#-----------------

##-------------------
## Calendar year ----
##-------------------

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
   startsWith(tolower(index_rate),"sknsknvg_rate") ~ "Skin to Skin following Vaginal birth",
   startsWith(tolower(index_rate),"sknskncs_rate") ~ "Skin to Skin following Caesarean birth",
   startsWith(tolower(index_rate),"neoreadm") ~ "Neonatal Readmission",
   startsWith(tolower(index_rate),"excbrst") ~ "Exclusive Breastfeeding",
   startsWith(tolower(index_rate),"nexcbrst") ~ "Non-exclusive Breastfeeding",
   startsWith(tolower(index_rate),"nbrst") ~ "No Breastfeeding",
   startsWith(tolower(index_rate),"brstinit") ~ "Breastfeeding Initiation",
   startsWith(tolower(index_rate),"anemia") ~ "Anemia",
   startsWith(tolower(index_rate),"spt") ~ "Severe Perineal Trauma with Spontaneous Vaginal Birth",
   startsWith(tolower(index_rate),"med") ~ "Mediolateral Episiotomy with Operative Vaginal Birth",
   startsWith(tolower(index_rate),"sptassvgmed_rate") ~ "Severe Perineal Trauma with Operative Vaginal Birth and mediolateral episiotomy",
   startsWith(tolower(index_rate),"sptassvgnmed_rate") ~ "Severe Perineal Trauma with Operative Vaginal Birth without mediolateral episiotomy",
   startsWith(tolower(index_rate),"pphbl") ~ "Postpartum hemorrhage treated with a blood transfusion",
   startsWith(tolower(index_rate),"pphpi") ~ "Postpartum hemorrhage resulting in procedural intervention",
   startsWith(tolower(index_rate),"pphiv") ~ "Postpartum blood products amongst those who received antenatal iron therapy",
   startsWith(tolower(index_rate),"pretster") ~ "Preterm infants who received steroids prior to birth ",
   startsWith(tolower(index_rate),"pretnnicu") ~ "Preterm infants born in facility without NICU",
   startsWith(tolower(index_rate),"newlwcs") ~ "Newborn respiratory distress associated with low-risk repeat Caesarean at term gestation",
   startsWith(tolower(index_rate),"delcord_rate") ~ "Delayed Cord Clamping",
   startsWith(tolower(index_rate),"delcordterm_rate") ~ "Delayed Cord Clamping amongst Term Newborns",
   startsWith(tolower(index_rate),"delcordtermvg_rate") ~ "Delayed Cord Clamping amongst Term Newborns after Spontaneous Vaginal Birth",
   startsWith(tolower(index_rate),"delcordtermcs_rate") ~ "Delayed Cord Clamping amongst Term Newborns after Caesarean Birth",
   startsWith(tolower(index_rate),"delcordpreterm_rate") ~ "Delayed Cord Clamping amongst Preterm Newborns",
   startsWith(tolower(index_rate),"delcordpretermvg_rate") ~ "Delayed Cord Clamping amongst Preterm Newborns after Spontaneous Vaginal Birth",
   startsWith(tolower(index_rate),"delcordpretermcs_rate") ~ "Delayed Cord Clamping amongst Preterm Newborns after Caesarean Birth",
   startsWith(tolower(index_rate),"icu") ~ "ICU Admission during Pregnancy or Postpartum",
   startsWith(tolower(index_rate),"smm") ~ "Severe Maternal Morbidity in Pregnancy or Postpartum"
  ),
  cond = case_when(
   startsWith(tolower(index_rate),"sknskn_rate") ~ "positive",
   startsWith(tolower(index_rate),"sknsknvg_rate") ~ "positive",
   startsWith(tolower(index_rate),"sknskncs_rate") ~ "positive",
   startsWith(tolower(index_rate),"excbrst") ~ "positive",
   startsWith(tolower(index_rate),"nexcbrst") ~ "positive",
   startsWith(tolower(index_rate),"brstinit") ~ "positive",
   startsWith(tolower(index_rate),"delcord_rate") ~ "positive",
   startsWith(tolower(index_rate),"delcordterm_rate") ~ "positive",
   startsWith(tolower(index_rate),"delcordtermvg_rate") ~ "positive",
   startsWith(tolower(index_rate),"delcordtermcs_rate") ~ "positive",
   startsWith(tolower(index_rate),"delcordpreterm_rate") ~ "positive",
   startsWith(tolower(index_rate),"delcordpretermvg_rate") ~ "positive",
   startsWith(tolower(index_rate),"delcordpretermcs_rate") ~ "positive",
   TRUE ~ "negative"
  )
 ) %>%
 relocate(name, .after = "BrthYear") %>%
 relocate(cond, .after = "rate") %>%
 relocate(index_rate, .after = "delta10") %>%
 mutate(
  status = case_when(
   delta > 0 ~ "Increased",
   delta < 0 ~ "Decreased",
   TRUE ~ "Stable"
  ),
  icon_colors = case_when(
   (tolower(cond) %in% "positive" & delta > 0) |
    (tolower(cond) %in% "negative" & delta < 0)~ "#44AD99",
   (tolower(cond) %in% "negative" & delta > 0) |
    (tolower(cond) %in% "positive" & delta < 0) ~ "#D9715F",
   TRUE ~ "#F2C577"
  ),
  icon10_colors = case_when(
   (tolower(cond) %in% "positive" & delta10 > 0) |
    (tolower(cond) %in% "negative" & delta10 < 0)~ "#44AD99",
   (tolower(cond) %in% "negative" & delta10 > 0) |
    (tolower(cond) %in% "positive" & delta10 < 0) ~ "#D9715F",
   TRUE ~ "#F2C577"
  ),
  category = case_when(
   startsWith(tolower(index_rate),"prehyp")  ~ "Antenatal",
   startsWith(tolower(index_rate),"gesthyp") ~ "Antenatal",
   startsWith(tolower(index_rate),"hyp") ~ "Antenatal",
   startsWith(tolower(index_rate),"anemia") ~ "Antenatal",
   startsWith(tolower(index_rate),"prediab") ~ "Antenatal",
   startsWith(tolower(index_rate),"gestdiab") ~ "Antenatal",
   startsWith(tolower(index_rate),"diab") ~ "Antenatal",
   startsWith(tolower(index_rate),"spt") ~ "Intrapartum",
   startsWith(tolower(index_rate),"med") ~ "Intrapartum",
   startsWith(tolower(index_rate),"sptassvgmed_rate") ~ "Intrapartum",
   startsWith(tolower(index_rate),"sptassvgnmed_rate") ~ "Intrapartum",
   startsWith(tolower(index_rate),"rbs1") ~ "Intrapartum",
   startsWith(tolower(index_rate),"rbs21") ~ "Intrapartum",
   startsWith(tolower(index_rate),"rbs51") ~ "Intrapartum",
   startsWith(tolower(index_rate),"pphbl") ~ "Postpartum",
   startsWith(tolower(index_rate),"pphpi") ~ "Postpartum",
   startsWith(tolower(index_rate),"pphiv") ~ "Postpartum",
   startsWith(tolower(index_rate),"ppreadm") ~ "Postpartum",
   startsWith(tolower(index_rate),"sknskn_rate") ~ "Postpartum",
   startsWith(tolower(index_rate),"sknsknvg_rate") ~ "Postpartum",
   startsWith(tolower(index_rate),"sknskncs_rate") ~ "Postpartum",
   startsWith(tolower(index_rate),"neoreadm") ~ "Newborn",
   startsWith(tolower(index_rate),"pretster") ~ "Newborn",
   startsWith(tolower(index_rate),"pretnnicu") ~ "Newborn",
   startsWith(tolower(index_rate),"newlwcs") ~ "Newborn",
   startsWith(tolower(index_rate),"delcord_rate") ~ "Newborn",
   startsWith(tolower(index_rate),"delcordterm_rate") ~ "Newborn",
   startsWith(tolower(index_rate),"delcordtermvg_rate") ~ "Newborn",
   startsWith(tolower(index_rate),"delcordtermcs_rate") ~ "Newborn",
   startsWith(tolower(index_rate),"delcordpreterm_rate") ~ "Newborn",
   startsWith(tolower(index_rate),"delcordpretermvg_rate") ~ "Newborn",
   startsWith(tolower(index_rate),"delcordpretermcs_rate") ~ "Newborn",
   startsWith(tolower(index_rate),"excbrst") ~ "Infant Feeding",
   startsWith(tolower(index_rate),"nexcbrst") ~ "Infant Feeding",
   startsWith(tolower(index_rate),"nbrst") ~ "Infant Feeding",
   startsWith(tolower(index_rate),"brstinit") ~ "Infant Feeding",
   startsWith(tolower(index_rate),"icu") ~ "Other",
   startsWith(tolower(index_rate),"smm") ~ "Other"
  )
 )

##-----------------
## Fiscal year ----
##-----------------

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
   startsWith(tolower(index_rate),"sknsknvg_rate") ~ "Skin to Skin following Vaginal birth",
   startsWith(tolower(index_rate),"sknskncs_rate") ~ "Skin to Skin following Caesarean birth",
   startsWith(tolower(index_rate),"neoreadm") ~ "Neonatal Readmission",
   startsWith(tolower(index_rate),"excbrst") ~ "Exclusive Breastfeeding",
   startsWith(tolower(index_rate),"nexcbrst") ~ "Non-exclusive Breastfeeding",
   startsWith(tolower(index_rate),"nbrst") ~ "No Breastfeeding",
   startsWith(tolower(index_rate),"brstinit") ~ "Breastfeeding Initiation",
   startsWith(tolower(index_rate),"anemia") ~ "Anemia",
   startsWith(tolower(index_rate),"spt") ~ "Severe Perineal Trauma with Spontaneous Vaginal Birth",
   startsWith(tolower(index_rate),"med") ~ "Mediolateral Episiotomy with Operative Vaginal Birth",
   startsWith(tolower(index_rate),"sptassvgmed_rate") ~ "Severe Perineal Trauma with Operative Vaginal Birth and mediolateral episiotomy",
   startsWith(tolower(index_rate),"sptassvgnmed_rate") ~ "Severe Perineal Trauma with Operative Vaginal Birth without mediolateral episiotomy",
   startsWith(tolower(index_rate),"pphbl") ~ "Postpartum hemorrhage treated with a blood transfusion",
   startsWith(tolower(index_rate),"pphpi") ~ "Postpartum hemorrhage resulting in procedural intervention",
   startsWith(tolower(index_rate),"pphiv") ~ "Postpartum blood products amongst those who received antenatal iron therapy",
   startsWith(tolower(index_rate),"pretster") ~ "Preterm infants who received steroids prior to birth ",
   startsWith(tolower(index_rate),"pretnnicu") ~ "Preterm infants born in facility without NICU",
   startsWith(tolower(index_rate),"newlwcs") ~ "Newborn respiratory distress associated with low-risk repeat Caesarean at term gestation",
   startsWith(tolower(index_rate),"delcord_rate") ~ "Delayed Cord Clamping",
   startsWith(tolower(index_rate),"delcordterm_rate") ~ "Delayed Cord Clamping amongst Term Newborns",
   startsWith(tolower(index_rate),"delcordtermvg_rate") ~ "Delayed Cord Clamping amongst Term Newborns after Spontaneous Vaginal Birth",
   startsWith(tolower(index_rate),"delcordtermcs_rate") ~ "Delayed Cord Clamping amongst Term Newborns after Caesarean Birth",
   startsWith(tolower(index_rate),"delcordpreterm_rate") ~ "Delayed Cord Clamping amongst Preterm Newborns",
   startsWith(tolower(index_rate),"delcordpretermvg_rate") ~ "Delayed Cord Clamping amongst Preterm Newborns after Spontaneous Vaginal Birth",
   startsWith(tolower(index_rate),"delcordpretermcs_rate") ~ "Delayed Cord Clamping amongst Preterm Newborns after Caesarean Birth",
   startsWith(tolower(index_rate),"icu") ~ "ICU Admission during Pregnancy or Postpartum",
   startsWith(tolower(index_rate),"smm") ~ "Severe Maternal Morbidity in Pregnancy or Postpartum"
  ),
  cond = case_when(
   startsWith(tolower(index_rate),"sknskn_rate") ~ "positive",
   startsWith(tolower(index_rate),"sknsknvg_rate") ~ "positive",
   startsWith(tolower(index_rate),"sknskncs_rate") ~ "positive",
   startsWith(tolower(index_rate),"excbrst") ~ "positive",
   startsWith(tolower(index_rate),"nexcbrst") ~ "positive",
   startsWith(tolower(index_rate),"brstinit") ~ "positive",
   startsWith(tolower(index_rate),"delcord_rate") ~ "positive",
   startsWith(tolower(index_rate),"delcordterm_rate") ~ "positive",
   startsWith(tolower(index_rate),"delcordtermvg_rate") ~ "positive",
   startsWith(tolower(index_rate),"delcordtermcs_rate") ~ "positive",
   startsWith(tolower(index_rate),"delcordpreterm_rate") ~ "positive",
   startsWith(tolower(index_rate),"delcordpretermvg_rate") ~ "positive",
   startsWith(tolower(index_rate),"delcordpretermcs_rate") ~ "positive",
   TRUE ~ "negative"
  ),
  status = case_when(
   delta > 0 ~ "Increased",
   delta < 0 ~ "Decreased",
   TRUE ~ "Stable"
  )
 ) %>%
 relocate(name, .after = "FiscalYear") %>%
 relocate(cond, .after = "rate") %>%
 relocate(index_rate, .after = "delta10") %>%
 mutate(
  icon_colors = case_when(
   (tolower(cond) %in% "positive" & delta > 0) |
    (tolower(cond) %in% "negative" & delta < 0)~ "#44AD99",
   (tolower(cond) %in% "negative" & delta > 0) |
    (tolower(cond) %in% "positive" & delta < 0) ~ "#D9715F",
   TRUE ~ "#F2C577"
  ),
  icon10_colors = case_when(
   (tolower(cond) %in% "positive" & delta10 > 0) |
    (tolower(cond) %in% "negative" & delta10 < 0)~ "#44AD99",
   (tolower(cond) %in% "negative" & delta10 > 0) |
    (tolower(cond) %in% "positive" & delta10 < 0) ~ "#D9715F",
   TRUE ~ "#F2C577"
  ),
  category = case_when(
   startsWith(tolower(index_rate),"prehyp")  ~ "Antenatal",
   startsWith(tolower(index_rate),"gesthyp") ~ "Antenatal",
   startsWith(tolower(index_rate),"hyp") ~ "Antenatal",
   startsWith(tolower(index_rate),"anemia") ~ "Antenatal",
   startsWith(tolower(index_rate),"prediab") ~ "Antenatal",
   startsWith(tolower(index_rate),"gestdiab") ~ "Antenatal",
   startsWith(tolower(index_rate),"diab") ~ "Antenatal",
   startsWith(tolower(index_rate),"spt") ~ "Intrapartum",
   startsWith(tolower(index_rate),"med") ~ "Intrapartum",
   startsWith(tolower(index_rate),"sptassvgmed_rate") ~ "Intrapartum",
   startsWith(tolower(index_rate),"sptassvgnmed_rate") ~ "Intrapartum",
   startsWith(tolower(index_rate),"rbs1") ~ "Intrapartum",
   startsWith(tolower(index_rate),"rbs21") ~ "Intrapartum",
   startsWith(tolower(index_rate),"rbs51") ~ "Intrapartum",
   startsWith(tolower(index_rate),"pphbl") ~ "Postpartum",
   startsWith(tolower(index_rate),"pphpi") ~ "Postpartum",
   startsWith(tolower(index_rate),"pphiv") ~ "Postpartum",
   startsWith(tolower(index_rate),"ppreadm") ~ "Postpartum",
   startsWith(tolower(index_rate),"sknskn_rate") ~ "Postpartum",
   startsWith(tolower(index_rate),"sknsknvg_rate") ~ "Postpartum",
   startsWith(tolower(index_rate),"sknskncs_rate") ~ "Postpartum",
   startsWith(tolower(index_rate),"neoreadm") ~ "Newborn",
   startsWith(tolower(index_rate),"pretster") ~ "Newborn",
   startsWith(tolower(index_rate),"pretnnicu") ~ "Newborn",
   startsWith(tolower(index_rate),"newlwcs") ~ "Newborn",
   startsWith(tolower(index_rate),"delcord_rate") ~ "Newborn",
   startsWith(tolower(index_rate),"delcordterm_rate") ~ "Newborn",
   startsWith(tolower(index_rate),"delcordtermvg_rate") ~ "Newborn",
   startsWith(tolower(index_rate),"delcordtermcs_rate") ~ "Newborn",
   startsWith(tolower(index_rate),"delcordpreterm_rate") ~ "Newborn",
   startsWith(tolower(index_rate),"delcordpretermvg_rate") ~ "Newborn",
   startsWith(tolower(index_rate),"delcordpretermcs_rate") ~ "Newborn",
   startsWith(tolower(index_rate),"excbrst") ~ "Infant Feeding",
   startsWith(tolower(index_rate),"nexcbrst") ~ "Infant Feeding",
   startsWith(tolower(index_rate),"nbrst") ~ "Infant Feeding",
   startsWith(tolower(index_rate),"brstinit") ~ "Infant Feeding",
   startsWith(tolower(index_rate),"icu") ~ "Other",
   startsWith(tolower(index_rate),"smm") ~ "Other"
  )
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
