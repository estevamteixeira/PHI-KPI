##-------------------
## Calendar year ----
##-------------------

### Census division (CD) ----
#### Hypertension ----

c1 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, PreExisting_Hypertension, CDuid) %>%
 distinct() %>%
 group_by(BrthYear, PreExisting_Hypertension, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(PreExisting_Hypertension, CDuid),
                 fill = list(
                  prehyp_count = 0,
                  prehyp_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(PreExisting_Hypertension, BrthYear),
                 fill = list(
                  prehyp_count = 0,
                  prehyp_rate = NA
                 )) %>%
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
  CDuid = as.integer(CDuid),
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
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(Gestational_Hypertension, CDuid),
                 fill = list(
                  gesthyp_count = 0,
                  gesthyp_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(Gestational_Hypertension, BrthYear),
                 fill = list(
                  gesthyp_count = 0,
                  gesthyp_rate = NA
                 )) %>%
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
  CDuid = as.integer(CDuid),
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
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(Any_Hypertension, CDuid),
                 fill = list(
                  hyp_count = 0,
                  hyp_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(Any_Hypertension, BrthYear),
                 fill = list(
                  hyp_count = 0,
                  hyp_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  hyp_delta = (hyp_rate - lag(hyp_rate))/lag(hyp_rate),
  hyp_delta = ifelse(BrthYear == 2014, NA, hyp_delta),
  hyp_deltap = (hyp_rate - lag(hyp_rate,9))/lag(hyp_rate,9)
 ) %>%
 ungroup()

#### Anaemia ----

c4 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, CDuid, MANEM, R014_01500, MO990, MD50_D53, MD55_D59, MD60_D64) %>%
 mutate(anemia = case_when(
  MANEM > 0 | R014_01500 > 0 | MO990 > 0 | MD50_D53 > 0 |
   MD55_D59 > 0 | MD60_D64 > 0 ~ 1,
  TRUE ~ NA_integer_
 )) %>%
 distinct() %>%
 group_by(BrthYear, anemia, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
  anemia_count = n()
 ) %>%
 group_by(BrthYear, CDuid) %>%
 mutate(
  total_year = n(),
  anemia_rate = anemia_count/total_year
 ) %>%
 filter(anemia %in% 1,
        !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, anemia, anemia_count, anemia_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(anemia, CDuid),
                 fill = list(
                  anemia_count = 0,
                  anemia_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(anemia, BrthYear),
                 fill = list(
                  anemia_count = 0,
                  anemia_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  anemia_delta = (anemia_rate - lag(anemia_rate))/lag(anemia_rate),
  anemia_delta = ifelse(BrthYear == 2014, NA, anemia_delta),
  anemia_deltap = (anemia_rate - lag(anemia_rate,9))/lag(anemia_rate,9)
 ) %>%
 ungroup()

#### Diabetes ----

c5 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, PreExisting_Diabetes, CDuid) %>%
 distinct() %>%
 group_by(BrthYear, PreExisting_Diabetes, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(PreExisting_Diabetes, CDuid),
                 fill = list(
                  prediab_count = 0,
                  prediab_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(PreExisting_Diabetes, BrthYear),
                 fill = list(
                  prediab_count = 0,
                  prediab_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  prediab_delta = (prediab_rate - lag(prediab_rate))/lag(prediab_rate),
  prediab_delta = ifelse(BrthYear == 2014, NA, prediab_delta),
  prediab_deltap = (prediab_rate - lag(prediab_rate,9))/lag(prediab_rate,9)
 ) %>%
 ungroup()

c6 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, GDM, CDuid) %>%
 distinct() %>%
 group_by(BrthYear, GDM, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(GDM, CDuid),
                 fill = list(
                  gestdiab_count = 0,
                  gestdiab_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(GDM, BrthYear),
                 fill = list(
                  gestdiab_count = 0,
                  gestdiab_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  gestdiab_delta = (gestdiab_rate - lag(gestdiab_rate))/lag(gestdiab_rate),
  gestdiab_delta = ifelse(BrthYear == 2014, NA, gestdiab_delta),
  gestdiab_deltap = (gestdiab_rate - lag(gestdiab_rate,9))/lag(gestdiab_rate,9)
 ) %>%
 ungroup()

c7 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, Any_Diabetes, CDuid) %>%
 distinct() %>%
 group_by(BrthYear, Any_Diabetes, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(Any_Diabetes, CDuid),
                 fill = list(
                  diab_count = 0,
                  diab_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(Any_Diabetes, BrthYear),
                 fill = list(
                  diab_count = 0,
                  diab_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  diab_delta = (diab_rate - lag(diab_rate))/lag(diab_rate),
  diab_delta = ifelse(BrthYear == 2014, NA, diab_delta),
  diab_deltap = (diab_rate - lag(diab_rate,9))/lag(diab_rate,9)
 ) %>%
 ungroup()

#### Severe Perineal Trauma with Spontaneous Vaginal Birth ----

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
        DMMETHOD,
        CDuid) %>%
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
 group_by(BrthYear, trdfrth, sptvg, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
  spt_count = n()
 ) %>%
 group_by(BrthYear, sptvg, CDuid) %>%
 mutate(
  total_year = n(),
  spt_rate = spt_count/total_year
 ) %>%
 ungroup() %>%
 filter(trdfrth %in% 1, sptvg %in% 1, !CDuid %in% 1200) %>%
 select(BrthYear,CDuid,sptvg,spt_count,spt_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sptvg, CDuid),
                 fill = list(
                  spt_count = 0,
                  spt_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(sptvg, BrthYear),
                 fill = list(
                  spt_count = 0,
                  spt_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  spt_delta = (spt_rate - lag(spt_rate))/lag(spt_rate),
  spt_delta = ifelse(BrthYear == 2014, NA, spt_delta),
  spt_deltap = (spt_rate - lag(spt_rate,9))/lag(spt_rate,9)
 ) %>%
 ungroup()

#### Mediolateral Episiotomy with Operative Vaginal Birth ----
#### ND = Not Done
#### MD = Midline
#### ML = Mediolateral

c9 <- dta %>%
 select(BrthYear,
        DMMETHOD,
        DMEPISIO,
        CDuid) %>%
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
 group_by(BrthYear, med, assvg, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
  med_count = n()
 ) %>%
 group_by(BrthYear, assvg, CDuid) %>%
 mutate(
  total_year = n(),
  med_rate = med_count/total_year
 ) %>%
 ungroup() %>%
 filter(med %in% 1, assvg %in% 1, !CDuid %in% 1200) %>%
 select(BrthYear,CDuid,med,med_count,med_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(med, CDuid),
                 fill = list(
                  med_count = 0,
                  med_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(med, BrthYear),
                 fill = list(
                  med_count = 0,
                  med_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  med_delta = (med_rate - lag(med_rate))/lag(med_rate),
  med_delta = ifelse(BrthYear == 2014, NA, med_delta),
  med_deltap = (med_rate - lag(med_rate,9))/lag(med_rate,9)
 ) %>%
 ungroup()

#### Severe perineal trauma with Operative Vaginal Birth and mediolateral episiotomy ----

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
        DMEPISIO,
        CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(BrthYear, trdfrth, assvg, med, CDuid) %>%
 mutate(
  sptassvgmed_count = n()
 ) %>%
 group_by(BrthYear, assvg, CDuid) %>%
 mutate(
  total_year = n(),
  sptassvgmed_rate = sptassvgmed_count/total_year
 ) %>%
 ungroup() %>%
 filter(sptassvgmed %in% 1, !CDuid %in% 1200) %>%
 select(BrthYear,CDuid,sptassvgmed,sptassvgmed_count,sptassvgmed_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sptassvgmed, CDuid),
                 fill = list(
                  sptassvgmed_count = 0,
                  sptassvgmed_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(sptassvgmed, BrthYear),
                 fill = list(
                  sptassvgmed_count = 0,
                  sptassvgmed_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  sptassvgmed_delta = (sptassvgmed_rate - lag(sptassvgmed_rate))/lag(sptassvgmed_rate),
  sptassvgmed_delta = ifelse(BrthYear == 2014, NA, sptassvgmed_delta),
  sptassvgmed_deltap = (sptassvgmed_rate - lag(sptassvgmed_rate,9))/lag(sptassvgmed_rate,9)
 ) %>%
 ungroup()

#### Severe perineal trauma with Operative Vaginal Birth without mediolateral episiotomy ----

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
        DMEPISIO,
        CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(BrthYear, trdfrth, assvg, med, CDuid) %>%
 mutate(
  sptassvgnmed_count = n()
 ) %>%
 group_by(BrthYear, assvg, CDuid) %>%
 mutate(
  total_year = n(),
  sptassvgnmed_rate = sptassvgnmed_count/total_year
 ) %>%
 ungroup() %>%
 filter(sptassvgnmed %in% 1, !CDuid %in% 1200) %>%
 select(BrthYear,CDuid,sptassvgnmed,sptassvgnmed_count,sptassvgnmed_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sptassvgnmed, CDuid),
                 fill = list(
                  sptassvgnmed_count = 0,
                  sptassvgnmed_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(sptassvgnmed, BrthYear),
                 fill = list(
                  sptassvgnmed_count = 0,
                  sptassvgnmed_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  sptassvgnmed_delta = (sptassvgnmed_rate - lag(sptassvgnmed_rate))/lag(sptassvgnmed_rate),
  sptassvgnmed_delta = ifelse(BrthYear == 2014, NA, sptassvgnmed_delta),
  sptassvgnmed_deltap = (sptassvgnmed_rate - lag(sptassvgnmed_rate,9))/lag(sptassvgnmed_rate,9)
 ) %>%
 ungroup()

#### Robson group ----

c12 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs1, RobsnGrp, CDuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs1, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(rbs1, CDuid),
                 fill = list(
                  rbs1_count = 0,
                  rbs1_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(rbs1, BrthYear),
                 fill = list(
                  rbs1_count = 0,
                  rbs1_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  rbs1_delta = (rbs1_rate - lag(rbs1_rate))/lag(rbs1_rate),
  rbs1_delta = ifelse(BrthYear == 2014, NA, rbs1_delta),
  rbs1_deltap = (rbs1_rate - lag(rbs1_rate,9))/lag(rbs1_rate,9)
 ) %>%
 ungroup()

c13 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs21, RobsnGrp, CDuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs21, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(rbs21, CDuid),
                 fill = list(
                  rbs21_count = 0,
                  rbs21_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(rbs21, BrthYear),
                 fill = list(
                  rbs21_count = 0,
                  rbs21_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  rbs21_delta = (rbs21_rate - lag(rbs21_rate))/lag(rbs21_rate),
  rbs21_delta = ifelse(BrthYear == 2014, NA, rbs21_delta),
  rbs21_deltap = (rbs21_rate - lag(rbs21_rate,9))/lag(rbs21_rate,9)
 ) %>%
 ungroup()

c14 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs51, RobsnGrp, CDuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs51, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(rbs51, CDuid),
                 fill = list(
                  rbs51_count = 0,
                  rbs51_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(rbs51, BrthYear),
                 fill = list(
                  rbs51_count = 0,
                  rbs51_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  rbs51_delta = (rbs51_rate - lag(rbs51_rate))/lag(rbs51_rate),
  rbs51_delta = ifelse(BrthYear == 2014, NA, rbs51_delta),
  rbs51_deltap = (rbs51_rate - lag(rbs51_rate,9))/lag(rbs51_rate,9)
 ) %>%
 ungroup()

#### Postpartum hemorrhage treated with a blood transfusion ----

c15 <- dta %>%
 select(BrthYear, Any_Blood_Product, Postpartum_Haemorrhage, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
  pphbl = case_when(
   Any_Blood_Product > 0 & Postpartum_Haemorrhage > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, Any_Blood_Product, Postpartum_Haemorrhage, CDuid) %>%
 mutate(
  pphbl_count = n()
 ) %>%
 group_by(BrthYear, CDuid) %>%
 mutate(
  total_year = n(),
  pphbl_rate = pphbl_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Blood_Product %in% 1, Postpartum_Haemorrhage %in% 1,
        !CDuid %in% 1200) %>%
 select(BrthYear,CDuid,pphbl, pphbl_count, pphbl_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pphbl, CDuid),
                 fill = list(
                  pphbl_count = 0,
                  pphbl_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(pphbl, BrthYear),
                 fill = list(
                  pphbl_count = 0,
                  pphbl_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  pphbl_delta = (pphbl_rate - lag(pphbl_rate))/lag(pphbl_rate),
  pphbl_delta = ifelse(BrthYear == 2014, NA, pphbl_delta),
  pphbl_deltap = (pphbl_rate - lag(pphbl_rate,9))/lag(pphbl_rate,9)
 ) %>%
 ungroup()

#### Postpartum hemorrhage resulting in procedural intervention ----

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
        MODEDEL,
        CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(BrthYear, pphpi, Postpartum_Haemorrhage, CDuid) %>%
 mutate(
  pphpi_count = n()
 ) %>%
 group_by(BrthYear, Postpartum_Haemorrhage, CDuid) %>%
 mutate(
  total_year = n(),
  pphpi_rate = pphpi_count/total_year
 ) %>%
 ungroup() %>%
 filter(pphpi %in% 1, Postpartum_Haemorrhage %in% 1,
        !CDuid %in% 1200) %>%
 select(BrthYear,CDuid,pphpi, pphpi_count, pphpi_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pphpi, CDuid),
                 fill = list(
                  pphpi_count = 0,
                  pphpi_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(pphpi, BrthYear),
                 fill = list(
                  pphpi_count = 0,
                  pphpi_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  pphpi_delta = (pphpi_rate - lag(pphpi_rate))/lag(pphpi_rate),
  pphpi_delta = ifelse(BrthYear == 2014, NA, pphpi_delta),
  pphpi_deltap = (pphpi_rate - lag(pphpi_rate,9))/lag(pphpi_rate,9)
 ) %>%
 ungroup()

#### Postpartum readmission ----

c17 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, ppreadm, CDuid) %>%
 distinct() %>%
 group_by(BrthYear, ppreadm, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(ppreadm, CDuid),
                 fill = list(
                  ppreadm_count = 0,
                  ppreadm_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(ppreadm, BrthYear),
                 fill = list(
                  ppreadm_count = 0,
                  ppreadm_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  ppreadm_delta = (ppreadm_rate - lag(ppreadm_rate))/lag(ppreadm_rate),
  ppreadm_delta = ifelse(BrthYear == 2014, NA, ppreadm_delta),
  ppreadm_deltap = (ppreadm_rate - lag(ppreadm_rate,9))/lag(ppreadm_rate,9)
 ) %>%
 ungroup()

#### Postpartum patients who received medical anticoagulation prophylaxis when indicated ----

#c18

#### Skin to skin ----

c19 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknskn, CDuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, sknskn, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sknskn, CDuid),
                 fill = list(
                  sknskn_count = 0,
                  sknskn_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(sknskn, BrthYear),
                 fill = list(
                  sknskn_count = 0,
                  sknskn_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  sknskn_delta = (sknskn_rate - lag(sknskn_rate))/lag(sknskn_rate),
  sknskn_delta = ifelse(BrthYear == 2014, NA, sknskn_delta),
  sknskn_deltap = (sknskn_rate - lag(sknskn_rate,9))/lag(sknskn_rate,9)
 ) %>%
 ungroup()

c20 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknsknvg, CDuid, lvbvg) %>%
 distinct() %>%
 group_by(BrthYear, sknsknvg, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sknsknvg, CDuid),
                 fill = list(
                  sknsknvg_count = 0,
                  sknsknvg_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(sknsknvg, BrthYear),
                 fill = list(
                  sknsknvg_count = 0,
                  sknsknvg_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  sknsknvg_delta = (sknsknvg_rate - lag(sknsknvg_rate))/lag(sknsknvg_rate),
  sknsknvg_delta = ifelse(BrthYear == 2014, NA, sknsknvg_delta),
  sknsknvg_deltap = (sknsknvg_rate - lag(sknsknvg_rate,9))/lag(sknsknvg_rate,9)
 ) %>%
 ungroup()

c21 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknskncs, CDuid, lvbcs) %>%
 distinct() %>%
 group_by(BrthYear, sknskncs, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sknskncs, CDuid),
                 fill = list(
                  sknskncs_count = 0,
                  sknskncs_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(sknskncs, BrthYear),
                 fill = list(
                  sknskncs_count = 0,
                  sknskncs_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  sknskncs_delta = (sknskncs_rate - lag(sknskncs_rate))/lag(sknskncs_rate),
  sknskncs_delta = ifelse(BrthYear == 2014, NA, sknskncs_delta),
  sknskncs_deltap = (sknskncs_rate - lag(sknskncs_rate,9))/lag(sknskncs_rate,9)
 ) %>%
 ungroup()

#### Postpartum blood products amongst those who received antenatal iron therapy ----

c22 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
  pphiv = case_when(
   Any_Blood_Product > 0 & R003_02100 > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, Any_Blood_Product, R003_02100, CDuid) %>%
 mutate(
  pphiv_count = n()
 ) %>%
 group_by(BrthYear, Any_Blood_Product, CDuid) %>%
 mutate(
  total_year = n(),
  pphiv_rate = pphiv_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Blood_Product %in% 1, R003_02100 %in% 1,
        !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, pphiv, pphiv_count, pphiv_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pphiv, CDuid),
                 fill = list(
                  pphiv_count = 0,
                  pphiv_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(pphiv, BrthYear),
                 fill = list(
                  pphiv_count = 0,
                  pphiv_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  pphiv_delta = (pphiv_rate - lag(pphiv_rate))/lag(pphiv_rate),
  pphiv_delta = ifelse(BrthYear == 2014, NA, pphiv_delta),
  pphiv_deltap = (pphiv_rate - lag(pphiv_rate,9))/lag(pphiv_rate,9)
 ) %>%
 ungroup()

#### Neonatal readmission ----

c23 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, neoreadm, CDuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, neoreadm, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(neoreadm, CDuid),
                 fill = list(
                  neoreadm_count = 0,
                  neoreadm_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(neoreadm, BrthYear),
                 fill = list(
                  neoreadm_count = 0,
                  neoreadm_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  neoreadm_delta = (neoreadm_rate - lag(neoreadm_rate))/lag(neoreadm_rate),
  neoreadm_delta = ifelse(BrthYear == 2014, NA, neoreadm_delta),
  neoreadm_deltap = (neoreadm_rate - lag(neoreadm_rate,9))/lag(neoreadm_rate,9)
 ) %>%
 ungroup()

#### Preterm infants who received complete course of steroids > 24h and < 7 days prior to birth ----

c24 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(BrthYear, pretster, CDuid) %>%
 mutate(
  pretster_count = n()
 ) %>%
 group_by(BrthYear, den, CDuid) %>%
 mutate(
  total_year = n(),
  pretster_rate = pretster_count/total_year
 ) %>%
 ungroup() %>%
 filter(pretster %in% 1, !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, pretster, pretster_count,pretster_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pretster, CDuid),
                 fill = list(
                  pretster_count = 0,
                  pretster_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(pretster, BrthYear),
                 fill = list(
                  pretster_count = 0,
                  pretster_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  pretster_delta = (pretster_rate - lag(pretster_rate))/lag(pretster_rate),
  pretster_delta = ifelse(BrthYear == 2014, NA, pretster_delta),
  pretster_deltap = (pretster_rate - lag(pretster_rate,9))/lag(pretster_rate,9)
 ) %>%
 ungroup()

#### Preterm babies born in facility without NICU ----

c25 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(BrthYear, pretnnicu, CDuid) %>%
 mutate(
  pretnnicu_count = n()
 ) %>%
 group_by(BrthYear, den, CDuid) %>%
 mutate(
  total_year = n(),
  pretnnicu_rate = pretnnicu_count/total_year
 ) %>%
 ungroup() %>%
 filter(pretnnicu %in% 1, !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, pretnnicu, pretnnicu_count,pretnnicu_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pretnnicu, CDuid),
                 fill = list(
                  pretnnicu_count = 0,
                  pretnnicu_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(pretnnicu, BrthYear),
                 fill = list(
                  pretnnicu_count = 0,
                  pretnnicu_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  pretnnicu_delta = (pretnnicu_rate - lag(pretnnicu_rate))/lag(pretnnicu_rate),
  pretnnicu_delta = ifelse(BrthYear == 2014, NA, pretnnicu_delta),
  pretnnicu_deltap = (pretnnicu_rate - lag(pretnnicu_rate,9))/lag(pretnnicu_rate,9)
 ) %>%
 ungroup()

#### Newborn respiratory distress associated with low-risk repeat cesarean at term gestation (≥ 37 weeks < 39 weeks) ----

c26 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(BrthYear, newlwcs, CDuid) %>%
 mutate(
  newlwcs_count = n()
 ) %>%
 group_by(BrthYear, den, CDuid) %>%
 mutate(
  total_year = n(),
  newlwcs_rate = newlwcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(newlwcs %in% 1) %>%
 select(BrthYear, CDuid, newlwcs, newlwcs_count,newlwcs_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(newlwcs, CDuid),
                 fill = list(
                  newlwcs_count = 0,
                  newlwcs_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(newlwcs, BrthYear),
                 fill = list(
                  newlwcs_count = 0,
                  newlwcs_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  newlwcs_delta = (newlwcs_rate - lag(newlwcs_rate))/lag(newlwcs_rate),
  newlwcs_delta = ifelse(BrthYear == 2014, NA, newlwcs_delta),
  newlwcs_deltap = (newlwcs_rate - lag(newlwcs_rate,9))/lag(newlwcs_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping  ----

c27 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
  delcord = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (DEL_CORD_CLAMP %in% 3) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, delcord, CDuid) %>%
 mutate(
  delcord_count = n()
 ) %>%
 group_by(BrthYear, lvb, CDuid) %>%
 mutate(
  total_year = n(),
  delcord_rate = delcord_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcord %in% 1, lvb %in% 1,
        !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, delcord, delcord_count,delcord_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcord, CDuid),
                 fill = list(
                  delcord_count = 0,
                  delcord_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(delcord, BrthYear),
                 fill = list(
                  delcord_count = 0,
                  delcord_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  delcord_delta = (delcord_rate - lag(delcord_rate))/lag(delcord_rate),
  delcord_delta = ifelse(BrthYear == 2014, NA, delcord_delta),
  delcord_deltap = (delcord_rate - lag(delcord_rate,9))/lag(delcord_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns ----

c28 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(BrthYear, delcordterm, CDuid) %>%
 mutate(
  delcordterm_count = n()
 ) %>%
 group_by(BrthYear, den, CDuid) %>%
 mutate(
  total_year = n(),
  delcordterm_rate = delcordterm_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordterm %in% 1, !CDuid %in% 1200) %>%
 select(BrthYear,CDuid,delcordterm,delcordterm_count,delcordterm_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordterm, CDuid),
                 fill = list(
                  delcordterm_count = 0,
                  delcordterm_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(delcordterm, BrthYear),
                 fill = list(
                  delcordterm_count = 0,
                  delcordterm_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  delcordterm_delta = (delcordterm_rate - lag(delcordterm_rate))/lag(delcordterm_rate),
  delcordterm_delta = ifelse(BrthYear == 2014, NA, delcordterm_delta),
  delcordterm_deltap = (delcordterm_rate - lag(delcordterm_rate,9))/lag(delcordterm_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns following spontaneous vaginal birth ----

c29 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(BrthYear, delcordtermvg, CDuid) %>%
 mutate(
  delcordtermvg_count = n()
 ) %>%
 group_by(BrthYear, den, CDuid) %>%
 mutate(
  total_year = n(),
  delcordtermvg_rate = delcordtermvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordtermvg %in% 1, !CDuid %in% 1200) %>%
 select(BrthYear,CDuid, delcordtermvg, delcordtermvg_count,delcordtermvg_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordtermvg, CDuid),
                 fill = list(
                  delcordtermvg_count = 0,
                  delcordtermvg_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(delcordtermvg, BrthYear),
                 fill = list(
                  delcordtermvg_count = 0,
                  delcordtermvg_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  delcordtermvg_delta = (delcordtermvg_rate - lag(delcordtermvg_rate))/lag(delcordtermvg_rate),
  delcordtermvg_delta = ifelse(BrthYear == 2014, NA, delcordtermvg_delta),
  delcordtermvg_deltap = (delcordtermvg_rate - lag(delcordtermvg_rate,9))/lag(delcordtermvg_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns following caesarean birth ----

c30 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(BrthYear, delcordtermcs, CDuid) %>%
 mutate(
  delcordtermcs_count = n()
 ) %>%
 group_by(BrthYear, den, CDuid) %>%
 mutate(
  total_year = n(),
  delcordtermcs_rate = delcordtermcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordtermcs %in% 1, !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, delcordtermcs, delcordtermcs_count,delcordtermcs_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordtermcs, CDuid),
                 fill = list(
                  delcordtermcs_count = 0,
                  delcordtermcs_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(delcordtermcs, BrthYear),
                 fill = list(
                  delcordtermcs_count = 0,
                  delcordtermcs_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  delcordtermcs_delta = (delcordtermcs_rate - lag(delcordtermcs_rate))/lag(delcordtermcs_rate),
  delcordtermcs_delta = ifelse(BrthYear == 2014, NA, delcordtermcs_delta),
  delcordtermcs_deltap = (delcordtermcs_rate - lag(delcordtermcs_rate,9))/lag(delcordtermcs_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns ----

c31 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(BrthYear, delcordpreterm, CDuid) %>%
 mutate(
  delcordpreterm_count = n()
 ) %>%
 group_by(BrthYear, den, CDuid) %>%
 mutate(
  total_year = n(),
  delcordpreterm_rate = delcordpreterm_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpreterm %in% 1, !CDuid %in% 1200) %>%
 select(BrthYear,CDuid, delcordpreterm, delcordpreterm_count,delcordpreterm_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordpreterm, CDuid),
                 fill = list(
                  delcordpreterm_count = 0,
                  delcordpreterm_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(delcordpreterm, BrthYear),
                 fill = list(
                  delcordpreterm_count = 0,
                  delcordpreterm_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  delcordpreterm_delta = (delcordpreterm_rate - lag(delcordpreterm_rate))/lag(delcordpreterm_rate),
  delcordpreterm_delta = ifelse(BrthYear == 2014, NA, delcordpreterm_delta),
  delcordpreterm_deltap = (delcordpreterm_rate - lag(delcordpreterm_rate,9))/lag(delcordpreterm_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns following spontaneous vaginal birth ----

c32 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(BrthYear, delcordpretermvg, CDuid) %>%
 mutate(
  delcordpretermvg_count = n()
 ) %>%
 group_by(BrthYear, den, CDuid) %>%
 mutate(
  total_year = n(),
  delcordpretermvg_rate = delcordpretermvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpretermvg %in% 1, !CDuid %in% 1200) %>%
 select(BrthYear,CDuid, delcordpretermvg, delcordpretermvg_count,delcordpretermvg_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordpretermvg, CDuid),
                 fill = list(
                  delcordpretermvg_count = 0,
                  delcordpretermvg_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(delcordpretermvg, BrthYear),
                 fill = list(
                  delcordpretermvg_count = 0,
                  delcordpretermvg_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  delcordpretermvg_delta = (delcordpretermvg_rate - lag(delcordpretermvg_rate))/lag(delcordpretermvg_rate),
  delcordpretermvg_delta = ifelse(BrthYear == 2014, NA, delcordpretermvg_delta),
  delcordpretermvg_deltap = (delcordpretermvg_rate - lag(delcordpretermvg_rate,9))/lag(delcordpretermvg_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns following caesarean birth ----

c33 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(BrthYear, delcordpretermcs, CDuid) %>%
 mutate(
  delcordpretermcs_count = n()
 ) %>%
 group_by(BrthYear, den, CDuid) %>%
 mutate(
  total_year = n(),
  delcordpretermcs_rate = delcordpretermcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpretermcs %in% 1, !CDuid %in% 1200) %>%
 select(BrthYear,CDuid, delcordpretermcs, delcordpretermcs_count,delcordpretermcs_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordpretermcs, CDuid),
                 fill = list(
                  delcordpretermcs_count = 0,
                  delcordpretermcs_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(delcordpretermcs, BrthYear),
                 fill = list(
                  delcordpretermcs_count = 0,
                  delcordpretermcs_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  delcordpretermcs_delta = (delcordpretermcs_rate - lag(delcordpretermcs_rate))/lag(delcordpretermcs_rate),
  delcordpretermcs_delta = ifelse(BrthYear == 2014, NA, delcordpretermcs_delta),
  delcordpretermcs_deltap = (delcordpretermcs_rate - lag(delcordpretermcs_rate,9))/lag(delcordpretermcs_rate,9)
 ) %>%
 ungroup()

#### Milk feeding ----

c34 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, excbrst, CDuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, excbrst, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(excbrst, CDuid),
                 fill = list(
                  excbrst_count = 0,
                  excbrst_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(excbrst, BrthYear),
                 fill = list(
                  excbrst_count = 0,
                  excbrst_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  excbrst_delta = (excbrst_rate - lag(excbrst_rate))/lag(excbrst_rate),
  excbrst_delta = ifelse(BrthYear == 2014, NA, excbrst_delta),
  excbrst_deltap = (excbrst_rate - lag(excbrst_rate,9))/lag(excbrst_rate,9)
 ) %>%
 ungroup()

c35 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, nexcbrst, CDuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, nexcbrst, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(nexcbrst, CDuid),
                 fill = list(
                  nexcbrst_count = 0,
                  nexcbrst_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(nexcbrst, BrthYear),
                 fill = list(
                  nexcbrst_count = 0,
                  nexcbrst_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  nexcbrst_delta = (nexcbrst_rate - lag(nexcbrst_rate))/lag(nexcbrst_rate),
  nexcbrst_delta = ifelse(BrthYear == 2014, NA, nexcbrst_delta),
  nexcbrst_deltap = (nexcbrst_rate - lag(nexcbrst_rate,9))/lag(nexcbrst_rate,9)
 ) %>%
 ungroup()

c36 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, nbrst, CDuid, lvbint) %>%
 distinct() %>%
 group_by(BrthYear, nbrst, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(nbrst, CDuid),
                 fill = list(
                  nbrst_count = 0,
                  nbrst_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(nbrst, BrthYear),
                 fill = list(
                  nbrst_count = 0,
                  nbrst_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  nbrst_delta = (nbrst_rate - lag(nbrst_rate))/lag(nbrst_rate),
  nbrst_delta = ifelse(BrthYear == 2014, NA, nbrst_delta),
  nbrst_deltap = (nbrst_rate - lag(nbrst_rate,9))/lag(nbrst_rate,9)
 ) %>%
 ungroup()

#### The breast/chest feeding initiation rate ----

c37 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, brstinit, CDuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, brstinit, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(brstinit, CDuid),
                 fill = list(
                  brstinit_count = 0,
                  brstinit_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(brstinit, BrthYear),
                 fill = list(
                  brstinit_count = 0,
                  brstinit_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  brstinit_delta = (brstinit_rate - lag(brstinit_rate))/lag(brstinit_rate),
  brstinit_delta = ifelse(BrthYear == 2014, NA, brstinit_delta),
  brstinit_deltap = (brstinit_rate - lag(brstinit_rate,9))/lag(brstinit_rate,9)
 ) %>%
 ungroup()

#### ICU Admission during Pregnancy or Postpartum ----

c38 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
  icu = case_when(
   JOGC_ICU > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, JOGC_ICU, CDuid) %>%
 mutate(
  icu_count = n()
 ) %>%
 group_by(BrthYear, CDuid) %>%
 mutate(
  total_year = n(),
  icu_rate = icu_count/total_year
 ) %>%
 ungroup() %>%
 filter(JOGC_ICU %in% 1, !CDuid %in% 1200) %>%
 select(BrthYear, CDuid, icu, icu_count,icu_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(icu, CDuid),
                 fill = list(
                  icu_count = 0,
                  icu_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(icu, BrthYear),
                 fill = list(
                  icu_count = 0,
                  icu_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  icu_delta = (icu_rate - lag(icu_rate))/lag(icu_rate),
  icu_delta = ifelse(BrthYear == 2014, NA, icu_delta),
  icu_deltap = (icu_rate - lag(icu_rate,9))/lag(icu_rate,9)
 ) %>%
 ungroup()

#### Rate of Severe Morbidity in Pregnancy or Postpartum ----

c39 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
  smm = case_when(
   JOGC_AnySMM > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, JOGC_AnySMM, CDuid) %>%
 mutate(
  smm_count = n()
 ) %>%
 group_by(BrthYear, lvb, CDuid) %>%
 mutate(
  total_year = n(),
  smm_rate = smm_count/total_year
 ) %>%
 ungroup() %>%
 filter(JOGC_AnySMM %in% 1, lvb %in% 1, !CDuid %in% 1200) %>%
 select(BrthYear,CDuid, smm, smm_count,smm_rate) %>%
 distinct() %>%
 arrange(CDuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(smm, CDuid),
                 fill = list(
                  smm_count = 0,
                  smm_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(smm, BrthYear),
                 fill = list(
                  smm_count = 0,
                  smm_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  smm_delta = (smm_rate - lag(smm_rate))/lag(smm_rate),
  smm_delta = ifelse(BrthYear == 2014, NA, smm_delta),
  smm_deltap = (smm_rate - lag(smm_rate,9))/lag(smm_rate,9)
 ) %>%
 ungroup()

ccd_stats <- cbind(
 c1 %>% select(-PreExisting_Hypertension),
 c2 %>% select(-Gestational_Hypertension,-CDuid,-BrthYear),
 c3 %>% select(-Any_Hypertension,-CDuid,-BrthYear),
 c4 %>% select(-anemia,-CDuid,-BrthYear),
 c5 %>% select(-PreExisting_Diabetes,-CDuid,-BrthYear),
 c6 %>% select(-GDM,-CDuid,-BrthYear),
 c7 %>% select(-Any_Diabetes,-CDuid,-BrthYear),
 c8 %>% select(-sptvg,-CDuid,-BrthYear),
 c9 %>% select(-med,-CDuid,-BrthYear),
 c10 %>% select(-sptassvgmed,-CDuid,-BrthYear),
 c11 %>% select(-sptassvgnmed,-CDuid,-BrthYear),
 c12 %>% select(-rbs1,-CDuid,-BrthYear),
 c13 %>% select(-rbs21,-CDuid,-BrthYear),
 c14 %>% select(-rbs51,-CDuid,-BrthYear),
 c15 %>% select(-pphbl,-CDuid,-BrthYear),
 c16 %>% select(-pphpi,-CDuid,-BrthYear),
 c17 %>% select(-ppreadm,-CDuid,-BrthYear),
 #c18 %>% select(-brstinit,-CDuid,-BrthYear),
 c19 %>% select(-sknskn,-CDuid,-BrthYear),
 c20 %>% select(-sknsknvg,-CDuid,-BrthYear),
 c21 %>% select(-sknskncs,-CDuid,-BrthYear),
 c22 %>% select(-pphiv,-CDuid,-BrthYear),
 c23 %>% select(-neoreadm,-CDuid,-BrthYear),
 c24 %>% select(-pretster,-CDuid,-BrthYear),
 c25 %>% select(-pretnnicu,-CDuid,-BrthYear),
 c26 %>% select(-newlwcs,-CDuid,-BrthYear),
 c27 %>% select(-delcord,-CDuid,-BrthYear),
 c28 %>% select(-delcordterm,-CDuid,-BrthYear),
 c29 %>% select(-delcordtermvg,-CDuid,-BrthYear),
 c30 %>% select(-delcordtermcs,-CDuid,-BrthYear),
 c31 %>% select(-delcordpreterm,-CDuid,-BrthYear),
 c32 %>% select(-delcordpretermvg,-CDuid,-BrthYear),
 c33 %>% select(-delcordpretermcs,-CDuid,-BrthYear),
 c34 %>% select(-excbrst,-CDuid,-BrthYear),
 c35 %>% select(-nexcbrst,-CDuid,-BrthYear),
 c36 %>% select(-nbrst,-CDuid,-BrthYear),
 c37 %>% select(-brstinit,-CDuid,-BrthYear),
 c38 %>% select(-icu,-CDuid,-BrthYear),
 c39 %>% select(-smm,-CDuid,-BrthYear)
)

### Community cluster (CL) ----
#### Hypertension ----

c1 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, PreExisting_Hypertension, CLuid) %>%
 distinct() %>%
 group_by(BrthYear, PreExisting_Hypertension, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  prehyp_count = n()
 ) %>%
 group_by(BrthYear, CLuid) %>%
 mutate(
  total_year = n(),
  prehyp_rate = prehyp_count/total_year
 ) %>%
 filter(PreExisting_Hypertension %in% 1,
        !is.na(CLuid)) %>%
 select(BrthYear, CLuid, PreExisting_Hypertension, prehyp_count, prehyp_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(PreExisting_Hypertension, CLuid),
                 fill = list(
                  prehyp_count = 0,
                  prehyp_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(PreExisting_Hypertension, BrthYear),
                 fill = list(
                  prehyp_count = 0,
                  prehyp_rate = NA
                 )) %>%
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
  CLuid = as.character(CLuid),
  gesthyp_count = n()
 ) %>%
 group_by(BrthYear, CLuid) %>%
 mutate(
  total_year = n(),
  gesthyp_rate = gesthyp_count/total_year
 ) %>%
 filter(Gestational_Hypertension %in% 1,
        !is.na(CLuid)) %>%
 select(BrthYear, CLuid, Gestational_Hypertension, gesthyp_count, gesthyp_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(Gestational_Hypertension, CLuid),
                 fill = list(
                  gesthyp_count = 0,
                  gesthyp_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(Gestational_Hypertension, BrthYear),
                 fill = list(
                  gesthyp_count = 0,
                  gesthyp_rate = NA
                 )) %>%
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
  CLuid = as.character(CLuid),
  hyp_count = n()
 ) %>%
 group_by(BrthYear, CLuid) %>%
 mutate(
  total_year = n(),
  hyp_rate = hyp_count/total_year
 ) %>%
 filter(Any_Hypertension %in% 1,
        !is.na(CLuid)) %>%
 select(BrthYear, CLuid, Any_Hypertension, hyp_count, hyp_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(Any_Hypertension, CLuid),
                 fill = list(
                  hyp_count = 0,
                  hyp_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(Any_Hypertension, BrthYear),
                 fill = list(
                  hyp_count = 0,
                  hyp_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  hyp_delta = (hyp_rate - lag(hyp_rate))/lag(hyp_rate),
  hyp_delta = ifelse(BrthYear == 2014, NA, hyp_delta),
  hyp_deltap = (hyp_rate - lag(hyp_rate,9))/lag(hyp_rate,9)
 ) %>%
 ungroup()

#### Anaemia ----

c4 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, CLuid, MANEM, R014_01500, MO990, MD50_D53, MD55_D59, MD60_D64) %>%
 mutate(anemia = case_when(
  MANEM > 0 | R014_01500 > 0 | MO990 > 0 | MD50_D53 > 0 |
   MD55_D59 > 0 | MD60_D64 > 0 ~ 1,
  TRUE ~ NA_integer_
 )) %>%
 distinct() %>%
 group_by(BrthYear, anemia, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  anemia_count = n()
 ) %>%
 group_by(BrthYear, CLuid) %>%
 mutate(
  total_year = n(),
  anemia_rate = anemia_count/total_year
 ) %>%
 filter(anemia %in% 1,
        !is.na(CLuid)) %>%
 select(BrthYear, CLuid, anemia, anemia_count, anemia_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(anemia, CLuid),
                 fill = list(
                  anemia_count = 0,
                  anemia_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(anemia, BrthYear),
                 fill = list(
                  anemia_count = 0,
                  anemia_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  anemia_delta = (anemia_rate - lag(anemia_rate))/lag(anemia_rate),
  anemia_delta = ifelse(BrthYear == 2014, NA, anemia_delta),
  anemia_deltap = (anemia_rate - lag(anemia_rate,9))/lag(anemia_rate,9)
 ) %>%
 ungroup()

#### Diabetes ----

c5 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, PreExisting_Diabetes, CLuid) %>%
 distinct() %>%
 group_by(BrthYear, PreExisting_Diabetes, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  prediab_count = n()
 ) %>%
 group_by(BrthYear, CLuid) %>%
 mutate(
  total_year = n(),
  prediab_rate = prediab_count/total_year
 ) %>%
 filter(PreExisting_Diabetes %in% 1,
        !is.na(CLuid)) %>%
 select(BrthYear, CLuid, PreExisting_Diabetes, prediab_count, prediab_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(PreExisting_Diabetes, CLuid),
                 fill = list(
                  prediab_count = 0,
                  prediab_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(PreExisting_Diabetes, BrthYear),
                 fill = list(
                  prediab_count = 0,
                  prediab_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  prediab_delta = (prediab_rate - lag(prediab_rate))/lag(prediab_rate),
  prediab_delta = ifelse(BrthYear == 2014, NA, prediab_delta),
  prediab_deltap = (prediab_rate - lag(prediab_rate,9))/lag(prediab_rate,9)
 ) %>%
 ungroup()

c6 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, GDM, CLuid) %>%
 distinct() %>%
 group_by(BrthYear, GDM, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  gestdiab_count = n()
 ) %>%
 group_by(BrthYear, CLuid) %>%
 mutate(
  total_year = n(),
  gestdiab_rate = gestdiab_count/total_year
 ) %>%
 filter(GDM %in% 1,
        !is.na(CLuid)) %>%
 select(BrthYear, CLuid, GDM, gestdiab_count, gestdiab_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(GDM, CLuid),
                 fill = list(
                  gestdiab_count = 0,
                  gestdiab_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(GDM, BrthYear),
                 fill = list(
                  gestdiab_count = 0,
                  gestdiab_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  gestdiab_delta = (gestdiab_rate - lag(gestdiab_rate))/lag(gestdiab_rate),
  gestdiab_delta = ifelse(BrthYear == 2014, NA, gestdiab_delta),
  gestdiab_deltap = (gestdiab_rate - lag(gestdiab_rate,9))/lag(gestdiab_rate,9)
 ) %>%
 ungroup()

c7 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, Any_Diabetes, CLuid) %>%
 distinct() %>%
 group_by(BrthYear, Any_Diabetes, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  diab_count = n()
 ) %>%
 group_by(BrthYear, CLuid) %>%
 mutate(
  total_year = n(),
  diab_rate = diab_count/total_year
 ) %>%
 filter(Any_Diabetes %in% 1,
        !is.na(CLuid)) %>%
 select(BrthYear, CLuid, Any_Diabetes, diab_count, diab_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(Any_Diabetes, CLuid),
                 fill = list(
                  diab_count = 0,
                  diab_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(Any_Diabetes, BrthYear),
                 fill = list(
                  diab_count = 0,
                  diab_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  diab_delta = (diab_rate - lag(diab_rate))/lag(diab_rate),
  diab_delta = ifelse(BrthYear == 2014, NA, diab_delta),
  diab_deltap = (diab_rate - lag(diab_rate,9))/lag(diab_rate,9)
 ) %>%
 ungroup()

#### Severe Perineal Trauma with Spontaneous Vaginal Birth ----

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
        DMMETHOD,
        CLuid) %>%
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
 group_by(BrthYear, trdfrth, sptvg, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  spt_count = n()
 ) %>%
 group_by(BrthYear, sptvg, CLuid) %>%
 mutate(
  total_year = n(),
  spt_rate = spt_count/total_year
 ) %>%
 ungroup() %>%
 filter(trdfrth %in% 1, sptvg %in% 1, !is.na(CLuid)) %>%
 select(BrthYear,CLuid,sptvg,spt_count,spt_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sptvg, CLuid),
                 fill = list(
                  spt_count = 0,
                  spt_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(sptvg, BrthYear),
                 fill = list(
                  spt_count = 0,
                  spt_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  spt_delta = (spt_rate - lag(spt_rate))/lag(spt_rate),
  spt_delta = ifelse(BrthYear == 2014, NA, spt_delta),
  spt_deltap = (spt_rate - lag(spt_rate,9))/lag(spt_rate,9)
 ) %>%
 ungroup()

#### Mediolateral Episiotomy with Operative Vaginal Birth ----
#### ND = Not Done
#### MD = Midline
#### ML = Mediolateral

c9 <- dta %>%
 select(BrthYear,
        DMMETHOD,
        DMEPISIO,
        CLuid) %>%
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
 group_by(BrthYear, med, assvg, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  med_count = n()
 ) %>%
 group_by(BrthYear, assvg, CLuid) %>%
 mutate(
  total_year = n(),
  med_rate = med_count/total_year
 ) %>%
 ungroup() %>%
 filter(med %in% 1, assvg %in% 1, !is.na(CLuid)) %>%
 select(BrthYear,CLuid,med,med_count,med_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(med, CLuid),
                 fill = list(
                  med_count = 0,
                  med_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(med, BrthYear),
                 fill = list(
                  med_count = 0,
                  med_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  med_delta = (med_rate - lag(med_rate))/lag(med_rate),
  med_delta = ifelse(BrthYear == 2014, NA, med_delta),
  med_deltap = (med_rate - lag(med_rate,9))/lag(med_rate,9)
 ) %>%
 ungroup()

#### Severe perineal trauma with Operative Vaginal Birth and mediolateral episiotomy ----

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
        DMEPISIO,
        CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(BrthYear, trdfrth, assvg, med, CLuid) %>%
 mutate(
  sptassvgmed_count = n()
 ) %>%
 group_by(BrthYear, assvg, CLuid) %>%
 mutate(
  total_year = n(),
  sptassvgmed_rate = sptassvgmed_count/total_year
 ) %>%
 ungroup() %>%
 filter(sptassvgmed %in% 1, !is.na(CLuid)) %>%
 select(BrthYear,CLuid,sptassvgmed,sptassvgmed_count,sptassvgmed_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sptassvgmed, CLuid),
                 fill = list(
                  sptassvgmed_count = 0,
                  sptassvgmed_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(sptassvgmed, BrthYear),
                 fill = list(
                  sptassvgmed_count = 0,
                  sptassvgmed_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  sptassvgmed_delta = (sptassvgmed_rate - lag(sptassvgmed_rate))/lag(sptassvgmed_rate),
  sptassvgmed_delta = ifelse(BrthYear == 2014, NA, sptassvgmed_delta),
  sptassvgmed_deltap = (sptassvgmed_rate - lag(sptassvgmed_rate,9))/lag(sptassvgmed_rate,9)
 ) %>%
 ungroup()

#### Severe perineal trauma with Operative Vaginal Birth without mediolateral episiotomy ----

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
        DMEPISIO,
        CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(BrthYear, trdfrth, assvg, med, CLuid) %>%
 mutate(
  sptassvgnmed_count = n()
 ) %>%
 group_by(BrthYear, assvg, CLuid) %>%
 mutate(
  total_year = n(),
  sptassvgnmed_rate = sptassvgnmed_count/total_year
 ) %>%
 ungroup() %>%
 filter(sptassvgnmed %in% 1, !is.na(CLuid)) %>%
 select(BrthYear,CLuid,sptassvgnmed,sptassvgnmed_count,sptassvgnmed_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sptassvgnmed, CLuid),
                 fill = list(
                  sptassvgnmed_count = 0,
                  sptassvgnmed_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(sptassvgnmed, BrthYear),
                 fill = list(
                  sptassvgnmed_count = 0,
                  sptassvgnmed_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  sptassvgnmed_delta = (sptassvgnmed_rate - lag(sptassvgnmed_rate))/lag(sptassvgnmed_rate),
  sptassvgnmed_delta = ifelse(BrthYear == 2014, NA, sptassvgnmed_delta),
  sptassvgnmed_deltap = (sptassvgnmed_rate - lag(sptassvgnmed_rate,9))/lag(sptassvgnmed_rate,9)
 ) %>%
 ungroup()

#### Robson group ----

c12 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs1, RobsnGrp, CLuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs1, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
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
        !is.na(CLuid)) %>%
 select(BrthYear, CLuid, rbs1, rbs1_count, rbs1_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(rbs1, CLuid),
                 fill = list(
                  rbs1_count = 0,
                  rbs1_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(rbs1, BrthYear),
                 fill = list(
                  rbs1_count = 0,
                  rbs1_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  rbs1_delta = (rbs1_rate - lag(rbs1_rate))/lag(rbs1_rate),
  rbs1_delta = ifelse(BrthYear == 2014, NA, rbs1_delta),
  rbs1_deltap = (rbs1_rate - lag(rbs1_rate,9))/lag(rbs1_rate,9)
 ) %>%
 ungroup()

c13 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs21, RobsnGrp, CLuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs21, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
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
        !is.na(CLuid)) %>%
 select(BrthYear, CLuid, rbs21, rbs21_count, rbs21_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(rbs21, CLuid),
                 fill = list(
                  rbs21_count = 0,
                  rbs21_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(rbs21, BrthYear),
                 fill = list(
                  rbs21_count = 0,
                  rbs21_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  rbs21_delta = (rbs21_rate - lag(rbs21_rate))/lag(rbs21_rate),
  rbs21_delta = ifelse(BrthYear == 2014, NA, rbs21_delta),
  rbs21_deltap = (rbs21_rate - lag(rbs21_rate,9))/lag(rbs21_rate,9)
 ) %>%
 ungroup()

c14 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs51, RobsnGrp, CLuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs51, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
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
        !is.na(CLuid)) %>%
 select(BrthYear, CLuid, rbs51, rbs51_count, rbs51_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(rbs51, CLuid),
                 fill = list(
                  rbs51_count = 0,
                  rbs51_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(rbs51, BrthYear),
                 fill = list(
                  rbs51_count = 0,
                  rbs51_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  rbs51_delta = (rbs51_rate - lag(rbs51_rate))/lag(rbs51_rate),
  rbs51_delta = ifelse(BrthYear == 2014, NA, rbs51_delta),
  rbs51_deltap = (rbs51_rate - lag(rbs51_rate,9))/lag(rbs51_rate,9)
 ) %>%
 ungroup()

#### Postpartum hemorrhage treated with a blood transfusion ----

c15 <- dta %>%
 select(BrthYear, Any_Blood_Product, Postpartum_Haemorrhage, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  pphbl = case_when(
   Any_Blood_Product > 0 & Postpartum_Haemorrhage > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, Any_Blood_Product, Postpartum_Haemorrhage, CLuid) %>%
 mutate(
  pphbl_count = n()
 ) %>%
 group_by(BrthYear, CLuid) %>%
 mutate(
  total_year = n(),
  pphbl_rate = pphbl_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Blood_Product %in% 1, Postpartum_Haemorrhage %in% 1,
        !is.na(CLuid)) %>%
 select(BrthYear,CLuid,pphbl, pphbl_count, pphbl_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pphbl, CLuid),
                 fill = list(
                  pphbl_count = 0,
                  pphbl_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(pphbl, BrthYear),
                 fill = list(
                  pphbl_count = 0,
                  pphbl_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  pphbl_delta = (pphbl_rate - lag(pphbl_rate))/lag(pphbl_rate),
  pphbl_delta = ifelse(BrthYear == 2014, NA, pphbl_delta),
  pphbl_deltap = (pphbl_rate - lag(pphbl_rate,9))/lag(pphbl_rate,9)
 ) %>%
 ungroup()

#### Postpartum hemorrhage resulting in procedural intervention ----

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
        MODEDEL,
        CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(BrthYear, pphpi, Postpartum_Haemorrhage, CLuid) %>%
 mutate(
  pphpi_count = n()
 ) %>%
 group_by(BrthYear, Postpartum_Haemorrhage, CLuid) %>%
 mutate(
  total_year = n(),
  pphpi_rate = pphpi_count/total_year
 ) %>%
 ungroup() %>%
 filter(pphpi %in% 1, Postpartum_Haemorrhage %in% 1,
        !is.na(CLuid)) %>%
 select(BrthYear,CLuid,pphpi, pphpi_count, pphpi_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pphpi, CLuid),
                 fill = list(
                  pphpi_count = 0,
                  pphpi_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(pphpi, BrthYear),
                 fill = list(
                  pphpi_count = 0,
                  pphpi_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  pphpi_delta = (pphpi_rate - lag(pphpi_rate))/lag(pphpi_rate),
  pphpi_delta = ifelse(BrthYear == 2014, NA, pphpi_delta),
  pphpi_deltap = (pphpi_rate - lag(pphpi_rate,9))/lag(pphpi_rate,9)
 ) %>%
 ungroup()

#### Postpartum readmission ----

c17 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, ppreadm, CLuid) %>%
 distinct() %>%
 group_by(BrthYear, ppreadm, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  ppreadm_count = n()
 ) %>%
 group_by(BrthYear, CLuid) %>%
 mutate(
  total_year = n(),
  ppreadm_rate = ppreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(ppreadm %in% 1,
        !is.na(CLuid)) %>%
 select(BrthYear, CLuid, ppreadm, ppreadm_count, ppreadm_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(ppreadm, CLuid),
                 fill = list(
                  ppreadm_count = 0,
                  ppreadm_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(ppreadm, BrthYear),
                 fill = list(
                  ppreadm_count = 0,
                  ppreadm_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  ppreadm_delta = (ppreadm_rate - lag(ppreadm_rate))/lag(ppreadm_rate),
  ppreadm_delta = ifelse(BrthYear == 2014, NA, ppreadm_delta),
  ppreadm_deltap = (ppreadm_rate - lag(ppreadm_rate,9))/lag(ppreadm_rate,9)
 ) %>%
 ungroup()

#### Postpartum patients who received medical anticoagulation prophylaxis when indicated ----

#c18

#### Skin to skin ----

c19 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknskn, CLuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, sknskn, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  sknskn_count = n()
 ) %>%
 group_by(BrthYear, CLuid, lvb) %>%
 mutate(
  total_year = n(),
  sknskn_rate = sknskn_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskn %in% 1,
        !is.na(CLuid)) %>%
 select(BrthYear, CLuid, sknskn, sknskn_count, sknskn_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sknskn, CLuid),
                 fill = list(
                  sknskn_count = 0,
                  sknskn_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(sknskn, BrthYear),
                 fill = list(
                  sknskn_count = 0,
                  sknskn_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  sknskn_delta = (sknskn_rate - lag(sknskn_rate))/lag(sknskn_rate),
  sknskn_delta = ifelse(BrthYear == 2014, NA, sknskn_delta),
  sknskn_deltap = (sknskn_rate - lag(sknskn_rate,9))/lag(sknskn_rate,9)
 ) %>%
 ungroup()

c20 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknsknvg, CLuid, lvbvg) %>%
 distinct() %>%
 group_by(BrthYear, sknsknvg, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  sknsknvg_count = n()
 ) %>%
 group_by(BrthYear, CLuid, lvbvg) %>%
 mutate(
  total_year = n(),
  sknsknvg_rate = sknsknvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknsknvg %in% 1,
        !is.na(CLuid)) %>%
 select(BrthYear, CLuid, sknsknvg, sknsknvg_count, sknsknvg_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sknsknvg, CLuid),
                 fill = list(
                  sknsknvg_count = 0,
                  sknsknvg_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(sknsknvg, BrthYear),
                 fill = list(
                  sknsknvg_count = 0,
                  sknsknvg_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  sknsknvg_delta = (sknsknvg_rate - lag(sknsknvg_rate))/lag(sknsknvg_rate),
  sknsknvg_delta = ifelse(BrthYear == 2014, NA, sknsknvg_delta),
  sknsknvg_deltap = (sknsknvg_rate - lag(sknsknvg_rate,9))/lag(sknsknvg_rate,9)
 ) %>%
 ungroup()

c21 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknskncs, CLuid, lvbcs) %>%
 distinct() %>%
 group_by(BrthYear, sknskncs, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  sknskncs_count = n()
 ) %>%
 group_by(BrthYear, CLuid, lvbcs) %>%
 mutate(
  total_year = n(),
  sknskncs_rate = sknskncs_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskncs %in% 1,
        !is.na(CLuid)) %>%
 select(BrthYear, CLuid, sknskncs, sknskncs_count, sknskncs_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sknskncs, CLuid),
                 fill = list(
                  sknskncs_count = 0,
                  sknskncs_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(sknskncs, BrthYear),
                 fill = list(
                  sknskncs_count = 0,
                  sknskncs_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  sknskncs_delta = (sknskncs_rate - lag(sknskncs_rate))/lag(sknskncs_rate),
  sknskncs_delta = ifelse(BrthYear == 2014, NA, sknskncs_delta),
  sknskncs_deltap = (sknskncs_rate - lag(sknskncs_rate,9))/lag(sknskncs_rate,9)
 ) %>%
 ungroup()

#### Postpartum blood products amongst those who received antenatal iron therapy ----

c22 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
  pphiv = case_when(
   Any_Blood_Product > 0 & R003_02100 > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, Any_Blood_Product, R003_02100, CLuid) %>%
 mutate(
  pphiv_count = n()
 ) %>%
 group_by(BrthYear, Any_Blood_Product, CLuid) %>%
 mutate(
  total_year = n(),
  pphiv_rate = pphiv_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Blood_Product %in% 1, R003_02100 %in% 1,
        !is.na(CLuid)) %>%
 select(BrthYear, CLuid, pphiv, pphiv_count, pphiv_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pphiv, CLuid),
                 fill = list(
                  pphiv_count = 0,
                  pphiv_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(pphiv, BrthYear),
                 fill = list(
                  pphiv_count = 0,
                  pphiv_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  pphiv_delta = (pphiv_rate - lag(pphiv_rate))/lag(pphiv_rate),
  pphiv_delta = ifelse(BrthYear == 2014, NA, pphiv_delta),
  pphiv_deltap = (pphiv_rate - lag(pphiv_rate,9))/lag(pphiv_rate,9)
 ) %>%
 ungroup()

#### Neonatal readmission ----

c23 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, neoreadm, CLuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, neoreadm, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  neoreadm_count = n()
 ) %>%
 group_by(BrthYear, CLuid, lvb) %>%
 mutate(
  total_year = n(),
  neoreadm_rate = neoreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(neoreadm %in% 1,
        !is.na(CLuid)) %>%
 select(BrthYear, CLuid, neoreadm, neoreadm_count, neoreadm_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(neoreadm, CLuid),
                 fill = list(
                  neoreadm_count = 0,
                  neoreadm_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(neoreadm, BrthYear),
                 fill = list(
                  neoreadm_count = 0,
                  neoreadm_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  neoreadm_delta = (neoreadm_rate - lag(neoreadm_rate))/lag(neoreadm_rate),
  neoreadm_delta = ifelse(BrthYear == 2014, NA, neoreadm_delta),
  neoreadm_deltap = (neoreadm_rate - lag(neoreadm_rate,9))/lag(neoreadm_rate,9)
 ) %>%
 ungroup()

#### Preterm infants who received complete course of steroids > 24h and < 7 days prior to birth ----

c24 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(BrthYear, pretster, CLuid) %>%
 mutate(
  pretster_count = n()
 ) %>%
 group_by(BrthYear, den, CLuid) %>%
 mutate(
  total_year = n(),
  pretster_rate = pretster_count/total_year
 ) %>%
 ungroup() %>%
 filter(pretster %in% 1, !is.na(CLuid)) %>%
 select(BrthYear, CLuid, pretster, pretster_count,pretster_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pretster, CLuid),
                 fill = list(
                  pretster_count = 0,
                  pretster_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(pretster, BrthYear),
                 fill = list(
                  pretster_count = 0,
                  pretster_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  pretster_delta = (pretster_rate - lag(pretster_rate))/lag(pretster_rate),
  pretster_delta = ifelse(BrthYear == 2014, NA, pretster_delta),
  pretster_deltap = (pretster_rate - lag(pretster_rate,9))/lag(pretster_rate,9)
 ) %>%
 ungroup()

#### Preterm babies born in facility without NICU ----

c25 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(BrthYear, pretnnicu, CLuid) %>%
 mutate(
  pretnnicu_count = n()
 ) %>%
 group_by(BrthYear, den, CLuid) %>%
 mutate(
  total_year = n(),
  pretnnicu_rate = pretnnicu_count/total_year
 ) %>%
 ungroup() %>%
 filter(pretnnicu %in% 1, !is.na(CLuid)) %>%
 select(BrthYear, CLuid, pretnnicu, pretnnicu_count,pretnnicu_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pretnnicu, CLuid),
                 fill = list(
                  pretnnicu_count = 0,
                  pretnnicu_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(pretnnicu, BrthYear),
                 fill = list(
                  pretnnicu_count = 0,
                  pretnnicu_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  pretnnicu_delta = (pretnnicu_rate - lag(pretnnicu_rate))/lag(pretnnicu_rate),
  pretnnicu_delta = ifelse(BrthYear == 2014, NA, pretnnicu_delta),
  pretnnicu_deltap = (pretnnicu_rate - lag(pretnnicu_rate,9))/lag(pretnnicu_rate,9)
 ) %>%
 ungroup()

#### Newborn respiratory distress associated with low-risk repeat cesarean at term gestation (≥ 37 weeks < 39 weeks) ----

c26 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(BrthYear, newlwcs, CLuid) %>%
 mutate(
  newlwcs_count = n()
 ) %>%
 group_by(BrthYear, den, CLuid) %>%
 mutate(
  total_year = n(),
  newlwcs_rate = newlwcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(newlwcs %in% 1) %>%
 select(BrthYear, CLuid, newlwcs, newlwcs_count,newlwcs_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(newlwcs, CLuid),
                 fill = list(
                  newlwcs_count = 0,
                  newlwcs_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(newlwcs, BrthYear),
                 fill = list(
                  newlwcs_count = 0,
                  newlwcs_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  newlwcs_delta = (newlwcs_rate - lag(newlwcs_rate))/lag(newlwcs_rate),
  newlwcs_delta = ifelse(BrthYear == 2014, NA, newlwcs_delta),
  newlwcs_deltap = (newlwcs_rate - lag(newlwcs_rate,9))/lag(newlwcs_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping  ----

c27 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
  delcord = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (DEL_CORD_CLAMP %in% 3) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, delcord, CLuid) %>%
 mutate(
  delcord_count = n()
 ) %>%
 group_by(BrthYear, lvb, CLuid) %>%
 mutate(
  total_year = n(),
  delcord_rate = delcord_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcord %in% 1, lvb %in% 1,
        !is.na(CLuid)) %>%
 select(BrthYear, CLuid, delcord, delcord_count,delcord_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcord, CLuid),
                 fill = list(
                  delcord_count = 0,
                  delcord_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(delcord, BrthYear),
                 fill = list(
                  delcord_count = 0,
                  delcord_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  delcord_delta = (delcord_rate - lag(delcord_rate))/lag(delcord_rate),
  delcord_delta = ifelse(BrthYear == 2014, NA, delcord_delta),
  delcord_deltap = (delcord_rate - lag(delcord_rate,9))/lag(delcord_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns ----

c28 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(BrthYear, delcordterm, CLuid) %>%
 mutate(
  delcordterm_count = n()
 ) %>%
 group_by(BrthYear, den, CLuid) %>%
 mutate(
  total_year = n(),
  delcordterm_rate = delcordterm_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordterm %in% 1, !is.na(CLuid)) %>%
 select(BrthYear,CLuid,delcordterm,delcordterm_count,delcordterm_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordterm, CLuid),
                 fill = list(
                  delcordterm_count = 0,
                  delcordterm_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(delcordterm, BrthYear),
                 fill = list(
                  delcordterm_count = 0,
                  delcordterm_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  delcordterm_delta = (delcordterm_rate - lag(delcordterm_rate))/lag(delcordterm_rate),
  delcordterm_delta = ifelse(BrthYear == 2014, NA, delcordterm_delta),
  delcordterm_deltap = (delcordterm_rate - lag(delcordterm_rate,9))/lag(delcordterm_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns following spontaneous vaginal birth ----

c29 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(BrthYear, delcordtermvg, CLuid) %>%
 mutate(
  delcordtermvg_count = n()
 ) %>%
 group_by(BrthYear, den, CLuid) %>%
 mutate(
  total_year = n(),
  delcordtermvg_rate = delcordtermvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordtermvg %in% 1, !is.na(CLuid)) %>%
 select(BrthYear,CLuid, delcordtermvg, delcordtermvg_count,delcordtermvg_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordtermvg, CLuid),
                 fill = list(
                  delcordtermvg_count = 0,
                  delcordtermvg_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(delcordtermvg, BrthYear),
                 fill = list(
                  delcordtermvg_count = 0,
                  delcordtermvg_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  delcordtermvg_delta = (delcordtermvg_rate - lag(delcordtermvg_rate))/lag(delcordtermvg_rate),
  delcordtermvg_delta = ifelse(BrthYear == 2014, NA, delcordtermvg_delta),
  delcordtermvg_deltap = (delcordtermvg_rate - lag(delcordtermvg_rate,9))/lag(delcordtermvg_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns following caesarean birth ----

c30 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(BrthYear, delcordtermcs, CLuid) %>%
 mutate(
  delcordtermcs_count = n()
 ) %>%
 group_by(BrthYear, den, CLuid) %>%
 mutate(
  total_year = n(),
  delcordtermcs_rate = delcordtermcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordtermcs %in% 1, !is.na(CLuid)) %>%
 select(BrthYear, CLuid, delcordtermcs, delcordtermcs_count,delcordtermcs_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordtermcs, CLuid),
                 fill = list(
                  delcordtermcs_count = 0,
                  delcordtermcs_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(delcordtermcs, BrthYear),
                 fill = list(
                  delcordtermcs_count = 0,
                  delcordtermcs_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  delcordtermcs_delta = (delcordtermcs_rate - lag(delcordtermcs_rate))/lag(delcordtermcs_rate),
  delcordtermcs_delta = ifelse(BrthYear == 2014, NA, delcordtermcs_delta),
  delcordtermcs_deltap = (delcordtermcs_rate - lag(delcordtermcs_rate,9))/lag(delcordtermcs_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns ----

c31 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(BrthYear, delcordpreterm, CLuid) %>%
 mutate(
  delcordpreterm_count = n()
 ) %>%
 group_by(BrthYear, den, CLuid) %>%
 mutate(
  total_year = n(),
  delcordpreterm_rate = delcordpreterm_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpreterm %in% 1, !is.na(CLuid)) %>%
 select(BrthYear,CLuid, delcordpreterm, delcordpreterm_count,delcordpreterm_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordpreterm, CLuid),
                 fill = list(
                  delcordpreterm_count = 0,
                  delcordpreterm_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(delcordpreterm, BrthYear),
                 fill = list(
                  delcordpreterm_count = 0,
                  delcordpreterm_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  delcordpreterm_delta = (delcordpreterm_rate - lag(delcordpreterm_rate))/lag(delcordpreterm_rate),
  delcordpreterm_delta = ifelse(BrthYear == 2014, NA, delcordpreterm_delta),
  delcordpreterm_deltap = (delcordpreterm_rate - lag(delcordpreterm_rate,9))/lag(delcordpreterm_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns following spontaneous vaginal birth ----

c32 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(BrthYear, delcordpretermvg, CLuid) %>%
 mutate(
  delcordpretermvg_count = n()
 ) %>%
 group_by(BrthYear, den, CLuid) %>%
 mutate(
  total_year = n(),
  delcordpretermvg_rate = delcordpretermvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpretermvg %in% 1, !is.na(CLuid)) %>%
 select(BrthYear,CLuid, delcordpretermvg, delcordpretermvg_count,delcordpretermvg_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordpretermvg, CLuid),
                 fill = list(
                  delcordpretermvg_count = 0,
                  delcordpretermvg_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(delcordpretermvg, BrthYear),
                 fill = list(
                  delcordpretermvg_count = 0,
                  delcordpretermvg_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  delcordpretermvg_delta = (delcordpretermvg_rate - lag(delcordpretermvg_rate))/lag(delcordpretermvg_rate),
  delcordpretermvg_delta = ifelse(BrthYear == 2014, NA, delcordpretermvg_delta),
  delcordpretermvg_deltap = (delcordpretermvg_rate - lag(delcordpretermvg_rate,9))/lag(delcordpretermvg_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns following caesarean birth ----

c33 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(BrthYear, delcordpretermcs, CLuid) %>%
 mutate(
  delcordpretermcs_count = n()
 ) %>%
 group_by(BrthYear, den, CLuid) %>%
 mutate(
  total_year = n(),
  delcordpretermcs_rate = delcordpretermcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpretermcs %in% 1, !is.na(CLuid)) %>%
 select(BrthYear,CLuid, delcordpretermcs, delcordpretermcs_count,delcordpretermcs_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordpretermcs, CLuid),
                 fill = list(
                  delcordpretermcs_count = 0,
                  delcordpretermcs_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(delcordpretermcs, BrthYear),
                 fill = list(
                  delcordpretermcs_count = 0,
                  delcordpretermcs_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  delcordpretermcs_delta = (delcordpretermcs_rate - lag(delcordpretermcs_rate))/lag(delcordpretermcs_rate),
  delcordpretermcs_delta = ifelse(BrthYear == 2014, NA, delcordpretermcs_delta),
  delcordpretermcs_deltap = (delcordpretermcs_rate - lag(delcordpretermcs_rate,9))/lag(delcordpretermcs_rate,9)
 ) %>%
 ungroup()

#### Milk feeding ----

c34 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, excbrst, CLuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, excbrst, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  excbrst_count = n()
 ) %>%
 group_by(BrthYear, CLuid, lvb) %>%
 mutate(
  total_year = n(),
  excbrst_rate = excbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(excbrst %in% 1,
        !is.na(CLuid)) %>%
 select(BrthYear, CLuid, excbrst, excbrst_count, excbrst_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(excbrst, CLuid),
                 fill = list(
                  excbrst_count = 0,
                  excbrst_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(excbrst, BrthYear),
                 fill = list(
                  excbrst_count = 0,
                  excbrst_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  excbrst_delta = (excbrst_rate - lag(excbrst_rate))/lag(excbrst_rate),
  excbrst_delta = ifelse(BrthYear == 2014, NA, excbrst_delta),
  excbrst_deltap = (excbrst_rate - lag(excbrst_rate,9))/lag(excbrst_rate,9)
 ) %>%
 ungroup()

c35 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, nexcbrst, CLuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, nexcbrst, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  nexcbrst_count = n()
 ) %>%
 group_by(BrthYear, CLuid, lvb) %>%
 mutate(
  total_year = n(),
  nexcbrst_rate = nexcbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nexcbrst %in% 1,
        !is.na(CLuid)) %>%
 select(BrthYear, CLuid, nexcbrst, nexcbrst_count, nexcbrst_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(nexcbrst, CLuid),
                 fill = list(
                  nexcbrst_count = 0,
                  nexcbrst_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(nexcbrst, BrthYear),
                 fill = list(
                  nexcbrst_count = 0,
                  nexcbrst_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  nexcbrst_delta = (nexcbrst_rate - lag(nexcbrst_rate))/lag(nexcbrst_rate),
  nexcbrst_delta = ifelse(BrthYear == 2014, NA, nexcbrst_delta),
  nexcbrst_deltap = (nexcbrst_rate - lag(nexcbrst_rate,9))/lag(nexcbrst_rate,9)
 ) %>%
 ungroup()

c36 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, nbrst, CLuid, lvbint) %>%
 distinct() %>%
 group_by(BrthYear, nbrst, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  nbrst_count = n()
 ) %>%
 group_by(BrthYear, CLuid, lvbint) %>%
 mutate(
  total_year = n(),
  nbrst_rate = nbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nbrst %in% 1,
        !is.na(CLuid)) %>%
 select(BrthYear, CLuid, nbrst, nbrst_count, nbrst_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(nbrst, CLuid),
                 fill = list(
                  nbrst_count = 0,
                  nbrst_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(nbrst, BrthYear),
                 fill = list(
                  nbrst_count = 0,
                  nbrst_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  nbrst_delta = (nbrst_rate - lag(nbrst_rate))/lag(nbrst_rate),
  nbrst_delta = ifelse(BrthYear == 2014, NA, nbrst_delta),
  nbrst_deltap = (nbrst_rate - lag(nbrst_rate,9))/lag(nbrst_rate,9)
 ) %>%
 ungroup()

#### The breast/chest feeding initiation rate ----

c37 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, brstinit, CLuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, brstinit, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  brstinit_count = n()
 ) %>%
 group_by(BrthYear, CLuid, lvb) %>%
 mutate(
  total_year = n(),
  brstinit_rate = brstinit_count/total_year
 ) %>%
 ungroup() %>%
 filter(brstinit %in% 1,
        !is.na(CLuid)) %>%
 select(BrthYear, CLuid, brstinit, brstinit_count, brstinit_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(brstinit, CLuid),
                 fill = list(
                  brstinit_count = 0,
                  brstinit_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(brstinit, BrthYear),
                 fill = list(
                  brstinit_count = 0,
                  brstinit_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  brstinit_delta = (brstinit_rate - lag(brstinit_rate))/lag(brstinit_rate),
  brstinit_delta = ifelse(BrthYear == 2014, NA, brstinit_delta),
  brstinit_deltap = (brstinit_rate - lag(brstinit_rate,9))/lag(brstinit_rate,9)
 ) %>%
 ungroup()

#### ICU Admission during Pregnancy or Postpartum ----

c38 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
  icu = case_when(
   JOGC_ICU > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, JOGC_ICU, CLuid) %>%
 mutate(
  icu_count = n()
 ) %>%
 group_by(BrthYear, CLuid) %>%
 mutate(
  total_year = n(),
  icu_rate = icu_count/total_year
 ) %>%
 ungroup() %>%
 filter(JOGC_ICU %in% 1, !is.na(CLuid)) %>%
 select(BrthYear, CLuid, icu, icu_count,icu_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(icu, CLuid),
                 fill = list(
                  icu_count = 0,
                  icu_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(icu, BrthYear),
                 fill = list(
                  icu_count = 0,
                  icu_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  icu_delta = (icu_rate - lag(icu_rate))/lag(icu_rate),
  icu_delta = ifelse(BrthYear == 2014, NA, icu_delta),
  icu_deltap = (icu_rate - lag(icu_rate,9))/lag(icu_rate,9)
 ) %>%
 ungroup()

#### Rate of Severe Morbidity in Pregnancy or Postpartum ----

c39 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
  smm = case_when(
   JOGC_AnySMM > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, JOGC_AnySMM, CLuid) %>%
 mutate(
  smm_count = n()
 ) %>%
 group_by(BrthYear, lvb, CLuid) %>%
 mutate(
  total_year = n(),
  smm_rate = smm_count/total_year
 ) %>%
 ungroup() %>%
 filter(JOGC_AnySMM %in% 1, lvb %in% 1, !is.na(CLuid)) %>%
 select(BrthYear,CLuid, smm, smm_count,smm_rate) %>%
 distinct() %>%
 arrange(CLuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(smm, CLuid),
                 fill = list(
                  smm_count = 0,
                  smm_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(smm, BrthYear),
                 fill = list(
                  smm_count = 0,
                  smm_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  smm_delta = (smm_rate - lag(smm_rate))/lag(smm_rate),
  smm_delta = ifelse(BrthYear == 2014, NA, smm_delta),
  smm_deltap = (smm_rate - lag(smm_rate,9))/lag(smm_rate,9)
 ) %>%
 ungroup()

ccl_stats <- cbind(
 c1 %>% select(-PreExisting_Hypertension),
 c2 %>% select(-Gestational_Hypertension,-CLuid,-BrthYear),
 c3 %>% select(-Any_Hypertension,-CLuid,-BrthYear),
 c4 %>% select(-anemia,-CLuid,-BrthYear),
 c5 %>% select(-PreExisting_Diabetes,-CLuid,-BrthYear),
 c6 %>% select(-GDM,-CLuid,-BrthYear),
 c7 %>% select(-Any_Diabetes,-CLuid,-BrthYear),
 c8 %>% select(-sptvg,-CLuid,-BrthYear),
 c9 %>% select(-med,-CLuid,-BrthYear),
 c10 %>% select(-sptassvgmed,-CLuid,-BrthYear),
 c11 %>% select(-sptassvgnmed,-CLuid,-BrthYear),
 c12 %>% select(-rbs1,-CLuid,-BrthYear),
 c13 %>% select(-rbs21,-CLuid,-BrthYear),
 c14 %>% select(-rbs51,-CLuid,-BrthYear),
 c15 %>% select(-pphbl,-CLuid,-BrthYear),
 c16 %>% select(-pphpi,-CLuid,-BrthYear),
 c17 %>% select(-ppreadm,-CLuid,-BrthYear),
 #c18 %>% select(-brstinit,-CLuid,-BrthYear),
 c19 %>% select(-sknskn,-CLuid,-BrthYear),
 c20 %>% select(-sknsknvg,-CLuid,-BrthYear),
 c21 %>% select(-sknskncs,-CLuid,-BrthYear),
 c22 %>% select(-pphiv,-CLuid,-BrthYear),
 c23 %>% select(-neoreadm,-CLuid,-BrthYear),
 c24 %>% select(-pretster,-CLuid,-BrthYear),
 c25 %>% select(-pretnnicu,-CLuid,-BrthYear),
 c26 %>% select(-newlwcs,-CLuid,-BrthYear),
 c27 %>% select(-delcord,-CLuid,-BrthYear),
 c28 %>% select(-delcordterm,-CLuid,-BrthYear),
 c29 %>% select(-delcordtermvg,-CLuid,-BrthYear),
 c30 %>% select(-delcordtermcs,-CLuid,-BrthYear),
 c31 %>% select(-delcordpreterm,-CLuid,-BrthYear),
 c32 %>% select(-delcordpretermvg,-CLuid,-BrthYear),
 c33 %>% select(-delcordpretermcs,-CLuid,-BrthYear),
 c34 %>% select(-excbrst,-CLuid,-BrthYear),
 c35 %>% select(-nexcbrst,-CLuid,-BrthYear),
 c36 %>% select(-nbrst,-CLuid,-BrthYear),
 c37 %>% select(-brstinit,-CLuid,-BrthYear),
 c38 %>% select(-icu,-CLuid,-BrthYear),
 c39 %>% select(-smm,-CLuid,-BrthYear)
)

### Community health network (CHN) ----
#### Hypertension ----

c1 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, PreExisting_Hypertension, CHNuid) %>%
 distinct() %>%
 group_by(BrthYear, PreExisting_Hypertension, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  prehyp_count = n()
 ) %>%
 group_by(BrthYear, CHNuid) %>%
 mutate(
  total_year = n(),
  prehyp_rate = prehyp_count/total_year
 ) %>%
 filter(PreExisting_Hypertension %in% 1,
        !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, PreExisting_Hypertension, prehyp_count, prehyp_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(PreExisting_Hypertension, CHNuid),
                 fill = list(
                  prehyp_count = 0,
                  prehyp_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(PreExisting_Hypertension, BrthYear),
                 fill = list(
                  prehyp_count = 0,
                  prehyp_rate = NA
                 )) %>%
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
  CHNuid = as.character(CHNuid),
  gesthyp_count = n()
 ) %>%
 group_by(BrthYear, CHNuid) %>%
 mutate(
  total_year = n(),
  gesthyp_rate = gesthyp_count/total_year
 ) %>%
 filter(Gestational_Hypertension %in% 1,
        !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, Gestational_Hypertension, gesthyp_count, gesthyp_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(Gestational_Hypertension, CHNuid),
                 fill = list(
                  gesthyp_count = 0,
                  gesthyp_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(Gestational_Hypertension, BrthYear),
                 fill = list(
                  gesthyp_count = 0,
                  gesthyp_rate = NA
                 )) %>%
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
  CHNuid = as.character(CHNuid),
  hyp_count = n()
 ) %>%
 group_by(BrthYear, CHNuid) %>%
 mutate(
  total_year = n(),
  hyp_rate = hyp_count/total_year
 ) %>%
 filter(Any_Hypertension %in% 1,
        !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, Any_Hypertension, hyp_count, hyp_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(Any_Hypertension, CHNuid),
                 fill = list(
                  hyp_count = 0,
                  hyp_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(Any_Hypertension, BrthYear),
                 fill = list(
                  hyp_count = 0,
                  hyp_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  hyp_delta = (hyp_rate - lag(hyp_rate))/lag(hyp_rate),
  hyp_delta = ifelse(BrthYear == 2014, NA, hyp_delta),
  hyp_deltap = (hyp_rate - lag(hyp_rate,9))/lag(hyp_rate,9)
 ) %>%
 ungroup()

#### Anaemia ----

c4 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, CHNuid, MANEM, R014_01500, MO990, MD50_D53, MD55_D59, MD60_D64) %>%
 mutate(anemia = case_when(
  MANEM > 0 | R014_01500 > 0 | MO990 > 0 | MD50_D53 > 0 |
   MD55_D59 > 0 | MD60_D64 > 0 ~ 1,
  TRUE ~ NA_integer_
 )) %>%
 distinct() %>%
 group_by(BrthYear, anemia, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  anemia_count = n()
 ) %>%
 group_by(BrthYear, CHNuid) %>%
 mutate(
  total_year = n(),
  anemia_rate = anemia_count/total_year
 ) %>%
 filter(anemia %in% 1,
        !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, anemia, anemia_count, anemia_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(anemia, CHNuid),
                 fill = list(
                  anemia_count = 0,
                  anemia_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(anemia, BrthYear),
                 fill = list(
                  anemia_count = 0,
                  anemia_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  anemia_delta = (anemia_rate - lag(anemia_rate))/lag(anemia_rate),
  anemia_delta = ifelse(BrthYear == 2014, NA, anemia_delta),
  anemia_deltap = (anemia_rate - lag(anemia_rate,9))/lag(anemia_rate,9)
 ) %>%
 ungroup()

#### Diabetes ----

c5 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, PreExisting_Diabetes, CHNuid) %>%
 distinct() %>%
 group_by(BrthYear, PreExisting_Diabetes, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  prediab_count = n()
 ) %>%
 group_by(BrthYear, CHNuid) %>%
 mutate(
  total_year = n(),
  prediab_rate = prediab_count/total_year
 ) %>%
 filter(PreExisting_Diabetes %in% 1,
        !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, PreExisting_Diabetes, prediab_count, prediab_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(PreExisting_Diabetes, CHNuid),
                 fill = list(
                  prediab_count = 0,
                  prediab_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(PreExisting_Diabetes, BrthYear),
                 fill = list(
                  prediab_count = 0,
                  prediab_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  prediab_delta = (prediab_rate - lag(prediab_rate))/lag(prediab_rate),
  prediab_delta = ifelse(BrthYear == 2014, NA, prediab_delta),
  prediab_deltap = (prediab_rate - lag(prediab_rate,9))/lag(prediab_rate,9)
 ) %>%
 ungroup()

c6 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, GDM, CHNuid) %>%
 distinct() %>%
 group_by(BrthYear, GDM, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  gestdiab_count = n()
 ) %>%
 group_by(BrthYear, CHNuid) %>%
 mutate(
  total_year = n(),
  gestdiab_rate = gestdiab_count/total_year
 ) %>%
 filter(GDM %in% 1,
        !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, GDM, gestdiab_count, gestdiab_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(GDM, CHNuid),
                 fill = list(
                  gestdiab_count = 0,
                  gestdiab_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(GDM, BrthYear),
                 fill = list(
                  gestdiab_count = 0,
                  gestdiab_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  gestdiab_delta = (gestdiab_rate - lag(gestdiab_rate))/lag(gestdiab_rate),
  gestdiab_delta = ifelse(BrthYear == 2014, NA, gestdiab_delta),
  gestdiab_deltap = (gestdiab_rate - lag(gestdiab_rate,9))/lag(gestdiab_rate,9)
 ) %>%
 ungroup()

c7 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, Any_Diabetes, CHNuid) %>%
 distinct() %>%
 group_by(BrthYear, Any_Diabetes, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  diab_count = n()
 ) %>%
 group_by(BrthYear, CHNuid) %>%
 mutate(
  total_year = n(),
  diab_rate = diab_count/total_year
 ) %>%
 filter(Any_Diabetes %in% 1,
        !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, Any_Diabetes, diab_count, diab_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(Any_Diabetes, CHNuid),
                 fill = list(
                  diab_count = 0,
                  diab_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(Any_Diabetes, BrthYear),
                 fill = list(
                  diab_count = 0,
                  diab_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  diab_delta = (diab_rate - lag(diab_rate))/lag(diab_rate),
  diab_delta = ifelse(BrthYear == 2014, NA, diab_delta),
  diab_deltap = (diab_rate - lag(diab_rate,9))/lag(diab_rate,9)
 ) %>%
 ungroup()

#### Severe Perineal Trauma with Spontaneous Vaginal Birth ----

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
        DMMETHOD,
        CHNuid) %>%
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
 group_by(BrthYear, trdfrth, sptvg, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  spt_count = n()
 ) %>%
 group_by(BrthYear, sptvg, CHNuid) %>%
 mutate(
  total_year = n(),
  spt_rate = spt_count/total_year
 ) %>%
 ungroup() %>%
 filter(trdfrth %in% 1, sptvg %in% 1, !is.na(CHNuid)) %>%
 select(BrthYear,CHNuid,sptvg,spt_count,spt_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sptvg, CHNuid),
                 fill = list(
                  spt_count = 0,
                  spt_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(sptvg, BrthYear),
                 fill = list(
                  spt_count = 0,
                  spt_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  spt_delta = (spt_rate - lag(spt_rate))/lag(spt_rate),
  spt_delta = ifelse(BrthYear == 2014, NA, spt_delta),
  spt_deltap = (spt_rate - lag(spt_rate,9))/lag(spt_rate,9)
 ) %>%
 ungroup()

#### Mediolateral Episiotomy with Operative Vaginal Birth ----
#### ND = Not Done
#### MD = Midline
#### ML = Mediolateral

c9 <- dta %>%
 select(BrthYear,
        DMMETHOD,
        DMEPISIO,
        CHNuid) %>%
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
 group_by(BrthYear, med, assvg, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  med_count = n()
 ) %>%
 group_by(BrthYear, assvg, CHNuid) %>%
 mutate(
  total_year = n(),
  med_rate = med_count/total_year
 ) %>%
 ungroup() %>%
 filter(med %in% 1, assvg %in% 1, !is.na(CHNuid)) %>%
 select(BrthYear,CHNuid,med,med_count,med_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(med, CHNuid),
                 fill = list(
                  med_count = 0,
                  med_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(med, BrthYear),
                 fill = list(
                  med_count = 0,
                  med_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  med_delta = (med_rate - lag(med_rate))/lag(med_rate),
  med_delta = ifelse(BrthYear == 2014, NA, med_delta),
  med_deltap = (med_rate - lag(med_rate,9))/lag(med_rate,9)
 ) %>%
 ungroup()

#### Severe perineal trauma with Operative Vaginal Birth and mediolateral episiotomy ----

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
        DMEPISIO,
        CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(BrthYear, trdfrth, assvg, med, CHNuid) %>%
 mutate(
  sptassvgmed_count = n()
 ) %>%
 group_by(BrthYear, assvg, CHNuid) %>%
 mutate(
  total_year = n(),
  sptassvgmed_rate = sptassvgmed_count/total_year
 ) %>%
 ungroup() %>%
 filter(sptassvgmed %in% 1, !is.na(CHNuid)) %>%
 select(BrthYear,CHNuid,sptassvgmed,sptassvgmed_count,sptassvgmed_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sptassvgmed, CHNuid),
                 fill = list(
                  sptassvgmed_count = 0,
                  sptassvgmed_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(sptassvgmed, BrthYear),
                 fill = list(
                  sptassvgmed_count = 0,
                  sptassvgmed_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  sptassvgmed_delta = (sptassvgmed_rate - lag(sptassvgmed_rate))/lag(sptassvgmed_rate),
  sptassvgmed_delta = ifelse(BrthYear == 2014, NA, sptassvgmed_delta),
  sptassvgmed_deltap = (sptassvgmed_rate - lag(sptassvgmed_rate,9))/lag(sptassvgmed_rate,9)
 ) %>%
 ungroup()

#### Severe perineal trauma with Operative Vaginal Birth without mediolateral episiotomy ----

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
        DMEPISIO,
        CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(BrthYear, trdfrth, assvg, med, CHNuid) %>%
 mutate(
  sptassvgnmed_count = n()
 ) %>%
 group_by(BrthYear, assvg, CHNuid) %>%
 mutate(
  total_year = n(),
  sptassvgnmed_rate = sptassvgnmed_count/total_year
 ) %>%
 ungroup() %>%
 filter(sptassvgnmed %in% 1, !is.na(CHNuid)) %>%
 select(BrthYear,CHNuid,sptassvgnmed,sptassvgnmed_count,sptassvgnmed_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sptassvgnmed, CHNuid),
                 fill = list(
                  sptassvgnmed_count = 0,
                  sptassvgnmed_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(sptassvgnmed, BrthYear),
                 fill = list(
                  sptassvgnmed_count = 0,
                  sptassvgnmed_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  sptassvgnmed_delta = (sptassvgnmed_rate - lag(sptassvgnmed_rate))/lag(sptassvgnmed_rate),
  sptassvgnmed_delta = ifelse(BrthYear == 2014, NA, sptassvgnmed_delta),
  sptassvgnmed_deltap = (sptassvgnmed_rate - lag(sptassvgnmed_rate,9))/lag(sptassvgnmed_rate,9)
 ) %>%
 ungroup()

#### Robson group ----

c12 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs1, RobsnGrp, CHNuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs1, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
        !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, rbs1, rbs1_count, rbs1_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(rbs1, CHNuid),
                 fill = list(
                  rbs1_count = 0,
                  rbs1_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(rbs1, BrthYear),
                 fill = list(
                  rbs1_count = 0,
                  rbs1_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  rbs1_delta = (rbs1_rate - lag(rbs1_rate))/lag(rbs1_rate),
  rbs1_delta = ifelse(BrthYear == 2014, NA, rbs1_delta),
  rbs1_deltap = (rbs1_rate - lag(rbs1_rate,9))/lag(rbs1_rate,9)
 ) %>%
 ungroup()

c13 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs21, RobsnGrp, CHNuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs21, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
        !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, rbs21, rbs21_count, rbs21_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(rbs21, CHNuid),
                 fill = list(
                  rbs21_count = 0,
                  rbs21_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(rbs21, BrthYear),
                 fill = list(
                  rbs21_count = 0,
                  rbs21_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  rbs21_delta = (rbs21_rate - lag(rbs21_rate))/lag(rbs21_rate),
  rbs21_delta = ifelse(BrthYear == 2014, NA, rbs21_delta),
  rbs21_deltap = (rbs21_rate - lag(rbs21_rate,9))/lag(rbs21_rate,9)
 ) %>%
 ungroup()

c14 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs51, RobsnGrp, CHNuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs51, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
        !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, rbs51, rbs51_count, rbs51_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(rbs51, CHNuid),
                 fill = list(
                  rbs51_count = 0,
                  rbs51_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(rbs51, BrthYear),
                 fill = list(
                  rbs51_count = 0,
                  rbs51_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  rbs51_delta = (rbs51_rate - lag(rbs51_rate))/lag(rbs51_rate),
  rbs51_delta = ifelse(BrthYear == 2014, NA, rbs51_delta),
  rbs51_deltap = (rbs51_rate - lag(rbs51_rate,9))/lag(rbs51_rate,9)
 ) %>%
 ungroup()

#### Postpartum hemorrhage treated with a blood transfusion ----

c15 <- dta %>%
 select(BrthYear, Any_Blood_Product, Postpartum_Haemorrhage, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  pphbl = case_when(
   Any_Blood_Product > 0 & Postpartum_Haemorrhage > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, Any_Blood_Product, Postpartum_Haemorrhage, CHNuid) %>%
 mutate(
  pphbl_count = n()
 ) %>%
 group_by(BrthYear, CHNuid) %>%
 mutate(
  total_year = n(),
  pphbl_rate = pphbl_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Blood_Product %in% 1, Postpartum_Haemorrhage %in% 1,
        !is.na(CHNuid)) %>%
 select(BrthYear,CHNuid,pphbl, pphbl_count, pphbl_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pphbl, CHNuid),
                 fill = list(
                  pphbl_count = 0,
                  pphbl_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(pphbl, BrthYear),
                 fill = list(
                  pphbl_count = 0,
                  pphbl_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  pphbl_delta = (pphbl_rate - lag(pphbl_rate))/lag(pphbl_rate),
  pphbl_delta = ifelse(BrthYear == 2014, NA, pphbl_delta),
  pphbl_deltap = (pphbl_rate - lag(pphbl_rate,9))/lag(pphbl_rate,9)
 ) %>%
 ungroup()

#### Postpartum hemorrhage resulting in procedural intervention ----

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
        MODEDEL,
        CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(BrthYear, pphpi, Postpartum_Haemorrhage, CHNuid) %>%
 mutate(
  pphpi_count = n()
 ) %>%
 group_by(BrthYear, Postpartum_Haemorrhage, CHNuid) %>%
 mutate(
  total_year = n(),
  pphpi_rate = pphpi_count/total_year
 ) %>%
 ungroup() %>%
 filter(pphpi %in% 1, Postpartum_Haemorrhage %in% 1,
        !is.na(CHNuid)) %>%
 select(BrthYear,CHNuid,pphpi, pphpi_count, pphpi_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pphpi, CHNuid),
                 fill = list(
                  pphpi_count = 0,
                  pphpi_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(pphpi, BrthYear),
                 fill = list(
                  pphpi_count = 0,
                  pphpi_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  pphpi_delta = (pphpi_rate - lag(pphpi_rate))/lag(pphpi_rate),
  pphpi_delta = ifelse(BrthYear == 2014, NA, pphpi_delta),
  pphpi_deltap = (pphpi_rate - lag(pphpi_rate,9))/lag(pphpi_rate,9)
 ) %>%
 ungroup()

#### Postpartum readmission ----

c17 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, ppreadm, CHNuid) %>%
 distinct() %>%
 group_by(BrthYear, ppreadm, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  ppreadm_count = n()
 ) %>%
 group_by(BrthYear, CHNuid) %>%
 mutate(
  total_year = n(),
  ppreadm_rate = ppreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(ppreadm %in% 1,
        !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, ppreadm, ppreadm_count, ppreadm_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(ppreadm, CHNuid),
                 fill = list(
                  ppreadm_count = 0,
                  ppreadm_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(ppreadm, BrthYear),
                 fill = list(
                  ppreadm_count = 0,
                  ppreadm_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  ppreadm_delta = (ppreadm_rate - lag(ppreadm_rate))/lag(ppreadm_rate),
  ppreadm_delta = ifelse(BrthYear == 2014, NA, ppreadm_delta),
  ppreadm_deltap = (ppreadm_rate - lag(ppreadm_rate,9))/lag(ppreadm_rate,9)
 ) %>%
 ungroup()

#### Postpartum patients who received medical anticoagulation prophylaxis when indicated ----

#c18

#### Skin to skin ----

c19 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknskn, CHNuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, sknskn, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  sknskn_count = n()
 ) %>%
 group_by(BrthYear, CHNuid, lvb) %>%
 mutate(
  total_year = n(),
  sknskn_rate = sknskn_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskn %in% 1,
        !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, sknskn, sknskn_count, sknskn_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sknskn, CHNuid),
                 fill = list(
                  sknskn_count = 0,
                  sknskn_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(sknskn, BrthYear),
                 fill = list(
                  sknskn_count = 0,
                  sknskn_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  sknskn_delta = (sknskn_rate - lag(sknskn_rate))/lag(sknskn_rate),
  sknskn_delta = ifelse(BrthYear == 2014, NA, sknskn_delta),
  sknskn_deltap = (sknskn_rate - lag(sknskn_rate,9))/lag(sknskn_rate,9)
 ) %>%
 ungroup()

c20 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknsknvg, CHNuid, lvbvg) %>%
 distinct() %>%
 group_by(BrthYear, sknsknvg, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  sknsknvg_count = n()
 ) %>%
 group_by(BrthYear, CHNuid, lvbvg) %>%
 mutate(
  total_year = n(),
  sknsknvg_rate = sknsknvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknsknvg %in% 1,
        !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, sknsknvg, sknsknvg_count, sknsknvg_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sknsknvg, CHNuid),
                 fill = list(
                  sknsknvg_count = 0,
                  sknsknvg_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(sknsknvg, BrthYear),
                 fill = list(
                  sknsknvg_count = 0,
                  sknsknvg_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  sknsknvg_delta = (sknsknvg_rate - lag(sknsknvg_rate))/lag(sknsknvg_rate),
  sknsknvg_delta = ifelse(BrthYear == 2014, NA, sknsknvg_delta),
  sknsknvg_deltap = (sknsknvg_rate - lag(sknsknvg_rate,9))/lag(sknsknvg_rate,9)
 ) %>%
 ungroup()

c21 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknskncs, CHNuid, lvbcs) %>%
 distinct() %>%
 group_by(BrthYear, sknskncs, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  sknskncs_count = n()
 ) %>%
 group_by(BrthYear, CHNuid, lvbcs) %>%
 mutate(
  total_year = n(),
  sknskncs_rate = sknskncs_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskncs %in% 1,
        !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, sknskncs, sknskncs_count, sknskncs_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sknskncs, CHNuid),
                 fill = list(
                  sknskncs_count = 0,
                  sknskncs_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(sknskncs, BrthYear),
                 fill = list(
                  sknskncs_count = 0,
                  sknskncs_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  sknskncs_delta = (sknskncs_rate - lag(sknskncs_rate))/lag(sknskncs_rate),
  sknskncs_delta = ifelse(BrthYear == 2014, NA, sknskncs_delta),
  sknskncs_deltap = (sknskncs_rate - lag(sknskncs_rate,9))/lag(sknskncs_rate,9)
 ) %>%
 ungroup()

#### Postpartum blood products amongst those who received antenatal iron therapy ----

c22 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
  pphiv = case_when(
   Any_Blood_Product > 0 & R003_02100 > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, Any_Blood_Product, R003_02100, CHNuid) %>%
 mutate(
  pphiv_count = n()
 ) %>%
 group_by(BrthYear, Any_Blood_Product, CHNuid) %>%
 mutate(
  total_year = n(),
  pphiv_rate = pphiv_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Blood_Product %in% 1, R003_02100 %in% 1,
        !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, pphiv, pphiv_count, pphiv_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pphiv, CHNuid),
                 fill = list(
                  pphiv_count = 0,
                  pphiv_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(pphiv, BrthYear),
                 fill = list(
                  pphiv_count = 0,
                  pphiv_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  pphiv_delta = (pphiv_rate - lag(pphiv_rate))/lag(pphiv_rate),
  pphiv_delta = ifelse(BrthYear == 2014, NA, pphiv_delta),
  pphiv_deltap = (pphiv_rate - lag(pphiv_rate,9))/lag(pphiv_rate,9)
 ) %>%
 ungroup()

#### Neonatal readmission ----

c23 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, neoreadm, CHNuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, neoreadm, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  neoreadm_count = n()
 ) %>%
 group_by(BrthYear, CHNuid, lvb) %>%
 mutate(
  total_year = n(),
  neoreadm_rate = neoreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(neoreadm %in% 1,
        !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, neoreadm, neoreadm_count, neoreadm_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(neoreadm, CHNuid),
                 fill = list(
                  neoreadm_count = 0,
                  neoreadm_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(neoreadm, BrthYear),
                 fill = list(
                  neoreadm_count = 0,
                  neoreadm_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  neoreadm_delta = (neoreadm_rate - lag(neoreadm_rate))/lag(neoreadm_rate),
  neoreadm_delta = ifelse(BrthYear == 2014, NA, neoreadm_delta),
  neoreadm_deltap = (neoreadm_rate - lag(neoreadm_rate,9))/lag(neoreadm_rate,9)
 ) %>%
 ungroup()

#### Preterm infants who received complete course of steroids > 24h and < 7 days prior to birth ----

c24 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(BrthYear, pretster, CHNuid) %>%
 mutate(
  pretster_count = n()
 ) %>%
 group_by(BrthYear, den, CHNuid) %>%
 mutate(
  total_year = n(),
  pretster_rate = pretster_count/total_year
 ) %>%
 ungroup() %>%
 filter(pretster %in% 1, !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, pretster, pretster_count,pretster_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pretster, CHNuid),
                 fill = list(
                  pretster_count = 0,
                  pretster_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(pretster, BrthYear),
                 fill = list(
                  pretster_count = 0,
                  pretster_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  pretster_delta = (pretster_rate - lag(pretster_rate))/lag(pretster_rate),
  pretster_delta = ifelse(BrthYear == 2014, NA, pretster_delta),
  pretster_deltap = (pretster_rate - lag(pretster_rate,9))/lag(pretster_rate,9)
 ) %>%
 ungroup()

#### Preterm babies born in facility without NICU ----

c25 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(BrthYear, pretnnicu, CHNuid) %>%
 mutate(
  pretnnicu_count = n()
 ) %>%
 group_by(BrthYear, den, CHNuid) %>%
 mutate(
  total_year = n(),
  pretnnicu_rate = pretnnicu_count/total_year
 ) %>%
 ungroup() %>%
 filter(pretnnicu %in% 1, !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, pretnnicu, pretnnicu_count,pretnnicu_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pretnnicu, CHNuid),
                 fill = list(
                  pretnnicu_count = 0,
                  pretnnicu_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(pretnnicu, BrthYear),
                 fill = list(
                  pretnnicu_count = 0,
                  pretnnicu_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  pretnnicu_delta = (pretnnicu_rate - lag(pretnnicu_rate))/lag(pretnnicu_rate),
  pretnnicu_delta = ifelse(BrthYear == 2014, NA, pretnnicu_delta),
  pretnnicu_deltap = (pretnnicu_rate - lag(pretnnicu_rate,9))/lag(pretnnicu_rate,9)
 ) %>%
 ungroup()

#### Newborn respiratory distress associated with low-risk repeat cesarean at term gestation (≥ 37 weeks < 39 weeks) ----

c26 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(BrthYear, newlwcs, CHNuid) %>%
 mutate(
  newlwcs_count = n()
 ) %>%
 group_by(BrthYear, den, CHNuid) %>%
 mutate(
  total_year = n(),
  newlwcs_rate = newlwcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(newlwcs %in% 1) %>%
 select(BrthYear, CHNuid, newlwcs, newlwcs_count,newlwcs_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(newlwcs, CHNuid),
                 fill = list(
                  newlwcs_count = 0,
                  newlwcs_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(newlwcs, BrthYear),
                 fill = list(
                  newlwcs_count = 0,
                  newlwcs_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  newlwcs_delta = (newlwcs_rate - lag(newlwcs_rate))/lag(newlwcs_rate),
  newlwcs_delta = ifelse(BrthYear == 2014, NA, newlwcs_delta),
  newlwcs_deltap = (newlwcs_rate - lag(newlwcs_rate,9))/lag(newlwcs_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping  ----

c27 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
  delcord = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (DEL_CORD_CLAMP %in% 3) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, delcord, CHNuid) %>%
 mutate(
  delcord_count = n()
 ) %>%
 group_by(BrthYear, lvb, CHNuid) %>%
 mutate(
  total_year = n(),
  delcord_rate = delcord_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcord %in% 1, lvb %in% 1,
        !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, delcord, delcord_count,delcord_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcord, CHNuid),
                 fill = list(
                  delcord_count = 0,
                  delcord_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(delcord, BrthYear),
                 fill = list(
                  delcord_count = 0,
                  delcord_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  delcord_delta = (delcord_rate - lag(delcord_rate))/lag(delcord_rate),
  delcord_delta = ifelse(BrthYear == 2014, NA, delcord_delta),
  delcord_deltap = (delcord_rate - lag(delcord_rate,9))/lag(delcord_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns ----

c28 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(BrthYear, delcordterm, CHNuid) %>%
 mutate(
  delcordterm_count = n()
 ) %>%
 group_by(BrthYear, den, CHNuid) %>%
 mutate(
  total_year = n(),
  delcordterm_rate = delcordterm_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordterm %in% 1, !is.na(CHNuid)) %>%
 select(BrthYear,CHNuid,delcordterm,delcordterm_count,delcordterm_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordterm, CHNuid),
                 fill = list(
                  delcordterm_count = 0,
                  delcordterm_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(delcordterm, BrthYear),
                 fill = list(
                  delcordterm_count = 0,
                  delcordterm_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  delcordterm_delta = (delcordterm_rate - lag(delcordterm_rate))/lag(delcordterm_rate),
  delcordterm_delta = ifelse(BrthYear == 2014, NA, delcordterm_delta),
  delcordterm_deltap = (delcordterm_rate - lag(delcordterm_rate,9))/lag(delcordterm_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns following spontaneous vaginal birth ----

c29 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(BrthYear, delcordtermvg, CHNuid) %>%
 mutate(
  delcordtermvg_count = n()
 ) %>%
 group_by(BrthYear, den, CHNuid) %>%
 mutate(
  total_year = n(),
  delcordtermvg_rate = delcordtermvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordtermvg %in% 1, !is.na(CHNuid)) %>%
 select(BrthYear,CHNuid, delcordtermvg, delcordtermvg_count,delcordtermvg_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordtermvg, CHNuid),
                 fill = list(
                  delcordtermvg_count = 0,
                  delcordtermvg_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(delcordtermvg, BrthYear),
                 fill = list(
                  delcordtermvg_count = 0,
                  delcordtermvg_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  delcordtermvg_delta = (delcordtermvg_rate - lag(delcordtermvg_rate))/lag(delcordtermvg_rate),
  delcordtermvg_delta = ifelse(BrthYear == 2014, NA, delcordtermvg_delta),
  delcordtermvg_deltap = (delcordtermvg_rate - lag(delcordtermvg_rate,9))/lag(delcordtermvg_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns following caesarean birth ----

c30 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(BrthYear, delcordtermcs, CHNuid) %>%
 mutate(
  delcordtermcs_count = n()
 ) %>%
 group_by(BrthYear, den, CHNuid) %>%
 mutate(
  total_year = n(),
  delcordtermcs_rate = delcordtermcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordtermcs %in% 1, !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, delcordtermcs, delcordtermcs_count,delcordtermcs_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordtermcs, CHNuid),
                 fill = list(
                  delcordtermcs_count = 0,
                  delcordtermcs_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(delcordtermcs, BrthYear),
                 fill = list(
                  delcordtermcs_count = 0,
                  delcordtermcs_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  delcordtermcs_delta = (delcordtermcs_rate - lag(delcordtermcs_rate))/lag(delcordtermcs_rate),
  delcordtermcs_delta = ifelse(BrthYear == 2014, NA, delcordtermcs_delta),
  delcordtermcs_deltap = (delcordtermcs_rate - lag(delcordtermcs_rate,9))/lag(delcordtermcs_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns ----

c31 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(BrthYear, delcordpreterm, CHNuid) %>%
 mutate(
  delcordpreterm_count = n()
 ) %>%
 group_by(BrthYear, den, CHNuid) %>%
 mutate(
  total_year = n(),
  delcordpreterm_rate = delcordpreterm_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpreterm %in% 1, !is.na(CHNuid)) %>%
 select(BrthYear,CHNuid, delcordpreterm, delcordpreterm_count,delcordpreterm_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordpreterm, CHNuid),
                 fill = list(
                  delcordpreterm_count = 0,
                  delcordpreterm_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(delcordpreterm, BrthYear),
                 fill = list(
                  delcordpreterm_count = 0,
                  delcordpreterm_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  delcordpreterm_delta = (delcordpreterm_rate - lag(delcordpreterm_rate))/lag(delcordpreterm_rate),
  delcordpreterm_delta = ifelse(BrthYear == 2014, NA, delcordpreterm_delta),
  delcordpreterm_deltap = (delcordpreterm_rate - lag(delcordpreterm_rate,9))/lag(delcordpreterm_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns following spontaneous vaginal birth ----

c32 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(BrthYear, delcordpretermvg, CHNuid) %>%
 mutate(
  delcordpretermvg_count = n()
 ) %>%
 group_by(BrthYear, den, CHNuid) %>%
 mutate(
  total_year = n(),
  delcordpretermvg_rate = delcordpretermvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpretermvg %in% 1, !is.na(CHNuid)) %>%
 select(BrthYear,CHNuid, delcordpretermvg, delcordpretermvg_count,delcordpretermvg_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordpretermvg, CHNuid),
                 fill = list(
                  delcordpretermvg_count = 0,
                  delcordpretermvg_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(delcordpretermvg, BrthYear),
                 fill = list(
                  delcordpretermvg_count = 0,
                  delcordpretermvg_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  delcordpretermvg_delta = (delcordpretermvg_rate - lag(delcordpretermvg_rate))/lag(delcordpretermvg_rate),
  delcordpretermvg_delta = ifelse(BrthYear == 2014, NA, delcordpretermvg_delta),
  delcordpretermvg_deltap = (delcordpretermvg_rate - lag(delcordpretermvg_rate,9))/lag(delcordpretermvg_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns following caesarean birth ----

c33 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(BrthYear, delcordpretermcs, CHNuid) %>%
 mutate(
  delcordpretermcs_count = n()
 ) %>%
 group_by(BrthYear, den, CHNuid) %>%
 mutate(
  total_year = n(),
  delcordpretermcs_rate = delcordpretermcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpretermcs %in% 1, !is.na(CHNuid)) %>%
 select(BrthYear,CHNuid, delcordpretermcs, delcordpretermcs_count,delcordpretermcs_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordpretermcs, CHNuid),
                 fill = list(
                  delcordpretermcs_count = 0,
                  delcordpretermcs_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(delcordpretermcs, BrthYear),
                 fill = list(
                  delcordpretermcs_count = 0,
                  delcordpretermcs_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  delcordpretermcs_delta = (delcordpretermcs_rate - lag(delcordpretermcs_rate))/lag(delcordpretermcs_rate),
  delcordpretermcs_delta = ifelse(BrthYear == 2014, NA, delcordpretermcs_delta),
  delcordpretermcs_deltap = (delcordpretermcs_rate - lag(delcordpretermcs_rate,9))/lag(delcordpretermcs_rate,9)
 ) %>%
 ungroup()

#### Milk feeding ----

c34 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, excbrst, CHNuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, excbrst, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  excbrst_count = n()
 ) %>%
 group_by(BrthYear, CHNuid, lvb) %>%
 mutate(
  total_year = n(),
  excbrst_rate = excbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(excbrst %in% 1,
        !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, excbrst, excbrst_count, excbrst_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(excbrst, CHNuid),
                 fill = list(
                  excbrst_count = 0,
                  excbrst_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(excbrst, BrthYear),
                 fill = list(
                  excbrst_count = 0,
                  excbrst_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  excbrst_delta = (excbrst_rate - lag(excbrst_rate))/lag(excbrst_rate),
  excbrst_delta = ifelse(BrthYear == 2014, NA, excbrst_delta),
  excbrst_deltap = (excbrst_rate - lag(excbrst_rate,9))/lag(excbrst_rate,9)
 ) %>%
 ungroup()

c35 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, nexcbrst, CHNuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, nexcbrst, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  nexcbrst_count = n()
 ) %>%
 group_by(BrthYear, CHNuid, lvb) %>%
 mutate(
  total_year = n(),
  nexcbrst_rate = nexcbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nexcbrst %in% 1,
        !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, nexcbrst, nexcbrst_count, nexcbrst_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(nexcbrst, CHNuid),
                 fill = list(
                  nexcbrst_count = 0,
                  nexcbrst_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(nexcbrst, BrthYear),
                 fill = list(
                  nexcbrst_count = 0,
                  nexcbrst_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  nexcbrst_delta = (nexcbrst_rate - lag(nexcbrst_rate))/lag(nexcbrst_rate),
  nexcbrst_delta = ifelse(BrthYear == 2014, NA, nexcbrst_delta),
  nexcbrst_deltap = (nexcbrst_rate - lag(nexcbrst_rate,9))/lag(nexcbrst_rate,9)
 ) %>%
 ungroup()

c36 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, nbrst, CHNuid, lvbint) %>%
 distinct() %>%
 group_by(BrthYear, nbrst, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  nbrst_count = n()
 ) %>%
 group_by(BrthYear, CHNuid, lvbint) %>%
 mutate(
  total_year = n(),
  nbrst_rate = nbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nbrst %in% 1,
        !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, nbrst, nbrst_count, nbrst_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(nbrst, CHNuid),
                 fill = list(
                  nbrst_count = 0,
                  nbrst_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(nbrst, BrthYear),
                 fill = list(
                  nbrst_count = 0,
                  nbrst_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  nbrst_delta = (nbrst_rate - lag(nbrst_rate))/lag(nbrst_rate),
  nbrst_delta = ifelse(BrthYear == 2014, NA, nbrst_delta),
  nbrst_deltap = (nbrst_rate - lag(nbrst_rate,9))/lag(nbrst_rate,9)
 ) %>%
 ungroup()

#### The breast/chest feeding initiation rate ----

c37 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, brstinit, CHNuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, brstinit, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  brstinit_count = n()
 ) %>%
 group_by(BrthYear, CHNuid, lvb) %>%
 mutate(
  total_year = n(),
  brstinit_rate = brstinit_count/total_year
 ) %>%
 ungroup() %>%
 filter(brstinit %in% 1,
        !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, brstinit, brstinit_count, brstinit_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(brstinit, CHNuid),
                 fill = list(
                  brstinit_count = 0,
                  brstinit_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(brstinit, BrthYear),
                 fill = list(
                  brstinit_count = 0,
                  brstinit_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  brstinit_delta = (brstinit_rate - lag(brstinit_rate))/lag(brstinit_rate),
  brstinit_delta = ifelse(BrthYear == 2014, NA, brstinit_delta),
  brstinit_deltap = (brstinit_rate - lag(brstinit_rate,9))/lag(brstinit_rate,9)
 ) %>%
 ungroup()

#### ICU Admission during Pregnancy or Postpartum ----

c38 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
  icu = case_when(
   JOGC_ICU > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, JOGC_ICU, CHNuid) %>%
 mutate(
  icu_count = n()
 ) %>%
 group_by(BrthYear, CHNuid) %>%
 mutate(
  total_year = n(),
  icu_rate = icu_count/total_year
 ) %>%
 ungroup() %>%
 filter(JOGC_ICU %in% 1, !is.na(CHNuid)) %>%
 select(BrthYear, CHNuid, icu, icu_count,icu_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(icu, CHNuid),
                 fill = list(
                  icu_count = 0,
                  icu_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(icu, BrthYear),
                 fill = list(
                  icu_count = 0,
                  icu_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  icu_delta = (icu_rate - lag(icu_rate))/lag(icu_rate),
  icu_delta = ifelse(BrthYear == 2014, NA, icu_delta),
  icu_deltap = (icu_rate - lag(icu_rate,9))/lag(icu_rate,9)
 ) %>%
 ungroup()

#### Rate of Severe Morbidity in Pregnancy or Postpartum ----

c39 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
  smm = case_when(
   JOGC_AnySMM > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, JOGC_AnySMM, CHNuid) %>%
 mutate(
  smm_count = n()
 ) %>%
 group_by(BrthYear, lvb, CHNuid) %>%
 mutate(
  total_year = n(),
  smm_rate = smm_count/total_year
 ) %>%
 ungroup() %>%
 filter(JOGC_AnySMM %in% 1, lvb %in% 1, !is.na(CHNuid)) %>%
 select(BrthYear,CHNuid, smm, smm_count,smm_rate) %>%
 distinct() %>%
 arrange(CHNuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(smm, CHNuid),
                 fill = list(
                  smm_count = 0,
                  smm_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(smm, BrthYear),
                 fill = list(
                  smm_count = 0,
                  smm_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  smm_delta = (smm_rate - lag(smm_rate))/lag(smm_rate),
  smm_delta = ifelse(BrthYear == 2014, NA, smm_delta),
  smm_deltap = (smm_rate - lag(smm_rate,9))/lag(smm_rate,9)
 ) %>%
 ungroup()

cchn_stats <- cbind(
 c1 %>% select(-PreExisting_Hypertension),
 c2 %>% select(-Gestational_Hypertension,-CHNuid,-BrthYear),
 c3 %>% select(-Any_Hypertension,-CHNuid,-BrthYear),
 c4 %>% select(-anemia,-CHNuid,-BrthYear),
 c5 %>% select(-PreExisting_Diabetes,-CHNuid,-BrthYear),
 c6 %>% select(-GDM,-CHNuid,-BrthYear),
 c7 %>% select(-Any_Diabetes,-CHNuid,-BrthYear),
 c8 %>% select(-sptvg,-CHNuid,-BrthYear),
 c9 %>% select(-med,-CHNuid,-BrthYear),
 c10 %>% select(-sptassvgmed,-CHNuid,-BrthYear),
 c11 %>% select(-sptassvgnmed,-CHNuid,-BrthYear),
 c12 %>% select(-rbs1,-CHNuid,-BrthYear),
 c13 %>% select(-rbs21,-CHNuid,-BrthYear),
 c14 %>% select(-rbs51,-CHNuid,-BrthYear),
 c15 %>% select(-pphbl,-CHNuid,-BrthYear),
 c16 %>% select(-pphpi,-CHNuid,-BrthYear),
 c17 %>% select(-ppreadm,-CHNuid,-BrthYear),
 #c18 %>% select(-brstinit,-CHNuid,-BrthYear),
 c19 %>% select(-sknskn,-CHNuid,-BrthYear),
 c20 %>% select(-sknsknvg,-CHNuid,-BrthYear),
 c21 %>% select(-sknskncs,-CHNuid,-BrthYear),
 c22 %>% select(-pphiv,-CHNuid,-BrthYear),
 c23 %>% select(-neoreadm,-CHNuid,-BrthYear),
 c24 %>% select(-pretster,-CHNuid,-BrthYear),
 c25 %>% select(-pretnnicu,-CHNuid,-BrthYear),
 c26 %>% select(-newlwcs,-CHNuid,-BrthYear),
 c27 %>% select(-delcord,-CHNuid,-BrthYear),
 c28 %>% select(-delcordterm,-CHNuid,-BrthYear),
 c29 %>% select(-delcordtermvg,-CHNuid,-BrthYear),
 c30 %>% select(-delcordtermcs,-CHNuid,-BrthYear),
 c31 %>% select(-delcordpreterm,-CHNuid,-BrthYear),
 c32 %>% select(-delcordpretermvg,-CHNuid,-BrthYear),
 c33 %>% select(-delcordpretermcs,-CHNuid,-BrthYear),
 c34 %>% select(-excbrst,-CHNuid,-BrthYear),
 c35 %>% select(-nexcbrst,-CHNuid,-BrthYear),
 c36 %>% select(-nbrst,-CHNuid,-BrthYear),
 c37 %>% select(-brstinit,-CHNuid,-BrthYear),
 c38 %>% select(-icu,-CHNuid,-BrthYear),
 c39 %>% select(-smm,-CHNuid,-BrthYear)
)

### Health authority zone (HR) ----
#### Hypertension ----

c1 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, PreExisting_Hypertension, HRuid) %>%
 distinct() %>%
 group_by(BrthYear, PreExisting_Hypertension, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  prehyp_count = n()
 ) %>%
 group_by(BrthYear, HRuid) %>%
 mutate(
  total_year = n(),
  prehyp_rate = prehyp_count/total_year
 ) %>%
 filter(PreExisting_Hypertension %in% 1,
        !is.na(HRuid)) %>%
 select(BrthYear, HRuid, PreExisting_Hypertension, prehyp_count, prehyp_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(PreExisting_Hypertension, HRuid),
                 fill = list(
                  prehyp_count = 0,
                  prehyp_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(PreExisting_Hypertension, BrthYear),
                 fill = list(
                  prehyp_count = 0,
                  prehyp_rate = NA
                 )) %>%
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
  HRuid = as.character(HRuid),
  gesthyp_count = n()
 ) %>%
 group_by(BrthYear, HRuid) %>%
 mutate(
  total_year = n(),
  gesthyp_rate = gesthyp_count/total_year
 ) %>%
 filter(Gestational_Hypertension %in% 1,
        !is.na(HRuid)) %>%
 select(BrthYear, HRuid, Gestational_Hypertension, gesthyp_count, gesthyp_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(Gestational_Hypertension, HRuid),
                 fill = list(
                  gesthyp_count = 0,
                  gesthyp_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(Gestational_Hypertension, BrthYear),
                 fill = list(
                  gesthyp_count = 0,
                  gesthyp_rate = NA
                 )) %>%
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
  HRuid = as.character(HRuid),
  hyp_count = n()
 ) %>%
 group_by(BrthYear, HRuid) %>%
 mutate(
  total_year = n(),
  hyp_rate = hyp_count/total_year
 ) %>%
 filter(Any_Hypertension %in% 1,
        !is.na(HRuid)) %>%
 select(BrthYear, HRuid, Any_Hypertension, hyp_count, hyp_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(Any_Hypertension, HRuid),
                 fill = list(
                  hyp_count = 0,
                  hyp_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(Any_Hypertension, BrthYear),
                 fill = list(
                  hyp_count = 0,
                  hyp_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  hyp_delta = (hyp_rate - lag(hyp_rate))/lag(hyp_rate),
  hyp_delta = ifelse(BrthYear == 2014, NA, hyp_delta),
  hyp_deltap = (hyp_rate - lag(hyp_rate,9))/lag(hyp_rate,9)
 ) %>%
 ungroup()

#### Anaemia ----

c4 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, HRuid, MANEM, R014_01500, MO990, MD50_D53, MD55_D59, MD60_D64) %>%
 mutate(anemia = case_when(
  MANEM > 0 | R014_01500 > 0 | MO990 > 0 | MD50_D53 > 0 |
   MD55_D59 > 0 | MD60_D64 > 0 ~ 1,
  TRUE ~ NA_integer_
 )) %>%
 distinct() %>%
 group_by(BrthYear, anemia, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  anemia_count = n()
 ) %>%
 group_by(BrthYear, HRuid) %>%
 mutate(
  total_year = n(),
  anemia_rate = anemia_count/total_year
 ) %>%
 filter(anemia %in% 1,
        !is.na(HRuid)) %>%
 select(BrthYear, HRuid, anemia, anemia_count, anemia_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(anemia, HRuid),
                 fill = list(
                  anemia_count = 0,
                  anemia_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(anemia, BrthYear),
                 fill = list(
                  anemia_count = 0,
                  anemia_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  anemia_delta = (anemia_rate - lag(anemia_rate))/lag(anemia_rate),
  anemia_delta = ifelse(BrthYear == 2014, NA, anemia_delta),
  anemia_deltap = (anemia_rate - lag(anemia_rate,9))/lag(anemia_rate,9)
 ) %>%
 ungroup()

#### Diabetes ----

c5 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, PreExisting_Diabetes, HRuid) %>%
 distinct() %>%
 group_by(BrthYear, PreExisting_Diabetes, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  prediab_count = n()
 ) %>%
 group_by(BrthYear, HRuid) %>%
 mutate(
  total_year = n(),
  prediab_rate = prediab_count/total_year
 ) %>%
 filter(PreExisting_Diabetes %in% 1,
        !is.na(HRuid)) %>%
 select(BrthYear, HRuid, PreExisting_Diabetes, prediab_count, prediab_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(PreExisting_Diabetes, HRuid),
                 fill = list(
                  prediab_count = 0,
                  prediab_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(PreExisting_Diabetes, BrthYear),
                 fill = list(
                  prediab_count = 0,
                  prediab_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  prediab_delta = (prediab_rate - lag(prediab_rate))/lag(prediab_rate),
  prediab_delta = ifelse(BrthYear == 2014, NA, prediab_delta),
  prediab_deltap = (prediab_rate - lag(prediab_rate,9))/lag(prediab_rate,9)
 ) %>%
 ungroup()

c6 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, GDM, HRuid) %>%
 distinct() %>%
 group_by(BrthYear, GDM, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  gestdiab_count = n()
 ) %>%
 group_by(BrthYear, HRuid) %>%
 mutate(
  total_year = n(),
  gestdiab_rate = gestdiab_count/total_year
 ) %>%
 filter(GDM %in% 1,
        !is.na(HRuid)) %>%
 select(BrthYear, HRuid, GDM, gestdiab_count, gestdiab_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(GDM, HRuid),
                 fill = list(
                  gestdiab_count = 0,
                  gestdiab_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(GDM, BrthYear),
                 fill = list(
                  gestdiab_count = 0,
                  gestdiab_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  gestdiab_delta = (gestdiab_rate - lag(gestdiab_rate))/lag(gestdiab_rate),
  gestdiab_delta = ifelse(BrthYear == 2014, NA, gestdiab_delta),
  gestdiab_deltap = (gestdiab_rate - lag(gestdiab_rate,9))/lag(gestdiab_rate,9)
 ) %>%
 ungroup()

c7 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, Any_Diabetes, HRuid) %>%
 distinct() %>%
 group_by(BrthYear, Any_Diabetes, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  diab_count = n()
 ) %>%
 group_by(BrthYear, HRuid) %>%
 mutate(
  total_year = n(),
  diab_rate = diab_count/total_year
 ) %>%
 filter(Any_Diabetes %in% 1,
        !is.na(HRuid)) %>%
 select(BrthYear, HRuid, Any_Diabetes, diab_count, diab_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(Any_Diabetes, HRuid),
                 fill = list(
                  diab_count = 0,
                  diab_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(Any_Diabetes, BrthYear),
                 fill = list(
                  diab_count = 0,
                  diab_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  diab_delta = (diab_rate - lag(diab_rate))/lag(diab_rate),
  diab_delta = ifelse(BrthYear == 2014, NA, diab_delta),
  diab_deltap = (diab_rate - lag(diab_rate,9))/lag(diab_rate,9)
 ) %>%
 ungroup()

#### Severe Perineal Trauma with Spontaneous Vaginal Birth ----

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
        DMMETHOD,
        HRuid) %>%
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
 group_by(BrthYear, trdfrth, sptvg, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  spt_count = n()
 ) %>%
 group_by(BrthYear, sptvg, HRuid) %>%
 mutate(
  total_year = n(),
  spt_rate = spt_count/total_year
 ) %>%
 ungroup() %>%
 filter(trdfrth %in% 1, sptvg %in% 1, !is.na(HRuid)) %>%
 select(BrthYear,HRuid,sptvg,spt_count,spt_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sptvg, HRuid),
                 fill = list(
                  spt_count = 0,
                  spt_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(sptvg, BrthYear),
                 fill = list(
                  spt_count = 0,
                  spt_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  spt_delta = (spt_rate - lag(spt_rate))/lag(spt_rate),
  spt_delta = ifelse(BrthYear == 2014, NA, spt_delta),
  spt_deltap = (spt_rate - lag(spt_rate,9))/lag(spt_rate,9)
 ) %>%
 ungroup()

#### Mediolateral Episiotomy with Operative Vaginal Birth ----
#### ND = Not Done
#### MD = Midline
#### ML = Mediolateral

c9 <- dta %>%
 select(BrthYear,
        DMMETHOD,
        DMEPISIO,
        HRuid) %>%
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
 group_by(BrthYear, med, assvg, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  med_count = n()
 ) %>%
 group_by(BrthYear, assvg, HRuid) %>%
 mutate(
  total_year = n(),
  med_rate = med_count/total_year
 ) %>%
 ungroup() %>%
 filter(med %in% 1, assvg %in% 1, !is.na(HRuid)) %>%
 select(BrthYear,HRuid,med,med_count,med_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(med, HRuid),
                 fill = list(
                  med_count = 0,
                  med_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(med, BrthYear),
                 fill = list(
                  med_count = 0,
                  med_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  med_delta = (med_rate - lag(med_rate))/lag(med_rate),
  med_delta = ifelse(BrthYear == 2014, NA, med_delta),
  med_deltap = (med_rate - lag(med_rate,9))/lag(med_rate,9)
 ) %>%
 ungroup()

#### Severe perineal trauma with Operative Vaginal Birth and mediolateral episiotomy ----

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
        DMEPISIO,
        HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(BrthYear, trdfrth, assvg, med, HRuid) %>%
 mutate(
  sptassvgmed_count = n()
 ) %>%
 group_by(BrthYear, assvg, HRuid) %>%
 mutate(
  total_year = n(),
  sptassvgmed_rate = sptassvgmed_count/total_year
 ) %>%
 ungroup() %>%
 filter(sptassvgmed %in% 1, !is.na(HRuid)) %>%
 select(BrthYear,HRuid,sptassvgmed,sptassvgmed_count,sptassvgmed_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sptassvgmed, HRuid),
                 fill = list(
                  sptassvgmed_count = 0,
                  sptassvgmed_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(sptassvgmed, BrthYear),
                 fill = list(
                  sptassvgmed_count = 0,
                  sptassvgmed_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  sptassvgmed_delta = (sptassvgmed_rate - lag(sptassvgmed_rate))/lag(sptassvgmed_rate),
  sptassvgmed_delta = ifelse(BrthYear == 2014, NA, sptassvgmed_delta),
  sptassvgmed_deltap = (sptassvgmed_rate - lag(sptassvgmed_rate,9))/lag(sptassvgmed_rate,9)
 ) %>%
 ungroup()

#### Severe perineal trauma with Operative Vaginal Birth without mediolateral episiotomy ----

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
        DMEPISIO,
        HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(BrthYear, trdfrth, assvg, med, HRuid) %>%
 mutate(
  sptassvgnmed_count = n()
 ) %>%
 group_by(BrthYear, assvg, HRuid) %>%
 mutate(
  total_year = n(),
  sptassvgnmed_rate = sptassvgnmed_count/total_year
 ) %>%
 ungroup() %>%
 filter(sptassvgnmed %in% 1, !is.na(HRuid)) %>%
 select(BrthYear,HRuid,sptassvgnmed,sptassvgnmed_count,sptassvgnmed_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sptassvgnmed, HRuid),
                 fill = list(
                  sptassvgnmed_count = 0,
                  sptassvgnmed_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(sptassvgnmed, BrthYear),
                 fill = list(
                  sptassvgnmed_count = 0,
                  sptassvgnmed_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  sptassvgnmed_delta = (sptassvgnmed_rate - lag(sptassvgnmed_rate))/lag(sptassvgnmed_rate),
  sptassvgnmed_delta = ifelse(BrthYear == 2014, NA, sptassvgnmed_delta),
  sptassvgnmed_deltap = (sptassvgnmed_rate - lag(sptassvgnmed_rate,9))/lag(sptassvgnmed_rate,9)
 ) %>%
 ungroup()

#### Robson group ----

c12 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs1, RobsnGrp, HRuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs1, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
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
        !is.na(HRuid)) %>%
 select(BrthYear, HRuid, rbs1, rbs1_count, rbs1_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(rbs1, HRuid),
                 fill = list(
                  rbs1_count = 0,
                  rbs1_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(rbs1, BrthYear),
                 fill = list(
                  rbs1_count = 0,
                  rbs1_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  rbs1_delta = (rbs1_rate - lag(rbs1_rate))/lag(rbs1_rate),
  rbs1_delta = ifelse(BrthYear == 2014, NA, rbs1_delta),
  rbs1_deltap = (rbs1_rate - lag(rbs1_rate,9))/lag(rbs1_rate,9)
 ) %>%
 ungroup()

c13 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs21, RobsnGrp, HRuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs21, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
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
        !is.na(HRuid)) %>%
 select(BrthYear, HRuid, rbs21, rbs21_count, rbs21_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(rbs21, HRuid),
                 fill = list(
                  rbs21_count = 0,
                  rbs21_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(rbs21, BrthYear),
                 fill = list(
                  rbs21_count = 0,
                  rbs21_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  rbs21_delta = (rbs21_rate - lag(rbs21_rate))/lag(rbs21_rate),
  rbs21_delta = ifelse(BrthYear == 2014, NA, rbs21_delta),
  rbs21_deltap = (rbs21_rate - lag(rbs21_rate,9))/lag(rbs21_rate,9)
 ) %>%
 ungroup()

c14 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, rbs51, RobsnGrp, HRuid) %>%
 distinct() %>%
 group_by(BrthYear, rbs51, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
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
        !is.na(HRuid)) %>%
 select(BrthYear, HRuid, rbs51, rbs51_count, rbs51_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(rbs51, HRuid),
                 fill = list(
                  rbs51_count = 0,
                  rbs51_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(rbs51, BrthYear),
                 fill = list(
                  rbs51_count = 0,
                  rbs51_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  rbs51_delta = (rbs51_rate - lag(rbs51_rate))/lag(rbs51_rate),
  rbs51_delta = ifelse(BrthYear == 2014, NA, rbs51_delta),
  rbs51_deltap = (rbs51_rate - lag(rbs51_rate,9))/lag(rbs51_rate,9)
 ) %>%
 ungroup()

#### Postpartum hemorrhage treated with a blood transfusion ----

c15 <- dta %>%
 select(BrthYear, Any_Blood_Product, Postpartum_Haemorrhage, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  pphbl = case_when(
   Any_Blood_Product > 0 & Postpartum_Haemorrhage > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, Any_Blood_Product, Postpartum_Haemorrhage, HRuid) %>%
 mutate(
  pphbl_count = n()
 ) %>%
 group_by(BrthYear, HRuid) %>%
 mutate(
  total_year = n(),
  pphbl_rate = pphbl_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Blood_Product %in% 1, Postpartum_Haemorrhage %in% 1,
        !is.na(HRuid)) %>%
 select(BrthYear,HRuid,pphbl, pphbl_count, pphbl_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pphbl, HRuid),
                 fill = list(
                  pphbl_count = 0,
                  pphbl_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(pphbl, BrthYear),
                 fill = list(
                  pphbl_count = 0,
                  pphbl_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  pphbl_delta = (pphbl_rate - lag(pphbl_rate))/lag(pphbl_rate),
  pphbl_delta = ifelse(BrthYear == 2014, NA, pphbl_delta),
  pphbl_deltap = (pphbl_rate - lag(pphbl_rate,9))/lag(pphbl_rate,9)
 ) %>%
 ungroup()

#### Postpartum hemorrhage resulting in procedural intervention ----

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
        MODEDEL,
        HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(BrthYear, pphpi, Postpartum_Haemorrhage, HRuid) %>%
 mutate(
  pphpi_count = n()
 ) %>%
 group_by(BrthYear, Postpartum_Haemorrhage, HRuid) %>%
 mutate(
  total_year = n(),
  pphpi_rate = pphpi_count/total_year
 ) %>%
 ungroup() %>%
 filter(pphpi %in% 1, Postpartum_Haemorrhage %in% 1,
        !is.na(HRuid)) %>%
 select(BrthYear,HRuid,pphpi, pphpi_count, pphpi_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pphpi, HRuid),
                 fill = list(
                  pphpi_count = 0,
                  pphpi_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(pphpi, BrthYear),
                 fill = list(
                  pphpi_count = 0,
                  pphpi_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  pphpi_delta = (pphpi_rate - lag(pphpi_rate))/lag(pphpi_rate),
  pphpi_delta = ifelse(BrthYear == 2014, NA, pphpi_delta),
  pphpi_deltap = (pphpi_rate - lag(pphpi_rate,9))/lag(pphpi_rate,9)
 ) %>%
 ungroup()

#### Postpartum readmission ----

c17 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, ppreadm, HRuid) %>%
 distinct() %>%
 group_by(BrthYear, ppreadm, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  ppreadm_count = n()
 ) %>%
 group_by(BrthYear, HRuid) %>%
 mutate(
  total_year = n(),
  ppreadm_rate = ppreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(ppreadm %in% 1,
        !is.na(HRuid)) %>%
 select(BrthYear, HRuid, ppreadm, ppreadm_count, ppreadm_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(ppreadm, HRuid),
                 fill = list(
                  ppreadm_count = 0,
                  ppreadm_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(ppreadm, BrthYear),
                 fill = list(
                  ppreadm_count = 0,
                  ppreadm_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  ppreadm_delta = (ppreadm_rate - lag(ppreadm_rate))/lag(ppreadm_rate),
  ppreadm_delta = ifelse(BrthYear == 2014, NA, ppreadm_delta),
  ppreadm_deltap = (ppreadm_rate - lag(ppreadm_rate,9))/lag(ppreadm_rate,9)
 ) %>%
 ungroup()

#### Postpartum patients who received medical anticoagulation prophylaxis when indicated ----

#c18

#### Skin to skin ----

c19 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknskn, HRuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, sknskn, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  sknskn_count = n()
 ) %>%
 group_by(BrthYear, HRuid, lvb) %>%
 mutate(
  total_year = n(),
  sknskn_rate = sknskn_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskn %in% 1,
        !is.na(HRuid)) %>%
 select(BrthYear, HRuid, sknskn, sknskn_count, sknskn_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sknskn, HRuid),
                 fill = list(
                  sknskn_count = 0,
                  sknskn_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(sknskn, BrthYear),
                 fill = list(
                  sknskn_count = 0,
                  sknskn_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  sknskn_delta = (sknskn_rate - lag(sknskn_rate))/lag(sknskn_rate),
  sknskn_delta = ifelse(BrthYear == 2014, NA, sknskn_delta),
  sknskn_deltap = (sknskn_rate - lag(sknskn_rate,9))/lag(sknskn_rate,9)
 ) %>%
 ungroup()

c20 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknsknvg, HRuid, lvbvg) %>%
 distinct() %>%
 group_by(BrthYear, sknsknvg, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  sknsknvg_count = n()
 ) %>%
 group_by(BrthYear, HRuid, lvbvg) %>%
 mutate(
  total_year = n(),
  sknsknvg_rate = sknsknvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknsknvg %in% 1,
        !is.na(HRuid)) %>%
 select(BrthYear, HRuid, sknsknvg, sknsknvg_count, sknsknvg_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sknsknvg, HRuid),
                 fill = list(
                  sknsknvg_count = 0,
                  sknsknvg_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(sknsknvg, BrthYear),
                 fill = list(
                  sknsknvg_count = 0,
                  sknsknvg_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  sknsknvg_delta = (sknsknvg_rate - lag(sknsknvg_rate))/lag(sknsknvg_rate),
  sknsknvg_delta = ifelse(BrthYear == 2014, NA, sknsknvg_delta),
  sknsknvg_deltap = (sknsknvg_rate - lag(sknsknvg_rate,9))/lag(sknsknvg_rate,9)
 ) %>%
 ungroup()

c21 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, sknskncs, HRuid, lvbcs) %>%
 distinct() %>%
 group_by(BrthYear, sknskncs, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  sknskncs_count = n()
 ) %>%
 group_by(BrthYear, HRuid, lvbcs) %>%
 mutate(
  total_year = n(),
  sknskncs_rate = sknskncs_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskncs %in% 1,
        !is.na(HRuid)) %>%
 select(BrthYear, HRuid, sknskncs, sknskncs_count, sknskncs_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(sknskncs, HRuid),
                 fill = list(
                  sknskncs_count = 0,
                  sknskncs_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(sknskncs, BrthYear),
                 fill = list(
                  sknskncs_count = 0,
                  sknskncs_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  sknskncs_delta = (sknskncs_rate - lag(sknskncs_rate))/lag(sknskncs_rate),
  sknskncs_delta = ifelse(BrthYear == 2014, NA, sknskncs_delta),
  sknskncs_deltap = (sknskncs_rate - lag(sknskncs_rate,9))/lag(sknskncs_rate,9)
 ) %>%
 ungroup()

#### Postpartum blood products amongst those who received antenatal iron therapy ----

c22 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
  pphiv = case_when(
   Any_Blood_Product > 0 & R003_02100 > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, Any_Blood_Product, R003_02100, HRuid) %>%
 mutate(
  pphiv_count = n()
 ) %>%
 group_by(BrthYear, Any_Blood_Product, HRuid) %>%
 mutate(
  total_year = n(),
  pphiv_rate = pphiv_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Blood_Product %in% 1, R003_02100 %in% 1,
        !is.na(HRuid)) %>%
 select(BrthYear, HRuid, pphiv, pphiv_count, pphiv_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pphiv, HRuid),
                 fill = list(
                  pphiv_count = 0,
                  pphiv_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(pphiv, BrthYear),
                 fill = list(
                  pphiv_count = 0,
                  pphiv_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  pphiv_delta = (pphiv_rate - lag(pphiv_rate))/lag(pphiv_rate),
  pphiv_delta = ifelse(BrthYear == 2014, NA, pphiv_delta),
  pphiv_deltap = (pphiv_rate - lag(pphiv_rate,9))/lag(pphiv_rate,9)
 ) %>%
 ungroup()

#### Neonatal readmission ----

c23 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, neoreadm, HRuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, neoreadm, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  neoreadm_count = n()
 ) %>%
 group_by(BrthYear, HRuid, lvb) %>%
 mutate(
  total_year = n(),
  neoreadm_rate = neoreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(neoreadm %in% 1,
        !is.na(HRuid)) %>%
 select(BrthYear, HRuid, neoreadm, neoreadm_count, neoreadm_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(neoreadm, HRuid),
                 fill = list(
                  neoreadm_count = 0,
                  neoreadm_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(neoreadm, BrthYear),
                 fill = list(
                  neoreadm_count = 0,
                  neoreadm_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  neoreadm_delta = (neoreadm_rate - lag(neoreadm_rate))/lag(neoreadm_rate),
  neoreadm_delta = ifelse(BrthYear == 2014, NA, neoreadm_delta),
  neoreadm_deltap = (neoreadm_rate - lag(neoreadm_rate,9))/lag(neoreadm_rate,9)
 ) %>%
 ungroup()

#### Preterm infants who received complete course of steroids > 24h and < 7 days prior to birth ----

c24 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(BrthYear, pretster, HRuid) %>%
 mutate(
  pretster_count = n()
 ) %>%
 group_by(BrthYear, den, HRuid) %>%
 mutate(
  total_year = n(),
  pretster_rate = pretster_count/total_year
 ) %>%
 ungroup() %>%
 filter(pretster %in% 1, !is.na(HRuid)) %>%
 select(BrthYear, HRuid, pretster, pretster_count,pretster_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pretster, HRuid),
                 fill = list(
                  pretster_count = 0,
                  pretster_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(pretster, BrthYear),
                 fill = list(
                  pretster_count = 0,
                  pretster_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  pretster_delta = (pretster_rate - lag(pretster_rate))/lag(pretster_rate),
  pretster_delta = ifelse(BrthYear == 2014, NA, pretster_delta),
  pretster_deltap = (pretster_rate - lag(pretster_rate,9))/lag(pretster_rate,9)
 ) %>%
 ungroup()

#### Preterm babies born in facility without NICU ----

c25 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(BrthYear, pretnnicu, HRuid) %>%
 mutate(
  pretnnicu_count = n()
 ) %>%
 group_by(BrthYear, den, HRuid) %>%
 mutate(
  total_year = n(),
  pretnnicu_rate = pretnnicu_count/total_year
 ) %>%
 ungroup() %>%
 filter(pretnnicu %in% 1, !is.na(HRuid)) %>%
 select(BrthYear, HRuid, pretnnicu, pretnnicu_count,pretnnicu_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(pretnnicu, HRuid),
                 fill = list(
                  pretnnicu_count = 0,
                  pretnnicu_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(pretnnicu, BrthYear),
                 fill = list(
                  pretnnicu_count = 0,
                  pretnnicu_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  pretnnicu_delta = (pretnnicu_rate - lag(pretnnicu_rate))/lag(pretnnicu_rate),
  pretnnicu_delta = ifelse(BrthYear == 2014, NA, pretnnicu_delta),
  pretnnicu_deltap = (pretnnicu_rate - lag(pretnnicu_rate,9))/lag(pretnnicu_rate,9)
 ) %>%
 ungroup()

#### Newborn respiratory distress associated with low-risk repeat cesarean at term gestation (≥ 37 weeks < 39 weeks) ----

c26 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(BrthYear, newlwcs, HRuid) %>%
 mutate(
  newlwcs_count = n()
 ) %>%
 group_by(BrthYear, den, HRuid) %>%
 mutate(
  total_year = n(),
  newlwcs_rate = newlwcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(newlwcs %in% 1) %>%
 select(BrthYear, HRuid, newlwcs, newlwcs_count,newlwcs_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(newlwcs, HRuid),
                 fill = list(
                  newlwcs_count = 0,
                  newlwcs_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(newlwcs, BrthYear),
                 fill = list(
                  newlwcs_count = 0,
                  newlwcs_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  newlwcs_delta = (newlwcs_rate - lag(newlwcs_rate))/lag(newlwcs_rate),
  newlwcs_delta = ifelse(BrthYear == 2014, NA, newlwcs_delta),
  newlwcs_deltap = (newlwcs_rate - lag(newlwcs_rate,9))/lag(newlwcs_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping  ----

c27 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
  delcord = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (DEL_CORD_CLAMP %in% 3) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, delcord, HRuid) %>%
 mutate(
  delcord_count = n()
 ) %>%
 group_by(BrthYear, lvb, HRuid) %>%
 mutate(
  total_year = n(),
  delcord_rate = delcord_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcord %in% 1, lvb %in% 1,
        !is.na(HRuid)) %>%
 select(BrthYear, HRuid, delcord, delcord_count,delcord_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcord, HRuid),
                 fill = list(
                  delcord_count = 0,
                  delcord_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(delcord, BrthYear),
                 fill = list(
                  delcord_count = 0,
                  delcord_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  delcord_delta = (delcord_rate - lag(delcord_rate))/lag(delcord_rate),
  delcord_delta = ifelse(BrthYear == 2014, NA, delcord_delta),
  delcord_deltap = (delcord_rate - lag(delcord_rate,9))/lag(delcord_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns ----

c28 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(BrthYear, delcordterm, HRuid) %>%
 mutate(
  delcordterm_count = n()
 ) %>%
 group_by(BrthYear, den, HRuid) %>%
 mutate(
  total_year = n(),
  delcordterm_rate = delcordterm_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordterm %in% 1, !is.na(HRuid)) %>%
 select(BrthYear,HRuid,delcordterm,delcordterm_count,delcordterm_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordterm, HRuid),
                 fill = list(
                  delcordterm_count = 0,
                  delcordterm_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(delcordterm, BrthYear),
                 fill = list(
                  delcordterm_count = 0,
                  delcordterm_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  delcordterm_delta = (delcordterm_rate - lag(delcordterm_rate))/lag(delcordterm_rate),
  delcordterm_delta = ifelse(BrthYear == 2014, NA, delcordterm_delta),
  delcordterm_deltap = (delcordterm_rate - lag(delcordterm_rate,9))/lag(delcordterm_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns following spontaneous vaginal birth ----

c29 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(BrthYear, delcordtermvg, HRuid) %>%
 mutate(
  delcordtermvg_count = n()
 ) %>%
 group_by(BrthYear, den, HRuid) %>%
 mutate(
  total_year = n(),
  delcordtermvg_rate = delcordtermvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordtermvg %in% 1, !is.na(HRuid)) %>%
 select(BrthYear,HRuid, delcordtermvg, delcordtermvg_count,delcordtermvg_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordtermvg, HRuid),
                 fill = list(
                  delcordtermvg_count = 0,
                  delcordtermvg_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(delcordtermvg, BrthYear),
                 fill = list(
                  delcordtermvg_count = 0,
                  delcordtermvg_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  delcordtermvg_delta = (delcordtermvg_rate - lag(delcordtermvg_rate))/lag(delcordtermvg_rate),
  delcordtermvg_delta = ifelse(BrthYear == 2014, NA, delcordtermvg_delta),
  delcordtermvg_deltap = (delcordtermvg_rate - lag(delcordtermvg_rate,9))/lag(delcordtermvg_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns following caesarean birth ----

c30 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(BrthYear, delcordtermcs, HRuid) %>%
 mutate(
  delcordtermcs_count = n()
 ) %>%
 group_by(BrthYear, den, HRuid) %>%
 mutate(
  total_year = n(),
  delcordtermcs_rate = delcordtermcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordtermcs %in% 1, !is.na(HRuid)) %>%
 select(BrthYear, HRuid, delcordtermcs, delcordtermcs_count,delcordtermcs_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordtermcs, HRuid),
                 fill = list(
                  delcordtermcs_count = 0,
                  delcordtermcs_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(delcordtermcs, BrthYear),
                 fill = list(
                  delcordtermcs_count = 0,
                  delcordtermcs_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  delcordtermcs_delta = (delcordtermcs_rate - lag(delcordtermcs_rate))/lag(delcordtermcs_rate),
  delcordtermcs_delta = ifelse(BrthYear == 2014, NA, delcordtermcs_delta),
  delcordtermcs_deltap = (delcordtermcs_rate - lag(delcordtermcs_rate,9))/lag(delcordtermcs_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns ----

c31 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(BrthYear, delcordpreterm, HRuid) %>%
 mutate(
  delcordpreterm_count = n()
 ) %>%
 group_by(BrthYear, den, HRuid) %>%
 mutate(
  total_year = n(),
  delcordpreterm_rate = delcordpreterm_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpreterm %in% 1, !is.na(HRuid)) %>%
 select(BrthYear,HRuid, delcordpreterm, delcordpreterm_count,delcordpreterm_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordpreterm, HRuid),
                 fill = list(
                  delcordpreterm_count = 0,
                  delcordpreterm_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(delcordpreterm, BrthYear),
                 fill = list(
                  delcordpreterm_count = 0,
                  delcordpreterm_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  delcordpreterm_delta = (delcordpreterm_rate - lag(delcordpreterm_rate))/lag(delcordpreterm_rate),
  delcordpreterm_delta = ifelse(BrthYear == 2014, NA, delcordpreterm_delta),
  delcordpreterm_deltap = (delcordpreterm_rate - lag(delcordpreterm_rate,9))/lag(delcordpreterm_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns following spontaneous vaginal birth ----

c32 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(BrthYear, delcordpretermvg, HRuid) %>%
 mutate(
  delcordpretermvg_count = n()
 ) %>%
 group_by(BrthYear, den, HRuid) %>%
 mutate(
  total_year = n(),
  delcordpretermvg_rate = delcordpretermvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpretermvg %in% 1, !is.na(HRuid)) %>%
 select(BrthYear,HRuid, delcordpretermvg, delcordpretermvg_count,delcordpretermvg_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordpretermvg, HRuid),
                 fill = list(
                  delcordpretermvg_count = 0,
                  delcordpretermvg_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(delcordpretermvg, BrthYear),
                 fill = list(
                  delcordpretermvg_count = 0,
                  delcordpretermvg_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  delcordpretermvg_delta = (delcordpretermvg_rate - lag(delcordpretermvg_rate))/lag(delcordpretermvg_rate),
  delcordpretermvg_delta = ifelse(BrthYear == 2014, NA, delcordpretermvg_delta),
  delcordpretermvg_deltap = (delcordpretermvg_rate - lag(delcordpretermvg_rate,9))/lag(delcordpretermvg_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns following caesarean birth ----

c33 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(BrthYear, delcordpretermcs, HRuid) %>%
 mutate(
  delcordpretermcs_count = n()
 ) %>%
 group_by(BrthYear, den, HRuid) %>%
 mutate(
  total_year = n(),
  delcordpretermcs_rate = delcordpretermcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpretermcs %in% 1, !is.na(HRuid)) %>%
 select(BrthYear,HRuid, delcordpretermcs, delcordpretermcs_count,delcordpretermcs_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(delcordpretermcs, HRuid),
                 fill = list(
                  delcordpretermcs_count = 0,
                  delcordpretermcs_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(delcordpretermcs, BrthYear),
                 fill = list(
                  delcordpretermcs_count = 0,
                  delcordpretermcs_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  delcordpretermcs_delta = (delcordpretermcs_rate - lag(delcordpretermcs_rate))/lag(delcordpretermcs_rate),
  delcordpretermcs_delta = ifelse(BrthYear == 2014, NA, delcordpretermcs_delta),
  delcordpretermcs_deltap = (delcordpretermcs_rate - lag(delcordpretermcs_rate,9))/lag(delcordpretermcs_rate,9)
 ) %>%
 ungroup()

#### Milk feeding ----

c34 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, excbrst, HRuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, excbrst, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  excbrst_count = n()
 ) %>%
 group_by(BrthYear, HRuid, lvb) %>%
 mutate(
  total_year = n(),
  excbrst_rate = excbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(excbrst %in% 1,
        !is.na(HRuid)) %>%
 select(BrthYear, HRuid, excbrst, excbrst_count, excbrst_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(excbrst, HRuid),
                 fill = list(
                  excbrst_count = 0,
                  excbrst_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(excbrst, BrthYear),
                 fill = list(
                  excbrst_count = 0,
                  excbrst_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  excbrst_delta = (excbrst_rate - lag(excbrst_rate))/lag(excbrst_rate),
  excbrst_delta = ifelse(BrthYear == 2014, NA, excbrst_delta),
  excbrst_deltap = (excbrst_rate - lag(excbrst_rate,9))/lag(excbrst_rate,9)
 ) %>%
 ungroup()

c35 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, nexcbrst, HRuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, nexcbrst, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  nexcbrst_count = n()
 ) %>%
 group_by(BrthYear, HRuid, lvb) %>%
 mutate(
  total_year = n(),
  nexcbrst_rate = nexcbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nexcbrst %in% 1,
        !is.na(HRuid)) %>%
 select(BrthYear, HRuid, nexcbrst, nexcbrst_count, nexcbrst_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(nexcbrst, HRuid),
                 fill = list(
                  nexcbrst_count = 0,
                  nexcbrst_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(nexcbrst, BrthYear),
                 fill = list(
                  nexcbrst_count = 0,
                  nexcbrst_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  nexcbrst_delta = (nexcbrst_rate - lag(nexcbrst_rate))/lag(nexcbrst_rate),
  nexcbrst_delta = ifelse(BrthYear == 2014, NA, nexcbrst_delta),
  nexcbrst_deltap = (nexcbrst_rate - lag(nexcbrst_rate,9))/lag(nexcbrst_rate,9)
 ) %>%
 ungroup()

c36 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, nbrst, HRuid, lvbint) %>%
 distinct() %>%
 group_by(BrthYear, nbrst, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  nbrst_count = n()
 ) %>%
 group_by(BrthYear, HRuid, lvbint) %>%
 mutate(
  total_year = n(),
  nbrst_rate = nbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nbrst %in% 1,
        !is.na(HRuid)) %>%
 select(BrthYear, HRuid, nbrst, nbrst_count, nbrst_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(nbrst, HRuid),
                 fill = list(
                  nbrst_count = 0,
                  nbrst_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(nbrst, BrthYear),
                 fill = list(
                  nbrst_count = 0,
                  nbrst_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  nbrst_delta = (nbrst_rate - lag(nbrst_rate))/lag(nbrst_rate),
  nbrst_delta = ifelse(BrthYear == 2014, NA, nbrst_delta),
  nbrst_deltap = (nbrst_rate - lag(nbrst_rate,9))/lag(nbrst_rate,9)
 ) %>%
 ungroup()

#### The breast/chest feeding initiation rate ----

c37 <- dta %>%
 select(BIRTHID, CONTCTID, BrthYear, brstinit, HRuid, lvb) %>%
 distinct() %>%
 group_by(BrthYear, brstinit, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  brstinit_count = n()
 ) %>%
 group_by(BrthYear, HRuid, lvb) %>%
 mutate(
  total_year = n(),
  brstinit_rate = brstinit_count/total_year
 ) %>%
 ungroup() %>%
 filter(brstinit %in% 1,
        !is.na(HRuid)) %>%
 select(BrthYear, HRuid, brstinit, brstinit_count, brstinit_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(brstinit, HRuid),
                 fill = list(
                  brstinit_count = 0,
                  brstinit_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(brstinit, BrthYear),
                 fill = list(
                  brstinit_count = 0,
                  brstinit_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  brstinit_delta = (brstinit_rate - lag(brstinit_rate))/lag(brstinit_rate),
  brstinit_delta = ifelse(BrthYear == 2014, NA, brstinit_delta),
  brstinit_deltap = (brstinit_rate - lag(brstinit_rate,9))/lag(brstinit_rate,9)
 ) %>%
 ungroup()

#### ICU Admission during Pregnancy or Postpartum ----

c38 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
  icu = case_when(
   JOGC_ICU > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, JOGC_ICU, HRuid) %>%
 mutate(
  icu_count = n()
 ) %>%
 group_by(BrthYear, HRuid) %>%
 mutate(
  total_year = n(),
  icu_rate = icu_count/total_year
 ) %>%
 ungroup() %>%
 filter(JOGC_ICU %in% 1, !is.na(HRuid)) %>%
 select(BrthYear, HRuid, icu, icu_count,icu_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(icu, HRuid),
                 fill = list(
                  icu_count = 0,
                  icu_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(icu, BrthYear),
                 fill = list(
                  icu_count = 0,
                  icu_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  icu_delta = (icu_rate - lag(icu_rate))/lag(icu_rate),
  icu_delta = ifelse(BrthYear == 2014, NA, icu_delta),
  icu_deltap = (icu_rate - lag(icu_rate,9))/lag(icu_rate,9)
 ) %>%
 ungroup()

#### Rate of Severe Morbidity in Pregnancy or Postpartum ----

c39 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
  smm = case_when(
   JOGC_AnySMM > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(BrthYear, JOGC_AnySMM, HRuid) %>%
 mutate(
  smm_count = n()
 ) %>%
 group_by(BrthYear, lvb, HRuid) %>%
 mutate(
  total_year = n(),
  smm_rate = smm_count/total_year
 ) %>%
 ungroup() %>%
 filter(JOGC_AnySMM %in% 1, lvb %in% 1, !is.na(HRuid)) %>%
 select(BrthYear,HRuid, smm, smm_count,smm_rate) %>%
 distinct() %>%
 arrange(HRuid, BrthYear) %>%
 ungroup() %>%
 tidyr::complete(BrthYear = min(dta$BrthYear):max(dta$BrthYear),
                 tidyr::nesting(smm, HRuid),
                 fill = list(
                  smm_count = 0,
                  smm_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(smm, BrthYear),
                 fill = list(
                  smm_count = 0,
                  smm_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  smm_delta = (smm_rate - lag(smm_rate))/lag(smm_rate),
  smm_delta = ifelse(BrthYear == 2014, NA, smm_delta),
  smm_deltap = (smm_rate - lag(smm_rate,9))/lag(smm_rate,9)
 ) %>%
 ungroup()

chr_stats <- cbind(
 c1 %>% select(-PreExisting_Hypertension),
 c2 %>% select(-Gestational_Hypertension,-HRuid,-BrthYear),
 c3 %>% select(-Any_Hypertension,-HRuid,-BrthYear),
 c4 %>% select(-anemia,-HRuid,-BrthYear),
 c5 %>% select(-PreExisting_Diabetes,-HRuid,-BrthYear),
 c6 %>% select(-GDM,-HRuid,-BrthYear),
 c7 %>% select(-Any_Diabetes,-HRuid,-BrthYear),
 c8 %>% select(-sptvg,-HRuid,-BrthYear),
 c9 %>% select(-med,-HRuid,-BrthYear),
 c10 %>% select(-sptassvgmed,-HRuid,-BrthYear),
 c11 %>% select(-sptassvgnmed,-HRuid,-BrthYear),
 c12 %>% select(-rbs1,-HRuid,-BrthYear),
 c13 %>% select(-rbs21,-HRuid,-BrthYear),
 c14 %>% select(-rbs51,-HRuid,-BrthYear),
 c15 %>% select(-pphbl,-HRuid,-BrthYear),
 c16 %>% select(-pphpi,-HRuid,-BrthYear),
 c17 %>% select(-ppreadm,-HRuid,-BrthYear),
 #c18 %>% select(-brstinit,-HRuid,-BrthYear),
 c19 %>% select(-sknskn,-HRuid,-BrthYear),
 c20 %>% select(-sknsknvg,-HRuid,-BrthYear),
 c21 %>% select(-sknskncs,-HRuid,-BrthYear),
 c22 %>% select(-pphiv,-HRuid,-BrthYear),
 c23 %>% select(-neoreadm,-HRuid,-BrthYear),
 c24 %>% select(-pretster,-HRuid,-BrthYear),
 c25 %>% select(-pretnnicu,-HRuid,-BrthYear),
 c26 %>% select(-newlwcs,-HRuid,-BrthYear),
 c27 %>% select(-delcord,-HRuid,-BrthYear),
 c28 %>% select(-delcordterm,-HRuid,-BrthYear),
 c29 %>% select(-delcordtermvg,-HRuid,-BrthYear),
 c30 %>% select(-delcordtermcs,-HRuid,-BrthYear),
 c31 %>% select(-delcordpreterm,-HRuid,-BrthYear),
 c32 %>% select(-delcordpretermvg,-HRuid,-BrthYear),
 c33 %>% select(-delcordpretermcs,-HRuid,-BrthYear),
 c34 %>% select(-excbrst,-HRuid,-BrthYear),
 c35 %>% select(-nexcbrst,-HRuid,-BrthYear),
 c36 %>% select(-nbrst,-HRuid,-BrthYear),
 c37 %>% select(-brstinit,-HRuid,-BrthYear),
 c38 %>% select(-icu,-HRuid,-BrthYear),
 c39 %>% select(-smm,-HRuid,-BrthYear)
) %>%
 filter(!HRuid %in% "12 ") %>%
 mutate(HRuid = as.character(trimws(substr(HRuid,3,4))))

##-----------------
## Fiscal year ----
##-----------------

### Census division (CD) ----
#### Hypertension ----

c1 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, PreExisting_Hypertension, CDuid) %>%
 distinct() %>%
 group_by(FiscalYear, PreExisting_Hypertension, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
                 tidyr::nesting(PreExisting_Hypertension, CDuid),
                 fill = list(
                  prehyp_count = 0,
                  prehyp_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(PreExisting_Hypertension, FiscalYear),
                 fill = list(
                  prehyp_count = 0,
                  prehyp_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  prehyp_delta = (prehyp_rate - lag(prehyp_rate))/lag(prehyp_rate),
  prehyp_delta = ifelse(FiscalYear == 2014, NA, prehyp_delta),
  prehyp_deltap = (prehyp_rate - lag(prehyp_rate,9))/lag(prehyp_rate,9)
 ) %>%
 ungroup()

c2 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Gestational_Hypertension, CDuid) %>%
 distinct() %>%
 group_by(FiscalYear, Gestational_Hypertension, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
                 tidyr::nesting(Gestational_Hypertension, CDuid),
                 fill = list(
                  gesthyp_count = 0,
                  gesthyp_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(Gestational_Hypertension, FiscalYear),
                 fill = list(
                  gesthyp_count = 0,
                  gesthyp_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  gesthyp_delta = (gesthyp_rate - lag(gesthyp_rate))/lag(gesthyp_rate),
  gesthyp_delta = ifelse(FiscalYear == 2014, NA, gesthyp_delta),
  gesthyp_deltap = (gesthyp_rate - lag(gesthyp_rate,9))/lag(gesthyp_rate,9)
 ) %>%
 ungroup()

c3 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Any_Hypertension, CDuid) %>%
 distinct() %>%
 group_by(FiscalYear, Any_Hypertension, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
                 tidyr::nesting(Any_Hypertension, CDuid),
                 fill = list(
                  hyp_count = 0,
                  hyp_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(Any_Hypertension, FiscalYear),
                 fill = list(
                  hyp_count = 0,
                  hyp_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  hyp_delta = (hyp_rate - lag(hyp_rate))/lag(hyp_rate),
  hyp_delta = ifelse(FiscalYear == 2014, NA, hyp_delta),
  hyp_deltap = (hyp_rate - lag(hyp_rate,9))/lag(hyp_rate,9)
 ) %>%
 ungroup()

#### Anaemia ----

c4 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, CDuid, MANEM, R014_01500, MO990, MD50_D53, MD55_D59, MD60_D64) %>%
 mutate(anemia = case_when(
  MANEM > 0 | R014_01500 > 0 | MO990 > 0 | MD50_D53 > 0 |
   MD55_D59 > 0 | MD60_D64 > 0 ~ 1,
  TRUE ~ NA_integer_
 )) %>%
 distinct() %>%
 group_by(FiscalYear, anemia, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
  anemia_count = n()
 ) %>%
 group_by(FiscalYear, CDuid) %>%
 mutate(
  total_year = n(),
  anemia_rate = anemia_count/total_year
 ) %>%
 filter(anemia %in% 1,
        !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, anemia, anemia_count, anemia_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(anemia, CDuid),
                 fill = list(
                  anemia_count = 0,
                  anemia_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(anemia, FiscalYear),
                 fill = list(
                  anemia_count = 0,
                  anemia_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  anemia_delta = (anemia_rate - lag(anemia_rate))/lag(anemia_rate),
  anemia_delta = ifelse(FiscalYear == 2014, NA, anemia_delta),
  anemia_deltap = (anemia_rate - lag(anemia_rate,9))/lag(anemia_rate,9)
 ) %>%
 ungroup()

#### Diabetes ----

c5 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, PreExisting_Diabetes, CDuid) %>%
 distinct() %>%
 group_by(FiscalYear, PreExisting_Diabetes, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
                 tidyr::nesting(PreExisting_Diabetes, CDuid),
                 fill = list(
                  prediab_count = 0,
                  prediab_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(PreExisting_Diabetes, FiscalYear),
                 fill = list(
                  prediab_count = 0,
                  prediab_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  prediab_delta = (prediab_rate - lag(prediab_rate))/lag(prediab_rate),
  prediab_delta = ifelse(FiscalYear == 2014, NA, prediab_delta),
  prediab_deltap = (prediab_rate - lag(prediab_rate,9))/lag(prediab_rate,9)
 ) %>%
 ungroup()

c6 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, GDM, CDuid) %>%
 distinct() %>%
 group_by(FiscalYear, GDM, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
                 tidyr::nesting(GDM, CDuid),
                 fill = list(
                  gestdiab_count = 0,
                  gestdiab_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(GDM, FiscalYear),
                 fill = list(
                  gestdiab_count = 0,
                  gestdiab_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  gestdiab_delta = (gestdiab_rate - lag(gestdiab_rate))/lag(gestdiab_rate),
  gestdiab_delta = ifelse(FiscalYear == 2014, NA, gestdiab_delta),
  gestdiab_deltap = (gestdiab_rate - lag(gestdiab_rate,9))/lag(gestdiab_rate,9)
 ) %>%
 ungroup()

c7 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Any_Diabetes, CDuid) %>%
 distinct() %>%
 group_by(FiscalYear, Any_Diabetes, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
                 tidyr::nesting(Any_Diabetes, CDuid),
                 fill = list(
                  diab_count = 0,
                  diab_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(Any_Diabetes, FiscalYear),
                 fill = list(
                  diab_count = 0,
                  diab_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  diab_delta = (diab_rate - lag(diab_rate))/lag(diab_rate),
  diab_delta = ifelse(FiscalYear == 2014, NA, diab_delta),
  diab_deltap = (diab_rate - lag(diab_rate,9))/lag(diab_rate,9)
 ) %>%
 ungroup()

#### Severe Perineal Trauma with Spontaneous Vaginal Birth ----

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
        DMMETHOD,
        CDuid) %>%
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
 group_by(FiscalYear, trdfrth, sptvg, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
  spt_count = n()
 ) %>%
 group_by(FiscalYear, sptvg, CDuid) %>%
 mutate(
  total_year = n(),
  spt_rate = spt_count/total_year
 ) %>%
 ungroup() %>%
 filter(trdfrth %in% 1, sptvg %in% 1, !CDuid %in% 1200) %>%
 select(FiscalYear,CDuid,sptvg,spt_count,spt_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(sptvg, CDuid),
                 fill = list(
                  spt_count = 0,
                  spt_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(sptvg, FiscalYear),
                 fill = list(
                  spt_count = 0,
                  spt_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  spt_delta = (spt_rate - lag(spt_rate))/lag(spt_rate),
  spt_delta = ifelse(FiscalYear == 2014, NA, spt_delta),
  spt_deltap = (spt_rate - lag(spt_rate,9))/lag(spt_rate,9)
 ) %>%
 ungroup()

#### Mediolateral Episiotomy with Operative Vaginal Birth ----
#### ND = Not Done
#### MD = Midline
#### ML = Mediolateral

c9 <- dta %>%
 select(FiscalYear,
        DMMETHOD,
        DMEPISIO,
        CDuid) %>%
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
 group_by(FiscalYear, med, assvg, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
  med_count = n()
 ) %>%
 group_by(FiscalYear, assvg, CDuid) %>%
 mutate(
  total_year = n(),
  med_rate = med_count/total_year
 ) %>%
 ungroup() %>%
 filter(med %in% 1, assvg %in% 1, !CDuid %in% 1200) %>%
 select(FiscalYear,CDuid,med,med_count,med_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(med, CDuid),
                 fill = list(
                  med_count = 0,
                  med_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(med, FiscalYear),
                 fill = list(
                  med_count = 0,
                  med_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  med_delta = (med_rate - lag(med_rate))/lag(med_rate),
  med_delta = ifelse(FiscalYear == 2014, NA, med_delta),
  med_deltap = (med_rate - lag(med_rate,9))/lag(med_rate,9)
 ) %>%
 ungroup()

#### Severe perineal trauma with Operative Vaginal Birth and mediolateral episiotomy ----

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
        DMEPISIO,
        CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(FiscalYear, trdfrth, assvg, med, CDuid) %>%
 mutate(
  sptassvgmed_count = n()
 ) %>%
 group_by(FiscalYear, assvg, CDuid) %>%
 mutate(
  total_year = n(),
  sptassvgmed_rate = sptassvgmed_count/total_year
 ) %>%
 ungroup() %>%
 filter(sptassvgmed %in% 1, !CDuid %in% 1200) %>%
 select(FiscalYear,CDuid,sptassvgmed,sptassvgmed_count,sptassvgmed_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(sptassvgmed, CDuid),
                 fill = list(
                  sptassvgmed_count = 0,
                  sptassvgmed_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(sptassvgmed, FiscalYear),
                 fill = list(
                  sptassvgmed_count = 0,
                  sptassvgmed_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  sptassvgmed_delta = (sptassvgmed_rate - lag(sptassvgmed_rate))/lag(sptassvgmed_rate),
  sptassvgmed_delta = ifelse(FiscalYear == 2014, NA, sptassvgmed_delta),
  sptassvgmed_deltap = (sptassvgmed_rate - lag(sptassvgmed_rate,9))/lag(sptassvgmed_rate,9)
 ) %>%
 ungroup()

#### Severe perineal trauma with Operative Vaginal Birth without mediolateral episiotomy ----

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
        DMEPISIO,
        CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(FiscalYear, trdfrth, assvg, med, CDuid) %>%
 mutate(
  sptassvgnmed_count = n()
 ) %>%
 group_by(FiscalYear, assvg, CDuid) %>%
 mutate(
  total_year = n(),
  sptassvgnmed_rate = sptassvgnmed_count/total_year
 ) %>%
 ungroup() %>%
 filter(sptassvgnmed %in% 1, !CDuid %in% 1200) %>%
 select(FiscalYear,CDuid,sptassvgnmed,sptassvgnmed_count,sptassvgnmed_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(sptassvgnmed, CDuid),
                 fill = list(
                  sptassvgnmed_count = 0,
                  sptassvgnmed_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(sptassvgnmed, FiscalYear),
                 fill = list(
                  sptassvgnmed_count = 0,
                  sptassvgnmed_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  sptassvgnmed_delta = (sptassvgnmed_rate - lag(sptassvgnmed_rate))/lag(sptassvgnmed_rate),
  sptassvgnmed_delta = ifelse(FiscalYear == 2014, NA, sptassvgnmed_delta),
  sptassvgnmed_deltap = (sptassvgnmed_rate - lag(sptassvgnmed_rate,9))/lag(sptassvgnmed_rate,9)
 ) %>%
 ungroup()

#### Robson group ----

c12 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs1, RobsnGrp, CDuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs1, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
                 tidyr::nesting(rbs1, CDuid),
                 fill = list(
                  rbs1_count = 0,
                  rbs1_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(rbs1, FiscalYear),
                 fill = list(
                  rbs1_count = 0,
                  rbs1_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  rbs1_delta = (rbs1_rate - lag(rbs1_rate))/lag(rbs1_rate),
  rbs1_delta = ifelse(FiscalYear == 2014, NA, rbs1_delta),
  rbs1_deltap = (rbs1_rate - lag(rbs1_rate,9))/lag(rbs1_rate,9)
 ) %>%
 ungroup()

c13 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs21, RobsnGrp, CDuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs21, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
                 tidyr::nesting(rbs21, CDuid),
                 fill = list(
                  rbs21_count = 0,
                  rbs21_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(rbs21, FiscalYear),
                 fill = list(
                  rbs21_count = 0,
                  rbs21_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  rbs21_delta = (rbs21_rate - lag(rbs21_rate))/lag(rbs21_rate),
  rbs21_delta = ifelse(FiscalYear == 2014, NA, rbs21_delta),
  rbs21_deltap = (rbs21_rate - lag(rbs21_rate,9))/lag(rbs21_rate,9)
 ) %>%
 ungroup()

c14 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs51, RobsnGrp, CDuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs51, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
                 tidyr::nesting(rbs51, CDuid),
                 fill = list(
                  rbs51_count = 0,
                  rbs51_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(rbs51, FiscalYear),
                 fill = list(
                  rbs51_count = 0,
                  rbs51_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  rbs51_delta = (rbs51_rate - lag(rbs51_rate))/lag(rbs51_rate),
  rbs51_delta = ifelse(FiscalYear == 2014, NA, rbs51_delta),
  rbs51_deltap = (rbs51_rate - lag(rbs51_rate,9))/lag(rbs51_rate,9)
 ) %>%
 ungroup()

#### Postpartum hemorrhage treated with a blood transfusion ----

c15 <- dta %>%
 select(FiscalYear, Any_Blood_Product, Postpartum_Haemorrhage, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
  pphbl = case_when(
   Any_Blood_Product > 0 & Postpartum_Haemorrhage > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, Any_Blood_Product, Postpartum_Haemorrhage, CDuid) %>%
 mutate(
  pphbl_count = n()
 ) %>%
 group_by(FiscalYear, CDuid) %>%
 mutate(
  total_year = n(),
  pphbl_rate = pphbl_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Blood_Product %in% 1, Postpartum_Haemorrhage %in% 1,
        !CDuid %in% 1200) %>%
 select(FiscalYear,CDuid,pphbl, pphbl_count, pphbl_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(pphbl, CDuid),
                 fill = list(
                  pphbl_count = 0,
                  pphbl_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(pphbl, FiscalYear),
                 fill = list(
                  pphbl_count = 0,
                  pphbl_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  pphbl_delta = (pphbl_rate - lag(pphbl_rate))/lag(pphbl_rate),
  pphbl_delta = ifelse(FiscalYear == 2014, NA, pphbl_delta),
  pphbl_deltap = (pphbl_rate - lag(pphbl_rate,9))/lag(pphbl_rate,9)
 ) %>%
 ungroup()

#### Postpartum hemorrhage resulting in procedural intervention ----

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
        MODEDEL,
        CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(FiscalYear, pphpi, Postpartum_Haemorrhage, CDuid) %>%
 mutate(
  pphpi_count = n()
 ) %>%
 group_by(FiscalYear, Postpartum_Haemorrhage, CDuid) %>%
 mutate(
  total_year = n(),
  pphpi_rate = pphpi_count/total_year
 ) %>%
 ungroup() %>%
 filter(pphpi %in% 1, Postpartum_Haemorrhage %in% 1,
        !CDuid %in% 1200) %>%
 select(FiscalYear,CDuid,pphpi, pphpi_count, pphpi_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(pphpi, CDuid),
                 fill = list(
                  pphpi_count = 0,
                  pphpi_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(pphpi, FiscalYear),
                 fill = list(
                  pphpi_count = 0,
                  pphpi_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  pphpi_delta = (pphpi_rate - lag(pphpi_rate))/lag(pphpi_rate),
  pphpi_delta = ifelse(FiscalYear == 2014, NA, pphpi_delta),
  pphpi_deltap = (pphpi_rate - lag(pphpi_rate,9))/lag(pphpi_rate,9)
 ) %>%
 ungroup()

#### Postpartum readmission ----

c17 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, ppreadm, CDuid) %>%
 distinct() %>%
 group_by(FiscalYear, ppreadm, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
                 tidyr::nesting(ppreadm, CDuid),
                 fill = list(
                  ppreadm_count = 0,
                  ppreadm_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(ppreadm, FiscalYear),
                 fill = list(
                  ppreadm_count = 0,
                  ppreadm_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  ppreadm_delta = (ppreadm_rate - lag(ppreadm_rate))/lag(ppreadm_rate),
  ppreadm_delta = ifelse(FiscalYear == 2014, NA, ppreadm_delta),
  ppreadm_deltap = (ppreadm_rate - lag(ppreadm_rate,9))/lag(ppreadm_rate,9)
 ) %>%
 ungroup()

#### Postpartum patients who received medical anticoagulation prophylaxis when indicated ----

#c18

#### Skin to skin ----

c19 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknskn, CDuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, sknskn, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
                 tidyr::nesting(sknskn, CDuid),
                 fill = list(
                  sknskn_count = 0,
                  sknskn_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(sknskn, FiscalYear),
                 fill = list(
                  sknskn_count = 0,
                  sknskn_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  sknskn_delta = (sknskn_rate - lag(sknskn_rate))/lag(sknskn_rate),
  sknskn_delta = ifelse(FiscalYear == 2014, NA, sknskn_delta),
  sknskn_deltap = (sknskn_rate - lag(sknskn_rate,9))/lag(sknskn_rate,9)
 ) %>%
 ungroup()

c20 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknsknvg, CDuid, lvbvg) %>%
 distinct() %>%
 group_by(FiscalYear, sknsknvg, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
                 tidyr::nesting(sknsknvg, CDuid),
                 fill = list(
                  sknsknvg_count = 0,
                  sknsknvg_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(sknsknvg, FiscalYear),
                 fill = list(
                  sknsknvg_count = 0,
                  sknsknvg_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  sknsknvg_delta = (sknsknvg_rate - lag(sknsknvg_rate))/lag(sknsknvg_rate),
  sknsknvg_delta = ifelse(FiscalYear == 2014, NA, sknsknvg_delta),
  sknsknvg_deltap = (sknsknvg_rate - lag(sknsknvg_rate,9))/lag(sknsknvg_rate,9)
 ) %>%
 ungroup()

c21 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknskncs, CDuid, lvbcs) %>%
 distinct() %>%
 group_by(FiscalYear, sknskncs, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
                 tidyr::nesting(sknskncs, CDuid),
                 fill = list(
                  sknskncs_count = 0,
                  sknskncs_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(sknskncs, FiscalYear),
                 fill = list(
                  sknskncs_count = 0,
                  sknskncs_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  sknskncs_delta = (sknskncs_rate - lag(sknskncs_rate))/lag(sknskncs_rate),
  sknskncs_delta = ifelse(FiscalYear == 2014, NA, sknskncs_delta),
  sknskncs_deltap = (sknskncs_rate - lag(sknskncs_rate,9))/lag(sknskncs_rate,9)
 ) %>%
 ungroup()

#### Postpartum blood products amongst those who received antenatal iron therapy ----

c22 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
  pphiv = case_when(
   Any_Blood_Product > 0 & R003_02100 > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, Any_Blood_Product, R003_02100, CDuid) %>%
 mutate(
  pphiv_count = n()
 ) %>%
 group_by(FiscalYear, Any_Blood_Product, CDuid) %>%
 mutate(
  total_year = n(),
  pphiv_rate = pphiv_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Blood_Product %in% 1, R003_02100 %in% 1,
        !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, pphiv, pphiv_count, pphiv_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(pphiv, CDuid),
                 fill = list(
                  pphiv_count = 0,
                  pphiv_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(pphiv, FiscalYear),
                 fill = list(
                  pphiv_count = 0,
                  pphiv_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  pphiv_delta = (pphiv_rate - lag(pphiv_rate))/lag(pphiv_rate),
  pphiv_delta = ifelse(FiscalYear == 2014, NA, pphiv_delta),
  pphiv_deltap = (pphiv_rate - lag(pphiv_rate,9))/lag(pphiv_rate,9)
 ) %>%
 ungroup()

#### Neonatal readmission ----

c23 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, neoreadm, CDuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, neoreadm, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
                 tidyr::nesting(neoreadm, CDuid),
                 fill = list(
                  neoreadm_count = 0,
                  neoreadm_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(neoreadm, FiscalYear),
                 fill = list(
                  neoreadm_count = 0,
                  neoreadm_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  neoreadm_delta = (neoreadm_rate - lag(neoreadm_rate))/lag(neoreadm_rate),
  neoreadm_delta = ifelse(FiscalYear == 2014, NA, neoreadm_delta),
  neoreadm_deltap = (neoreadm_rate - lag(neoreadm_rate,9))/lag(neoreadm_rate,9)
 ) %>%
 ungroup()

#### Preterm infants who received complete course of steroids > 24h and < 7 days prior to birth ----

c24 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(FiscalYear, pretster, CDuid) %>%
 mutate(
  pretster_count = n()
 ) %>%
 group_by(FiscalYear, den, CDuid) %>%
 mutate(
  total_year = n(),
  pretster_rate = pretster_count/total_year
 ) %>%
 ungroup() %>%
 filter(pretster %in% 1, !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, pretster, pretster_count,pretster_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(pretster, CDuid),
                 fill = list(
                  pretster_count = 0,
                  pretster_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(pretster, FiscalYear),
                 fill = list(
                  pretster_count = 0,
                  pretster_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  pretster_delta = (pretster_rate - lag(pretster_rate))/lag(pretster_rate),
  pretster_delta = ifelse(FiscalYear == 2014, NA, pretster_delta),
  pretster_deltap = (pretster_rate - lag(pretster_rate,9))/lag(pretster_rate,9)
 ) %>%
 ungroup()

#### Preterm babies born in facility without NICU ----

c25 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(FiscalYear, pretnnicu, CDuid) %>%
 mutate(
  pretnnicu_count = n()
 ) %>%
 group_by(FiscalYear, den, CDuid) %>%
 mutate(
  total_year = n(),
  pretnnicu_rate = pretnnicu_count/total_year
 ) %>%
 ungroup() %>%
 filter(pretnnicu %in% 1, !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, pretnnicu, pretnnicu_count,pretnnicu_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(pretnnicu, CDuid),
                 fill = list(
                  pretnnicu_count = 0,
                  pretnnicu_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(pretnnicu, FiscalYear),
                 fill = list(
                  pretnnicu_count = 0,
                  pretnnicu_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  pretnnicu_delta = (pretnnicu_rate - lag(pretnnicu_rate))/lag(pretnnicu_rate),
  pretnnicu_delta = ifelse(FiscalYear == 2014, NA, pretnnicu_delta),
  pretnnicu_deltap = (pretnnicu_rate - lag(pretnnicu_rate,9))/lag(pretnnicu_rate,9)
 ) %>%
 ungroup()

#### Newborn respiratory distress associated with low-risk repeat cesarean at term gestation (≥ 37 weeks < 39 weeks) ----

c26 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(FiscalYear, newlwcs, CDuid) %>%
 mutate(
  newlwcs_count = n()
 ) %>%
 group_by(FiscalYear, den, CDuid) %>%
 mutate(
  total_year = n(),
  newlwcs_rate = newlwcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(newlwcs %in% 1) %>%
 select(FiscalYear, CDuid, newlwcs, newlwcs_count,newlwcs_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(newlwcs, CDuid),
                 fill = list(
                  newlwcs_count = 0,
                  newlwcs_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(newlwcs, FiscalYear),
                 fill = list(
                  newlwcs_count = 0,
                  newlwcs_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  newlwcs_delta = (newlwcs_rate - lag(newlwcs_rate))/lag(newlwcs_rate),
  newlwcs_delta = ifelse(FiscalYear == 2014, NA, newlwcs_delta),
  newlwcs_deltap = (newlwcs_rate - lag(newlwcs_rate,9))/lag(newlwcs_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping  ----

c27 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
  delcord = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (DEL_CORD_CLAMP %in% 3) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, delcord, CDuid) %>%
 mutate(
  delcord_count = n()
 ) %>%
 group_by(FiscalYear, lvb, CDuid) %>%
 mutate(
  total_year = n(),
  delcord_rate = delcord_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcord %in% 1, lvb %in% 1,
        !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, delcord, delcord_count,delcord_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcord, CDuid),
                 fill = list(
                  delcord_count = 0,
                  delcord_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(delcord, FiscalYear),
                 fill = list(
                  delcord_count = 0,
                  delcord_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  delcord_delta = (delcord_rate - lag(delcord_rate))/lag(delcord_rate),
  delcord_delta = ifelse(FiscalYear == 2014, NA, delcord_delta),
  delcord_deltap = (delcord_rate - lag(delcord_rate,9))/lag(delcord_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns ----

c28 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(FiscalYear, delcordterm, CDuid) %>%
 mutate(
  delcordterm_count = n()
 ) %>%
 group_by(FiscalYear, den, CDuid) %>%
 mutate(
  total_year = n(),
  delcordterm_rate = delcordterm_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordterm %in% 1, !CDuid %in% 1200) %>%
 select(FiscalYear,CDuid,delcordterm,delcordterm_count,delcordterm_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordterm, CDuid),
                 fill = list(
                  delcordterm_count = 0,
                  delcordterm_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(delcordterm, FiscalYear),
                 fill = list(
                  delcordterm_count = 0,
                  delcordterm_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  delcordterm_delta = (delcordterm_rate - lag(delcordterm_rate))/lag(delcordterm_rate),
  delcordterm_delta = ifelse(FiscalYear == 2014, NA, delcordterm_delta),
  delcordterm_deltap = (delcordterm_rate - lag(delcordterm_rate,9))/lag(delcordterm_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns following spontaneous vaginal birth ----

c29 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(FiscalYear, delcordtermvg, CDuid) %>%
 mutate(
  delcordtermvg_count = n()
 ) %>%
 group_by(FiscalYear, den, CDuid) %>%
 mutate(
  total_year = n(),
  delcordtermvg_rate = delcordtermvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordtermvg %in% 1, !CDuid %in% 1200) %>%
 select(FiscalYear,CDuid, delcordtermvg, delcordtermvg_count,delcordtermvg_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordtermvg, CDuid),
                 fill = list(
                  delcordtermvg_count = 0,
                  delcordtermvg_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(delcordtermvg, FiscalYear),
                 fill = list(
                  delcordtermvg_count = 0,
                  delcordtermvg_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  delcordtermvg_delta = (delcordtermvg_rate - lag(delcordtermvg_rate))/lag(delcordtermvg_rate),
  delcordtermvg_delta = ifelse(FiscalYear == 2014, NA, delcordtermvg_delta),
  delcordtermvg_deltap = (delcordtermvg_rate - lag(delcordtermvg_rate,9))/lag(delcordtermvg_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns following caesarean birth ----

c30 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(FiscalYear, delcordtermcs, CDuid) %>%
 mutate(
  delcordtermcs_count = n()
 ) %>%
 group_by(FiscalYear, den, CDuid) %>%
 mutate(
  total_year = n(),
  delcordtermcs_rate = delcordtermcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordtermcs %in% 1, !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, delcordtermcs, delcordtermcs_count,delcordtermcs_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordtermcs, CDuid),
                 fill = list(
                  delcordtermcs_count = 0,
                  delcordtermcs_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(delcordtermcs, FiscalYear),
                 fill = list(
                  delcordtermcs_count = 0,
                  delcordtermcs_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  delcordtermcs_delta = (delcordtermcs_rate - lag(delcordtermcs_rate))/lag(delcordtermcs_rate),
  delcordtermcs_delta = ifelse(FiscalYear == 2014, NA, delcordtermcs_delta),
  delcordtermcs_deltap = (delcordtermcs_rate - lag(delcordtermcs_rate,9))/lag(delcordtermcs_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns ----

c31 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(FiscalYear, delcordpreterm, CDuid) %>%
 mutate(
  delcordpreterm_count = n()
 ) %>%
 group_by(FiscalYear, den, CDuid) %>%
 mutate(
  total_year = n(),
  delcordpreterm_rate = delcordpreterm_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpreterm %in% 1, !CDuid %in% 1200) %>%
 select(FiscalYear,CDuid, delcordpreterm, delcordpreterm_count,delcordpreterm_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordpreterm, CDuid),
                 fill = list(
                  delcordpreterm_count = 0,
                  delcordpreterm_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(delcordpreterm, FiscalYear),
                 fill = list(
                  delcordpreterm_count = 0,
                  delcordpreterm_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  delcordpreterm_delta = (delcordpreterm_rate - lag(delcordpreterm_rate))/lag(delcordpreterm_rate),
  delcordpreterm_delta = ifelse(FiscalYear == 2014, NA, delcordpreterm_delta),
  delcordpreterm_deltap = (delcordpreterm_rate - lag(delcordpreterm_rate,9))/lag(delcordpreterm_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns following spontaneous vaginal birth ----

c32 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(FiscalYear, delcordpretermvg, CDuid) %>%
 mutate(
  delcordpretermvg_count = n()
 ) %>%
 group_by(FiscalYear, den, CDuid) %>%
 mutate(
  total_year = n(),
  delcordpretermvg_rate = delcordpretermvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpretermvg %in% 1, !CDuid %in% 1200) %>%
 select(FiscalYear,CDuid, delcordpretermvg, delcordpretermvg_count,delcordpretermvg_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordpretermvg, CDuid),
                 fill = list(
                  delcordpretermvg_count = 0,
                  delcordpretermvg_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(delcordpretermvg, FiscalYear),
                 fill = list(
                  delcordpretermvg_count = 0,
                  delcordpretermvg_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  delcordpretermvg_delta = (delcordpretermvg_rate - lag(delcordpretermvg_rate))/lag(delcordpretermvg_rate),
  delcordpretermvg_delta = ifelse(FiscalYear == 2014, NA, delcordpretermvg_delta),
  delcordpretermvg_deltap = (delcordpretermvg_rate - lag(delcordpretermvg_rate,9))/lag(delcordpretermvg_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns following caesarean birth ----

c33 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
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
 group_by(FiscalYear, delcordpretermcs, CDuid) %>%
 mutate(
  delcordpretermcs_count = n()
 ) %>%
 group_by(FiscalYear, den, CDuid) %>%
 mutate(
  total_year = n(),
  delcordpretermcs_rate = delcordpretermcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpretermcs %in% 1, !CDuid %in% 1200) %>%
 select(FiscalYear,CDuid, delcordpretermcs, delcordpretermcs_count,delcordpretermcs_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordpretermcs, CDuid),
                 fill = list(
                  delcordpretermcs_count = 0,
                  delcordpretermcs_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(delcordpretermcs, FiscalYear),
                 fill = list(
                  delcordpretermcs_count = 0,
                  delcordpretermcs_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  delcordpretermcs_delta = (delcordpretermcs_rate - lag(delcordpretermcs_rate))/lag(delcordpretermcs_rate),
  delcordpretermcs_delta = ifelse(FiscalYear == 2014, NA, delcordpretermcs_delta),
  delcordpretermcs_deltap = (delcordpretermcs_rate - lag(delcordpretermcs_rate,9))/lag(delcordpretermcs_rate,9)
 ) %>%
 ungroup()

#### Milk feeding ----

c34 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, excbrst, CDuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, excbrst, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
                 tidyr::nesting(excbrst, CDuid),
                 fill = list(
                  excbrst_count = 0,
                  excbrst_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(excbrst, FiscalYear),
                 fill = list(
                  excbrst_count = 0,
                  excbrst_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  excbrst_delta = (excbrst_rate - lag(excbrst_rate))/lag(excbrst_rate),
  excbrst_delta = ifelse(FiscalYear == 2014, NA, excbrst_delta),
  excbrst_deltap = (excbrst_rate - lag(excbrst_rate,9))/lag(excbrst_rate,9)
 ) %>%
 ungroup()

c35 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, nexcbrst, CDuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, nexcbrst, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
                 tidyr::nesting(nexcbrst, CDuid),
                 fill = list(
                  nexcbrst_count = 0,
                  nexcbrst_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(nexcbrst, FiscalYear),
                 fill = list(
                  nexcbrst_count = 0,
                  nexcbrst_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  nexcbrst_delta = (nexcbrst_rate - lag(nexcbrst_rate))/lag(nexcbrst_rate),
  nexcbrst_delta = ifelse(FiscalYear == 2014, NA, nexcbrst_delta),
  nexcbrst_deltap = (nexcbrst_rate - lag(nexcbrst_rate,9))/lag(nexcbrst_rate,9)
 ) %>%
 ungroup()

c36 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, nbrst, CDuid, lvbint) %>%
 distinct() %>%
 group_by(FiscalYear, nbrst, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
                 tidyr::nesting(nbrst, CDuid),
                 fill = list(
                  nbrst_count = 0,
                  nbrst_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(nbrst, FiscalYear),
                 fill = list(
                  nbrst_count = 0,
                  nbrst_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  nbrst_delta = (nbrst_rate - lag(nbrst_rate))/lag(nbrst_rate),
  nbrst_delta = ifelse(FiscalYear == 2014, NA, nbrst_delta),
  nbrst_deltap = (nbrst_rate - lag(nbrst_rate,9))/lag(nbrst_rate,9)
 ) %>%
 ungroup()

#### The breast/chest feeding initiation rate ----

c37 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, brstinit, CDuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, brstinit, CDuid) %>%
 mutate(
  CDuid = as.integer(CDuid),
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
                 tidyr::nesting(brstinit, CDuid),
                 fill = list(
                  brstinit_count = 0,
                  brstinit_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(brstinit, FiscalYear),
                 fill = list(
                  brstinit_count = 0,
                  brstinit_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  brstinit_delta = (brstinit_rate - lag(brstinit_rate))/lag(brstinit_rate),
  brstinit_delta = ifelse(FiscalYear == 2014, NA, brstinit_delta),
  brstinit_deltap = (brstinit_rate - lag(brstinit_rate,9))/lag(brstinit_rate,9)
 ) %>%
 ungroup()

#### ICU Admission during Pregnancy or Postpartum ----

c38 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
  icu = case_when(
   JOGC_ICU > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, JOGC_ICU, CDuid) %>%
 mutate(
  icu_count = n()
 ) %>%
 group_by(FiscalYear, CDuid) %>%
 mutate(
  total_year = n(),
  icu_rate = icu_count/total_year
 ) %>%
 ungroup() %>%
 filter(JOGC_ICU %in% 1, !CDuid %in% 1200) %>%
 select(FiscalYear, CDuid, icu, icu_count,icu_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(icu, CDuid),
                 fill = list(
                  icu_count = 0,
                  icu_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(icu, FiscalYear),
                 fill = list(
                  icu_count = 0,
                  icu_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  icu_delta = (icu_rate - lag(icu_rate))/lag(icu_rate),
  icu_delta = ifelse(FiscalYear == 2014, NA, icu_delta),
  icu_deltap = (icu_rate - lag(icu_rate,9))/lag(icu_rate,9)
 ) %>%
 ungroup()

#### Rate of Severe Morbidity in Pregnancy or Postpartum ----

c39 <- dta %>%
 mutate(
  CDuid = as.integer(CDuid),
  smm = case_when(
   JOGC_AnySMM > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, JOGC_AnySMM, CDuid) %>%
 mutate(
  smm_count = n()
 ) %>%
 group_by(FiscalYear, lvb, CDuid) %>%
 mutate(
  total_year = n(),
  smm_rate = smm_count/total_year
 ) %>%
 ungroup() %>%
 filter(JOGC_AnySMM %in% 1, lvb %in% 1, !CDuid %in% 1200) %>%
 select(FiscalYear,CDuid, smm, smm_count,smm_rate) %>%
 distinct() %>%
 arrange(CDuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(smm, CDuid),
                 fill = list(
                  smm_count = 0,
                  smm_rate = NA
                 )) %>%
 tidyr::complete(CDuid = min(as.integer(cd_shp$GeoUID)):max(as.integer(cd_shp$GeoUID)),
                 tidyr::nesting(smm, FiscalYear),
                 fill = list(
                  smm_count = 0,
                  smm_rate = NA
                 )) %>%
 group_by(CDuid) %>%
 mutate(
  smm_delta = (smm_rate - lag(smm_rate))/lag(smm_rate),
  smm_delta = ifelse(FiscalYear == 2014, NA, smm_delta),
  smm_deltap = (smm_rate - lag(smm_rate,9))/lag(smm_rate,9)
 ) %>%
 ungroup()

fcd_stats <- cbind(
 c1 %>% select(-PreExisting_Hypertension),
 c2 %>% select(-Gestational_Hypertension,-CDuid,-FiscalYear),
 c3 %>% select(-Any_Hypertension,-CDuid,-FiscalYear),
 c4 %>% select(-anemia,-CDuid,-FiscalYear),
 c5 %>% select(-PreExisting_Diabetes,-CDuid,-FiscalYear),
 c6 %>% select(-GDM,-CDuid,-FiscalYear),
 c7 %>% select(-Any_Diabetes,-CDuid,-FiscalYear),
 c8 %>% select(-sptvg,-CDuid,-FiscalYear),
 c9 %>% select(-med,-CDuid,-FiscalYear),
 c10 %>% select(-sptassvgmed,-CDuid,-FiscalYear),
 c11 %>% select(-sptassvgnmed,-CDuid,-FiscalYear),
 c12 %>% select(-rbs1,-CDuid,-FiscalYear),
 c13 %>% select(-rbs21,-CDuid,-FiscalYear),
 c14 %>% select(-rbs51,-CDuid,-FiscalYear),
 c15 %>% select(-pphbl,-CDuid,-FiscalYear),
 c16 %>% select(-pphpi,-CDuid,-FiscalYear),
 c17 %>% select(-ppreadm,-CDuid,-FiscalYear),
 #c18 %>% select(-brstinit,-CDuid,-FiscalYear),
 c19 %>% select(-sknskn,-CDuid,-FiscalYear),
 c20 %>% select(-sknsknvg,-CDuid,-FiscalYear),
 c21 %>% select(-sknskncs,-CDuid,-FiscalYear),
 c22 %>% select(-pphiv,-CDuid,-FiscalYear),
 c23 %>% select(-neoreadm,-CDuid,-FiscalYear),
 c24 %>% select(-pretster,-CDuid,-FiscalYear),
 c25 %>% select(-pretnnicu,-CDuid,-FiscalYear),
 c26 %>% select(-newlwcs,-CDuid,-FiscalYear),
 c27 %>% select(-delcord,-CDuid,-FiscalYear),
 c28 %>% select(-delcordterm,-CDuid,-FiscalYear),
 c29 %>% select(-delcordtermvg,-CDuid,-FiscalYear),
 c30 %>% select(-delcordtermcs,-CDuid,-FiscalYear),
 c31 %>% select(-delcordpreterm,-CDuid,-FiscalYear),
 c32 %>% select(-delcordpretermvg,-CDuid,-FiscalYear),
 c33 %>% select(-delcordpretermcs,-CDuid,-FiscalYear),
 c34 %>% select(-excbrst,-CDuid,-FiscalYear),
 c35 %>% select(-nexcbrst,-CDuid,-FiscalYear),
 c36 %>% select(-nbrst,-CDuid,-FiscalYear),
 c37 %>% select(-brstinit,-CDuid,-FiscalYear),
 c38 %>% select(-icu,-CDuid,-FiscalYear),
 c39 %>% select(-smm,-CDuid,-FiscalYear)
)

### Community cluster (CL) ----
#### Hypertension ----

c1 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, PreExisting_Hypertension, CLuid) %>%
 distinct() %>%
 group_by(FiscalYear, PreExisting_Hypertension, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  prehyp_count = n()
 ) %>%
 group_by(FiscalYear, CLuid) %>%
 mutate(
  total_year = n(),
  prehyp_rate = prehyp_count/total_year
 ) %>%
 filter(PreExisting_Hypertension %in% 1,
        !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, PreExisting_Hypertension, prehyp_count, prehyp_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(PreExisting_Hypertension, CLuid),
                 fill = list(
                  prehyp_count = 0,
                  prehyp_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(PreExisting_Hypertension, FiscalYear),
                 fill = list(
                  prehyp_count = 0,
                  prehyp_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  prehyp_delta = (prehyp_rate - lag(prehyp_rate))/lag(prehyp_rate),
  prehyp_delta = ifelse(FiscalYear == 2014, NA, prehyp_delta),
  prehyp_deltap = (prehyp_rate - lag(prehyp_rate,9))/lag(prehyp_rate,9)
 ) %>%
 ungroup()

c2 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Gestational_Hypertension, CLuid) %>%
 distinct() %>%
 group_by(FiscalYear, Gestational_Hypertension, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  gesthyp_count = n()
 ) %>%
 group_by(FiscalYear, CLuid) %>%
 mutate(
  total_year = n(),
  gesthyp_rate = gesthyp_count/total_year
 ) %>%
 filter(Gestational_Hypertension %in% 1,
        !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, Gestational_Hypertension, gesthyp_count, gesthyp_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(Gestational_Hypertension, CLuid),
                 fill = list(
                  gesthyp_count = 0,
                  gesthyp_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(Gestational_Hypertension, FiscalYear),
                 fill = list(
                  gesthyp_count = 0,
                  gesthyp_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  gesthyp_delta = (gesthyp_rate - lag(gesthyp_rate))/lag(gesthyp_rate),
  gesthyp_delta = ifelse(FiscalYear == 2014, NA, gesthyp_delta),
  gesthyp_deltap = (gesthyp_rate - lag(gesthyp_rate,9))/lag(gesthyp_rate,9)
 ) %>%
 ungroup()

c3 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Any_Hypertension, CLuid) %>%
 distinct() %>%
 group_by(FiscalYear, Any_Hypertension, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  hyp_count = n()
 ) %>%
 group_by(FiscalYear, CLuid) %>%
 mutate(
  total_year = n(),
  hyp_rate = hyp_count/total_year
 ) %>%
 filter(Any_Hypertension %in% 1,
        !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, Any_Hypertension, hyp_count, hyp_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(Any_Hypertension, CLuid),
                 fill = list(
                  hyp_count = 0,
                  hyp_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(Any_Hypertension, FiscalYear),
                 fill = list(
                  hyp_count = 0,
                  hyp_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  hyp_delta = (hyp_rate - lag(hyp_rate))/lag(hyp_rate),
  hyp_delta = ifelse(FiscalYear == 2014, NA, hyp_delta),
  hyp_deltap = (hyp_rate - lag(hyp_rate,9))/lag(hyp_rate,9)
 ) %>%
 ungroup()

#### Anaemia ----

c4 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, CLuid, MANEM, R014_01500, MO990, MD50_D53, MD55_D59, MD60_D64) %>%
 mutate(anemia = case_when(
  MANEM > 0 | R014_01500 > 0 | MO990 > 0 | MD50_D53 > 0 |
   MD55_D59 > 0 | MD60_D64 > 0 ~ 1,
  TRUE ~ NA_integer_
 )) %>%
 distinct() %>%
 group_by(FiscalYear, anemia, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  anemia_count = n()
 ) %>%
 group_by(FiscalYear, CLuid) %>%
 mutate(
  total_year = n(),
  anemia_rate = anemia_count/total_year
 ) %>%
 filter(anemia %in% 1,
        !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, anemia, anemia_count, anemia_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(anemia, CLuid),
                 fill = list(
                  anemia_count = 0,
                  anemia_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(anemia, FiscalYear),
                 fill = list(
                  anemia_count = 0,
                  anemia_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  anemia_delta = (anemia_rate - lag(anemia_rate))/lag(anemia_rate),
  anemia_delta = ifelse(FiscalYear == 2014, NA, anemia_delta),
  anemia_deltap = (anemia_rate - lag(anemia_rate,9))/lag(anemia_rate,9)
 ) %>%
 ungroup()

#### Diabetes ----

c5 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, PreExisting_Diabetes, CLuid) %>%
 distinct() %>%
 group_by(FiscalYear, PreExisting_Diabetes, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  prediab_count = n()
 ) %>%
 group_by(FiscalYear, CLuid) %>%
 mutate(
  total_year = n(),
  prediab_rate = prediab_count/total_year
 ) %>%
 filter(PreExisting_Diabetes %in% 1,
        !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, PreExisting_Diabetes, prediab_count, prediab_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(PreExisting_Diabetes, CLuid),
                 fill = list(
                  prediab_count = 0,
                  prediab_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(PreExisting_Diabetes, FiscalYear),
                 fill = list(
                  prediab_count = 0,
                  prediab_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  prediab_delta = (prediab_rate - lag(prediab_rate))/lag(prediab_rate),
  prediab_delta = ifelse(FiscalYear == 2014, NA, prediab_delta),
  prediab_deltap = (prediab_rate - lag(prediab_rate,9))/lag(prediab_rate,9)
 ) %>%
 ungroup()

c6 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, GDM, CLuid) %>%
 distinct() %>%
 group_by(FiscalYear, GDM, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  gestdiab_count = n()
 ) %>%
 group_by(FiscalYear, CLuid) %>%
 mutate(
  total_year = n(),
  gestdiab_rate = gestdiab_count/total_year
 ) %>%
 filter(GDM %in% 1,
        !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, GDM, gestdiab_count, gestdiab_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(GDM, CLuid),
                 fill = list(
                  gestdiab_count = 0,
                  gestdiab_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(GDM, FiscalYear),
                 fill = list(
                  gestdiab_count = 0,
                  gestdiab_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  gestdiab_delta = (gestdiab_rate - lag(gestdiab_rate))/lag(gestdiab_rate),
  gestdiab_delta = ifelse(FiscalYear == 2014, NA, gestdiab_delta),
  gestdiab_deltap = (gestdiab_rate - lag(gestdiab_rate,9))/lag(gestdiab_rate,9)
 ) %>%
 ungroup()

c7 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Any_Diabetes, CLuid) %>%
 distinct() %>%
 group_by(FiscalYear, Any_Diabetes, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  diab_count = n()
 ) %>%
 group_by(FiscalYear, CLuid) %>%
 mutate(
  total_year = n(),
  diab_rate = diab_count/total_year
 ) %>%
 filter(Any_Diabetes %in% 1,
        !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, Any_Diabetes, diab_count, diab_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(Any_Diabetes, CLuid),
                 fill = list(
                  diab_count = 0,
                  diab_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(Any_Diabetes, FiscalYear),
                 fill = list(
                  diab_count = 0,
                  diab_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  diab_delta = (diab_rate - lag(diab_rate))/lag(diab_rate),
  diab_delta = ifelse(FiscalYear == 2014, NA, diab_delta),
  diab_deltap = (diab_rate - lag(diab_rate,9))/lag(diab_rate,9)
 ) %>%
 ungroup()

#### Severe Perineal Trauma with Spontaneous Vaginal Birth ----

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
        DMMETHOD,
        CLuid) %>%
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
 group_by(FiscalYear, trdfrth, sptvg, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  spt_count = n()
 ) %>%
 group_by(FiscalYear, sptvg, CLuid) %>%
 mutate(
  total_year = n(),
  spt_rate = spt_count/total_year
 ) %>%
 ungroup() %>%
 filter(trdfrth %in% 1, sptvg %in% 1, !is.na(CLuid)) %>%
 select(FiscalYear,CLuid,sptvg,spt_count,spt_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(sptvg, CLuid),
                 fill = list(
                  spt_count = 0,
                  spt_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(sptvg, FiscalYear),
                 fill = list(
                  spt_count = 0,
                  spt_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  spt_delta = (spt_rate - lag(spt_rate))/lag(spt_rate),
  spt_delta = ifelse(FiscalYear == 2014, NA, spt_delta),
  spt_deltap = (spt_rate - lag(spt_rate,9))/lag(spt_rate,9)
 ) %>%
 ungroup()

#### Mediolateral Episiotomy with Operative Vaginal Birth ----
#### ND = Not Done
#### MD = Midline
#### ML = Mediolateral

c9 <- dta %>%
 select(FiscalYear,
        DMMETHOD,
        DMEPISIO,
        CLuid) %>%
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
 group_by(FiscalYear, med, assvg, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  med_count = n()
 ) %>%
 group_by(FiscalYear, assvg, CLuid) %>%
 mutate(
  total_year = n(),
  med_rate = med_count/total_year
 ) %>%
 ungroup() %>%
 filter(med %in% 1, assvg %in% 1, !is.na(CLuid)) %>%
 select(FiscalYear,CLuid,med,med_count,med_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(med, CLuid),
                 fill = list(
                  med_count = 0,
                  med_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(med, FiscalYear),
                 fill = list(
                  med_count = 0,
                  med_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  med_delta = (med_rate - lag(med_rate))/lag(med_rate),
  med_delta = ifelse(FiscalYear == 2014, NA, med_delta),
  med_deltap = (med_rate - lag(med_rate,9))/lag(med_rate,9)
 ) %>%
 ungroup()

#### Severe perineal trauma with Operative Vaginal Birth and mediolateral episiotomy ----

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
        DMEPISIO,
        CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(FiscalYear, trdfrth, assvg, med, CLuid) %>%
 mutate(
  sptassvgmed_count = n()
 ) %>%
 group_by(FiscalYear, assvg, CLuid) %>%
 mutate(
  total_year = n(),
  sptassvgmed_rate = sptassvgmed_count/total_year
 ) %>%
 ungroup() %>%
 filter(sptassvgmed %in% 1, !is.na(CLuid)) %>%
 select(FiscalYear,CLuid,sptassvgmed,sptassvgmed_count,sptassvgmed_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(sptassvgmed, CLuid),
                 fill = list(
                  sptassvgmed_count = 0,
                  sptassvgmed_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(sptassvgmed, FiscalYear),
                 fill = list(
                  sptassvgmed_count = 0,
                  sptassvgmed_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  sptassvgmed_delta = (sptassvgmed_rate - lag(sptassvgmed_rate))/lag(sptassvgmed_rate),
  sptassvgmed_delta = ifelse(FiscalYear == 2014, NA, sptassvgmed_delta),
  sptassvgmed_deltap = (sptassvgmed_rate - lag(sptassvgmed_rate,9))/lag(sptassvgmed_rate,9)
 ) %>%
 ungroup()

#### Severe perineal trauma with Operative Vaginal Birth without mediolateral episiotomy ----

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
        DMEPISIO,
        CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(FiscalYear, trdfrth, assvg, med, CLuid) %>%
 mutate(
  sptassvgnmed_count = n()
 ) %>%
 group_by(FiscalYear, assvg, CLuid) %>%
 mutate(
  total_year = n(),
  sptassvgnmed_rate = sptassvgnmed_count/total_year
 ) %>%
 ungroup() %>%
 filter(sptassvgnmed %in% 1, !is.na(CLuid)) %>%
 select(FiscalYear,CLuid,sptassvgnmed,sptassvgnmed_count,sptassvgnmed_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(sptassvgnmed, CLuid),
                 fill = list(
                  sptassvgnmed_count = 0,
                  sptassvgnmed_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(sptassvgnmed, FiscalYear),
                 fill = list(
                  sptassvgnmed_count = 0,
                  sptassvgnmed_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  sptassvgnmed_delta = (sptassvgnmed_rate - lag(sptassvgnmed_rate))/lag(sptassvgnmed_rate),
  sptassvgnmed_delta = ifelse(FiscalYear == 2014, NA, sptassvgnmed_delta),
  sptassvgnmed_deltap = (sptassvgnmed_rate - lag(sptassvgnmed_rate,9))/lag(sptassvgnmed_rate,9)
 ) %>%
 ungroup()

#### Robson group ----

c12 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs1, RobsnGrp, CLuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs1, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
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
        !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, rbs1, rbs1_count, rbs1_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(rbs1, CLuid),
                 fill = list(
                  rbs1_count = 0,
                  rbs1_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(rbs1, FiscalYear),
                 fill = list(
                  rbs1_count = 0,
                  rbs1_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  rbs1_delta = (rbs1_rate - lag(rbs1_rate))/lag(rbs1_rate),
  rbs1_delta = ifelse(FiscalYear == 2014, NA, rbs1_delta),
  rbs1_deltap = (rbs1_rate - lag(rbs1_rate,9))/lag(rbs1_rate,9)
 ) %>%
 ungroup()

c13 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs21, RobsnGrp, CLuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs21, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
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
        !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, rbs21, rbs21_count, rbs21_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(rbs21, CLuid),
                 fill = list(
                  rbs21_count = 0,
                  rbs21_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(rbs21, FiscalYear),
                 fill = list(
                  rbs21_count = 0,
                  rbs21_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  rbs21_delta = (rbs21_rate - lag(rbs21_rate))/lag(rbs21_rate),
  rbs21_delta = ifelse(FiscalYear == 2014, NA, rbs21_delta),
  rbs21_deltap = (rbs21_rate - lag(rbs21_rate,9))/lag(rbs21_rate,9)
 ) %>%
 ungroup()

c14 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs51, RobsnGrp, CLuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs51, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
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
        !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, rbs51, rbs51_count, rbs51_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(rbs51, CLuid),
                 fill = list(
                  rbs51_count = 0,
                  rbs51_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(rbs51, FiscalYear),
                 fill = list(
                  rbs51_count = 0,
                  rbs51_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  rbs51_delta = (rbs51_rate - lag(rbs51_rate))/lag(rbs51_rate),
  rbs51_delta = ifelse(FiscalYear == 2014, NA, rbs51_delta),
  rbs51_deltap = (rbs51_rate - lag(rbs51_rate,9))/lag(rbs51_rate,9)
 ) %>%
 ungroup()

#### Postpartum hemorrhage treated with a blood transfusion ----

c15 <- dta %>%
 select(FiscalYear, Any_Blood_Product, Postpartum_Haemorrhage, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  pphbl = case_when(
   Any_Blood_Product > 0 & Postpartum_Haemorrhage > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, Any_Blood_Product, Postpartum_Haemorrhage, CLuid) %>%
 mutate(
  pphbl_count = n()
 ) %>%
 group_by(FiscalYear, CLuid) %>%
 mutate(
  total_year = n(),
  pphbl_rate = pphbl_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Blood_Product %in% 1, Postpartum_Haemorrhage %in% 1,
        !is.na(CLuid)) %>%
 select(FiscalYear,CLuid,pphbl, pphbl_count, pphbl_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(pphbl, CLuid),
                 fill = list(
                  pphbl_count = 0,
                  pphbl_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(pphbl, FiscalYear),
                 fill = list(
                  pphbl_count = 0,
                  pphbl_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  pphbl_delta = (pphbl_rate - lag(pphbl_rate))/lag(pphbl_rate),
  pphbl_delta = ifelse(FiscalYear == 2014, NA, pphbl_delta),
  pphbl_deltap = (pphbl_rate - lag(pphbl_rate,9))/lag(pphbl_rate,9)
 ) %>%
 ungroup()

#### Postpartum hemorrhage resulting in procedural intervention ----

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
        MODEDEL,
        CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(FiscalYear, pphpi, Postpartum_Haemorrhage, CLuid) %>%
 mutate(
  pphpi_count = n()
 ) %>%
 group_by(FiscalYear, Postpartum_Haemorrhage, CLuid) %>%
 mutate(
  total_year = n(),
  pphpi_rate = pphpi_count/total_year
 ) %>%
 ungroup() %>%
 filter(pphpi %in% 1, Postpartum_Haemorrhage %in% 1,
        !is.na(CLuid)) %>%
 select(FiscalYear,CLuid,pphpi, pphpi_count, pphpi_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(pphpi, CLuid),
                 fill = list(
                  pphpi_count = 0,
                  pphpi_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(pphpi, FiscalYear),
                 fill = list(
                  pphpi_count = 0,
                  pphpi_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  pphpi_delta = (pphpi_rate - lag(pphpi_rate))/lag(pphpi_rate),
  pphpi_delta = ifelse(FiscalYear == 2014, NA, pphpi_delta),
  pphpi_deltap = (pphpi_rate - lag(pphpi_rate,9))/lag(pphpi_rate,9)
 ) %>%
 ungroup()

#### Postpartum readmission ----

c17 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, ppreadm, CLuid) %>%
 distinct() %>%
 group_by(FiscalYear, ppreadm, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  ppreadm_count = n()
 ) %>%
 group_by(FiscalYear, CLuid) %>%
 mutate(
  total_year = n(),
  ppreadm_rate = ppreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(ppreadm %in% 1,
        !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, ppreadm, ppreadm_count, ppreadm_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(ppreadm, CLuid),
                 fill = list(
                  ppreadm_count = 0,
                  ppreadm_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(ppreadm, FiscalYear),
                 fill = list(
                  ppreadm_count = 0,
                  ppreadm_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  ppreadm_delta = (ppreadm_rate - lag(ppreadm_rate))/lag(ppreadm_rate),
  ppreadm_delta = ifelse(FiscalYear == 2014, NA, ppreadm_delta),
  ppreadm_deltap = (ppreadm_rate - lag(ppreadm_rate,9))/lag(ppreadm_rate,9)
 ) %>%
 ungroup()

#### Postpartum patients who received medical anticoagulation prophylaxis when indicated ----

#c18

#### Skin to skin ----

c19 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknskn, CLuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, sknskn, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  sknskn_count = n()
 ) %>%
 group_by(FiscalYear, CLuid, lvb) %>%
 mutate(
  total_year = n(),
  sknskn_rate = sknskn_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskn %in% 1,
        !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, sknskn, sknskn_count, sknskn_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(sknskn, CLuid),
                 fill = list(
                  sknskn_count = 0,
                  sknskn_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(sknskn, FiscalYear),
                 fill = list(
                  sknskn_count = 0,
                  sknskn_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  sknskn_delta = (sknskn_rate - lag(sknskn_rate))/lag(sknskn_rate),
  sknskn_delta = ifelse(FiscalYear == 2014, NA, sknskn_delta),
  sknskn_deltap = (sknskn_rate - lag(sknskn_rate,9))/lag(sknskn_rate,9)
 ) %>%
 ungroup()

c20 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknsknvg, CLuid, lvbvg) %>%
 distinct() %>%
 group_by(FiscalYear, sknsknvg, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  sknsknvg_count = n()
 ) %>%
 group_by(FiscalYear, CLuid, lvbvg) %>%
 mutate(
  total_year = n(),
  sknsknvg_rate = sknsknvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknsknvg %in% 1,
        !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, sknsknvg, sknsknvg_count, sknsknvg_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(sknsknvg, CLuid),
                 fill = list(
                  sknsknvg_count = 0,
                  sknsknvg_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(sknsknvg, FiscalYear),
                 fill = list(
                  sknsknvg_count = 0,
                  sknsknvg_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  sknsknvg_delta = (sknsknvg_rate - lag(sknsknvg_rate))/lag(sknsknvg_rate),
  sknsknvg_delta = ifelse(FiscalYear == 2014, NA, sknsknvg_delta),
  sknsknvg_deltap = (sknsknvg_rate - lag(sknsknvg_rate,9))/lag(sknsknvg_rate,9)
 ) %>%
 ungroup()

c21 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknskncs, CLuid, lvbcs) %>%
 distinct() %>%
 group_by(FiscalYear, sknskncs, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  sknskncs_count = n()
 ) %>%
 group_by(FiscalYear, CLuid, lvbcs) %>%
 mutate(
  total_year = n(),
  sknskncs_rate = sknskncs_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskncs %in% 1,
        !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, sknskncs, sknskncs_count, sknskncs_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(sknskncs, CLuid),
                 fill = list(
                  sknskncs_count = 0,
                  sknskncs_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(sknskncs, FiscalYear),
                 fill = list(
                  sknskncs_count = 0,
                  sknskncs_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  sknskncs_delta = (sknskncs_rate - lag(sknskncs_rate))/lag(sknskncs_rate),
  sknskncs_delta = ifelse(FiscalYear == 2014, NA, sknskncs_delta),
  sknskncs_deltap = (sknskncs_rate - lag(sknskncs_rate,9))/lag(sknskncs_rate,9)
 ) %>%
 ungroup()

#### Postpartum blood products amongst those who received antenatal iron therapy ----

c22 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
  pphiv = case_when(
   Any_Blood_Product > 0 & R003_02100 > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, Any_Blood_Product, R003_02100, CLuid) %>%
 mutate(
  pphiv_count = n()
 ) %>%
 group_by(FiscalYear, Any_Blood_Product, CLuid) %>%
 mutate(
  total_year = n(),
  pphiv_rate = pphiv_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Blood_Product %in% 1, R003_02100 %in% 1,
        !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, pphiv, pphiv_count, pphiv_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(pphiv, CLuid),
                 fill = list(
                  pphiv_count = 0,
                  pphiv_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(pphiv, FiscalYear),
                 fill = list(
                  pphiv_count = 0,
                  pphiv_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  pphiv_delta = (pphiv_rate - lag(pphiv_rate))/lag(pphiv_rate),
  pphiv_delta = ifelse(FiscalYear == 2014, NA, pphiv_delta),
  pphiv_deltap = (pphiv_rate - lag(pphiv_rate,9))/lag(pphiv_rate,9)
 ) %>%
 ungroup()

#### Neonatal readmission ----

c23 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, neoreadm, CLuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, neoreadm, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  neoreadm_count = n()
 ) %>%
 group_by(FiscalYear, CLuid, lvb) %>%
 mutate(
  total_year = n(),
  neoreadm_rate = neoreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(neoreadm %in% 1,
        !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, neoreadm, neoreadm_count, neoreadm_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(neoreadm, CLuid),
                 fill = list(
                  neoreadm_count = 0,
                  neoreadm_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(neoreadm, FiscalYear),
                 fill = list(
                  neoreadm_count = 0,
                  neoreadm_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  neoreadm_delta = (neoreadm_rate - lag(neoreadm_rate))/lag(neoreadm_rate),
  neoreadm_delta = ifelse(FiscalYear == 2014, NA, neoreadm_delta),
  neoreadm_deltap = (neoreadm_rate - lag(neoreadm_rate,9))/lag(neoreadm_rate,9)
 ) %>%
 ungroup()

#### Preterm infants who received complete course of steroids > 24h and < 7 days prior to birth ----

c24 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(FiscalYear, pretster, CLuid) %>%
 mutate(
  pretster_count = n()
 ) %>%
 group_by(FiscalYear, den, CLuid) %>%
 mutate(
  total_year = n(),
  pretster_rate = pretster_count/total_year
 ) %>%
 ungroup() %>%
 filter(pretster %in% 1, !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, pretster, pretster_count,pretster_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(pretster, CLuid),
                 fill = list(
                  pretster_count = 0,
                  pretster_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(pretster, FiscalYear),
                 fill = list(
                  pretster_count = 0,
                  pretster_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  pretster_delta = (pretster_rate - lag(pretster_rate))/lag(pretster_rate),
  pretster_delta = ifelse(FiscalYear == 2014, NA, pretster_delta),
  pretster_deltap = (pretster_rate - lag(pretster_rate,9))/lag(pretster_rate,9)
 ) %>%
 ungroup()

#### Preterm babies born in facility without NICU ----

c25 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(FiscalYear, pretnnicu, CLuid) %>%
 mutate(
  pretnnicu_count = n()
 ) %>%
 group_by(FiscalYear, den, CLuid) %>%
 mutate(
  total_year = n(),
  pretnnicu_rate = pretnnicu_count/total_year
 ) %>%
 ungroup() %>%
 filter(pretnnicu %in% 1, !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, pretnnicu, pretnnicu_count,pretnnicu_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(pretnnicu, CLuid),
                 fill = list(
                  pretnnicu_count = 0,
                  pretnnicu_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(pretnnicu, FiscalYear),
                 fill = list(
                  pretnnicu_count = 0,
                  pretnnicu_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  pretnnicu_delta = (pretnnicu_rate - lag(pretnnicu_rate))/lag(pretnnicu_rate),
  pretnnicu_delta = ifelse(FiscalYear == 2014, NA, pretnnicu_delta),
  pretnnicu_deltap = (pretnnicu_rate - lag(pretnnicu_rate,9))/lag(pretnnicu_rate,9)
 ) %>%
 ungroup()

#### Newborn respiratory distress associated with low-risk repeat cesarean at term gestation (≥ 37 weeks < 39 weeks) ----

c26 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(FiscalYear, newlwcs, CLuid) %>%
 mutate(
  newlwcs_count = n()
 ) %>%
 group_by(FiscalYear, den, CLuid) %>%
 mutate(
  total_year = n(),
  newlwcs_rate = newlwcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(newlwcs %in% 1) %>%
 select(FiscalYear, CLuid, newlwcs, newlwcs_count,newlwcs_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(newlwcs, CLuid),
                 fill = list(
                  newlwcs_count = 0,
                  newlwcs_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(newlwcs, FiscalYear),
                 fill = list(
                  newlwcs_count = 0,
                  newlwcs_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  newlwcs_delta = (newlwcs_rate - lag(newlwcs_rate))/lag(newlwcs_rate),
  newlwcs_delta = ifelse(FiscalYear == 2014, NA, newlwcs_delta),
  newlwcs_deltap = (newlwcs_rate - lag(newlwcs_rate,9))/lag(newlwcs_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping  ----

c27 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
  delcord = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (DEL_CORD_CLAMP %in% 3) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, delcord, CLuid) %>%
 mutate(
  delcord_count = n()
 ) %>%
 group_by(FiscalYear, lvb, CLuid) %>%
 mutate(
  total_year = n(),
  delcord_rate = delcord_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcord %in% 1, lvb %in% 1,
        !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, delcord, delcord_count,delcord_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcord, CLuid),
                 fill = list(
                  delcord_count = 0,
                  delcord_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(delcord, FiscalYear),
                 fill = list(
                  delcord_count = 0,
                  delcord_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  delcord_delta = (delcord_rate - lag(delcord_rate))/lag(delcord_rate),
  delcord_delta = ifelse(FiscalYear == 2014, NA, delcord_delta),
  delcord_deltap = (delcord_rate - lag(delcord_rate,9))/lag(delcord_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns ----

c28 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(FiscalYear, delcordterm, CLuid) %>%
 mutate(
  delcordterm_count = n()
 ) %>%
 group_by(FiscalYear, den, CLuid) %>%
 mutate(
  total_year = n(),
  delcordterm_rate = delcordterm_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordterm %in% 1, !is.na(CLuid)) %>%
 select(FiscalYear,CLuid,delcordterm,delcordterm_count,delcordterm_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordterm, CLuid),
                 fill = list(
                  delcordterm_count = 0,
                  delcordterm_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(delcordterm, FiscalYear),
                 fill = list(
                  delcordterm_count = 0,
                  delcordterm_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  delcordterm_delta = (delcordterm_rate - lag(delcordterm_rate))/lag(delcordterm_rate),
  delcordterm_delta = ifelse(FiscalYear == 2014, NA, delcordterm_delta),
  delcordterm_deltap = (delcordterm_rate - lag(delcordterm_rate,9))/lag(delcordterm_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns following spontaneous vaginal birth ----

c29 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(FiscalYear, delcordtermvg, CLuid) %>%
 mutate(
  delcordtermvg_count = n()
 ) %>%
 group_by(FiscalYear, den, CLuid) %>%
 mutate(
  total_year = n(),
  delcordtermvg_rate = delcordtermvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordtermvg %in% 1, !is.na(CLuid)) %>%
 select(FiscalYear,CLuid, delcordtermvg, delcordtermvg_count,delcordtermvg_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordtermvg, CLuid),
                 fill = list(
                  delcordtermvg_count = 0,
                  delcordtermvg_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(delcordtermvg, FiscalYear),
                 fill = list(
                  delcordtermvg_count = 0,
                  delcordtermvg_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  delcordtermvg_delta = (delcordtermvg_rate - lag(delcordtermvg_rate))/lag(delcordtermvg_rate),
  delcordtermvg_delta = ifelse(FiscalYear == 2014, NA, delcordtermvg_delta),
  delcordtermvg_deltap = (delcordtermvg_rate - lag(delcordtermvg_rate,9))/lag(delcordtermvg_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns following caesarean birth ----

c30 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(FiscalYear, delcordtermcs, CLuid) %>%
 mutate(
  delcordtermcs_count = n()
 ) %>%
 group_by(FiscalYear, den, CLuid) %>%
 mutate(
  total_year = n(),
  delcordtermcs_rate = delcordtermcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordtermcs %in% 1, !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, delcordtermcs, delcordtermcs_count,delcordtermcs_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordtermcs, CLuid),
                 fill = list(
                  delcordtermcs_count = 0,
                  delcordtermcs_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(delcordtermcs, FiscalYear),
                 fill = list(
                  delcordtermcs_count = 0,
                  delcordtermcs_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  delcordtermcs_delta = (delcordtermcs_rate - lag(delcordtermcs_rate))/lag(delcordtermcs_rate),
  delcordtermcs_delta = ifelse(FiscalYear == 2014, NA, delcordtermcs_delta),
  delcordtermcs_deltap = (delcordtermcs_rate - lag(delcordtermcs_rate,9))/lag(delcordtermcs_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns ----

c31 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(FiscalYear, delcordpreterm, CLuid) %>%
 mutate(
  delcordpreterm_count = n()
 ) %>%
 group_by(FiscalYear, den, CLuid) %>%
 mutate(
  total_year = n(),
  delcordpreterm_rate = delcordpreterm_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpreterm %in% 1, !is.na(CLuid)) %>%
 select(FiscalYear,CLuid, delcordpreterm, delcordpreterm_count,delcordpreterm_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordpreterm, CLuid),
                 fill = list(
                  delcordpreterm_count = 0,
                  delcordpreterm_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(delcordpreterm, FiscalYear),
                 fill = list(
                  delcordpreterm_count = 0,
                  delcordpreterm_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  delcordpreterm_delta = (delcordpreterm_rate - lag(delcordpreterm_rate))/lag(delcordpreterm_rate),
  delcordpreterm_delta = ifelse(FiscalYear == 2014, NA, delcordpreterm_delta),
  delcordpreterm_deltap = (delcordpreterm_rate - lag(delcordpreterm_rate,9))/lag(delcordpreterm_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns following spontaneous vaginal birth ----

c32 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(FiscalYear, delcordpretermvg, CLuid) %>%
 mutate(
  delcordpretermvg_count = n()
 ) %>%
 group_by(FiscalYear, den, CLuid) %>%
 mutate(
  total_year = n(),
  delcordpretermvg_rate = delcordpretermvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpretermvg %in% 1, !is.na(CLuid)) %>%
 select(FiscalYear,CLuid, delcordpretermvg, delcordpretermvg_count,delcordpretermvg_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordpretermvg, CLuid),
                 fill = list(
                  delcordpretermvg_count = 0,
                  delcordpretermvg_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(delcordpretermvg, FiscalYear),
                 fill = list(
                  delcordpretermvg_count = 0,
                  delcordpretermvg_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  delcordpretermvg_delta = (delcordpretermvg_rate - lag(delcordpretermvg_rate))/lag(delcordpretermvg_rate),
  delcordpretermvg_delta = ifelse(FiscalYear == 2014, NA, delcordpretermvg_delta),
  delcordpretermvg_deltap = (delcordpretermvg_rate - lag(delcordpretermvg_rate,9))/lag(delcordpretermvg_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns following caesarean birth ----

c33 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
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
 group_by(FiscalYear, delcordpretermcs, CLuid) %>%
 mutate(
  delcordpretermcs_count = n()
 ) %>%
 group_by(FiscalYear, den, CLuid) %>%
 mutate(
  total_year = n(),
  delcordpretermcs_rate = delcordpretermcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpretermcs %in% 1, !is.na(CLuid)) %>%
 select(FiscalYear,CLuid, delcordpretermcs, delcordpretermcs_count,delcordpretermcs_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordpretermcs, CLuid),
                 fill = list(
                  delcordpretermcs_count = 0,
                  delcordpretermcs_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(delcordpretermcs, FiscalYear),
                 fill = list(
                  delcordpretermcs_count = 0,
                  delcordpretermcs_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  delcordpretermcs_delta = (delcordpretermcs_rate - lag(delcordpretermcs_rate))/lag(delcordpretermcs_rate),
  delcordpretermcs_delta = ifelse(FiscalYear == 2014, NA, delcordpretermcs_delta),
  delcordpretermcs_deltap = (delcordpretermcs_rate - lag(delcordpretermcs_rate,9))/lag(delcordpretermcs_rate,9)
 ) %>%
 ungroup()

#### Milk feeding ----

c34 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, excbrst, CLuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, excbrst, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  excbrst_count = n()
 ) %>%
 group_by(FiscalYear, CLuid, lvb) %>%
 mutate(
  total_year = n(),
  excbrst_rate = excbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(excbrst %in% 1,
        !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, excbrst, excbrst_count, excbrst_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(excbrst, CLuid),
                 fill = list(
                  excbrst_count = 0,
                  excbrst_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(excbrst, FiscalYear),
                 fill = list(
                  excbrst_count = 0,
                  excbrst_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  excbrst_delta = (excbrst_rate - lag(excbrst_rate))/lag(excbrst_rate),
  excbrst_delta = ifelse(FiscalYear == 2014, NA, excbrst_delta),
  excbrst_deltap = (excbrst_rate - lag(excbrst_rate,9))/lag(excbrst_rate,9)
 ) %>%
 ungroup()

c35 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, nexcbrst, CLuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, nexcbrst, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  nexcbrst_count = n()
 ) %>%
 group_by(FiscalYear, CLuid, lvb) %>%
 mutate(
  total_year = n(),
  nexcbrst_rate = nexcbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nexcbrst %in% 1,
        !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, nexcbrst, nexcbrst_count, nexcbrst_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(nexcbrst, CLuid),
                 fill = list(
                  nexcbrst_count = 0,
                  nexcbrst_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(nexcbrst, FiscalYear),
                 fill = list(
                  nexcbrst_count = 0,
                  nexcbrst_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  nexcbrst_delta = (nexcbrst_rate - lag(nexcbrst_rate))/lag(nexcbrst_rate),
  nexcbrst_delta = ifelse(FiscalYear == 2014, NA, nexcbrst_delta),
  nexcbrst_deltap = (nexcbrst_rate - lag(nexcbrst_rate,9))/lag(nexcbrst_rate,9)
 ) %>%
 ungroup()

c36 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, nbrst, CLuid, lvbint) %>%
 distinct() %>%
 group_by(FiscalYear, nbrst, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  nbrst_count = n()
 ) %>%
 group_by(FiscalYear, CLuid, lvbint) %>%
 mutate(
  total_year = n(),
  nbrst_rate = nbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nbrst %in% 1,
        !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, nbrst, nbrst_count, nbrst_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(nbrst, CLuid),
                 fill = list(
                  nbrst_count = 0,
                  nbrst_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(nbrst, FiscalYear),
                 fill = list(
                  nbrst_count = 0,
                  nbrst_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  nbrst_delta = (nbrst_rate - lag(nbrst_rate))/lag(nbrst_rate),
  nbrst_delta = ifelse(FiscalYear == 2014, NA, nbrst_delta),
  nbrst_deltap = (nbrst_rate - lag(nbrst_rate,9))/lag(nbrst_rate,9)
 ) %>%
 ungroup()

#### The breast/chest feeding initiation rate ----

c37 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, brstinit, CLuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, brstinit, CLuid) %>%
 mutate(
  CLuid = as.character(CLuid),
  brstinit_count = n()
 ) %>%
 group_by(FiscalYear, CLuid, lvb) %>%
 mutate(
  total_year = n(),
  brstinit_rate = brstinit_count/total_year
 ) %>%
 ungroup() %>%
 filter(brstinit %in% 1,
        !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, brstinit, brstinit_count, brstinit_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(brstinit, CLuid),
                 fill = list(
                  brstinit_count = 0,
                  brstinit_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(brstinit, FiscalYear),
                 fill = list(
                  brstinit_count = 0,
                  brstinit_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  brstinit_delta = (brstinit_rate - lag(brstinit_rate))/lag(brstinit_rate),
  brstinit_delta = ifelse(FiscalYear == 2014, NA, brstinit_delta),
  brstinit_deltap = (brstinit_rate - lag(brstinit_rate,9))/lag(brstinit_rate,9)
 ) %>%
 ungroup()

#### ICU Admission during Pregnancy or Postpartum ----

c38 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
  icu = case_when(
   JOGC_ICU > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, JOGC_ICU, CLuid) %>%
 mutate(
  icu_count = n()
 ) %>%
 group_by(FiscalYear, CLuid) %>%
 mutate(
  total_year = n(),
  icu_rate = icu_count/total_year
 ) %>%
 ungroup() %>%
 filter(JOGC_ICU %in% 1, !is.na(CLuid)) %>%
 select(FiscalYear, CLuid, icu, icu_count,icu_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(icu, CLuid),
                 fill = list(
                  icu_count = 0,
                  icu_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(icu, FiscalYear),
                 fill = list(
                  icu_count = 0,
                  icu_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  icu_delta = (icu_rate - lag(icu_rate))/lag(icu_rate),
  icu_delta = ifelse(FiscalYear == 2014, NA, icu_delta),
  icu_deltap = (icu_rate - lag(icu_rate,9))/lag(icu_rate,9)
 ) %>%
 ungroup()

#### Rate of Severe Morbidity in Pregnancy or Postpartum ----

c39 <- dta %>%
 mutate(
  CLuid = as.character(CLuid),
  smm = case_when(
   JOGC_AnySMM > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, JOGC_AnySMM, CLuid) %>%
 mutate(
  smm_count = n()
 ) %>%
 group_by(FiscalYear, lvb, CLuid) %>%
 mutate(
  total_year = n(),
  smm_rate = smm_count/total_year
 ) %>%
 ungroup() %>%
 filter(JOGC_AnySMM %in% 1, lvb %in% 1, !is.na(CLuid)) %>%
 select(FiscalYear,CLuid, smm, smm_count,smm_rate) %>%
 distinct() %>%
 arrange(CLuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(smm, CLuid),
                 fill = list(
                  smm_count = 0,
                  smm_rate = NA
                 )) %>%
 tidyr::complete(CLuid = unique(as.character(cl_shp$GeoUID)),
                 tidyr::nesting(smm, FiscalYear),
                 fill = list(
                  smm_count = 0,
                  smm_rate = NA
                 )) %>%
 group_by(CLuid) %>%
 mutate(
  smm_delta = (smm_rate - lag(smm_rate))/lag(smm_rate),
  smm_delta = ifelse(FiscalYear == 2014, NA, smm_delta),
  smm_deltap = (smm_rate - lag(smm_rate,9))/lag(smm_rate,9)
 ) %>%
 ungroup()

fcl_stats <- cbind(
 c1 %>% select(-PreExisting_Hypertension),
 c2 %>% select(-Gestational_Hypertension,-CLuid,-FiscalYear),
 c3 %>% select(-Any_Hypertension,-CLuid,-FiscalYear),
 c4 %>% select(-anemia,-CLuid,-FiscalYear),
 c5 %>% select(-PreExisting_Diabetes,-CLuid,-FiscalYear),
 c6 %>% select(-GDM,-CLuid,-FiscalYear),
 c7 %>% select(-Any_Diabetes,-CLuid,-FiscalYear),
 c8 %>% select(-sptvg,-CLuid,-FiscalYear),
 c9 %>% select(-med,-CLuid,-FiscalYear),
 c10 %>% select(-sptassvgmed,-CLuid,-FiscalYear),
 c11 %>% select(-sptassvgnmed,-CLuid,-FiscalYear),
 c12 %>% select(-rbs1,-CLuid,-FiscalYear),
 c13 %>% select(-rbs21,-CLuid,-FiscalYear),
 c14 %>% select(-rbs51,-CLuid,-FiscalYear),
 c15 %>% select(-pphbl,-CLuid,-FiscalYear),
 c16 %>% select(-pphpi,-CLuid,-FiscalYear),
 c17 %>% select(-ppreadm,-CLuid,-FiscalYear),
 #c18 %>% select(-brstinit,-CLuid,-FiscalYear),
 c19 %>% select(-sknskn,-CLuid,-FiscalYear),
 c20 %>% select(-sknsknvg,-CLuid,-FiscalYear),
 c21 %>% select(-sknskncs,-CLuid,-FiscalYear),
 c22 %>% select(-pphiv,-CLuid,-FiscalYear),
 c23 %>% select(-neoreadm,-CLuid,-FiscalYear),
 c24 %>% select(-pretster,-CLuid,-FiscalYear),
 c25 %>% select(-pretnnicu,-CLuid,-FiscalYear),
 c26 %>% select(-newlwcs,-CLuid,-FiscalYear),
 c27 %>% select(-delcord,-CLuid,-FiscalYear),
 c28 %>% select(-delcordterm,-CLuid,-FiscalYear),
 c29 %>% select(-delcordtermvg,-CLuid,-FiscalYear),
 c30 %>% select(-delcordtermcs,-CLuid,-FiscalYear),
 c31 %>% select(-delcordpreterm,-CLuid,-FiscalYear),
 c32 %>% select(-delcordpretermvg,-CLuid,-FiscalYear),
 c33 %>% select(-delcordpretermcs,-CLuid,-FiscalYear),
 c34 %>% select(-excbrst,-CLuid,-FiscalYear),
 c35 %>% select(-nexcbrst,-CLuid,-FiscalYear),
 c36 %>% select(-nbrst,-CLuid,-FiscalYear),
 c37 %>% select(-brstinit,-CLuid,-FiscalYear),
 c38 %>% select(-icu,-CLuid,-FiscalYear),
 c39 %>% select(-smm,-CLuid,-FiscalYear)
)

### Community health network (CHN) ----
#### Hypertension ----

c1 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, PreExisting_Hypertension, CHNuid) %>%
 distinct() %>%
 group_by(FiscalYear, PreExisting_Hypertension, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  prehyp_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid) %>%
 mutate(
  total_year = n(),
  prehyp_rate = prehyp_count/total_year
 ) %>%
 filter(PreExisting_Hypertension %in% 1,
        !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, PreExisting_Hypertension, prehyp_count, prehyp_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(PreExisting_Hypertension, CHNuid),
                 fill = list(
                  prehyp_count = 0,
                  prehyp_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(PreExisting_Hypertension, FiscalYear),
                 fill = list(
                  prehyp_count = 0,
                  prehyp_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  prehyp_delta = (prehyp_rate - lag(prehyp_rate))/lag(prehyp_rate),
  prehyp_delta = ifelse(FiscalYear == 2014, NA, prehyp_delta),
  prehyp_deltap = (prehyp_rate - lag(prehyp_rate,9))/lag(prehyp_rate,9)
 ) %>%
 ungroup()

c2 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Gestational_Hypertension, CHNuid) %>%
 distinct() %>%
 group_by(FiscalYear, Gestational_Hypertension, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  gesthyp_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid) %>%
 mutate(
  total_year = n(),
  gesthyp_rate = gesthyp_count/total_year
 ) %>%
 filter(Gestational_Hypertension %in% 1,
        !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, Gestational_Hypertension, gesthyp_count, gesthyp_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(Gestational_Hypertension, CHNuid),
                 fill = list(
                  gesthyp_count = 0,
                  gesthyp_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(Gestational_Hypertension, FiscalYear),
                 fill = list(
                  gesthyp_count = 0,
                  gesthyp_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  gesthyp_delta = (gesthyp_rate - lag(gesthyp_rate))/lag(gesthyp_rate),
  gesthyp_delta = ifelse(FiscalYear == 2014, NA, gesthyp_delta),
  gesthyp_deltap = (gesthyp_rate - lag(gesthyp_rate,9))/lag(gesthyp_rate,9)
 ) %>%
 ungroup()

c3 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Any_Hypertension, CHNuid) %>%
 distinct() %>%
 group_by(FiscalYear, Any_Hypertension, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  hyp_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid) %>%
 mutate(
  total_year = n(),
  hyp_rate = hyp_count/total_year
 ) %>%
 filter(Any_Hypertension %in% 1,
        !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, Any_Hypertension, hyp_count, hyp_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(Any_Hypertension, CHNuid),
                 fill = list(
                  hyp_count = 0,
                  hyp_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(Any_Hypertension, FiscalYear),
                 fill = list(
                  hyp_count = 0,
                  hyp_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  hyp_delta = (hyp_rate - lag(hyp_rate))/lag(hyp_rate),
  hyp_delta = ifelse(FiscalYear == 2014, NA, hyp_delta),
  hyp_deltap = (hyp_rate - lag(hyp_rate,9))/lag(hyp_rate,9)
 ) %>%
 ungroup()

#### Anaemia ----

c4 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, CHNuid, MANEM, R014_01500, MO990, MD50_D53, MD55_D59, MD60_D64) %>%
 mutate(anemia = case_when(
  MANEM > 0 | R014_01500 > 0 | MO990 > 0 | MD50_D53 > 0 |
   MD55_D59 > 0 | MD60_D64 > 0 ~ 1,
  TRUE ~ NA_integer_
 )) %>%
 distinct() %>%
 group_by(FiscalYear, anemia, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  anemia_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid) %>%
 mutate(
  total_year = n(),
  anemia_rate = anemia_count/total_year
 ) %>%
 filter(anemia %in% 1,
        !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, anemia, anemia_count, anemia_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(anemia, CHNuid),
                 fill = list(
                  anemia_count = 0,
                  anemia_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(anemia, FiscalYear),
                 fill = list(
                  anemia_count = 0,
                  anemia_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  anemia_delta = (anemia_rate - lag(anemia_rate))/lag(anemia_rate),
  anemia_delta = ifelse(FiscalYear == 2014, NA, anemia_delta),
  anemia_deltap = (anemia_rate - lag(anemia_rate,9))/lag(anemia_rate,9)
 ) %>%
 ungroup()

#### Diabetes ----

c5 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, PreExisting_Diabetes, CHNuid) %>%
 distinct() %>%
 group_by(FiscalYear, PreExisting_Diabetes, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  prediab_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid) %>%
 mutate(
  total_year = n(),
  prediab_rate = prediab_count/total_year
 ) %>%
 filter(PreExisting_Diabetes %in% 1,
        !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, PreExisting_Diabetes, prediab_count, prediab_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(PreExisting_Diabetes, CHNuid),
                 fill = list(
                  prediab_count = 0,
                  prediab_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(PreExisting_Diabetes, FiscalYear),
                 fill = list(
                  prediab_count = 0,
                  prediab_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  prediab_delta = (prediab_rate - lag(prediab_rate))/lag(prediab_rate),
  prediab_delta = ifelse(FiscalYear == 2014, NA, prediab_delta),
  prediab_deltap = (prediab_rate - lag(prediab_rate,9))/lag(prediab_rate,9)
 ) %>%
 ungroup()

c6 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, GDM, CHNuid) %>%
 distinct() %>%
 group_by(FiscalYear, GDM, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  gestdiab_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid) %>%
 mutate(
  total_year = n(),
  gestdiab_rate = gestdiab_count/total_year
 ) %>%
 filter(GDM %in% 1,
        !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, GDM, gestdiab_count, gestdiab_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(GDM, CHNuid),
                 fill = list(
                  gestdiab_count = 0,
                  gestdiab_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(GDM, FiscalYear),
                 fill = list(
                  gestdiab_count = 0,
                  gestdiab_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  gestdiab_delta = (gestdiab_rate - lag(gestdiab_rate))/lag(gestdiab_rate),
  gestdiab_delta = ifelse(FiscalYear == 2014, NA, gestdiab_delta),
  gestdiab_deltap = (gestdiab_rate - lag(gestdiab_rate,9))/lag(gestdiab_rate,9)
 ) %>%
 ungroup()

c7 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Any_Diabetes, CHNuid) %>%
 distinct() %>%
 group_by(FiscalYear, Any_Diabetes, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  diab_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid) %>%
 mutate(
  total_year = n(),
  diab_rate = diab_count/total_year
 ) %>%
 filter(Any_Diabetes %in% 1,
        !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, Any_Diabetes, diab_count, diab_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(Any_Diabetes, CHNuid),
                 fill = list(
                  diab_count = 0,
                  diab_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(Any_Diabetes, FiscalYear),
                 fill = list(
                  diab_count = 0,
                  diab_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  diab_delta = (diab_rate - lag(diab_rate))/lag(diab_rate),
  diab_delta = ifelse(FiscalYear == 2014, NA, diab_delta),
  diab_deltap = (diab_rate - lag(diab_rate,9))/lag(diab_rate,9)
 ) %>%
 ungroup()

#### Severe Perineal Trauma with Spontaneous Vaginal Birth ----

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
        DMMETHOD,
        CHNuid) %>%
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
 group_by(FiscalYear, trdfrth, sptvg, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  spt_count = n()
 ) %>%
 group_by(FiscalYear, sptvg, CHNuid) %>%
 mutate(
  total_year = n(),
  spt_rate = spt_count/total_year
 ) %>%
 ungroup() %>%
 filter(trdfrth %in% 1, sptvg %in% 1, !is.na(CHNuid)) %>%
 select(FiscalYear,CHNuid,sptvg,spt_count,spt_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(sptvg, CHNuid),
                 fill = list(
                  spt_count = 0,
                  spt_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(sptvg, FiscalYear),
                 fill = list(
                  spt_count = 0,
                  spt_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  spt_delta = (spt_rate - lag(spt_rate))/lag(spt_rate),
  spt_delta = ifelse(FiscalYear == 2014, NA, spt_delta),
  spt_deltap = (spt_rate - lag(spt_rate,9))/lag(spt_rate,9)
 ) %>%
 ungroup()

#### Mediolateral Episiotomy with Operative Vaginal Birth ----
#### ND = Not Done
#### MD = Midline
#### ML = Mediolateral

c9 <- dta %>%
 select(FiscalYear,
        DMMETHOD,
        DMEPISIO,
        CHNuid) %>%
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
 group_by(FiscalYear, med, assvg, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  med_count = n()
 ) %>%
 group_by(FiscalYear, assvg, CHNuid) %>%
 mutate(
  total_year = n(),
  med_rate = med_count/total_year
 ) %>%
 ungroup() %>%
 filter(med %in% 1, assvg %in% 1, !is.na(CHNuid)) %>%
 select(FiscalYear,CHNuid,med,med_count,med_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(med, CHNuid),
                 fill = list(
                  med_count = 0,
                  med_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(med, FiscalYear),
                 fill = list(
                  med_count = 0,
                  med_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  med_delta = (med_rate - lag(med_rate))/lag(med_rate),
  med_delta = ifelse(FiscalYear == 2014, NA, med_delta),
  med_deltap = (med_rate - lag(med_rate,9))/lag(med_rate,9)
 ) %>%
 ungroup()

#### Severe perineal trauma with Operative Vaginal Birth and mediolateral episiotomy ----

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
        DMEPISIO,
        CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(FiscalYear, trdfrth, assvg, med, CHNuid) %>%
 mutate(
  sptassvgmed_count = n()
 ) %>%
 group_by(FiscalYear, assvg, CHNuid) %>%
 mutate(
  total_year = n(),
  sptassvgmed_rate = sptassvgmed_count/total_year
 ) %>%
 ungroup() %>%
 filter(sptassvgmed %in% 1, !is.na(CHNuid)) %>%
 select(FiscalYear,CHNuid,sptassvgmed,sptassvgmed_count,sptassvgmed_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(sptassvgmed, CHNuid),
                 fill = list(
                  sptassvgmed_count = 0,
                  sptassvgmed_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(sptassvgmed, FiscalYear),
                 fill = list(
                  sptassvgmed_count = 0,
                  sptassvgmed_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  sptassvgmed_delta = (sptassvgmed_rate - lag(sptassvgmed_rate))/lag(sptassvgmed_rate),
  sptassvgmed_delta = ifelse(FiscalYear == 2014, NA, sptassvgmed_delta),
  sptassvgmed_deltap = (sptassvgmed_rate - lag(sptassvgmed_rate,9))/lag(sptassvgmed_rate,9)
 ) %>%
 ungroup()

#### Severe perineal trauma with Operative Vaginal Birth without mediolateral episiotomy ----

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
        DMEPISIO,
        CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(FiscalYear, trdfrth, assvg, med, CHNuid) %>%
 mutate(
  sptassvgnmed_count = n()
 ) %>%
 group_by(FiscalYear, assvg, CHNuid) %>%
 mutate(
  total_year = n(),
  sptassvgnmed_rate = sptassvgnmed_count/total_year
 ) %>%
 ungroup() %>%
 filter(sptassvgnmed %in% 1, !is.na(CHNuid)) %>%
 select(FiscalYear,CHNuid,sptassvgnmed,sptassvgnmed_count,sptassvgnmed_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(sptassvgnmed, CHNuid),
                 fill = list(
                  sptassvgnmed_count = 0,
                  sptassvgnmed_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(sptassvgnmed, FiscalYear),
                 fill = list(
                  sptassvgnmed_count = 0,
                  sptassvgnmed_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  sptassvgnmed_delta = (sptassvgnmed_rate - lag(sptassvgnmed_rate))/lag(sptassvgnmed_rate),
  sptassvgnmed_delta = ifelse(FiscalYear == 2014, NA, sptassvgnmed_delta),
  sptassvgnmed_deltap = (sptassvgnmed_rate - lag(sptassvgnmed_rate,9))/lag(sptassvgnmed_rate,9)
 ) %>%
 ungroup()

#### Robson group ----

c12 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs1, RobsnGrp, CHNuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs1, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
        !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, rbs1, rbs1_count, rbs1_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(rbs1, CHNuid),
                 fill = list(
                  rbs1_count = 0,
                  rbs1_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(rbs1, FiscalYear),
                 fill = list(
                  rbs1_count = 0,
                  rbs1_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  rbs1_delta = (rbs1_rate - lag(rbs1_rate))/lag(rbs1_rate),
  rbs1_delta = ifelse(FiscalYear == 2014, NA, rbs1_delta),
  rbs1_deltap = (rbs1_rate - lag(rbs1_rate,9))/lag(rbs1_rate,9)
 ) %>%
 ungroup()

c13 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs21, RobsnGrp, CHNuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs21, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
        !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, rbs21, rbs21_count, rbs21_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(rbs21, CHNuid),
                 fill = list(
                  rbs21_count = 0,
                  rbs21_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(rbs21, FiscalYear),
                 fill = list(
                  rbs21_count = 0,
                  rbs21_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  rbs21_delta = (rbs21_rate - lag(rbs21_rate))/lag(rbs21_rate),
  rbs21_delta = ifelse(FiscalYear == 2014, NA, rbs21_delta),
  rbs21_deltap = (rbs21_rate - lag(rbs21_rate,9))/lag(rbs21_rate,9)
 ) %>%
 ungroup()

c14 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs51, RobsnGrp, CHNuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs51, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
        !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, rbs51, rbs51_count, rbs51_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(rbs51, CHNuid),
                 fill = list(
                  rbs51_count = 0,
                  rbs51_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(rbs51, FiscalYear),
                 fill = list(
                  rbs51_count = 0,
                  rbs51_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  rbs51_delta = (rbs51_rate - lag(rbs51_rate))/lag(rbs51_rate),
  rbs51_delta = ifelse(FiscalYear == 2014, NA, rbs51_delta),
  rbs51_deltap = (rbs51_rate - lag(rbs51_rate,9))/lag(rbs51_rate,9)
 ) %>%
 ungroup()

#### Postpartum hemorrhage treated with a blood transfusion ----

c15 <- dta %>%
 select(FiscalYear, Any_Blood_Product, Postpartum_Haemorrhage, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  pphbl = case_when(
   Any_Blood_Product > 0 & Postpartum_Haemorrhage > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, Any_Blood_Product, Postpartum_Haemorrhage, CHNuid) %>%
 mutate(
  pphbl_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid) %>%
 mutate(
  total_year = n(),
  pphbl_rate = pphbl_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Blood_Product %in% 1, Postpartum_Haemorrhage %in% 1,
        !is.na(CHNuid)) %>%
 select(FiscalYear,CHNuid,pphbl, pphbl_count, pphbl_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(pphbl, CHNuid),
                 fill = list(
                  pphbl_count = 0,
                  pphbl_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(pphbl, FiscalYear),
                 fill = list(
                  pphbl_count = 0,
                  pphbl_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  pphbl_delta = (pphbl_rate - lag(pphbl_rate))/lag(pphbl_rate),
  pphbl_delta = ifelse(FiscalYear == 2014, NA, pphbl_delta),
  pphbl_deltap = (pphbl_rate - lag(pphbl_rate,9))/lag(pphbl_rate,9)
 ) %>%
 ungroup()

#### Postpartum hemorrhage resulting in procedural intervention ----

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
        MODEDEL,
        CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(FiscalYear, pphpi, Postpartum_Haemorrhage, CHNuid) %>%
 mutate(
  pphpi_count = n()
 ) %>%
 group_by(FiscalYear, Postpartum_Haemorrhage, CHNuid) %>%
 mutate(
  total_year = n(),
  pphpi_rate = pphpi_count/total_year
 ) %>%
 ungroup() %>%
 filter(pphpi %in% 1, Postpartum_Haemorrhage %in% 1,
        !is.na(CHNuid)) %>%
 select(FiscalYear,CHNuid,pphpi, pphpi_count, pphpi_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(pphpi, CHNuid),
                 fill = list(
                  pphpi_count = 0,
                  pphpi_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(pphpi, FiscalYear),
                 fill = list(
                  pphpi_count = 0,
                  pphpi_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  pphpi_delta = (pphpi_rate - lag(pphpi_rate))/lag(pphpi_rate),
  pphpi_delta = ifelse(FiscalYear == 2014, NA, pphpi_delta),
  pphpi_deltap = (pphpi_rate - lag(pphpi_rate,9))/lag(pphpi_rate,9)
 ) %>%
 ungroup()

#### Postpartum readmission ----

c17 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, ppreadm, CHNuid) %>%
 distinct() %>%
 group_by(FiscalYear, ppreadm, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  ppreadm_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid) %>%
 mutate(
  total_year = n(),
  ppreadm_rate = ppreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(ppreadm %in% 1,
        !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, ppreadm, ppreadm_count, ppreadm_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(ppreadm, CHNuid),
                 fill = list(
                  ppreadm_count = 0,
                  ppreadm_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(ppreadm, FiscalYear),
                 fill = list(
                  ppreadm_count = 0,
                  ppreadm_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  ppreadm_delta = (ppreadm_rate - lag(ppreadm_rate))/lag(ppreadm_rate),
  ppreadm_delta = ifelse(FiscalYear == 2014, NA, ppreadm_delta),
  ppreadm_deltap = (ppreadm_rate - lag(ppreadm_rate,9))/lag(ppreadm_rate,9)
 ) %>%
 ungroup()

#### Postpartum patients who received medical anticoagulation prophylaxis when indicated ----

#c18

#### Skin to skin ----

c19 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknskn, CHNuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, sknskn, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  sknskn_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid, lvb) %>%
 mutate(
  total_year = n(),
  sknskn_rate = sknskn_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskn %in% 1,
        !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, sknskn, sknskn_count, sknskn_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(sknskn, CHNuid),
                 fill = list(
                  sknskn_count = 0,
                  sknskn_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(sknskn, FiscalYear),
                 fill = list(
                  sknskn_count = 0,
                  sknskn_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  sknskn_delta = (sknskn_rate - lag(sknskn_rate))/lag(sknskn_rate),
  sknskn_delta = ifelse(FiscalYear == 2014, NA, sknskn_delta),
  sknskn_deltap = (sknskn_rate - lag(sknskn_rate,9))/lag(sknskn_rate,9)
 ) %>%
 ungroup()

c20 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknsknvg, CHNuid, lvbvg) %>%
 distinct() %>%
 group_by(FiscalYear, sknsknvg, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  sknsknvg_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid, lvbvg) %>%
 mutate(
  total_year = n(),
  sknsknvg_rate = sknsknvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknsknvg %in% 1,
        !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, sknsknvg, sknsknvg_count, sknsknvg_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(sknsknvg, CHNuid),
                 fill = list(
                  sknsknvg_count = 0,
                  sknsknvg_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(sknsknvg, FiscalYear),
                 fill = list(
                  sknsknvg_count = 0,
                  sknsknvg_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  sknsknvg_delta = (sknsknvg_rate - lag(sknsknvg_rate))/lag(sknsknvg_rate),
  sknsknvg_delta = ifelse(FiscalYear == 2014, NA, sknsknvg_delta),
  sknsknvg_deltap = (sknsknvg_rate - lag(sknsknvg_rate,9))/lag(sknsknvg_rate,9)
 ) %>%
 ungroup()

c21 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknskncs, CHNuid, lvbcs) %>%
 distinct() %>%
 group_by(FiscalYear, sknskncs, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  sknskncs_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid, lvbcs) %>%
 mutate(
  total_year = n(),
  sknskncs_rate = sknskncs_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskncs %in% 1,
        !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, sknskncs, sknskncs_count, sknskncs_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(sknskncs, CHNuid),
                 fill = list(
                  sknskncs_count = 0,
                  sknskncs_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(sknskncs, FiscalYear),
                 fill = list(
                  sknskncs_count = 0,
                  sknskncs_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  sknskncs_delta = (sknskncs_rate - lag(sknskncs_rate))/lag(sknskncs_rate),
  sknskncs_delta = ifelse(FiscalYear == 2014, NA, sknskncs_delta),
  sknskncs_deltap = (sknskncs_rate - lag(sknskncs_rate,9))/lag(sknskncs_rate,9)
 ) %>%
 ungroup()

#### Postpartum blood products amongst those who received antenatal iron therapy ----

c22 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
  pphiv = case_when(
   Any_Blood_Product > 0 & R003_02100 > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, Any_Blood_Product, R003_02100, CHNuid) %>%
 mutate(
  pphiv_count = n()
 ) %>%
 group_by(FiscalYear, Any_Blood_Product, CHNuid) %>%
 mutate(
  total_year = n(),
  pphiv_rate = pphiv_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Blood_Product %in% 1, R003_02100 %in% 1,
        !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, pphiv, pphiv_count, pphiv_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(pphiv, CHNuid),
                 fill = list(
                  pphiv_count = 0,
                  pphiv_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(pphiv, FiscalYear),
                 fill = list(
                  pphiv_count = 0,
                  pphiv_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  pphiv_delta = (pphiv_rate - lag(pphiv_rate))/lag(pphiv_rate),
  pphiv_delta = ifelse(FiscalYear == 2014, NA, pphiv_delta),
  pphiv_deltap = (pphiv_rate - lag(pphiv_rate,9))/lag(pphiv_rate,9)
 ) %>%
 ungroup()

#### Neonatal readmission ----

c23 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, neoreadm, CHNuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, neoreadm, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  neoreadm_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid, lvb) %>%
 mutate(
  total_year = n(),
  neoreadm_rate = neoreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(neoreadm %in% 1,
        !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, neoreadm, neoreadm_count, neoreadm_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(neoreadm, CHNuid),
                 fill = list(
                  neoreadm_count = 0,
                  neoreadm_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(neoreadm, FiscalYear),
                 fill = list(
                  neoreadm_count = 0,
                  neoreadm_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  neoreadm_delta = (neoreadm_rate - lag(neoreadm_rate))/lag(neoreadm_rate),
  neoreadm_delta = ifelse(FiscalYear == 2014, NA, neoreadm_delta),
  neoreadm_deltap = (neoreadm_rate - lag(neoreadm_rate,9))/lag(neoreadm_rate,9)
 ) %>%
 ungroup()

#### Preterm infants who received complete course of steroids > 24h and < 7 days prior to birth ----

c24 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(FiscalYear, pretster, CHNuid) %>%
 mutate(
  pretster_count = n()
 ) %>%
 group_by(FiscalYear, den, CHNuid) %>%
 mutate(
  total_year = n(),
  pretster_rate = pretster_count/total_year
 ) %>%
 ungroup() %>%
 filter(pretster %in% 1, !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, pretster, pretster_count,pretster_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(pretster, CHNuid),
                 fill = list(
                  pretster_count = 0,
                  pretster_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(pretster, FiscalYear),
                 fill = list(
                  pretster_count = 0,
                  pretster_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  pretster_delta = (pretster_rate - lag(pretster_rate))/lag(pretster_rate),
  pretster_delta = ifelse(FiscalYear == 2014, NA, pretster_delta),
  pretster_deltap = (pretster_rate - lag(pretster_rate,9))/lag(pretster_rate,9)
 ) %>%
 ungroup()

#### Preterm babies born in facility without NICU ----

c25 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(FiscalYear, pretnnicu, CHNuid) %>%
 mutate(
  pretnnicu_count = n()
 ) %>%
 group_by(FiscalYear, den, CHNuid) %>%
 mutate(
  total_year = n(),
  pretnnicu_rate = pretnnicu_count/total_year
 ) %>%
 ungroup() %>%
 filter(pretnnicu %in% 1, !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, pretnnicu, pretnnicu_count,pretnnicu_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(pretnnicu, CHNuid),
                 fill = list(
                  pretnnicu_count = 0,
                  pretnnicu_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(pretnnicu, FiscalYear),
                 fill = list(
                  pretnnicu_count = 0,
                  pretnnicu_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  pretnnicu_delta = (pretnnicu_rate - lag(pretnnicu_rate))/lag(pretnnicu_rate),
  pretnnicu_delta = ifelse(FiscalYear == 2014, NA, pretnnicu_delta),
  pretnnicu_deltap = (pretnnicu_rate - lag(pretnnicu_rate,9))/lag(pretnnicu_rate,9)
 ) %>%
 ungroup()

#### Newborn respiratory distress associated with low-risk repeat cesarean at term gestation (≥ 37 weeks < 39 weeks) ----

c26 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(FiscalYear, newlwcs, CHNuid) %>%
 mutate(
  newlwcs_count = n()
 ) %>%
 group_by(FiscalYear, den, CHNuid) %>%
 mutate(
  total_year = n(),
  newlwcs_rate = newlwcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(newlwcs %in% 1) %>%
 select(FiscalYear, CHNuid, newlwcs, newlwcs_count,newlwcs_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(newlwcs, CHNuid),
                 fill = list(
                  newlwcs_count = 0,
                  newlwcs_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(newlwcs, FiscalYear),
                 fill = list(
                  newlwcs_count = 0,
                  newlwcs_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  newlwcs_delta = (newlwcs_rate - lag(newlwcs_rate))/lag(newlwcs_rate),
  newlwcs_delta = ifelse(FiscalYear == 2014, NA, newlwcs_delta),
  newlwcs_deltap = (newlwcs_rate - lag(newlwcs_rate,9))/lag(newlwcs_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping  ----

c27 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
  delcord = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (DEL_CORD_CLAMP %in% 3) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, delcord, CHNuid) %>%
 mutate(
  delcord_count = n()
 ) %>%
 group_by(FiscalYear, lvb, CHNuid) %>%
 mutate(
  total_year = n(),
  delcord_rate = delcord_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcord %in% 1, lvb %in% 1,
        !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, delcord, delcord_count,delcord_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcord, CHNuid),
                 fill = list(
                  delcord_count = 0,
                  delcord_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(delcord, FiscalYear),
                 fill = list(
                  delcord_count = 0,
                  delcord_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  delcord_delta = (delcord_rate - lag(delcord_rate))/lag(delcord_rate),
  delcord_delta = ifelse(FiscalYear == 2014, NA, delcord_delta),
  delcord_deltap = (delcord_rate - lag(delcord_rate,9))/lag(delcord_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns ----

c28 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(FiscalYear, delcordterm, CHNuid) %>%
 mutate(
  delcordterm_count = n()
 ) %>%
 group_by(FiscalYear, den, CHNuid) %>%
 mutate(
  total_year = n(),
  delcordterm_rate = delcordterm_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordterm %in% 1, !is.na(CHNuid)) %>%
 select(FiscalYear,CHNuid,delcordterm,delcordterm_count,delcordterm_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordterm, CHNuid),
                 fill = list(
                  delcordterm_count = 0,
                  delcordterm_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(delcordterm, FiscalYear),
                 fill = list(
                  delcordterm_count = 0,
                  delcordterm_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  delcordterm_delta = (delcordterm_rate - lag(delcordterm_rate))/lag(delcordterm_rate),
  delcordterm_delta = ifelse(FiscalYear == 2014, NA, delcordterm_delta),
  delcordterm_deltap = (delcordterm_rate - lag(delcordterm_rate,9))/lag(delcordterm_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns following spontaneous vaginal birth ----

c29 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(FiscalYear, delcordtermvg, CHNuid) %>%
 mutate(
  delcordtermvg_count = n()
 ) %>%
 group_by(FiscalYear, den, CHNuid) %>%
 mutate(
  total_year = n(),
  delcordtermvg_rate = delcordtermvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordtermvg %in% 1, !is.na(CHNuid)) %>%
 select(FiscalYear,CHNuid, delcordtermvg, delcordtermvg_count,delcordtermvg_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordtermvg, CHNuid),
                 fill = list(
                  delcordtermvg_count = 0,
                  delcordtermvg_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(delcordtermvg, FiscalYear),
                 fill = list(
                  delcordtermvg_count = 0,
                  delcordtermvg_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  delcordtermvg_delta = (delcordtermvg_rate - lag(delcordtermvg_rate))/lag(delcordtermvg_rate),
  delcordtermvg_delta = ifelse(FiscalYear == 2014, NA, delcordtermvg_delta),
  delcordtermvg_deltap = (delcordtermvg_rate - lag(delcordtermvg_rate,9))/lag(delcordtermvg_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns following caesarean birth ----

c30 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(FiscalYear, delcordtermcs, CHNuid) %>%
 mutate(
  delcordtermcs_count = n()
 ) %>%
 group_by(FiscalYear, den, CHNuid) %>%
 mutate(
  total_year = n(),
  delcordtermcs_rate = delcordtermcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordtermcs %in% 1, !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, delcordtermcs, delcordtermcs_count,delcordtermcs_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordtermcs, CHNuid),
                 fill = list(
                  delcordtermcs_count = 0,
                  delcordtermcs_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(delcordtermcs, FiscalYear),
                 fill = list(
                  delcordtermcs_count = 0,
                  delcordtermcs_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  delcordtermcs_delta = (delcordtermcs_rate - lag(delcordtermcs_rate))/lag(delcordtermcs_rate),
  delcordtermcs_delta = ifelse(FiscalYear == 2014, NA, delcordtermcs_delta),
  delcordtermcs_deltap = (delcordtermcs_rate - lag(delcordtermcs_rate,9))/lag(delcordtermcs_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns ----

c31 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(FiscalYear, delcordpreterm, CHNuid) %>%
 mutate(
  delcordpreterm_count = n()
 ) %>%
 group_by(FiscalYear, den, CHNuid) %>%
 mutate(
  total_year = n(),
  delcordpreterm_rate = delcordpreterm_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpreterm %in% 1, !is.na(CHNuid)) %>%
 select(FiscalYear,CHNuid, delcordpreterm, delcordpreterm_count,delcordpreterm_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordpreterm, CHNuid),
                 fill = list(
                  delcordpreterm_count = 0,
                  delcordpreterm_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(delcordpreterm, FiscalYear),
                 fill = list(
                  delcordpreterm_count = 0,
                  delcordpreterm_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  delcordpreterm_delta = (delcordpreterm_rate - lag(delcordpreterm_rate))/lag(delcordpreterm_rate),
  delcordpreterm_delta = ifelse(FiscalYear == 2014, NA, delcordpreterm_delta),
  delcordpreterm_deltap = (delcordpreterm_rate - lag(delcordpreterm_rate,9))/lag(delcordpreterm_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns following spontaneous vaginal birth ----

c32 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(FiscalYear, delcordpretermvg, CHNuid) %>%
 mutate(
  delcordpretermvg_count = n()
 ) %>%
 group_by(FiscalYear, den, CHNuid) %>%
 mutate(
  total_year = n(),
  delcordpretermvg_rate = delcordpretermvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpretermvg %in% 1, !is.na(CHNuid)) %>%
 select(FiscalYear,CHNuid, delcordpretermvg, delcordpretermvg_count,delcordpretermvg_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordpretermvg, CHNuid),
                 fill = list(
                  delcordpretermvg_count = 0,
                  delcordpretermvg_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(delcordpretermvg, FiscalYear),
                 fill = list(
                  delcordpretermvg_count = 0,
                  delcordpretermvg_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  delcordpretermvg_delta = (delcordpretermvg_rate - lag(delcordpretermvg_rate))/lag(delcordpretermvg_rate),
  delcordpretermvg_delta = ifelse(FiscalYear == 2014, NA, delcordpretermvg_delta),
  delcordpretermvg_deltap = (delcordpretermvg_rate - lag(delcordpretermvg_rate,9))/lag(delcordpretermvg_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns following caesarean birth ----

c33 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
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
 group_by(FiscalYear, delcordpretermcs, CHNuid) %>%
 mutate(
  delcordpretermcs_count = n()
 ) %>%
 group_by(FiscalYear, den, CHNuid) %>%
 mutate(
  total_year = n(),
  delcordpretermcs_rate = delcordpretermcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpretermcs %in% 1, !is.na(CHNuid)) %>%
 select(FiscalYear,CHNuid, delcordpretermcs, delcordpretermcs_count,delcordpretermcs_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordpretermcs, CHNuid),
                 fill = list(
                  delcordpretermcs_count = 0,
                  delcordpretermcs_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(delcordpretermcs, FiscalYear),
                 fill = list(
                  delcordpretermcs_count = 0,
                  delcordpretermcs_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  delcordpretermcs_delta = (delcordpretermcs_rate - lag(delcordpretermcs_rate))/lag(delcordpretermcs_rate),
  delcordpretermcs_delta = ifelse(FiscalYear == 2014, NA, delcordpretermcs_delta),
  delcordpretermcs_deltap = (delcordpretermcs_rate - lag(delcordpretermcs_rate,9))/lag(delcordpretermcs_rate,9)
 ) %>%
 ungroup()

#### Milk feeding ----

c34 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, excbrst, CHNuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, excbrst, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  excbrst_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid, lvb) %>%
 mutate(
  total_year = n(),
  excbrst_rate = excbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(excbrst %in% 1,
        !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, excbrst, excbrst_count, excbrst_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(excbrst, CHNuid),
                 fill = list(
                  excbrst_count = 0,
                  excbrst_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(excbrst, FiscalYear),
                 fill = list(
                  excbrst_count = 0,
                  excbrst_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  excbrst_delta = (excbrst_rate - lag(excbrst_rate))/lag(excbrst_rate),
  excbrst_delta = ifelse(FiscalYear == 2014, NA, excbrst_delta),
  excbrst_deltap = (excbrst_rate - lag(excbrst_rate,9))/lag(excbrst_rate,9)
 ) %>%
 ungroup()

c35 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, nexcbrst, CHNuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, nexcbrst, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  nexcbrst_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid, lvb) %>%
 mutate(
  total_year = n(),
  nexcbrst_rate = nexcbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nexcbrst %in% 1,
        !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, nexcbrst, nexcbrst_count, nexcbrst_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(nexcbrst, CHNuid),
                 fill = list(
                  nexcbrst_count = 0,
                  nexcbrst_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(nexcbrst, FiscalYear),
                 fill = list(
                  nexcbrst_count = 0,
                  nexcbrst_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  nexcbrst_delta = (nexcbrst_rate - lag(nexcbrst_rate))/lag(nexcbrst_rate),
  nexcbrst_delta = ifelse(FiscalYear == 2014, NA, nexcbrst_delta),
  nexcbrst_deltap = (nexcbrst_rate - lag(nexcbrst_rate,9))/lag(nexcbrst_rate,9)
 ) %>%
 ungroup()

c36 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, nbrst, CHNuid, lvbint) %>%
 distinct() %>%
 group_by(FiscalYear, nbrst, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  nbrst_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid, lvbint) %>%
 mutate(
  total_year = n(),
  nbrst_rate = nbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nbrst %in% 1,
        !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, nbrst, nbrst_count, nbrst_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(nbrst, CHNuid),
                 fill = list(
                  nbrst_count = 0,
                  nbrst_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(nbrst, FiscalYear),
                 fill = list(
                  nbrst_count = 0,
                  nbrst_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  nbrst_delta = (nbrst_rate - lag(nbrst_rate))/lag(nbrst_rate),
  nbrst_delta = ifelse(FiscalYear == 2014, NA, nbrst_delta),
  nbrst_deltap = (nbrst_rate - lag(nbrst_rate,9))/lag(nbrst_rate,9)
 ) %>%
 ungroup()

#### The breast/chest feeding initiation rate ----

c37 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, brstinit, CHNuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, brstinit, CHNuid) %>%
 mutate(
  CHNuid = as.character(CHNuid),
  brstinit_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid, lvb) %>%
 mutate(
  total_year = n(),
  brstinit_rate = brstinit_count/total_year
 ) %>%
 ungroup() %>%
 filter(brstinit %in% 1,
        !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, brstinit, brstinit_count, brstinit_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(brstinit, CHNuid),
                 fill = list(
                  brstinit_count = 0,
                  brstinit_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(brstinit, FiscalYear),
                 fill = list(
                  brstinit_count = 0,
                  brstinit_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  brstinit_delta = (brstinit_rate - lag(brstinit_rate))/lag(brstinit_rate),
  brstinit_delta = ifelse(FiscalYear == 2014, NA, brstinit_delta),
  brstinit_deltap = (brstinit_rate - lag(brstinit_rate,9))/lag(brstinit_rate,9)
 ) %>%
 ungroup()

#### ICU Admission during Pregnancy or Postpartum ----

c38 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
  icu = case_when(
   JOGC_ICU > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, JOGC_ICU, CHNuid) %>%
 mutate(
  icu_count = n()
 ) %>%
 group_by(FiscalYear, CHNuid) %>%
 mutate(
  total_year = n(),
  icu_rate = icu_count/total_year
 ) %>%
 ungroup() %>%
 filter(JOGC_ICU %in% 1, !is.na(CHNuid)) %>%
 select(FiscalYear, CHNuid, icu, icu_count,icu_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(icu, CHNuid),
                 fill = list(
                  icu_count = 0,
                  icu_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(icu, FiscalYear),
                 fill = list(
                  icu_count = 0,
                  icu_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  icu_delta = (icu_rate - lag(icu_rate))/lag(icu_rate),
  icu_delta = ifelse(FiscalYear == 2014, NA, icu_delta),
  icu_deltap = (icu_rate - lag(icu_rate,9))/lag(icu_rate,9)
 ) %>%
 ungroup()

#### Rate of Severe Morbidity in Pregnancy or Postpartum ----

c39 <- dta %>%
 mutate(
  CHNuid = as.character(CHNuid),
  smm = case_when(
   JOGC_AnySMM > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, JOGC_AnySMM, CHNuid) %>%
 mutate(
  smm_count = n()
 ) %>%
 group_by(FiscalYear, lvb, CHNuid) %>%
 mutate(
  total_year = n(),
  smm_rate = smm_count/total_year
 ) %>%
 ungroup() %>%
 filter(JOGC_AnySMM %in% 1, lvb %in% 1, !is.na(CHNuid)) %>%
 select(FiscalYear,CHNuid, smm, smm_count,smm_rate) %>%
 distinct() %>%
 arrange(CHNuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(smm, CHNuid),
                 fill = list(
                  smm_count = 0,
                  smm_rate = NA
                 )) %>%
 tidyr::complete(CHNuid = unique(as.character(chn_shp$GeoUID)),
                 tidyr::nesting(smm, FiscalYear),
                 fill = list(
                  smm_count = 0,
                  smm_rate = NA
                 )) %>%
 group_by(CHNuid) %>%
 mutate(
  smm_delta = (smm_rate - lag(smm_rate))/lag(smm_rate),
  smm_delta = ifelse(FiscalYear == 2014, NA, smm_delta),
  smm_deltap = (smm_rate - lag(smm_rate,9))/lag(smm_rate,9)
 ) %>%
 ungroup()

fchn_stats <- cbind(
 c1 %>% select(-PreExisting_Hypertension),
 c2 %>% select(-Gestational_Hypertension,-CHNuid,-FiscalYear),
 c3 %>% select(-Any_Hypertension,-CHNuid,-FiscalYear),
 c4 %>% select(-anemia,-CHNuid,-FiscalYear),
 c5 %>% select(-PreExisting_Diabetes,-CHNuid,-FiscalYear),
 c6 %>% select(-GDM,-CHNuid,-FiscalYear),
 c7 %>% select(-Any_Diabetes,-CHNuid,-FiscalYear),
 c8 %>% select(-sptvg,-CHNuid,-FiscalYear),
 c9 %>% select(-med,-CHNuid,-FiscalYear),
 c10 %>% select(-sptassvgmed,-CHNuid,-FiscalYear),
 c11 %>% select(-sptassvgnmed,-CHNuid,-FiscalYear),
 c12 %>% select(-rbs1,-CHNuid,-FiscalYear),
 c13 %>% select(-rbs21,-CHNuid,-FiscalYear),
 c14 %>% select(-rbs51,-CHNuid,-FiscalYear),
 c15 %>% select(-pphbl,-CHNuid,-FiscalYear),
 c16 %>% select(-pphpi,-CHNuid,-FiscalYear),
 c17 %>% select(-ppreadm,-CHNuid,-FiscalYear),
 #c18 %>% select(-brstinit,-CHNuid,-FiscalYear),
 c19 %>% select(-sknskn,-CHNuid,-FiscalYear),
 c20 %>% select(-sknsknvg,-CHNuid,-FiscalYear),
 c21 %>% select(-sknskncs,-CHNuid,-FiscalYear),
 c22 %>% select(-pphiv,-CHNuid,-FiscalYear),
 c23 %>% select(-neoreadm,-CHNuid,-FiscalYear),
 c24 %>% select(-pretster,-CHNuid,-FiscalYear),
 c25 %>% select(-pretnnicu,-CHNuid,-FiscalYear),
 c26 %>% select(-newlwcs,-CHNuid,-FiscalYear),
 c27 %>% select(-delcord,-CHNuid,-FiscalYear),
 c28 %>% select(-delcordterm,-CHNuid,-FiscalYear),
 c29 %>% select(-delcordtermvg,-CHNuid,-FiscalYear),
 c30 %>% select(-delcordtermcs,-CHNuid,-FiscalYear),
 c31 %>% select(-delcordpreterm,-CHNuid,-FiscalYear),
 c32 %>% select(-delcordpretermvg,-CHNuid,-FiscalYear),
 c33 %>% select(-delcordpretermcs,-CHNuid,-FiscalYear),
 c34 %>% select(-excbrst,-CHNuid,-FiscalYear),
 c35 %>% select(-nexcbrst,-CHNuid,-FiscalYear),
 c36 %>% select(-nbrst,-CHNuid,-FiscalYear),
 c37 %>% select(-brstinit,-CHNuid,-FiscalYear),
 c38 %>% select(-icu,-CHNuid,-FiscalYear),
 c39 %>% select(-smm,-CHNuid,-FiscalYear)
)

### Health authority zone (HR) ----
#### Hypertension ----

c1 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, PreExisting_Hypertension, HRuid) %>%
 distinct() %>%
 group_by(FiscalYear, PreExisting_Hypertension, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  prehyp_count = n()
 ) %>%
 group_by(FiscalYear, HRuid) %>%
 mutate(
  total_year = n(),
  prehyp_rate = prehyp_count/total_year
 ) %>%
 filter(PreExisting_Hypertension %in% 1,
        !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, PreExisting_Hypertension, prehyp_count, prehyp_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(PreExisting_Hypertension, HRuid),
                 fill = list(
                  prehyp_count = 0,
                  prehyp_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(PreExisting_Hypertension, FiscalYear),
                 fill = list(
                  prehyp_count = 0,
                  prehyp_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  prehyp_delta = (prehyp_rate - lag(prehyp_rate))/lag(prehyp_rate),
  prehyp_delta = ifelse(FiscalYear == 2014, NA, prehyp_delta),
  prehyp_deltap = (prehyp_rate - lag(prehyp_rate,9))/lag(prehyp_rate,9)
 ) %>%
 ungroup()

c2 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Gestational_Hypertension, HRuid) %>%
 distinct() %>%
 group_by(FiscalYear, Gestational_Hypertension, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  gesthyp_count = n()
 ) %>%
 group_by(FiscalYear, HRuid) %>%
 mutate(
  total_year = n(),
  gesthyp_rate = gesthyp_count/total_year
 ) %>%
 filter(Gestational_Hypertension %in% 1,
        !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, Gestational_Hypertension, gesthyp_count, gesthyp_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(Gestational_Hypertension, HRuid),
                 fill = list(
                  gesthyp_count = 0,
                  gesthyp_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(Gestational_Hypertension, FiscalYear),
                 fill = list(
                  gesthyp_count = 0,
                  gesthyp_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  gesthyp_delta = (gesthyp_rate - lag(gesthyp_rate))/lag(gesthyp_rate),
  gesthyp_delta = ifelse(FiscalYear == 2014, NA, gesthyp_delta),
  gesthyp_deltap = (gesthyp_rate - lag(gesthyp_rate,9))/lag(gesthyp_rate,9)
 ) %>%
 ungroup()

c3 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Any_Hypertension, HRuid) %>%
 distinct() %>%
 group_by(FiscalYear, Any_Hypertension, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  hyp_count = n()
 ) %>%
 group_by(FiscalYear, HRuid) %>%
 mutate(
  total_year = n(),
  hyp_rate = hyp_count/total_year
 ) %>%
 filter(Any_Hypertension %in% 1,
        !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, Any_Hypertension, hyp_count, hyp_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(Any_Hypertension, HRuid),
                 fill = list(
                  hyp_count = 0,
                  hyp_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(Any_Hypertension, FiscalYear),
                 fill = list(
                  hyp_count = 0,
                  hyp_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  hyp_delta = (hyp_rate - lag(hyp_rate))/lag(hyp_rate),
  hyp_delta = ifelse(FiscalYear == 2014, NA, hyp_delta),
  hyp_deltap = (hyp_rate - lag(hyp_rate,9))/lag(hyp_rate,9)
 ) %>%
 ungroup()

#### Anaemia ----

c4 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, HRuid, MANEM, R014_01500, MO990, MD50_D53, MD55_D59, MD60_D64) %>%
 mutate(anemia = case_when(
  MANEM > 0 | R014_01500 > 0 | MO990 > 0 | MD50_D53 > 0 |
   MD55_D59 > 0 | MD60_D64 > 0 ~ 1,
  TRUE ~ NA_integer_
 )) %>%
 distinct() %>%
 group_by(FiscalYear, anemia, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  anemia_count = n()
 ) %>%
 group_by(FiscalYear, HRuid) %>%
 mutate(
  total_year = n(),
  anemia_rate = anemia_count/total_year
 ) %>%
 filter(anemia %in% 1,
        !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, anemia, anemia_count, anemia_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(anemia, HRuid),
                 fill = list(
                  anemia_count = 0,
                  anemia_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(anemia, FiscalYear),
                 fill = list(
                  anemia_count = 0,
                  anemia_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  anemia_delta = (anemia_rate - lag(anemia_rate))/lag(anemia_rate),
  anemia_delta = ifelse(FiscalYear == 2014, NA, anemia_delta),
  anemia_deltap = (anemia_rate - lag(anemia_rate,9))/lag(anemia_rate,9)
 ) %>%
 ungroup()

#### Diabetes ----

c5 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, PreExisting_Diabetes, HRuid) %>%
 distinct() %>%
 group_by(FiscalYear, PreExisting_Diabetes, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  prediab_count = n()
 ) %>%
 group_by(FiscalYear, HRuid) %>%
 mutate(
  total_year = n(),
  prediab_rate = prediab_count/total_year
 ) %>%
 filter(PreExisting_Diabetes %in% 1,
        !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, PreExisting_Diabetes, prediab_count, prediab_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(PreExisting_Diabetes, HRuid),
                 fill = list(
                  prediab_count = 0,
                  prediab_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(PreExisting_Diabetes, FiscalYear),
                 fill = list(
                  prediab_count = 0,
                  prediab_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  prediab_delta = (prediab_rate - lag(prediab_rate))/lag(prediab_rate),
  prediab_delta = ifelse(FiscalYear == 2014, NA, prediab_delta),
  prediab_deltap = (prediab_rate - lag(prediab_rate,9))/lag(prediab_rate,9)
 ) %>%
 ungroup()

c6 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, GDM, HRuid) %>%
 distinct() %>%
 group_by(FiscalYear, GDM, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  gestdiab_count = n()
 ) %>%
 group_by(FiscalYear, HRuid) %>%
 mutate(
  total_year = n(),
  gestdiab_rate = gestdiab_count/total_year
 ) %>%
 filter(GDM %in% 1,
        !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, GDM, gestdiab_count, gestdiab_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(GDM, HRuid),
                 fill = list(
                  gestdiab_count = 0,
                  gestdiab_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(GDM, FiscalYear),
                 fill = list(
                  gestdiab_count = 0,
                  gestdiab_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  gestdiab_delta = (gestdiab_rate - lag(gestdiab_rate))/lag(gestdiab_rate),
  gestdiab_delta = ifelse(FiscalYear == 2014, NA, gestdiab_delta),
  gestdiab_deltap = (gestdiab_rate - lag(gestdiab_rate,9))/lag(gestdiab_rate,9)
 ) %>%
 ungroup()

c7 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, Any_Diabetes, HRuid) %>%
 distinct() %>%
 group_by(FiscalYear, Any_Diabetes, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  diab_count = n()
 ) %>%
 group_by(FiscalYear, HRuid) %>%
 mutate(
  total_year = n(),
  diab_rate = diab_count/total_year
 ) %>%
 filter(Any_Diabetes %in% 1,
        !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, Any_Diabetes, diab_count, diab_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(Any_Diabetes, HRuid),
                 fill = list(
                  diab_count = 0,
                  diab_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(Any_Diabetes, FiscalYear),
                 fill = list(
                  diab_count = 0,
                  diab_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  diab_delta = (diab_rate - lag(diab_rate))/lag(diab_rate),
  diab_delta = ifelse(FiscalYear == 2014, NA, diab_delta),
  diab_deltap = (diab_rate - lag(diab_rate,9))/lag(diab_rate,9)
 ) %>%
 ungroup()

#### Severe Perineal Trauma with Spontaneous Vaginal Birth ----

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
        DMMETHOD,
        HRuid) %>%
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
 group_by(FiscalYear, trdfrth, sptvg, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  spt_count = n()
 ) %>%
 group_by(FiscalYear, sptvg, HRuid) %>%
 mutate(
  total_year = n(),
  spt_rate = spt_count/total_year
 ) %>%
 ungroup() %>%
 filter(trdfrth %in% 1, sptvg %in% 1, !is.na(HRuid)) %>%
 select(FiscalYear,HRuid,sptvg,spt_count,spt_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(sptvg, HRuid),
                 fill = list(
                  spt_count = 0,
                  spt_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(sptvg, FiscalYear),
                 fill = list(
                  spt_count = 0,
                  spt_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  spt_delta = (spt_rate - lag(spt_rate))/lag(spt_rate),
  spt_delta = ifelse(FiscalYear == 2014, NA, spt_delta),
  spt_deltap = (spt_rate - lag(spt_rate,9))/lag(spt_rate,9)
 ) %>%
 ungroup()

#### Mediolateral Episiotomy with Operative Vaginal Birth ----
#### ND = Not Done
#### MD = Midline
#### ML = Mediolateral

c9 <- dta %>%
 select(FiscalYear,
        DMMETHOD,
        DMEPISIO,
        HRuid) %>%
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
 group_by(FiscalYear, med, assvg, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  med_count = n()
 ) %>%
 group_by(FiscalYear, assvg, HRuid) %>%
 mutate(
  total_year = n(),
  med_rate = med_count/total_year
 ) %>%
 ungroup() %>%
 filter(med %in% 1, assvg %in% 1, !is.na(HRuid)) %>%
 select(FiscalYear,HRuid,med,med_count,med_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(med, HRuid),
                 fill = list(
                  med_count = 0,
                  med_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(med, FiscalYear),
                 fill = list(
                  med_count = 0,
                  med_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  med_delta = (med_rate - lag(med_rate))/lag(med_rate),
  med_delta = ifelse(FiscalYear == 2014, NA, med_delta),
  med_deltap = (med_rate - lag(med_rate,9))/lag(med_rate,9)
 ) %>%
 ungroup()

#### Severe perineal trauma with Operative Vaginal Birth and mediolateral episiotomy ----

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
        DMEPISIO,
        HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(FiscalYear, trdfrth, assvg, med, HRuid) %>%
 mutate(
  sptassvgmed_count = n()
 ) %>%
 group_by(FiscalYear, assvg, HRuid) %>%
 mutate(
  total_year = n(),
  sptassvgmed_rate = sptassvgmed_count/total_year
 ) %>%
 ungroup() %>%
 filter(sptassvgmed %in% 1, !is.na(HRuid)) %>%
 select(FiscalYear,HRuid,sptassvgmed,sptassvgmed_count,sptassvgmed_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(sptassvgmed, HRuid),
                 fill = list(
                  sptassvgmed_count = 0,
                  sptassvgmed_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(sptassvgmed, FiscalYear),
                 fill = list(
                  sptassvgmed_count = 0,
                  sptassvgmed_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  sptassvgmed_delta = (sptassvgmed_rate - lag(sptassvgmed_rate))/lag(sptassvgmed_rate),
  sptassvgmed_delta = ifelse(FiscalYear == 2014, NA, sptassvgmed_delta),
  sptassvgmed_deltap = (sptassvgmed_rate - lag(sptassvgmed_rate,9))/lag(sptassvgmed_rate,9)
 ) %>%
 ungroup()

#### Severe perineal trauma with Operative Vaginal Birth without mediolateral episiotomy ----

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
        DMEPISIO,
        HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(FiscalYear, trdfrth, assvg, med, HRuid) %>%
 mutate(
  sptassvgnmed_count = n()
 ) %>%
 group_by(FiscalYear, assvg, HRuid) %>%
 mutate(
  total_year = n(),
  sptassvgnmed_rate = sptassvgnmed_count/total_year
 ) %>%
 ungroup() %>%
 filter(sptassvgnmed %in% 1, !is.na(HRuid)) %>%
 select(FiscalYear,HRuid,sptassvgnmed,sptassvgnmed_count,sptassvgnmed_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(sptassvgnmed, HRuid),
                 fill = list(
                  sptassvgnmed_count = 0,
                  sptassvgnmed_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(sptassvgnmed, FiscalYear),
                 fill = list(
                  sptassvgnmed_count = 0,
                  sptassvgnmed_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  sptassvgnmed_delta = (sptassvgnmed_rate - lag(sptassvgnmed_rate))/lag(sptassvgnmed_rate),
  sptassvgnmed_delta = ifelse(FiscalYear == 2014, NA, sptassvgnmed_delta),
  sptassvgnmed_deltap = (sptassvgnmed_rate - lag(sptassvgnmed_rate,9))/lag(sptassvgnmed_rate,9)
 ) %>%
 ungroup()

#### Robson group ----

c12 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs1, RobsnGrp, HRuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs1, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
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
        !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, rbs1, rbs1_count, rbs1_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(rbs1, HRuid),
                 fill = list(
                  rbs1_count = 0,
                  rbs1_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(rbs1, FiscalYear),
                 fill = list(
                  rbs1_count = 0,
                  rbs1_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  rbs1_delta = (rbs1_rate - lag(rbs1_rate))/lag(rbs1_rate),
  rbs1_delta = ifelse(FiscalYear == 2014, NA, rbs1_delta),
  rbs1_deltap = (rbs1_rate - lag(rbs1_rate,9))/lag(rbs1_rate,9)
 ) %>%
 ungroup()

c13 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs21, RobsnGrp, HRuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs21, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
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
        !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, rbs21, rbs21_count, rbs21_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(rbs21, HRuid),
                 fill = list(
                  rbs21_count = 0,
                  rbs21_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(rbs21, FiscalYear),
                 fill = list(
                  rbs21_count = 0,
                  rbs21_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  rbs21_delta = (rbs21_rate - lag(rbs21_rate))/lag(rbs21_rate),
  rbs21_delta = ifelse(FiscalYear == 2014, NA, rbs21_delta),
  rbs21_deltap = (rbs21_rate - lag(rbs21_rate,9))/lag(rbs21_rate,9)
 ) %>%
 ungroup()

c14 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, rbs51, RobsnGrp, HRuid) %>%
 distinct() %>%
 group_by(FiscalYear, rbs51, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
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
        !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, rbs51, rbs51_count, rbs51_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(rbs51, HRuid),
                 fill = list(
                  rbs51_count = 0,
                  rbs51_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(rbs51, FiscalYear),
                 fill = list(
                  rbs51_count = 0,
                  rbs51_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  rbs51_delta = (rbs51_rate - lag(rbs51_rate))/lag(rbs51_rate),
  rbs51_delta = ifelse(FiscalYear == 2014, NA, rbs51_delta),
  rbs51_deltap = (rbs51_rate - lag(rbs51_rate,9))/lag(rbs51_rate,9)
 ) %>%
 ungroup()

#### Postpartum hemorrhage treated with a blood transfusion ----

c15 <- dta %>%
 select(FiscalYear, Any_Blood_Product, Postpartum_Haemorrhage, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  pphbl = case_when(
   Any_Blood_Product > 0 & Postpartum_Haemorrhage > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, Any_Blood_Product, Postpartum_Haemorrhage, HRuid) %>%
 mutate(
  pphbl_count = n()
 ) %>%
 group_by(FiscalYear, HRuid) %>%
 mutate(
  total_year = n(),
  pphbl_rate = pphbl_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Blood_Product %in% 1, Postpartum_Haemorrhage %in% 1,
        !is.na(HRuid)) %>%
 select(FiscalYear,HRuid,pphbl, pphbl_count, pphbl_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(pphbl, HRuid),
                 fill = list(
                  pphbl_count = 0,
                  pphbl_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(pphbl, FiscalYear),
                 fill = list(
                  pphbl_count = 0,
                  pphbl_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  pphbl_delta = (pphbl_rate - lag(pphbl_rate))/lag(pphbl_rate),
  pphbl_delta = ifelse(FiscalYear == 2014, NA, pphbl_delta),
  pphbl_deltap = (pphbl_rate - lag(pphbl_rate,9))/lag(pphbl_rate,9)
 ) %>%
 ungroup()

#### Postpartum hemorrhage resulting in procedural intervention ----

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
        MODEDEL,
        HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(FiscalYear, pphpi, Postpartum_Haemorrhage, HRuid) %>%
 mutate(
  pphpi_count = n()
 ) %>%
 group_by(FiscalYear, Postpartum_Haemorrhage, HRuid) %>%
 mutate(
  total_year = n(),
  pphpi_rate = pphpi_count/total_year
 ) %>%
 ungroup() %>%
 filter(pphpi %in% 1, Postpartum_Haemorrhage %in% 1,
        !is.na(HRuid)) %>%
 select(FiscalYear,HRuid,pphpi, pphpi_count, pphpi_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(pphpi, HRuid),
                 fill = list(
                  pphpi_count = 0,
                  pphpi_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(pphpi, FiscalYear),
                 fill = list(
                  pphpi_count = 0,
                  pphpi_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  pphpi_delta = (pphpi_rate - lag(pphpi_rate))/lag(pphpi_rate),
  pphpi_delta = ifelse(FiscalYear == 2014, NA, pphpi_delta),
  pphpi_deltap = (pphpi_rate - lag(pphpi_rate,9))/lag(pphpi_rate,9)
 ) %>%
 ungroup()

#### Postpartum readmission ----

c17 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, ppreadm, HRuid) %>%
 distinct() %>%
 group_by(FiscalYear, ppreadm, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  ppreadm_count = n()
 ) %>%
 group_by(FiscalYear, HRuid) %>%
 mutate(
  total_year = n(),
  ppreadm_rate = ppreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(ppreadm %in% 1,
        !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, ppreadm, ppreadm_count, ppreadm_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(ppreadm, HRuid),
                 fill = list(
                  ppreadm_count = 0,
                  ppreadm_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(ppreadm, FiscalYear),
                 fill = list(
                  ppreadm_count = 0,
                  ppreadm_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  ppreadm_delta = (ppreadm_rate - lag(ppreadm_rate))/lag(ppreadm_rate),
  ppreadm_delta = ifelse(FiscalYear == 2014, NA, ppreadm_delta),
  ppreadm_deltap = (ppreadm_rate - lag(ppreadm_rate,9))/lag(ppreadm_rate,9)
 ) %>%
 ungroup()

#### Postpartum patients who received medical anticoagulation prophylaxis when indicated ----

#c18

#### Skin to skin ----

c19 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknskn, HRuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, sknskn, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  sknskn_count = n()
 ) %>%
 group_by(FiscalYear, HRuid, lvb) %>%
 mutate(
  total_year = n(),
  sknskn_rate = sknskn_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskn %in% 1,
        !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, sknskn, sknskn_count, sknskn_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(sknskn, HRuid),
                 fill = list(
                  sknskn_count = 0,
                  sknskn_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(sknskn, FiscalYear),
                 fill = list(
                  sknskn_count = 0,
                  sknskn_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  sknskn_delta = (sknskn_rate - lag(sknskn_rate))/lag(sknskn_rate),
  sknskn_delta = ifelse(FiscalYear == 2014, NA, sknskn_delta),
  sknskn_deltap = (sknskn_rate - lag(sknskn_rate,9))/lag(sknskn_rate,9)
 ) %>%
 ungroup()

c20 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknsknvg, HRuid, lvbvg) %>%
 distinct() %>%
 group_by(FiscalYear, sknsknvg, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  sknsknvg_count = n()
 ) %>%
 group_by(FiscalYear, HRuid, lvbvg) %>%
 mutate(
  total_year = n(),
  sknsknvg_rate = sknsknvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknsknvg %in% 1,
        !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, sknsknvg, sknsknvg_count, sknsknvg_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(sknsknvg, HRuid),
                 fill = list(
                  sknsknvg_count = 0,
                  sknsknvg_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(sknsknvg, FiscalYear),
                 fill = list(
                  sknsknvg_count = 0,
                  sknsknvg_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  sknsknvg_delta = (sknsknvg_rate - lag(sknsknvg_rate))/lag(sknsknvg_rate),
  sknsknvg_delta = ifelse(FiscalYear == 2014, NA, sknsknvg_delta),
  sknsknvg_deltap = (sknsknvg_rate - lag(sknsknvg_rate,9))/lag(sknsknvg_rate,9)
 ) %>%
 ungroup()

c21 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, sknskncs, HRuid, lvbcs) %>%
 distinct() %>%
 group_by(FiscalYear, sknskncs, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  sknskncs_count = n()
 ) %>%
 group_by(FiscalYear, HRuid, lvbcs) %>%
 mutate(
  total_year = n(),
  sknskncs_rate = sknskncs_count/total_year
 ) %>%
 ungroup() %>%
 filter(sknskncs %in% 1,
        !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, sknskncs, sknskncs_count, sknskncs_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(sknskncs, HRuid),
                 fill = list(
                  sknskncs_count = 0,
                  sknskncs_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(sknskncs, FiscalYear),
                 fill = list(
                  sknskncs_count = 0,
                  sknskncs_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  sknskncs_delta = (sknskncs_rate - lag(sknskncs_rate))/lag(sknskncs_rate),
  sknskncs_delta = ifelse(FiscalYear == 2014, NA, sknskncs_delta),
  sknskncs_deltap = (sknskncs_rate - lag(sknskncs_rate,9))/lag(sknskncs_rate,9)
 ) %>%
 ungroup()

#### Postpartum blood products amongst those who received antenatal iron therapy ----

c22 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
  pphiv = case_when(
   Any_Blood_Product > 0 & R003_02100 > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, Any_Blood_Product, R003_02100, HRuid) %>%
 mutate(
  pphiv_count = n()
 ) %>%
 group_by(FiscalYear, Any_Blood_Product, HRuid) %>%
 mutate(
  total_year = n(),
  pphiv_rate = pphiv_count/total_year
 ) %>%
 ungroup() %>%
 filter(Any_Blood_Product %in% 1, R003_02100 %in% 1,
        !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, pphiv, pphiv_count, pphiv_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(pphiv, HRuid),
                 fill = list(
                  pphiv_count = 0,
                  pphiv_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(pphiv, FiscalYear),
                 fill = list(
                  pphiv_count = 0,
                  pphiv_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  pphiv_delta = (pphiv_rate - lag(pphiv_rate))/lag(pphiv_rate),
  pphiv_delta = ifelse(FiscalYear == 2014, NA, pphiv_delta),
  pphiv_deltap = (pphiv_rate - lag(pphiv_rate,9))/lag(pphiv_rate,9)
 ) %>%
 ungroup()

#### Neonatal readmission ----

c23 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, neoreadm, HRuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, neoreadm, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  neoreadm_count = n()
 ) %>%
 group_by(FiscalYear, HRuid, lvb) %>%
 mutate(
  total_year = n(),
  neoreadm_rate = neoreadm_count/total_year
 ) %>%
 ungroup() %>%
 filter(neoreadm %in% 1,
        !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, neoreadm, neoreadm_count, neoreadm_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(neoreadm, HRuid),
                 fill = list(
                  neoreadm_count = 0,
                  neoreadm_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(neoreadm, FiscalYear),
                 fill = list(
                  neoreadm_count = 0,
                  neoreadm_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  neoreadm_delta = (neoreadm_rate - lag(neoreadm_rate))/lag(neoreadm_rate),
  neoreadm_delta = ifelse(FiscalYear == 2014, NA, neoreadm_delta),
  neoreadm_deltap = (neoreadm_rate - lag(neoreadm_rate,9))/lag(neoreadm_rate,9)
 ) %>%
 ungroup()

#### Preterm infants who received complete course of steroids > 24h and < 7 days prior to birth ----

c24 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(FiscalYear, pretster, HRuid) %>%
 mutate(
  pretster_count = n()
 ) %>%
 group_by(FiscalYear, den, HRuid) %>%
 mutate(
  total_year = n(),
  pretster_rate = pretster_count/total_year
 ) %>%
 ungroup() %>%
 filter(pretster %in% 1, !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, pretster, pretster_count,pretster_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(pretster, HRuid),
                 fill = list(
                  pretster_count = 0,
                  pretster_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(pretster, FiscalYear),
                 fill = list(
                  pretster_count = 0,
                  pretster_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  pretster_delta = (pretster_rate - lag(pretster_rate))/lag(pretster_rate),
  pretster_delta = ifelse(FiscalYear == 2014, NA, pretster_delta),
  pretster_deltap = (pretster_rate - lag(pretster_rate,9))/lag(pretster_rate,9)
 ) %>%
 ungroup()

#### Preterm babies born in facility without NICU ----

c25 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(FiscalYear, pretnnicu, HRuid) %>%
 mutate(
  pretnnicu_count = n()
 ) %>%
 group_by(FiscalYear, den, HRuid) %>%
 mutate(
  total_year = n(),
  pretnnicu_rate = pretnnicu_count/total_year
 ) %>%
 ungroup() %>%
 filter(pretnnicu %in% 1, !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, pretnnicu, pretnnicu_count,pretnnicu_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(pretnnicu, HRuid),
                 fill = list(
                  pretnnicu_count = 0,
                  pretnnicu_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(pretnnicu, FiscalYear),
                 fill = list(
                  pretnnicu_count = 0,
                  pretnnicu_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  pretnnicu_delta = (pretnnicu_rate - lag(pretnnicu_rate))/lag(pretnnicu_rate),
  pretnnicu_delta = ifelse(FiscalYear == 2014, NA, pretnnicu_delta),
  pretnnicu_deltap = (pretnnicu_rate - lag(pretnnicu_rate,9))/lag(pretnnicu_rate,9)
 ) %>%
 ungroup()

#### Newborn respiratory distress associated with low-risk repeat cesarean at term gestation (≥ 37 weeks < 39 weeks) ----

c26 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(FiscalYear, newlwcs, HRuid) %>%
 mutate(
  newlwcs_count = n()
 ) %>%
 group_by(FiscalYear, den, HRuid) %>%
 mutate(
  total_year = n(),
  newlwcs_rate = newlwcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(newlwcs %in% 1) %>%
 select(FiscalYear, HRuid, newlwcs, newlwcs_count,newlwcs_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(newlwcs, HRuid),
                 fill = list(
                  newlwcs_count = 0,
                  newlwcs_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(newlwcs, FiscalYear),
                 fill = list(
                  newlwcs_count = 0,
                  newlwcs_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  newlwcs_delta = (newlwcs_rate - lag(newlwcs_rate))/lag(newlwcs_rate),
  newlwcs_delta = ifelse(FiscalYear == 2014, NA, newlwcs_delta),
  newlwcs_deltap = (newlwcs_rate - lag(newlwcs_rate,9))/lag(newlwcs_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping  ----

c27 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
  delcord = case_when(
   (!toupper(BTOUTCOM) %in% "FTD") &
    (DEL_CORD_CLAMP %in% 3) ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, delcord, HRuid) %>%
 mutate(
  delcord_count = n()
 ) %>%
 group_by(FiscalYear, lvb, HRuid) %>%
 mutate(
  total_year = n(),
  delcord_rate = delcord_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcord %in% 1, lvb %in% 1,
        !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, delcord, delcord_count,delcord_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcord, HRuid),
                 fill = list(
                  delcord_count = 0,
                  delcord_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(delcord, FiscalYear),
                 fill = list(
                  delcord_count = 0,
                  delcord_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  delcord_delta = (delcord_rate - lag(delcord_rate))/lag(delcord_rate),
  delcord_delta = ifelse(FiscalYear == 2014, NA, delcord_delta),
  delcord_deltap = (delcord_rate - lag(delcord_rate,9))/lag(delcord_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns ----

c28 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(FiscalYear, delcordterm, HRuid) %>%
 mutate(
  delcordterm_count = n()
 ) %>%
 group_by(FiscalYear, den, HRuid) %>%
 mutate(
  total_year = n(),
  delcordterm_rate = delcordterm_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordterm %in% 1, !is.na(HRuid)) %>%
 select(FiscalYear,HRuid,delcordterm,delcordterm_count,delcordterm_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordterm, HRuid),
                 fill = list(
                  delcordterm_count = 0,
                  delcordterm_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(delcordterm, FiscalYear),
                 fill = list(
                  delcordterm_count = 0,
                  delcordterm_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  delcordterm_delta = (delcordterm_rate - lag(delcordterm_rate))/lag(delcordterm_rate),
  delcordterm_delta = ifelse(FiscalYear == 2014, NA, delcordterm_delta),
  delcordterm_deltap = (delcordterm_rate - lag(delcordterm_rate,9))/lag(delcordterm_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns following spontaneous vaginal birth ----

c29 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(FiscalYear, delcordtermvg, HRuid) %>%
 mutate(
  delcordtermvg_count = n()
 ) %>%
 group_by(FiscalYear, den, HRuid) %>%
 mutate(
  total_year = n(),
  delcordtermvg_rate = delcordtermvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordtermvg %in% 1, !is.na(HRuid)) %>%
 select(FiscalYear,HRuid, delcordtermvg, delcordtermvg_count,delcordtermvg_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordtermvg, HRuid),
                 fill = list(
                  delcordtermvg_count = 0,
                  delcordtermvg_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(delcordtermvg, FiscalYear),
                 fill = list(
                  delcordtermvg_count = 0,
                  delcordtermvg_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  delcordtermvg_delta = (delcordtermvg_rate - lag(delcordtermvg_rate))/lag(delcordtermvg_rate),
  delcordtermvg_delta = ifelse(FiscalYear == 2014, NA, delcordtermvg_delta),
  delcordtermvg_deltap = (delcordtermvg_rate - lag(delcordtermvg_rate,9))/lag(delcordtermvg_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst Term Newborns following caesarean birth ----

c30 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(FiscalYear, delcordtermcs, HRuid) %>%
 mutate(
  delcordtermcs_count = n()
 ) %>%
 group_by(FiscalYear, den, HRuid) %>%
 mutate(
  total_year = n(),
  delcordtermcs_rate = delcordtermcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordtermcs %in% 1, !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, delcordtermcs, delcordtermcs_count,delcordtermcs_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordtermcs, HRuid),
                 fill = list(
                  delcordtermcs_count = 0,
                  delcordtermcs_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(delcordtermcs, FiscalYear),
                 fill = list(
                  delcordtermcs_count = 0,
                  delcordtermcs_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  delcordtermcs_delta = (delcordtermcs_rate - lag(delcordtermcs_rate))/lag(delcordtermcs_rate),
  delcordtermcs_delta = ifelse(FiscalYear == 2014, NA, delcordtermcs_delta),
  delcordtermcs_deltap = (delcordtermcs_rate - lag(delcordtermcs_rate,9))/lag(delcordtermcs_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns ----

c31 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(FiscalYear, delcordpreterm, HRuid) %>%
 mutate(
  delcordpreterm_count = n()
 ) %>%
 group_by(FiscalYear, den, HRuid) %>%
 mutate(
  total_year = n(),
  delcordpreterm_rate = delcordpreterm_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpreterm %in% 1, !is.na(HRuid)) %>%
 select(FiscalYear,HRuid, delcordpreterm, delcordpreterm_count,delcordpreterm_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordpreterm, HRuid),
                 fill = list(
                  delcordpreterm_count = 0,
                  delcordpreterm_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(delcordpreterm, FiscalYear),
                 fill = list(
                  delcordpreterm_count = 0,
                  delcordpreterm_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  delcordpreterm_delta = (delcordpreterm_rate - lag(delcordpreterm_rate))/lag(delcordpreterm_rate),
  delcordpreterm_delta = ifelse(FiscalYear == 2014, NA, delcordpreterm_delta),
  delcordpreterm_deltap = (delcordpreterm_rate - lag(delcordpreterm_rate,9))/lag(delcordpreterm_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns following spontaneous vaginal birth ----

c32 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(FiscalYear, delcordpretermvg, HRuid) %>%
 mutate(
  delcordpretermvg_count = n()
 ) %>%
 group_by(FiscalYear, den, HRuid) %>%
 mutate(
  total_year = n(),
  delcordpretermvg_rate = delcordpretermvg_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpretermvg %in% 1, !is.na(HRuid)) %>%
 select(FiscalYear,HRuid, delcordpretermvg, delcordpretermvg_count,delcordpretermvg_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordpretermvg, HRuid),
                 fill = list(
                  delcordpretermvg_count = 0,
                  delcordpretermvg_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(delcordpretermvg, FiscalYear),
                 fill = list(
                  delcordpretermvg_count = 0,
                  delcordpretermvg_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  delcordpretermvg_delta = (delcordpretermvg_rate - lag(delcordpretermvg_rate))/lag(delcordpretermvg_rate),
  delcordpretermvg_delta = ifelse(FiscalYear == 2014, NA, delcordpretermvg_delta),
  delcordpretermvg_deltap = (delcordpretermvg_rate - lag(delcordpretermvg_rate,9))/lag(delcordpretermvg_rate,9)
 ) %>%
 ungroup()

#### Delayed Cord Clamping amongst preterm Newborns following caesarean birth ----

c33 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
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
 group_by(FiscalYear, delcordpretermcs, HRuid) %>%
 mutate(
  delcordpretermcs_count = n()
 ) %>%
 group_by(FiscalYear, den, HRuid) %>%
 mutate(
  total_year = n(),
  delcordpretermcs_rate = delcordpretermcs_count/total_year
 ) %>%
 ungroup() %>%
 filter(delcordpretermcs %in% 1, !is.na(HRuid)) %>%
 select(FiscalYear,HRuid, delcordpretermcs, delcordpretermcs_count,delcordpretermcs_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(delcordpretermcs, HRuid),
                 fill = list(
                  delcordpretermcs_count = 0,
                  delcordpretermcs_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(delcordpretermcs, FiscalYear),
                 fill = list(
                  delcordpretermcs_count = 0,
                  delcordpretermcs_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  delcordpretermcs_delta = (delcordpretermcs_rate - lag(delcordpretermcs_rate))/lag(delcordpretermcs_rate),
  delcordpretermcs_delta = ifelse(FiscalYear == 2014, NA, delcordpretermcs_delta),
  delcordpretermcs_deltap = (delcordpretermcs_rate - lag(delcordpretermcs_rate,9))/lag(delcordpretermcs_rate,9)
 ) %>%
 ungroup()

#### Milk feeding ----

c34 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, excbrst, HRuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, excbrst, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  excbrst_count = n()
 ) %>%
 group_by(FiscalYear, HRuid, lvb) %>%
 mutate(
  total_year = n(),
  excbrst_rate = excbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(excbrst %in% 1,
        !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, excbrst, excbrst_count, excbrst_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(excbrst, HRuid),
                 fill = list(
                  excbrst_count = 0,
                  excbrst_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(excbrst, FiscalYear),
                 fill = list(
                  excbrst_count = 0,
                  excbrst_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  excbrst_delta = (excbrst_rate - lag(excbrst_rate))/lag(excbrst_rate),
  excbrst_delta = ifelse(FiscalYear == 2014, NA, excbrst_delta),
  excbrst_deltap = (excbrst_rate - lag(excbrst_rate,9))/lag(excbrst_rate,9)
 ) %>%
 ungroup()

c35 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, nexcbrst, HRuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, nexcbrst, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  nexcbrst_count = n()
 ) %>%
 group_by(FiscalYear, HRuid, lvb) %>%
 mutate(
  total_year = n(),
  nexcbrst_rate = nexcbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nexcbrst %in% 1,
        !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, nexcbrst, nexcbrst_count, nexcbrst_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(nexcbrst, HRuid),
                 fill = list(
                  nexcbrst_count = 0,
                  nexcbrst_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(nexcbrst, FiscalYear),
                 fill = list(
                  nexcbrst_count = 0,
                  nexcbrst_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  nexcbrst_delta = (nexcbrst_rate - lag(nexcbrst_rate))/lag(nexcbrst_rate),
  nexcbrst_delta = ifelse(FiscalYear == 2014, NA, nexcbrst_delta),
  nexcbrst_deltap = (nexcbrst_rate - lag(nexcbrst_rate,9))/lag(nexcbrst_rate,9)
 ) %>%
 ungroup()

c36 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, nbrst, HRuid, lvbint) %>%
 distinct() %>%
 group_by(FiscalYear, nbrst, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  nbrst_count = n()
 ) %>%
 group_by(FiscalYear, HRuid, lvbint) %>%
 mutate(
  total_year = n(),
  nbrst_rate = nbrst_count/total_year
 ) %>%
 ungroup() %>%
 filter(nbrst %in% 1,
        !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, nbrst, nbrst_count, nbrst_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(nbrst, HRuid),
                 fill = list(
                  nbrst_count = 0,
                  nbrst_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(nbrst, FiscalYear),
                 fill = list(
                  nbrst_count = 0,
                  nbrst_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  nbrst_delta = (nbrst_rate - lag(nbrst_rate))/lag(nbrst_rate),
  nbrst_delta = ifelse(FiscalYear == 2014, NA, nbrst_delta),
  nbrst_deltap = (nbrst_rate - lag(nbrst_rate,9))/lag(nbrst_rate,9)
 ) %>%
 ungroup()

#### The breast/chest feeding initiation rate ----

c37 <- dta %>%
 select(BIRTHID, CONTCTID, FiscalYear, brstinit, HRuid, lvb) %>%
 distinct() %>%
 group_by(FiscalYear, brstinit, HRuid) %>%
 mutate(
  HRuid = as.character(HRuid),
  brstinit_count = n()
 ) %>%
 group_by(FiscalYear, HRuid, lvb) %>%
 mutate(
  total_year = n(),
  brstinit_rate = brstinit_count/total_year
 ) %>%
 ungroup() %>%
 filter(brstinit %in% 1,
        !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, brstinit, brstinit_count, brstinit_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(brstinit, HRuid),
                 fill = list(
                  brstinit_count = 0,
                  brstinit_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(brstinit, FiscalYear),
                 fill = list(
                  brstinit_count = 0,
                  brstinit_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  brstinit_delta = (brstinit_rate - lag(brstinit_rate))/lag(brstinit_rate),
  brstinit_delta = ifelse(FiscalYear == 2014, NA, brstinit_delta),
  brstinit_deltap = (brstinit_rate - lag(brstinit_rate,9))/lag(brstinit_rate,9)
 ) %>%
 ungroup()

#### ICU Admission during Pregnancy or Postpartum ----

c38 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
  icu = case_when(
   JOGC_ICU > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, JOGC_ICU, HRuid) %>%
 mutate(
  icu_count = n()
 ) %>%
 group_by(FiscalYear, HRuid) %>%
 mutate(
  total_year = n(),
  icu_rate = icu_count/total_year
 ) %>%
 ungroup() %>%
 filter(JOGC_ICU %in% 1, !is.na(HRuid)) %>%
 select(FiscalYear, HRuid, icu, icu_count,icu_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(icu, HRuid),
                 fill = list(
                  icu_count = 0,
                  icu_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(icu, FiscalYear),
                 fill = list(
                  icu_count = 0,
                  icu_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  icu_delta = (icu_rate - lag(icu_rate))/lag(icu_rate),
  icu_delta = ifelse(FiscalYear == 2014, NA, icu_delta),
  icu_deltap = (icu_rate - lag(icu_rate,9))/lag(icu_rate,9)
 ) %>%
 ungroup()

#### Rate of Severe Morbidity in Pregnancy or Postpartum ----

c39 <- dta %>%
 mutate(
  HRuid = as.character(HRuid),
  smm = case_when(
   JOGC_AnySMM > 0 ~ 1,
   TRUE ~ NA_integer_
  )
 ) %>%
 group_by(FiscalYear, JOGC_AnySMM, HRuid) %>%
 mutate(
  smm_count = n()
 ) %>%
 group_by(FiscalYear, lvb, HRuid) %>%
 mutate(
  total_year = n(),
  smm_rate = smm_count/total_year
 ) %>%
 ungroup() %>%
 filter(JOGC_AnySMM %in% 1, lvb %in% 1, !is.na(HRuid)) %>%
 select(FiscalYear,HRuid, smm, smm_count,smm_rate) %>%
 distinct() %>%
 arrange(HRuid, FiscalYear) %>%
 ungroup() %>%
 tidyr::complete(FiscalYear = unique(levels(dta$FiscalYear)),
                 tidyr::nesting(smm, HRuid),
                 fill = list(
                  smm_count = 0,
                  smm_rate = NA
                 )) %>%
 tidyr::complete(HRuid = unique(paste0("12",as.character(hr_shp$GeoUID))),
                 tidyr::nesting(smm, FiscalYear),
                 fill = list(
                  smm_count = 0,
                  smm_rate = NA
                 )) %>%
 group_by(HRuid) %>%
 mutate(
  smm_delta = (smm_rate - lag(smm_rate))/lag(smm_rate),
  smm_delta = ifelse(FiscalYear == 2014, NA, smm_delta),
  smm_deltap = (smm_rate - lag(smm_rate,9))/lag(smm_rate,9)
 ) %>%
 ungroup()

fhr_stats <- cbind(
 c1 %>% select(-PreExisting_Hypertension),
 c2 %>% select(-Gestational_Hypertension,-HRuid,-FiscalYear),
 c3 %>% select(-Any_Hypertension,-HRuid,-FiscalYear),
 c4 %>% select(-anemia,-HRuid,-FiscalYear),
 c5 %>% select(-PreExisting_Diabetes,-HRuid,-FiscalYear),
 c6 %>% select(-GDM,-HRuid,-FiscalYear),
 c7 %>% select(-Any_Diabetes,-HRuid,-FiscalYear),
 c8 %>% select(-sptvg,-HRuid,-FiscalYear),
 c9 %>% select(-med,-HRuid,-FiscalYear),
 c10 %>% select(-sptassvgmed,-HRuid,-FiscalYear),
 c11 %>% select(-sptassvgnmed,-HRuid,-FiscalYear),
 c12 %>% select(-rbs1,-HRuid,-FiscalYear),
 c13 %>% select(-rbs21,-HRuid,-FiscalYear),
 c14 %>% select(-rbs51,-HRuid,-FiscalYear),
 c15 %>% select(-pphbl,-HRuid,-FiscalYear),
 c16 %>% select(-pphpi,-HRuid,-FiscalYear),
 c17 %>% select(-ppreadm,-HRuid,-FiscalYear),
 #c18 %>% select(-brstinit,-HRuid,-FiscalYear),
 c19 %>% select(-sknskn,-HRuid,-FiscalYear),
 c20 %>% select(-sknsknvg,-HRuid,-FiscalYear),
 c21 %>% select(-sknskncs,-HRuid,-FiscalYear),
 c22 %>% select(-pphiv,-HRuid,-FiscalYear),
 c23 %>% select(-neoreadm,-HRuid,-FiscalYear),
 c24 %>% select(-pretster,-HRuid,-FiscalYear),
 c25 %>% select(-pretnnicu,-HRuid,-FiscalYear),
 c26 %>% select(-newlwcs,-HRuid,-FiscalYear),
 c27 %>% select(-delcord,-HRuid,-FiscalYear),
 c28 %>% select(-delcordterm,-HRuid,-FiscalYear),
 c29 %>% select(-delcordtermvg,-HRuid,-FiscalYear),
 c30 %>% select(-delcordtermcs,-HRuid,-FiscalYear),
 c31 %>% select(-delcordpreterm,-HRuid,-FiscalYear),
 c32 %>% select(-delcordpretermvg,-HRuid,-FiscalYear),
 c33 %>% select(-delcordpretermcs,-HRuid,-FiscalYear),
 c34 %>% select(-excbrst,-HRuid,-FiscalYear),
 c35 %>% select(-nexcbrst,-HRuid,-FiscalYear),
 c36 %>% select(-nbrst,-HRuid,-FiscalYear),
 c37 %>% select(-brstinit,-HRuid,-FiscalYear),
 c38 %>% select(-icu,-HRuid,-FiscalYear),
 c39 %>% select(-smm,-HRuid,-FiscalYear)
) %>%
 filter(!HRuid %in% "12 ") %>%
 mutate(HRuid = as.character(trimws(substr(HRuid,3,4))))
