### Regional facilities ----

# Computing stats ----
cstats <- function(data, id_col, period_col, num_col, fac_col, den_col = NULL,
                   count_col, rate_col, total_col, delta_col, delta_colp,
                   ref_year = t0, valid_period, valid_fac) {
 library(dplyr)
 library(tidyr)

 result <- data %>%
  select(!!sym(id_col), !!sym(period_col), !!sym(num_col), !!sym(fac_col), all_of(den_col)) %>%
  distinct()

 # If `den_col` is provided, use it; otherwise, count all cases per period
 if (!is.null(den_col)) {
  result <- result %>% filter(!!sym(den_col) == 1)
 }

 result <- result %>%
  # Count occurrences of the condition
  group_by(!!sym(period_col), !!sym(num_col), !!sym(fac_col)) %>%
  mutate(!!sym(count_col) := n()) %>%
  group_by(!!sym(period_col), !!sym(fac_col)) %>%
  mutate(!!total_col := n(),
         !!sym(rate_col) := !!sym(count_col) / !!sym(total_col)) %>%

  # Filter for relevant condition cases
  filter(!!sym(num_col) == 1) %>%

  # Select relevant columns
  select(!!sym(period_col), !!sym(fac_col), !!sym(num_col), !!sym(count_col), !!sym(total_col), !!sym(rate_col)) %>%
  arrange(!!sym(fac_col), !!sym(period_col)) %>%
  ungroup() %>%
  distinct() %>%

  complete(!!sym(period_col) := valid_period,
           tidyr::nesting(!!sym(num_col), !!sym(fac_col))
  ) %>%
  complete(!!sym(fac_col) := valid_fac,
           tidyr::nesting(!!sym(num_col), !!sym(period_col))
  )

 t0 <- result %>% filter(!is.na(!!sym(rate_col))) %>% pull(!!sym(period_col)) %>% row_number() %>% min()
 tn <- result %>% filter(!is.na(!!sym(rate_col))) %>% pull(!!sym(period_col)) %>% row_number() %>% max()
 lag_period <- (tn-t0)

 result <- result %>%
  group_by(!!sym(fac_col)) %>%
  # Calculating deltas
  mutate(
   !!sym(delta_col) := (!!sym(rate_col) - lag(!!sym(rate_col))) / lag(!!sym(rate_col)),
   !!sym(delta_col) := ifelse(!!sym(period_col) == ref_year, NA, !!sym(delta_col)),
   !!sym(delta_colp) := (!!sym(rate_col) - lag(!!sym(rate_col), lag_period)) / lag(!!sym(rate_col), lag_period)
  ) %>%
  ungroup()

 return(result)
}

##-------------------
## Calendar year ----
##-------------------
t0 <- 2014
tn <- 2023

period <- t0:tn
fac <- sort(unique(dta$DLHosp))

c1 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "prehyp_num", count_col = "prehyp_count", rate_col = "prehyp_rate", total_col = "prehyp_total", delta_col = "prehyp_delta", delta_colp = "prehyp_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c2 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "gesthyp_num", count_col = "gesthyp_count", rate_col = "gesthyp_rate", total_col = "gesthyp_total", delta_col = "gesthyp_delta", delta_colp = "gesthyp_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c3 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "hyp_num", count_col = "hyp_count", rate_col = "hyp_rate", total_col = "hyp_total", delta_col = "hyp_delta", delta_colp = "hyp_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c4 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "anemia_num", count_col = "anemia_count", rate_col = "anemia_rate", total_col = "anemia_total", delta_col = "anemia_delta", delta_colp = "anemia_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c5 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "prediab_num", count_col = "prediab_count", rate_col = "prediab_rate", total_col = "prediab_total", delta_col = "prediab_delta", delta_colp = "prediab_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c6 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "gestdiab_num", count_col = "gestdiab_count", rate_col = "gestdiab_rate", total_col = "gestdiab_total", delta_col = "gestdiab_delta", delta_colp = "gestdiab_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c7 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "diab_num", count_col = "diab_count", rate_col = "diab_rate", total_col = "diab_total", delta_col = "diab_delta", delta_colp = "diab_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c8 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "spt_num", count_col = "spt_count", rate_col = "spt_rate", total_col = "spt_total", delta_col = "spt_delta", delta_colp = "spt_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c9 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "med_num", den_col = "assvg", count_col = "med_count", rate_col = "med_rate", total_col = "med_total", delta_col = "med_delta", delta_colp = "med_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c10 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "sptassvgmed_num", den_col = "assvg", count_col = "sptassvgmed_count", rate_col = "sptassvgmed_rate", total_col = "sptassvgmed_total", delta_col = "sptassvgmed_delta", delta_colp = "sptassvgmed_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c11 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "sptassvgnmed_num", den_col = "assvg", count_col = "sptassvgnmed_count", rate_col = "sptassvgnmed_rate", total_col = "sptassvgnmed_total", delta_col = "sptassvgnmed_delta", delta_colp = "sptassvgnmed_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c12 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "rbs1_num", den_col = "rbs1_den", count_col = "rbs1_count", rate_col = "rbs1_rate", total_col = "rbs1_total", delta_col = "rbs1_delta", delta_colp = "rbs1_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c13 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "rbs21_num", den_col = "rbs21_den", count_col = "rbs21_count", rate_col = "rbs21_rate", total_col = "rbs21_total", delta_col = "rbs21_delta", delta_colp = "rbs21_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c14 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "rbs51_num", den_col = "rbs51_den", count_col = "rbs51_count", rate_col = "rbs51_rate", total_col = "rbs51_total", delta_col = "rbs51_delta", delta_colp = "rbs51_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c15 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "pphbl_num", count_col = "pphbl_count", rate_col = "pphbl_rate", total_col = "pphbl_total", delta_col = "pphbl_delta", delta_colp = "pphbl_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c16 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "pphpi_num", den_col = "pphpi_den", count_col = "pphpi_count", rate_col = "pphpi_rate", total_col = "pphpi_total", delta_col = "pphpi_delta", delta_colp = "pphpi_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c17 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "ppreadm_num", count_col = "ppreadm_count", rate_col = "ppreadm_rate", total_col = "ppreadm_total", delta_col = "ppreadm_delta", delta_colp = "ppreadm_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
#c18 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "hyp_num", count_col = "hyp_count", rate_col = "hyp_rate", total_col = "hyp_total", delta_col = "hyp_delta", delta_colp = "hyp_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c19 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "sknskn_num", den_col = "lvb", count_col = "sknskn_count", rate_col = "sknskn_rate", total_col = "sknskn_total", delta_col = "sknskn_delta", delta_colp = "sknskn_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c20 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "sknsknvg_num", den_col = "lvbvg", count_col = "sknsknvg_count", rate_col = "sknsknvg_rate", total_col = "sknsknvg_total", delta_col = "sknsknvg_delta", delta_colp = "sknsknvg_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c21 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "sknskncs_num", den_col = "lvbcs", count_col = "sknskncs_count", rate_col = "sknskncs_rate", total_col = "sknskncs_total", delta_col = "sknskncs_delta", delta_colp = "sknskncs_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c22 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "pphiv_num", den_col = "pphiv_den", count_col = "pphiv_count", rate_col = "pphiv_rate", total_col = "pphiv_total", delta_col = "pphiv_delta", delta_colp = "pphiv_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c23 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "neoreadm_num", den_col = "lvb", count_col = "neoreadm_count", rate_col = "neoreadm_rate", total_col = "neoreadm_total", delta_col = "neoreadm_delta", delta_colp = "neoreadm_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c24 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "pretster_num", den_col = "pretster_den", count_col = "pretster_count", rate_col = "pretster_rate", total_col = "pretster_total", delta_col = "pretster_delta", delta_colp = "pretster_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c25 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "pretnnicu_num", den_col = "pretnnicu_den", count_col = "pretnnicu_count", rate_col = "pretnnicu_rate", total_col = "pretnnicu_total", delta_col = "pretnnicu_delta", delta_colp = "pretnnicu_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c26 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "newlwcs_num", den_col = "newlwcs_den", count_col = "newlwcs_count", rate_col = "newlwcs_rate", total_col = "newlwcs_total", delta_col = "newlwcs_delta", delta_colp = "newlwcs_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c27 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "delcord_num", den_col = "lvb", count_col = "delcord_count", rate_col = "delcord_rate", total_col = "delcord_total", delta_col = "delcord_delta", delta_colp = "delcord_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c28 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "delcordterm_num", den_col = "delcordterm_den", count_col = "delcordterm_count", rate_col = "delcordterm_rate", total_col = "delcordterm_total", delta_col = "delcordterm_delta", delta_colp = "delcordterm_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c29 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "delcordtermvg_num", den_col = "delcordtermvg_den", count_col = "delcordtermvg_count", rate_col = "delcordtermvg_rate", total_col = "delcordtermvg_total", delta_col = "delcordtermvg_delta", delta_colp = "delcordtermvg_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c30 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "delcordtermcs_num", den_col = "delcordtermcs_den", count_col = "delcordtermcs_count", rate_col = "delcordtermcs_rate", total_col = "delcordtermcs_total", delta_col = "delcordtermcs_delta", delta_colp = "delcordtermcs_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c31 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "delcordpreterm_num", den_col = "delcordpreterm_den", count_col = "delcordpreterm_count", rate_col = "delcordpreterm_rate", total_col = "delcordpreterm_total", delta_col = "delcordpreterm_delta", delta_colp = "delcordpreterm_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c32 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "delcordpretermvg_num", den_col = "delcordpretermvg_den", count_col = "delcordpretermvg_count", rate_col = "delcordpretermvg_rate", total_col = "delcordpretermvg_total", delta_col = "delcordpretermvg_delta", delta_colp = "delcordpretermvg_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c33 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "delcordpretermcs_num", den_col = "delcordpretermcs_den", count_col = "delcordpretermcs_count", rate_col = "delcordpretermcs_rate", total_col = "delcordpretermcs_total", delta_col = "delcordpretermcs_delta", delta_colp = "delcordpretermcs_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c34 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "excbrst_num", den_col = "lvb", count_col = "excbrst_count", rate_col = "excbrst_rate", total_col = "excbrst_total", delta_col = "excbrst_delta", delta_colp = "excbrst_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c35 <- cstats(dta, id_col = "BIRTHID", period_col = "BrthYear", num_col = "nexcbrst_num", den_col = "lvb", count_col = "nexcbrst_count", rate_col = "nexcbrst_rate", total_col = "nexcbrst_total", delta_col = "nexcbrst_delta", delta_colp = "nexcbrst_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c36 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "nbrst_num", den_col = "lvbint", count_col = "nbrst_count", rate_col = "nbrst_rate", total_col = "nbrst_total", delta_col = "nbrst_delta", delta_colp = "nbrst_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c37 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "brstinit_num", den_col = "lvb", count_col = "brstinit_count", rate_col = "brstinit_rate", total_col = "brstinit_total", delta_col = "brstinit_delta", delta_colp = "brstinit_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c38 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "icu_num", count_col = "icu_count", rate_col = "icu_rate", total_col = "icu_total", delta_col = "icu_delta", delta_colp = "icu_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c39 <- cstats(dta, id_col = "MOTHERID", period_col = "BrthYear", num_col = "smm_num", count_col = "smm_count", rate_col = "smm_rate", total_col = "smm_total", delta_col = "smm_delta", delta_colp = "smm_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)

chosp_stats <- cbind(
 c1 %>% select(-ends_with("_num")),
 c2 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c3 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c4 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c5 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c6 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c7 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c8 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c9 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c10 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c11 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c12 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c13 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c14 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c15 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c16 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c17 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 #c18 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c19 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c20 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c21 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c22 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c23 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c24 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c25 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c26 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c27 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c28 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c29 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c30 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c31 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c32 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c33 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c34 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c35 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c36 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c37 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c38 %>% select(-ends_with("_num"),-BrthYear,-DLHosp),
 c39 %>% select(-ends_with("_num"),-BrthYear,-DLHosp)
)


##-----------------
## Fiscal year ----
##-----------------

### Regional facilities ----

period <- unique(levels(dta$FiscalYear))

c1 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "prehyp_num", count_col = "prehyp_count", rate_col = "prehyp_rate", total_col = "prehyp_total", delta_col = "prehyp_delta", delta_colp = "prehyp_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c2 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "gesthyp_num", count_col = "gesthyp_count", rate_col = "gesthyp_rate", total_col = "gesthyp_total", delta_col = "gesthyp_delta", delta_colp = "gesthyp_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c3 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "hyp_num", count_col = "hyp_count", rate_col = "hyp_rate", total_col = "hyp_total", delta_col = "hyp_delta", delta_colp = "hyp_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c4 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "anemia_num", count_col = "anemia_count", rate_col = "anemia_rate", total_col = "anemia_total", delta_col = "anemia_delta", delta_colp = "anemia_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c5 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "prediab_num", count_col = "prediab_count", rate_col = "prediab_rate", total_col = "prediab_total", delta_col = "prediab_delta", delta_colp = "prediab_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c6 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "gestdiab_num", count_col = "gestdiab_count", rate_col = "gestdiab_rate", total_col = "gestdiab_total", delta_col = "gestdiab_delta", delta_colp = "gestdiab_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c7 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "diab_num", count_col = "diab_count", rate_col = "diab_rate", total_col = "diab_total", delta_col = "diab_delta", delta_colp = "diab_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c8 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "spt_num", count_col = "spt_count", rate_col = "spt_rate", total_col = "spt_total", delta_col = "spt_delta", delta_colp = "spt_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c9 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "med_num", den_col = "assvg", count_col = "med_count", rate_col = "med_rate", total_col = "med_total", delta_col = "med_delta", delta_colp = "med_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c10 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "sptassvgmed_num", den_col = "assvg", count_col = "sptassvgmed_count", rate_col = "sptassvgmed_rate", total_col = "sptassvgmed_total", delta_col = "sptassvgmed_delta", delta_colp = "sptassvgmed_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c11 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "sptassvgnmed_num", den_col = "assvg", count_col = "sptassvgnmed_count", rate_col = "sptassvgnmed_rate", total_col = "sptassvgnmed_total", delta_col = "sptassvgnmed_delta", delta_colp = "sptassvgnmed_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c12 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "rbs1_num", den_col = "rbs1_den", count_col = "rbs1_count", rate_col = "rbs1_rate", total_col = "rbs1_total", delta_col = "rbs1_delta", delta_colp = "rbs1_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c13 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "rbs21_num", den_col = "rbs21_den", count_col = "rbs21_count", rate_col = "rbs21_rate", total_col = "rbs21_total", delta_col = "rbs21_delta", delta_colp = "rbs21_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c14 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "rbs51_num", den_col = "rbs51_den", count_col = "rbs51_count", rate_col = "rbs51_rate", total_col = "rbs51_total", delta_col = "rbs51_delta", delta_colp = "rbs51_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c15 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "pphbl_num", count_col = "pphbl_count", rate_col = "pphbl_rate", total_col = "pphbl_total", delta_col = "pphbl_delta", delta_colp = "pphbl_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c16 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "pphpi_num", den_col = "pphpi_den", count_col = "pphpi_count", rate_col = "pphpi_rate", total_col = "pphpi_total", delta_col = "pphpi_delta", delta_colp = "pphpi_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c17 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "ppreadm_num", count_col = "ppreadm_count", rate_col = "ppreadm_rate", total_col = "ppreadm_total", delta_col = "ppreadm_delta", delta_colp = "ppreadm_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
#c18 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "hyp_num", count_col = "hyp_count", rate_col = "hyp_rate", total_col = "hyp_total", delta_col = "hyp_delta", delta_colp = "hyp_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c19 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "sknskn_num", den_col = "lvb", count_col = "sknskn_count", rate_col = "sknskn_rate", total_col = "sknskn_total", delta_col = "sknskn_delta", delta_colp = "sknskn_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c20 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "sknsknvg_num", den_col = "lvbvg", count_col = "sknsknvg_count", rate_col = "sknsknvg_rate", total_col = "sknsknvg_total", delta_col = "sknsknvg_delta", delta_colp = "sknsknvg_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c21 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "sknskncs_num", den_col = "lvbcs", count_col = "sknskncs_count", rate_col = "sknskncs_rate", total_col = "sknskncs_total", delta_col = "sknskncs_delta", delta_colp = "sknskncs_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c22 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "pphiv_num", den_col = "pphiv_den", count_col = "pphiv_count", rate_col = "pphiv_rate", total_col = "pphiv_total", delta_col = "pphiv_delta", delta_colp = "pphiv_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c23 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "neoreadm_num", den_col = "lvb", count_col = "neoreadm_count", rate_col = "neoreadm_rate", total_col = "neoreadm_total", delta_col = "neoreadm_delta", delta_colp = "neoreadm_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c24 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "pretster_num", den_col = "pretster_den", count_col = "pretster_count", rate_col = "pretster_rate", total_col = "pretster_total", delta_col = "pretster_delta", delta_colp = "pretster_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c25 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "pretnnicu_num", den_col = "pretnnicu_den", count_col = "pretnnicu_count", rate_col = "pretnnicu_rate", total_col = "pretnnicu_total", delta_col = "pretnnicu_delta", delta_colp = "pretnnicu_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c26 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "newlwcs_num", den_col = "newlwcs_den", count_col = "newlwcs_count", rate_col = "newlwcs_rate", total_col = "newlwcs_total", delta_col = "newlwcs_delta", delta_colp = "newlwcs_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c27 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "delcord_num", den_col = "lvb", count_col = "delcord_count", rate_col = "delcord_rate", total_col = "delcord_total", delta_col = "delcord_delta", delta_colp = "delcord_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c28 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "delcordterm_num", den_col = "delcordterm_den", count_col = "delcordterm_count", rate_col = "delcordterm_rate", total_col = "delcordterm_total", delta_col = "delcordterm_delta", delta_colp = "delcordterm_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c29 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "delcordtermvg_num", den_col = "delcordtermvg_den", count_col = "delcordtermvg_count", rate_col = "delcordtermvg_rate", total_col = "delcordtermvg_total", delta_col = "delcordtermvg_delta", delta_colp = "delcordtermvg_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c30 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "delcordtermcs_num", den_col = "delcordtermcs_den", count_col = "delcordtermcs_count", rate_col = "delcordtermcs_rate", total_col = "delcordtermcs_total", delta_col = "delcordtermcs_delta", delta_colp = "delcordtermcs_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c31 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "delcordpreterm_num", den_col = "delcordpreterm_den", count_col = "delcordpreterm_count", rate_col = "delcordpreterm_rate", total_col = "delcordpreterm_total", delta_col = "delcordpreterm_delta", delta_colp = "delcordpreterm_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c32 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "delcordpretermvg_num", den_col = "delcordpretermvg_den", count_col = "delcordpretermvg_count", rate_col = "delcordpretermvg_rate", total_col = "delcordpretermvg_total", delta_col = "delcordpretermvg_delta", delta_colp = "delcordpretermvg_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c33 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "delcordpretermcs_num", den_col = "delcordpretermcs_den", count_col = "delcordpretermcs_count", rate_col = "delcordpretermcs_rate", total_col = "delcordpretermcs_total", delta_col = "delcordpretermcs_delta", delta_colp = "delcordpretermcs_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c34 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "excbrst_num", den_col = "lvb", count_col = "excbrst_count", rate_col = "excbrst_rate", total_col = "excbrst_total", delta_col = "excbrst_delta", delta_colp = "excbrst_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c35 <- cstats(dta, id_col = "BIRTHID", period_col = "FiscalYear", num_col = "nexcbrst_num", den_col = "lvb", count_col = "nexcbrst_count", rate_col = "nexcbrst_rate", total_col = "nexcbrst_total", delta_col = "nexcbrst_delta", delta_colp = "nexcbrst_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c36 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "nbrst_num", den_col = "lvbint", count_col = "nbrst_count", rate_col = "nbrst_rate", total_col = "nbrst_total", delta_col = "nbrst_delta", delta_colp = "nbrst_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c37 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "brstinit_num", den_col = "lvb", count_col = "brstinit_count", rate_col = "brstinit_rate", total_col = "brstinit_total", delta_col = "brstinit_delta", delta_colp = "brstinit_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c38 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "icu_num", count_col = "icu_count", rate_col = "icu_rate", total_col = "icu_total", delta_col = "icu_delta", delta_colp = "icu_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)
c39 <- cstats(dta, id_col = "MOTHERID", period_col = "FiscalYear", num_col = "smm_num", count_col = "smm_count", rate_col = "smm_rate", total_col = "smm_total", delta_col = "smm_delta", delta_colp = "smm_deltap", valid_period = period, fac_col = "DLHosp", valid_fac = fac)

fhosp_stats <- cbind(
 c1 %>% select(-ends_with("_num")),
 c2 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c3 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c4 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c5 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c6 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c7 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c8 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c9 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c10 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c11 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c12 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c13 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c14 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c15 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c16 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c17 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 #c18 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c19 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c20 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c21 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c22 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c23 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c24 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c25 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c26 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c27 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c28 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c29 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c30 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c31 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c32 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c33 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c34 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c35 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c36 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c37 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c38 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp),
 c39 %>% select(-ends_with("_num"),-FiscalYear,-DLHosp)
) %>%
 mutate(FiscalYear = factor(FiscalYear))
