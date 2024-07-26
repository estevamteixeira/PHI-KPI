# Script to prepare data for the
# Perinatal Health Indicators and Key Performance Indicators ----
## Time frame: 10 years if possible
## Geography: provincial, compared with national data if available

# Loading required packages ----

library(haven) # read SAS data
library(dplyr)

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
 "PreExisting_Hypertension",# Pre-existing/Pre-pregnancy Hypertension
 "Gestational_Hypertension",# Gestational Hypertension
 "Any_Hypertension",# Any Hypertension
 "Proteinuria",# Any protein in urine
 "Pre_Eclampsia"# Pre_Eclampsia incl. HELLP Syndrome, Eclampsia
)

# Load dataset ----
dta <- haven::read_sas("H:\\RCP\\RCP_Data\\TeixeiEC\\Monster\\Monster.sas7bdat"
                           #, n_max = 2
                           ,col_select = myvec
)
