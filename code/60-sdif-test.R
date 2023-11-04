# SDIF test

# Reset
rm(list = ls())
gc()

# SDIF load

dat_sdif <- fread("result/CY2_Final_SDIF_LVA.csvy", yaml = TRUE) |>
  setnames(tolower)

# Records in SDIF where DISP_CIBQ in 1 or 90 but not in BQR
# Check for missing records in BQR, e.g. zip files not loaded or data for non-interviews not loaded. Check for invalid records in SDIF.
dat_sdif[persid == "13611880019"]

x <- c(
  "10346266019",
  "11673583028",
  "14109959028",
  "15454034028"
)

dat_sdif[persid %in% x]
dat_sdif[caseid %in% substr(x, 1, 8)]

# Check ID 92	
# Check Description	Number of unique values for TRIMGRPS
# Country Advice	Number should be consistent with oversampling domains or strata. Please verify.
dat_sdif[, .N, keyby = .(trimgrps)]


# Check ID 111	
# Check Description	Cases where the combination of ID_PSU, ID_SSU, and ID_HH are not unique to a single STRAT_HH.
# Country Advice	Every record within a household should have the same value of STRAT_HH. Check the returned cases, and determine the correct value of STRAT_HH for the household.

# dat_sdif[id_psu == 137 & id_hh == 219]

tab_check_id_111 <- read.xlsx(
  xlsxFile = "doc/checks_19JUL2023_ML.xlsx",
  sheet = "SCF_Sampling _ID_PSU,_ID_SSU,_a",
  startRow = 6
) |> setDT() |> setnames(tolower)

map_chr(.x = tab_check_id_111, .f = class)
tab_check_id_111[, id_psu := as.integer(id_psu)]
tab_check_id_111[, id_hh  := as.integer(id_hh)]
map_chr(.x = tab_check_id_111, .f = class)

tab_check_id_111

tab_test <- dat_sdif[, .N, keyby = .(caseid, id_psu, id_ssu, id_hh)]
tab_test[, .N, keyby = .(N)]

merge(
  x = tab_test,
  y = tab_check_id_111,
  by = c("id_psu", "id_hh"),
  all = T
)[, .N, keyby = .(N, `count(*)`)]

tab_test[, .(id_psu, id_ssu, id_hh)] |> anyDuplicated()


# Check ID 112	
# Check Description	Cases where the combination of ID_PSU, ID_SSU, and ID_HH are not unique to a single SORT_HH.
# Country Advice	Every record within a household should have the same value of SORT_HH. Check the returned cases, and determine the correct value of SORT_HH for the household

tab_check_id_112 <- read.xlsx(
  xlsxFile = "doc/checks_19JUL2023_ML.xlsx",
  sheet = "SCF_Sampling _ID_PSU,_ID_SS(01)",
  startRow = 6
) |> setDT() |> setnames(tolower)

map_chr(.x = tab_check_id_112, .f = class)
tab_check_id_112[, id_psu := as.integer(id_psu)]
tab_check_id_112[, id_hh  := as.integer(id_hh)]
map_chr(.x = tab_check_id_112, .f = class)

tab_check_id_112

tab_test <- dat_sdif[, .N, keyby = .(caseid, id_psu, id_ssu, id_hh)]
tab_test[, .N, keyby = .(N)]

merge(
  x = tab_test,
  y = tab_check_id_112,
  by = c("id_psu", "id_hh"),
  all = T
)[, .N, keyby = .(N, `count(*)`)]



# THEOR_HBWT & THEOR_PBWT

dat_sdif[, .(theor_hbwt, theor_pbwt)]

dat_sdif[caseid %in% c("11453963", "12266050"), .(theor_hbwt, theor_pbwt)]
