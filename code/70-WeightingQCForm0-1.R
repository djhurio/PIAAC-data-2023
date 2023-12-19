# WeightingQCForm

# Reset
rm(list = ls())
gc()

# Alternative Published Totals

fname_codes <- "result/CY2_Final_MS_Benchmark_WIF_Codebook_LVA.xlsx"
fname_total <- "result/CY2_Final_MS_Benchmark_WIF_LVA.xlsx"

tab_rakedim1_codes <- read.xlsx(fname_codes, 1) |> setDT() |> setnames(tolower)
tab_rakedim2_codes <- read.xlsx(fname_codes, 2) |> setDT() |> setnames(tolower)
tab_rakedim3_codes <- read.xlsx(fname_codes, 3) |> setDT() |> setnames(tolower)
tab_rakedim1_total <- read.xlsx(fname_total, 1) |> setDT() |> setnames(tolower)
tab_rakedim2_total <- read.xlsx(fname_total, 2) |> setDT() |> setnames(tolower)
tab_rakedim3_total <- read.xlsx(fname_total, 3) |> setDT() |> setnames(tolower)

tab_rakedim1 <- merge(tab_rakedim1_codes, tab_rakedim1_total, by = "rakedim1")
tab_rakedim2 <- merge(tab_rakedim2_codes, tab_rakedim2_total, by = "rakedim2")
tab_rakedim3 <- merge(tab_rakedim3_codes, tab_rakedim3_total, by = "rakedim3")

tab_rakedim1[, sum(total)]
tab_rakedim1[, sum(total), keyby = .(gender)]
tab_rakedim1[, sum(total), keyby = .(age)]

tab_rakedim1[, age2 := trunc((as.integer(factor(age)) - 1) / 2) + 1]
tab_rakedim1[, sum(total), keyby = .(age, age2)]
tab_rakedim1[, sum(total), keyby = .(age2)]

tab_rakedim2[, ethnicity := factor(x = ethnicity, levels = ethnicity)]
tab_rakedim2[, sum(total), keyby = .(ethnicity)] |>
  clipr::write_clip(col.names = FALSE)

tab_rakedim3[, country_of_birth := factor(x = country_of_birth,
                                          levels = country_of_birth)]
tab_rakedim3[, sum(total), keyby = .(country_of_birth)] |>
  clipr::write_clip(col.names = FALSE)


# Theoretical HH base weights
dat_sdif <- fread("result/CY2_Final_SDIF_LVA.csvy", yaml = TRUE)

dat_sdif[, PROB_OVERALL_HH := round(PROB_PSU * PROB_HH, 12)]
dat_sdif[, THEOR_HBWT := round(1 / PROB_OVERALL_HH, 6)]

dat_sdif_case <- unique(dat_sdif, by = "CASEID")

dat_sdif_case[is.na(THEOR_HBWT), .N]
dat_sdif_case[!is.na(THEOR_HBWT), .N]
dat_sdif_case[, sum(THEOR_HBWT)]
dat_sdif_case[, summary(THEOR_HBWT)]

tab_cv <- dat_sdif_case[, .(cv = sd(THEOR_HBWT) / mean(THEOR_HBWT))]
tab_cv[, cv2 := 1 + cv ^ 2]
tab_cv[, map(.SD, round, digits = 2)]

dat_sdif_case[order(-THEOR_HBWT),
         .(ID_PSU, ID_SSU, ID_HH, ID_OTH, THEOR_HBWT,
           PROB_PSU, PROB_SSU, PROB_HH, PROB_OTH)] |> first(20) |>
  clipr::write_clip(col.names = FALSE)


# Theoretical pers base weights
dat_sdif[, PROB_OVERALL_PERS := round(PROB_OVERALL_HH * PROB_PERS, 12)]
dat_sdif[, THEOR_PBWT := round(1 / PROB_OVERALL_PERS, 6)]

dat_sdif[!is.na(PERSID) & is.na(THEOR_PBWT), .N]
dat_sdif[!is.na(PERSID) & !is.na(THEOR_PBWT), .N]
dat_sdif[!is.na(PERSID), sum(THEOR_PBWT)]
dat_sdif[!is.na(PERSID), summary(THEOR_PBWT)]

tab_cv <- dat_sdif[!is.na(PERSID), .(cv = sd(THEOR_PBWT) / mean(THEOR_PBWT))]
tab_cv[, cv2 := 1 + cv ^ 2]
tab_cv
tab_cv[, map(.SD, round, digits = 2)]

dat_sdif[!is.na(PERSID)][
  order(PROB_PERS),
  .(ID_PSU, ID_SSU, ID_HH, ID_OTH, PROB_PERS, NUMSEL1, NUMELG1)
] |> first(20) |> clipr::write_clip(col.names = FALSE)

dat_sdif[!is.na(PERSID)][
  order(-THEOR_PBWT),
  .(ID_PSU, ID_SSU, ID_HH, ID_OTH, PERSID, THEOR_PBWT,
    PROB_PSU, PROB_SSU, PROB_HH, PROB_OTH, PROB_PERS)
] |> first(20) |> clipr::write_clip(col.names = FALSE)



# Imputation

imp_var_names <- grep("^IFLG_", names(dat_sdif), value = TRUE)
dat_sdif[, c(imp_var_names) := map(.SD, as.integer), .SDcols = imp_var_names]

dat_sdif[, map(.SD, sum, na.rm = TRUE), .SDcols = imp_var_names]

tab_imp <- melt.data.table(
  data = dat_sdif,
  id.vars = c("CASEID", "PERSID", "DISP_CIBQ", "DISP_DS", "WEIGHTFLG"),
  measure.vars = imp_var_names,
  variable.factor = FALSE,
  na.rm = TRUE
)

tab_imp[
  !grepl("RAKEDIM", variable),
  .(n_imp = sum(value), n_tot = .N, rate_imp = round(100 * sum(value) / .N, 1)),
  by = .(variable = sub("IFLG_", "", variable))
][n_imp > 0]


# BQ Literacy Related Nonrespondents
tab_imp[grepl("RAKEDIM", variable) & DISP_CIBQ == 7 & DISP_DS == 1,
        .(n_imp = sum(value), n_tot = .N),
        keyby = .(variable)][n_imp > 0]

# BQ respondents
tab_imp[grepl("RAKEDIM", variable) & DISP_CIBQ == 1,
        .(n_imp = sum(value), n_tot = .N),
        keyby = .(variable)][n_imp > 0]
