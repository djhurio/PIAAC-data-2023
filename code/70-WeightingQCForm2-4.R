# WeightingQCForms 2-4

# Reset
rm(list = ls())
gc()

# SDIF
dat_sdif_du <- fread(
  file = "../PIAAC-sample-2022/data2/sample_piaac_sdif.csvy",
  yaml = TRUE
)


# WeightingQCForm-2_BaseWeights_Replicates_LVA.xlsx
grep("^VAR", names(dat_sdif_du))

dat_check1_1 <- openxlsx2::read_xlsx(
  file = "WeightingQCForms/WeightingQCForm-2_BaseWeights_Replicates_LVA.xlsx",
  sheet = "Check #1",
  rows = 2:172,
  skip_empty_cols = TRUE
) |> setDT()

dat_check1_1

dat_check1_1[, .(count = .N, sum = sum(FREQUENCY))]
dat_sdif_du[, .N]

dat_check1_1[, .(count = .N, sum = sum(FREQUENCY)), keyby = .(VARSTRAT)]
dat_sdif_du[, .N, keyby = .(STRAT_PSU)]



dat_check1_2a <- openxlsx2::read_xlsx(
  file = "WeightingQCForms/WeightingQCForm-2_BaseWeights_Replicates_LVA.xlsx",
  sheet = "Check #1",
  rows = 176:376,
  skip_empty_cols = TRUE
) |> setDT()

dat_check1_2b <- openxlsx2::read_xlsx(
  file = "WeightingQCForms/WeightingQCForm-2_BaseWeights_Replicates_LVA.xlsx",
  sheet = "Check #1",
  rows = 381:581,
  skip_empty_cols = TRUE
) |> setDT()

dat_check1_2a
dat_check1_2b

dat_check1_2 <- rbindlist(list(dat_check1_2a, dat_check1_2b), fill = TRUE)

dat_check1_2[, .N]
dat_check1_2[, .N, keyby = .(CERTFLAG, FREQUENCY)]


dat_sdif_du[, summary(PROB_PSU)]

dat_sdif_du[, CERTFLAG := as.integer(abs(PROB_PSU - 1) < .Machine$double.eps)]
dat_sdif_du[, .N, keyby = .(STRAT_PSU, CERTFLAG)]
dat_sdif_du[CERTFLAG == 1, .N, keyby = .(STRAT_PSU, ID_PSU)]


names(dat_check1_2a)
names(dat_check1_2b)

dat_sdif_du[, .(CERTFLAG, STRAT_PSU, ID_PSU, SORT_PSU, ID_HH, SORT_HH)]

dat_sdif_du[CERTFLAG == 0L, psu_str := STRAT_PSU]
dat_sdif_du[CERTFLAG == 0L, psu_id  := ID_PSU]
dat_sdif_du[CERTFLAG == 0L, psu_ord := SORT_PSU]

dat_sdif_du[CERTFLAG == 1L, psu_str := ID_PSU]
dat_sdif_du[CERTFLAG == 1L, psu_id  := ID_HH]
dat_sdif_du[CERTFLAG == 1L, psu_ord := SORT_HH]

tab_check1_2 <- dat_sdif_du[
  ,
  .(FREQUENCY = .N),
  keyby = .(CERTFLAG, STRAT_PSU, psu_str, psu_id, psu_ord)
]

tab_check1_2[, .N, keyby = .(CERTFLAG, FREQUENCY)]
tab_check1_2[, .N, keyby = .(CERTFLAG, FREQUENCY, STRAT_PSU, psu_str)]

tab_check1_2[, i := 1:.N, by = .(STRAT_PSU)]
tab_check1_2[, n :=   .N, by = .(STRAT_PSU)]

tab_check1_2[, VARSTRAT := cumsum(i %% 2 == 1L & i < n), by = .(CERTFLAG)]
tab_check1_2[, VARUNIT := 1:.N, by = .(CERTFLAG, VARSTRAT)]

tab_check1_2[, i := NULL]
tab_check1_2[, n := NULL]

tab_check1_2[, VARSTRAT := (VARSTRAT - 1) %% ifelse(CERTFLAG == 0L, 80, 16) + 1]

tab_check1_2[CERTFLAG == 0L] |> first(200)
tab_check1_2[CERTFLAG == 1L] |> first(200)

tab_check1_2[CERTFLAG == 0L] |> first(50)
tab_check1_2[CERTFLAG == 1L] |> first(50)

tab_check1_1 <- tab_check1_2[
  ,
  map(.SD, sum),
  .SDcols = "FREQUENCY",
  keyby = .(VARSTRAT, VARUNIT)
]

dat_check1_1
tab_check1_1

dat_check1_1[, map(.SD, sum), .SDcols = "FREQUENCY", keyby = .(VARSTRAT)]
tab_check1_1[, map(.SD, sum), .SDcols = "FREQUENCY", keyby = .(VARSTRAT)]

merge(
  x = dat_check1_1,
  y = tab_check1_1,
  by = c("VARSTRAT", "VARUNIT"),
  all = TRUE
)[, all.equal(FREQUENCY.x, FREQUENCY.y)]

setkeyv(x = dat_check1_1, cols = key(tab_check1_1))
all.equal(dat_check1_1, tab_check1_1)


dat_check4 <- read_xlsx(
  file = "WeightingQCForms/WeightingQCForm-2_BaseWeights_Replicates_LVA.xlsx",
  sheet = "Check #4",
  start_row = 5,
  col_names = FALSE
) |> setDT()

varnames <- paste0(
  "VARSTRAT", rep(sprintf("%02d", 1:80), each = 3), "_VARUNIT", 1:3
)
length(varnames) == 80 * 3

setnames(
  x = dat_check4,
  new = c("REPLICATES", varnames)
)

map_chr(dat_check4, class)

for (v in varnames) {
  y <- dat_check4[[v]]
  if (is.character(y)) set(
    x = dat_check4,
    j = v,
    value = as.numeric(y)
  )
}
rm(v, y)

dat_check4[, REPLICATES := factor(x = REPLICATES, levels = REPLICATES)]
dat_check4[, REPLICATES]

dat_check4 <- melt.data.table(
  data = dat_check4,
  id.vars = "REPLICATES",
  na.rm = TRUE
) |> setorder() |> print()

dat_check4[, .N]
dat_check4[, .N, keyby = .(REPLICATES)]
dat_check4[, .N, keyby = .(variable)]

dat_check4[, sum(value), keyby = .(REPLICATES)]

dat_sdif_du[, sum(1 / (PROB_PSU * PROB_HH))]


dat_check4[, .(variable)] |> unique()

all(dat_check1_1[, paste0(
  "VARSTRAT", sprintf("%02d", VARSTRAT), "_VARUNIT", VARUNIT
)] == dat_check4[, variable] |> levels())



dat_sdif_du[, .(psu_str, psu_id)]
tab_check1_2[, .(psu_str, psu_id, VARSTRAT, VARUNIT)]

dat_sdif_du <- merge(
  x = dat_sdif_du,
  y = tab_check1_2[, .(psu_str, psu_id, VARSTRAT, VARUNIT)],
  by = c("psu_str", "psu_id")
)

dat_sdif_du[, sum(1 / PROB_PSU / PROB_HH)]

tab_weights <- dat_sdif_du[
  ,
  sum(1 / PROB_PSU / PROB_HH),
  keyby = .(VARSTRAT, VARUNIT)
]

tab_weights[, variable := paste0(
  "VARSTRAT", sprintf("%02d", VARSTRAT), "_VARUNIT", VARUNIT
)]

dat_check4[REPLICATES == "FULL SAMPLE BASE WEIGHT"]

merge(
  x = tab_weights,
  y = dat_check4[REPLICATES == "FULL SAMPLE BASE WEIGHT"],
  by = "variable"
)[, all.equal(round(V1, 2), value)]


g <- 80L
k <- 0.3
1 / (g * (1 - k) ^ 2)
1 / g


# WeightingQCForm-3_BasicNRBA_Screener_LVA

# SDIF
dat_sdif <- fread("result/CY2_Final_SDIF_LVA.csvy", yaml = TRUE)
dat_sdif[, .N]
