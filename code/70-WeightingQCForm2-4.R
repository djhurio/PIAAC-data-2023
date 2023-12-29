# WeightingQCForms 2-6

# Reset
rm(list = ls())
gc()


# SDIF DU - sample file
dat_sdif_sample <- fread(
  file = "../PIAAC-sample-2022/data2/sample_piaac_sdif.csvy",
  yaml = TRUE
)
dat_sdif_sample[, .N]
dat_sdif_sample[, CASEID := as.numeric(CASEID)]

# SDIF - data file after FW
dat_sdif <- fread(
  file = "result/CY2_Final_SDIF_LVA.csvy",
  yaml = TRUE
)
dat_sdif[, .N]

dat_sdif[, REGION := factor(
  x = REGION,
  levels = 1:6,
  labels = c("R", "P", "V", "K", "Z", "L")
)]

# WIF
dat_wif <- haven::read_sas(
  data_file = "data-weights/WIF_QCChecks_LVA.sas7bdat"
) |> setDT()
dat_wif[, .N]

intersect(names(dat_wif), names(dat_sdif_sample))
intersect(names(dat_wif), names(dat_sdif))

class(dat_sdif_sample$CASEID)
class(dat_sdif$CASEID)
class(dat_wif$CASEID)

merge(
  x = dat_sdif_sample[, .(CNTRYID, CASEID)],
  y = unique(dat_wif[, .(CNTRYID, CASEID)]),
  by = c("CNTRYID", "CASEID"),
  all = TRUE
)

merge(
  x = dat_sdif[, .(CNTRYID, CASEID, PERSID)],
  y = dat_wif[, .(CNTRYID, CASEID, PERSID)],
  by = c("CNTRYID", "CASEID", "PERSID"),
  all = TRUE
)




# WeightingQCForm-2_BaseWeights_Replicates_LVA
grep("^VAR", names(dat_sdif_sample))

dat_check1_1 <- openxlsx2::read_xlsx(
  file = "WeightingQCForms/WeightingQCForm-2_BaseWeights_Replicates_LVA.xlsx",
  sheet = "Check #1",
  rows = 2:172,
  skip_empty_cols = TRUE
) |> setDT()

dat_check1_1

dat_check1_1[, .(count = .N, sum = sum(FREQUENCY))]
dat_sdif_sample[, .N]

dat_check1_1[, .(count = .N, sum = sum(FREQUENCY)), keyby = .(VARSTRAT)]
dat_sdif_sample[, .N, keyby = .(STRAT_PSU)]



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


dat_sdif_sample[, summary(PROB_PSU)]

dat_sdif_sample[, CERTFLAG := as.integer(abs(PROB_PSU - 1) < .Machine$double.eps)]
dat_sdif_sample[, .N, keyby = .(STRAT_PSU, CERTFLAG)]
dat_sdif_sample[CERTFLAG == 1, .N, keyby = .(STRAT_PSU, ID_PSU)]


names(dat_check1_2a)
names(dat_check1_2b)

dat_sdif_sample[, .(CERTFLAG, STRAT_PSU, ID_PSU, SORT_PSU, ID_HH, SORT_HH)]

dat_sdif_sample[CERTFLAG == 0L, psu_str := STRAT_PSU]
dat_sdif_sample[CERTFLAG == 0L, psu_id  := ID_PSU]
dat_sdif_sample[CERTFLAG == 0L, psu_ord := SORT_PSU]

dat_sdif_sample[CERTFLAG == 1L, psu_str := ID_PSU]
dat_sdif_sample[CERTFLAG == 1L, psu_id  := ID_HH]
dat_sdif_sample[CERTFLAG == 1L, psu_ord := SORT_HH]

tab_check1_2 <- dat_sdif_sample[
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


dat_check4 <- openxlsx2::read_xlsx(
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

dat_sdif_sample[, sum(1 / (PROB_PSU * PROB_HH))]


dat_check4[, .(variable)] |> unique()

all(dat_check1_1[, paste0(
  "VARSTRAT", sprintf("%02d", VARSTRAT), "_VARUNIT", VARUNIT
)] == dat_check4[, variable] |> levels())



dat_sdif_sample[, .(psu_str, psu_id)]
tab_check1_2[, .(psu_str, psu_id, VARSTRAT, VARUNIT)]

dat_sdif_sample <- merge(
  x = dat_sdif_sample,
  y = tab_check1_2[, .(psu_str, psu_id, VARSTRAT, VARUNIT)],
  by = c("psu_str", "psu_id")
)

dat_sdif_sample[, sum(1 / PROB_PSU / PROB_HH)]

tab_weights <- dat_sdif_sample[
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
rm(g, k)


dat_sdif_sample[, .(CNTRYID, CASEID, VARSTRAT, VARUNIT)]
unique(dat_wif[, .(CNTRYID, CASEID, VARSTRAT, VARUNIT)])

test_varstr <- merge(
  x = dat_sdif_sample[, .(CNTRYID, CASEID, VARSTRAT, VARUNIT)],
  y = unique(dat_wif[, .(CNTRYID, CASEID, VARSTRAT, VARUNIT)]),
  by = c("CNTRYID", "CASEID"),
  all = TRUE
)
test_varstr[, all.equal(VARSTRAT.x, VARSTRAT.y, check.attributes = FALSE)]
test_varstr[, all.equal(VARUNIT.x, VARUNIT.y, check.attributes = FALSE)]
# VARUNITS are randomly assigned - I can't reproduce


# OK


# WeightingQCForm-3_BasicNRBA_Screener_LVA
# OK

# Weighting QC Form W-4 LVA
# https://piaac.ets.org/portal/weighting-qc-form-w-4-lva/
# WeightingQCForm-4_HH_LVA

dat_sdif[, .(CASEID)] |> unique()
dat_sdif[, .(CASEID, DISP_SCR)] |> unique()

dat_sdif_du <- unique(x = dat_sdif, by = "CASEID")
dat_sdif_du[, .N]
dat_sdif_sample[, .N]


# PIAAC_CY2(2023_05)Sampling Plan Part II Weighting
# Table 2-4.
# Classification of Sampled Households

dat_sdif_du[, .N, keyby = .(DISP_SCR)]

dat_sdif_du[, hh_class := car::recode(
  var = DISP_SCR,
  recodes = "
    c(1, 2) = 'R';
    c(7) = 'L';
    c(3, 9, 12, 13, 14, 15, 16) = 'NR';
    c(19, 22, 26, 28) = 'I';
    c(4, 5, 17, 20, 21, 24) = 'U'
  ",
  as.factor = TRUE,
  levels = c("R", "L", "NR", "I", "U")
)]

# Cases with falsified data (QCFLAG=2) should be treated as nonrespondents.
dat_sdif_du[, .N, keyby = .(QCFLAG)]
dat_sdif_du[QCFLAG == 2, .N, keyby = .(QCFLAG, DISP_SCR)]

dat_sdif_du[QCFLAG == 2 & DISP_SCR == 17, .N]
dat_sdif_du[QCFLAG == 2 & DISP_SCR == 17, hh_class := "NR"]
dat_sdif_du[QCFLAG == 2, .N, keyby = .(QCFLAG, DISP_SCR, hh_class)]


dat_sdif_du[, class(hh_class)]

dat_sdif_du[, .N, keyby = .(DISP_SCR, hh_class)]
dat_sdif_du[, .N, keyby = .(hh_class, DISP_SCR)]
dat_sdif_du[, .N, keyby = .(hh_class)]


# Include all sampled households except for those known to be ineligible
# (all except for DISP_SCR=19, 22, 26, 27, 28)
dat_sdif_du[hh_class != "I", .N]
# 23734

# Include responding households only (DISP_SCR=1, 2)
dat_sdif_du[hh_class == "R", .N]
# 7551

grep("^HH", names(dat_sdif_du), value = T)

weight_descr_stat <- function(x, digits = 2) {
  tab <- data.table(
    `Descriptive statistics` = c(
      "Number of cases with missing weights",
      "Number of cases with nonmissing and nonzero weights",
      "Sum of weights",
      "Mean of weights",
      "Minimum weight",
      "Maximum weight",
      "CV(weight)",
      "1+(CV(weight))^2"
    ),
    Value = c(
      sum(is.na(x)),
      sum(!is.na(x)),
      sum(x),
      mean(x),
      min(x),
      max(x),
      sd(x) / mean(x),
      1 + (sd(x) / mean(x)) ^ 2
    )
  )
  tab[, Value := sprintf(
    fmt = ifelse(.I > 2, glue::glue("%.{digits}f"), "%.0f"), 
    Value
  )]
  tab[, `Descriptive statistics` := factor(
    x = `Descriptive statistics`,
    levels = `Descriptive statistics`,
    labels = `Descriptive statistics`
  )]
  return(tab[])
}


# add cells and weights

names(dat_wif)
grep("^SPFWT", names(dat_wif), value = T, invert = T)
grep("CELL$", names(dat_wif), value = T)

dat_wif[, .N, keyby = .(HHUNKCELL)][order(-N)]
dat_wif[, .N, keyby = .(HHNRCELL)][order(-N)]
dat_wif[, .N, keyby = .(BQUNKCELL)][order(-N)]
dat_wif[, .N, keyby = .(SPNRCELL)][order(-N)]
dat_wif[, .N, keyby = .(SPLNRCELL)][order(-N)]

dat_sdif_du <- merge(
  x = dat_sdif_du,
  y = unique(dat_wif[, .(CASEID, HHUNKCELL, HHNRCELL,
                         HHBWT0_test = HHBWT0,
                         HHUEWT0_test = HHUEWT0,
                         HHNRWT0_test = HHNRWT0)]),
  by = "CASEID",
  all.x = T
)


# Screener base weight

dat_sdif_du[, HHBWT0 := round(1 / PROB_PSU / PROB_HH, 6)]
dat_sdif_du[, summary(HHBWT0)]
dat_sdif_du[, as.list(summary(HHBWT0)), keyby = .(hh_class)]

dat_sdif_du[hh_class != "I", weight_descr_stat(HHBWT0)]
dat_sdif_du[, all.equal(HHBWT0, HHBWT0_test, check.attributes = F)]

dat_sdif_du[, .(
  N = .N,
  WGT_SUM = sprintf("%.2f", sum(HHBWT0))
), keyby = .(hh_class)]



# Unknown eligibility adjustment
dat_sdif_du[, .N, keyby = .(HHUNKCELL)]

dat_sdif_du[, S_total := sum(HHBWT0),
            keyby = .(HHUNKCELL)]
dat_sdif_du[, S_known := sum(HHBWT0[hh_class != "U"]),
            keyby = .(HHUNKCELL)]
dat_sdif_du[, S_elig1 := sum(HHBWT0[hh_class %in% c("L", "R", "NR")]),
            keyby = .(HHUNKCELL)]

dat_sdif_du[hh_class == "I", F_1 := S_total / S_known]
dat_sdif_du[hh_class == "U", F_1 := S_elig1 / S_known]
dat_sdif_du[hh_class %in% c("L", "R", "NR"), F_1 := 1]

dat_sdif_du[, summary(F_1)]
dat_sdif_du[, table(hh_class, is.na(F_1))]

dat_sdif_du[, HHUEWT0 := round(HHBWT0 * F_1, 6)]

dat_sdif_du[, map(.SD, sum), .SDcols = c("HHBWT0", "HHUEWT0")]
dat_sdif_du[, map(.SD, sum), .SDcols = c("HHBWT0", "HHUEWT0"),
            keyby = .(hh_class)][, diff := HHUEWT0 - HHBWT0][]

dat_sdif_du[hh_class != "I", weight_descr_stat(HHUEWT0)]
dat_sdif_du[, all.equal(HHUEWT0, HHUEWT0_test, check.attributes = F)]

# dat_sdif_du[, as.character(all.equal(HHUEWT0, HHUEWT0_test, check.attributes = F)),
#             keyby = .(hh_class)]
# dat_sdif_du[, as.character(all.equal(HHUEWT0, HHUEWT0_test, check.attributes = F)),
#             keyby = .(hh_class, HHUNKCELL)][V1 != "TRUE"]

dat_sdif_du[, round(sum(HHUEWT0) - sum(HHUEWT0_test)),
            keyby = .(hh_class)]
dat_sdif_du[, .(CASEID, hh_class, HHUNKCELL,
                HHBWT0, HHUEWT0, HHUEWT0_test,
                diff = HHUEWT0 - HHUEWT0_test)][abs(diff) > 1][order(abs(diff))]



# Nonresponse adjustment

# U is treated as NR after the unknown eligibility adjustment
dat_sdif_du[, S_elig2 := sum(HHUEWT0[hh_class %in% c("R", "NR", "U")]),
            keyby = .(HHNRCELL)]
dat_sdif_du[, S_resp := sum(HHUEWT0[hh_class == "R"]),
            keyby = .(HHNRCELL)]

dat_sdif_du[hh_class %in% c("L", "I"), F_2 := 1]
dat_sdif_du[hh_class == "R", F_2 := S_elig2 / S_resp]
dat_sdif_du[hh_class %in% c("NR", "U"), F_2 := 0]

dat_sdif_du[, summary(F_2)]
dat_sdif_du[, table(hh_class, is.na(F_2))]

dat_sdif_du[, HHNRWT0 := round(HHBWT0 * F_1 * F_2, 6)]

dat_sdif_du[, summary(HHNRWT0)]
dat_sdif_du[, table(hh_class, is.na(HHNRWT0))]

dat_sdif_du[DISP_SCR %in% 1:2, weight_descr_stat(HHNRWT0)]

dat_sdif_du[, map(.SD, sum), .SDcols = c("HHBWT0", "HHUEWT0", "HHNRWT0")]
dat_sdif_du[
  , map(.SD, sum), .SDcols = c("HHBWT0", "HHUEWT0", "HHNRWT0"),
  keyby = .(hh_class)
][
  , diff_UE := HHUEWT0 - HHBWT0
][
  , diff_NR := HHNRWT0 - HHUEWT0
][]



grep("^SPFWT", names(dat_wif), value = T, invert = T)

dat_sdif_du[hh_class != "I", weight_descr_stat(HHBWT0)]
dat_sdif_du[hh_class != "I", weight_descr_stat(HHUEWT0)]
dat_sdif_du[hh_class == "R", weight_descr_stat(HHNRWT0)]

# tab_HHBWT0 <- dat_sdif_du[hh_class != "I", weight_descr_stat(HHBWT0_test),
#                           keyby = .(REGION)]
# tab_HHUEWT0 <- dat_sdif_du[hh_class != "I", weight_descr_stat(HHUEWT0_test),
#                            keyby = .(REGION)]
# tab_HHNRWT0 <- dat_sdif_du[hh_class != "I", weight_descr_stat(HHNRWT0_test),
#                            keyby = .(REGION)]

tab_HH_WT0 <- rbindlist(
  l = list(HHBWT0 = dat_sdif_du[hh_class != "I",
                                weight_descr_stat(HHBWT0_test),
                                keyby = .(REGION)],
           HHUEWT0 = dat_sdif_du[hh_class != "I",
                                 weight_descr_stat(HHUEWT0_test),
                                 keyby = .(REGION)],
           HHNRWT0 = dat_sdif_du[hh_class == "R",
                                 weight_descr_stat(HHNRWT0_test),
                                 keyby = .(REGION)]),
  idcol = "WT0"
)

tab_HH_WT0[, val := as.numeric(Value)]
tab_HH_WT0[, WT0 := factor(x = WT0, levels = c("HHBWT0", "HHUEWT0", "HHNRWT0"))]

ggplot(data = tab_HH_WT0) +
  geom_col(mapping = aes(x = REGION, y = val, fill = WT0), position = "dodge") +
  facet_wrap(facets = vars(`Descriptive statistics`), scales = "free_y") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(
  filename = "WeightingQCForms/WeightingQCForm-4_HH_LVA.pdf",
  scale = 2
)

tab_w4_2 <- openxlsx2::read_xlsx(
  file = "WeightingQCForms/WeightingQCForm-4_HH_LVA.xlsx",
  rows = 35:83,
  skip_empty_cols = T
) |> setDT()

tab_w4_2[, `Geographic area` := nafill(x = `Geographic area`, type = "locf")]

tab_w4_2 <- melt.data.table(
  data = tab_w4_2,
  id.vars = c("Geographic area", "Descriptive statistics")
)

all.equal(tab_w4_2[, round(value, 2)], tab_HH_WT0[, val])


tab_w4_3 <- openxlsx2::read_xlsx(
  file = "WeightingQCForms/WeightingQCForm-4_HH_LVA.xlsx",
  rows = 89:170,
  skip_empty_cols = T
) |> setDT()

tab_w4_3[, `Replicate number (#)` := factor(
  x = `Replicate number (#)`,
  levels = `Replicate number (#)`,
  labels = `Replicate number (#)`
)]

names(tab_w4_3)
tab_w4_3[, cor(.SD), .SDcols = -1]

ggplot(data = tab_w4_3) +
  geom_point(
    mapping = aes(x = `Base weighta\n(HHBWT#)`,
                  y = `Unknown eligibility adjusted weighta\n(HHUEWT#)`)) +
  theme_bw()

ggplot(data = tab_w4_3) +
  geom_point(
    mapping = aes(x = `Unknown eligibility adjusted weighta\n(HHUEWT#)`,
                  y = `Nonresponse adjusted weightb\n(HHNRWT#)`)) +
  theme_bw()

tab_w4_3 <- melt.data.table(
  data = tab_w4_3,
  id.vars = "Replicate number (#)"
)

tab_w4_3[, as.list(summary(value)), keyby = .(variable)]

ggplot() +
  geom_hline(
    mapping = aes(yintercept = value),
    data = tab_w4_3[`Replicate number (#)` == "Full Sample"]
  ) +
  geom_point(
    mapping = aes(x = `Replicate number (#)`, y = value),
    data = tab_w4_3[`Replicate number (#)` != "Full Sample"]
  ) +
  facet_grid(rows = vars(variable), scales = "free_y") +
  theme_bw()



# W4 Table 5.1
dat_sdif_du[, .(
  N = as.numeric(.N),
  WGT_SUM = sprintf("%.2f", sum(HHBWT0))
), keyby = .(hh_class)]


# W4 Table 5.3

dat_sdif_du[, .(
  N = as.numeric(.N),
  WGT_SUM = sprintf("%.2f", sum(HHUEWT0))
), keyby = .(hh_class)]

names(dat_sdif_du)

tab_w4_5_3 <- dat_sdif_du[, .(
  N = as.numeric(.N),
  WGT_SUM = sum(HHUEWT0)
), keyby = .(HHUNKCELL, hh_class, ADJ_FACTOR = F_1)] |> melt.data.table(
  id.vars = c("HHUNKCELL", "hh_class")
)

tab_w4_5_3[, variable := factor(x = variable, levels = c("N", "ADJ_FACTOR", "WGT_SUM"))]
tab_w4_5_3[, levels(variable)]
setorder(tab_w4_5_3, HHUNKCELL, hh_class, variable)

tab_w4_5_3[, ADJ_CELL := as.integer(factor(HHUNKCELL))]



tab_w4_5_3_test <- openxlsx2::read_xlsx(
  file = "WeightingQCForms/WeightingQCForm-4_HH_LVA.xlsx",
  rows = 222:335,
  skip_empty_cols = T,
  na.strings = ".",
  fill_merged_cells = T
) |> setDT()

names(tab_w4_5_3_test)

setnames(
  tab_w4_5_3_test,
  c(
    "ADJ_CELL",
    paste(
      rep(dat_sdif_du[, levels(hh_class)], each = 3),
      c("N", "ADJ_FACTOR", "WGT_SUM"),
      sep = "|"
    ),
    paste("Total", c("N", "WGT_SUM"), sep = "|")
  )
)

tab_w4_5_3_test <- melt.data.table(
  data = tab_w4_5_3_test[, .SD, .SDcols = !patterns("Total")],
  id.vars = "ADJ_CELL",
  na.rm = T
)

tab_w4_5_3_test[, c("hh_class", "variable") := tstrsplit(
  x = variable, split = "|", fixed = T
)]

tab_w4_5_3_test[, hh_class := factor(
  x = hh_class, levels = dat_sdif_du[, levels(hh_class)]
)]
tab_w4_5_3_test[, variable := factor(
  x = variable, levels = c("N", "ADJ_FACTOR", "WGT_SUM")
)]

setorder(tab_w4_5_3_test, ADJ_CELL, hh_class, variable)

tab_w4_5_3_test
tab_w4_5_3

tab_w4_5_3_tmp <- merge(
  x = tab_w4_5_3,
  y = tab_w4_5_3_test,
  by = c("ADJ_CELL", "hh_class", "variable"),
  all = TRUE
)

tab_w4_5_3_tmp[, all.equal(value.x, value.y)]

tab_w4_5_3_tmp[is.na(value.x)]


# W4 Table 5.5

dat_sdif_du[, .(
  N = as.numeric(.N),
  WGT_SUM = sprintf("%.2f", sum(HHNRWT0))
), keyby = .(hh_class)]

names(dat_sdif_du)

tab_w4_5_5 <- dat_sdif_du[, .(
  N = as.numeric(.N),
  WGT_SUM = sum(HHNRWT0)
), keyby = .(HHNRCELL, hh_class, ADJ_FACTOR = F_2)] |> melt.data.table(
  id.vars = c("HHNRCELL", "hh_class")
)

tab_w4_5_5[variable == "WGT_SUM" & value == 0]
tab_w4_5_5 <- tab_w4_5_5[variable != "WGT_SUM" | value > 0]
tab_w4_5_5[variable == "WGT_SUM" & value == 0]


tab_w4_5_5[, variable := factor(x = variable, levels = c("N", "ADJ_FACTOR", "WGT_SUM"))]
tab_w4_5_5[, levels(variable)]
setorder(tab_w4_5_5, HHNRCELL, hh_class, variable)

tab_w4_5_5[, ADJ_CELL := as.integer(factor(HHNRCELL))]




tab_w4_5_5_test <- openxlsx2::read_xlsx(
  file = "WeightingQCForms/WeightingQCForm-4_HH_LVA.xlsx",
  rows = 353:423,
  skip_empty_cols = T,
  na.strings = ".",
  fill_merged_cells = T
) |> setDT()

names(tab_w4_5_5_test)

setnames(
  tab_w4_5_5_test,
  c(
    "ADJ_CELL",
    paste(
      rep(dat_sdif_du[, levels(hh_class)], each = 3),
      c("N", "ADJ_FACTOR", "WGT_SUM"),
      sep = "|"
    ),
    paste("Total", c("N", "WGT_SUM"), sep = "|")
  )
)

tab_w4_5_5_test <- melt.data.table(
  data = tab_w4_5_5_test[, .SD, .SDcols = !patterns("Total")],
  id.vars = "ADJ_CELL",
  na.rm = T
)

tab_w4_5_5_test[, c("hh_class", "variable") := tstrsplit(
  x = variable, split = "|", fixed = T
)]

tab_w4_5_5_test[, hh_class := factor(
  x = hh_class, levels = dat_sdif_du[, levels(hh_class)]
)]
tab_w4_5_5_test[, variable := factor(
  x = variable, levels = c("N", "ADJ_FACTOR", "WGT_SUM")
)]

setorder(tab_w4_5_5_test, ADJ_CELL, hh_class, variable)

tab_w4_5_5_test
tab_w4_5_5

tab_w4_5_5_tmp <- merge(
  x = tab_w4_5_5,
  y = tab_w4_5_5_test,
  by = c("ADJ_CELL", "hh_class", "variable"),
  all = TRUE
)

tab_w4_5_5_tmp[, all.equal(value.x, value.y)]

tab_w4_5_5_tmp[is.na(value.x)]
tab_w4_5_5_tmp[is.na(value.y)]



# Weighting QC Form W-5 (Screener) LVA
# https://piaac.ets.org/portal/weighting-qc-form-w-5-screener-lva/
# WeightingQCForm-5_Person (Screener)_LVA.xlsx

dat_sdif
dat_wif

names(dat_sdif)
names(dat_wif)

intersect(names(dat_sdif), names(dat_wif))

setkeyv(dat_sdif, intersect(names(dat_sdif), names(dat_wif)))
setkeyv(dat_wif, intersect(names(dat_sdif), names(dat_wif)))

dat_sdif[, .N] == dat_wif[, .N]

dat_pers <- merge(
  x = dat_sdif,
  y = dat_wif,
  all = T
)

dat_pers

grep("^SP", names(dat_pers), value = T)

dat_pers[, .(SPBWT0, SPNRWT0, SPLNRWT0, SPTWT0, SPFWT0)]

dat_pers[, .(DISP_CIBQ, DISP_MAIN, DISP_DS)]

dat_pers[, .N, keyby = .(DISP_CIBQ)]
dat_pers[, .N, keyby = .(DISP_MAIN)]
dat_pers[, .N, keyby = .(DISP_DS)]

dat_pers[DISP_CIBQ == 1, .N, keyby = .(DISP_MAIN)]

# NOTE: The factors and weights shown here are for a household k or person l.
# The households and persons can be classified as
# R: respondent,
# L: literacy-related nonrespondent for the screener,
# L1: BQ literacy-related nonrespondent that
# completed the Doorstep Interview or assessment literacy-related nonrespondent,
# L2: BQ literacy-related nonrespondent that
# did not complete the Doorstep Interview,
# NR: nonliteracy-related nonrespondent,
# I: ineligible,
# D: sampled person with a disability, or
# U: unknown eligibility.
# S represents the sum of the prior-stage weights over records
# in the same adjustment cell as household k or person l,
# S’ is the sum of screener base weights, and
# S* is the control total for the cell.
# P represents the selection probability.

# Duplicate person records (DISP_CIBQ=27) should be deleted and probabilities
# of selection adjusted for associated records.
# Otherwise, they can be treated as ineligibles
# if the total count of duplicates is less than one per cent.

dat_pers[!is.na(DISP_CIBQ), .N, keyby = .(DISP_CIBQ)][
  , P := prop.table(N) * 100
][]

if ("pers_cat" %in% names(dat_pers)) dat_pers[, pers_cat := NA]
dat_pers[DISP_CIBQ  %in% 1L & !DISP_MAIN %in% 7:9, pers_cat := "R"]
dat_pers[(DISP_CIBQ %in% 7L &  DISP_DS %in% 1L) |
         (DISP_CIBQ %in% 1L &  DISP_MAIN %in% 7:9), pers_cat := "L1"]
dat_pers[DISP_CIBQ %in% 7:9 & !DISP_DS %in% 1L, pers_cat := "L2"]
dat_pers[DISP_CIBQ %in% c(3:5, 14L, 17L, 21L, 23:24, 90L), pers_cat := "NR"]
dat_pers[DISP_CIBQ %in% c(12:13, 15:16), pers_cat := "D"]
dat_pers[DISP_CIBQ %in% c(18L, 25L, 27L), pers_cat := "I"]

# Cases with falsified data (QCFLAG=2) should be treated as nonrespondents.
dat_pers[!is.na(DISP_CIBQ) & QCFLAG == 2L, .N]
dat_pers[!is.na(DISP_CIBQ) & QCFLAG == 2L, pers_cat := "NR"]

dat_pers[, pers_cat := factor(
  x = as.character(pers_cat),
  levels = c("R", "L1", "L2", "NR", "D", "I")
)]

dat_pers[!is.na(DISP_CIBQ), .N, keyby = .(QCFLAG)]
dat_pers[!is.na(DISP_CIBQ) & QCFLAG == 2L, .N, keyby = .(pers_cat)]

dat_pers[!is.na(pers_cat), .N]
dat_pers[!is.na(pers_cat), .N, keyby = .(pers_cat)]
dat_pers[!is.na(pers_cat), .N, keyby = .(pers_cat, DISP_CIBQ)]


# Base weight
dat_pers[, all.equal(SPBWT0, round(HHNRWT0 / PROB_PERS, 6),
                     check.attributes = FALSE)]
# TRUE

dat_pers[, sum(SPBWT0, na.rm = TRUE)]
dat_pers[, sum(SPBWT0, na.rm = TRUE), keyby = .(pers_cat)]
# dat_pers[is.na(pers_cat) & !is.na(SPBWT0), .N]
# dat_pers[is.na(pers_cat) & !is.na(SPBWT0),
#          .(CASEID, PERSID, DISP_CIBQ, DISP_MAIN, DISP_DS, QCFLAG)]

# Nonliteracy-related Nonresponse adjustment
names(dat_wif)
dat_pers[, .N, keyby = .(SPNRCELL)]

dat_pers[, .N, keyby = .(pers_cat = !is.na(pers_cat),
                         SPNRCELL = !is.na(SPNRCELL),
                         SPBWT0   = !is.na(SPBWT0))]

dat_pers[!is.na(pers_cat) & is.na(SPNRCELL), .N]
dat_pers[!is.na(pers_cat) & is.na(SPNRCELL),
         .(CASEID, PERSID, DISP_SCR, DISP_CIBQ, DISP_MAIN, DISP_DS, QCFLAG)]

# dat_pers[, c("S_R", "S_NR", "S_D") := NULL]
dat_pers[, S_R  := sum(SPBWT0 * (pers_cat == "R")),  by = .(SPNRCELL)]
dat_pers[, S_NR := sum(SPBWT0 * (pers_cat == "NR")), by = .(SPNRCELL)]
dat_pers[, S_D  := sum(SPBWT0 * (pers_cat == "D")),  by = .(SPNRCELL)]
# dat_pers[, class(S_R)]

dat_pers[pers_cat %in% c("L1", "L2", "I"), F_3 := 1]
dat_pers[pers_cat %in% "R", F_3 := (S_R + S_NR + S_D) / S_R]
dat_pers[pers_cat %in% c("NR", "D"), F_3 := 0]

dat_pers[!is.na(pers_cat), as.list(summary(F_3)), keyby = .(pers_cat)]

dat_pers[, all.equal(SPNRWT0, round(SPBWT0 * F_3, 6), check.attributes = FALSE)]
