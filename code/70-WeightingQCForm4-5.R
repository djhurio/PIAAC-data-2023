# Weighting QC Forms W-4 & W-5

# Options
options(max.print = 10e3)
getOption("max.print")

# Reset
rm(list = ls())
gc()


# Function for weight descriptive statistics
weight_descr_stat <- function(x, digits = 2) {
  
  y <- x[x > 0]
  cv <- sd(y) / mean(y)
  
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
      sum(x > 0),
      sum(y),
      mean(y),
      min(y),
      max(y),
      cv,
      1 + cv ^ 2
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



# Weighting QC Form W-4 LVA
# https://piaac.ets.org/portal/weighting-qc-form-w-4-lva/
# WeightingQCForm-4_HH_LVA

# SDIF - final
dat_sdif <- fread(
  file = "result/CY2_Final_SDIF_LVA.csvy",
  yaml = TRUE
)
dat_sdif[, .N]

# WIF
dat_wif <- haven::read_sas(
  data_file = "data-weights/WIF_QCChecks_LVA.sas7bdat"
) |> setDT()
dat_wif[, .N]

intersect(names(dat_wif), names(dat_sdif))

# Number of persons by DU
dat_sdif[, pers_n := .N, by = .(CASEID)]
dat_sdif[, .N, keyby = .(pers_n)]


# PIAAC_CY2(2023_05)Sampling Plan Part II Weighting
# Table 2-4.
# Classification of Sampled Households

dat_sdif[, .N, keyby = .(DISP_SCR)]

dat_sdif[, hh_cat := car::recode(
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
dat_sdif[, .N, keyby = .(QCFLAG)]
dat_sdif[QCFLAG == 2, .N, keyby = .(QCFLAG, DISP_SCR)]
# The 3 cases with QCFLAG=2 and DISP_SCR=1 were falsified at the BQ stage
# instead of Screener, so they were treated as
# Screener respondents and BQ nonrespondents.

dat_sdif[QCFLAG == 2 & DISP_SCR == 17, .N]
dat_sdif[QCFLAG == 2 & DISP_SCR == 17, hh_cat := "NR"]
dat_sdif[QCFLAG == 2, .N, keyby = .(QCFLAG, DISP_SCR, hh_cat)]


dat_sdif[, class(hh_cat)]

dat_sdif[, .N, keyby = .(DISP_SCR, hh_cat)]
dat_sdif[, .N, keyby = .(hh_cat, DISP_SCR)]
dat_sdif[, .N, keyby = .(hh_cat)]



dat_sdif[, .(CASEID)] |> unique()
dat_sdif[, .(CASEID, DISP_SCR, hh_cat)] |> unique()

dat_sdif_du <- unique(x = dat_sdif, by = "CASEID")
dat_sdif_du[, .N]

# Include all sampled households except for those known to be ineligible
# (all except for DISP_SCR=19, 22, 26, 27, 28)
dat_sdif_du[hh_cat != "I", .N]
# 23734

# Include responding households only (DISP_SCR=1, 2)
dat_sdif_du[hh_cat == "R", .N]
# 7551

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
dat_sdif_du[, as.list(summary(HHBWT0)), keyby = .(hh_cat)]

dat_sdif_du[hh_cat != "I", weight_descr_stat(HHBWT0)]

if (
  !isTRUE(dat_sdif_du[, all.equal(HHBWT0, HHBWT0_test, check.attributes = F)])
) {
  stop("HHBWT0 does not match")
}

dat_sdif_du[, .(
  N = .N,
  WGT_SUM = sprintf("%.2f", sum(HHBWT0))
), keyby = .(hh_cat)]



# Unknown eligibility adjustment
dat_sdif_du[, .N, keyby = .(HHUNKCELL)]

dat_sdif_du[, S_total := sum(HHBWT0),
            keyby = .(HHUNKCELL)]
dat_sdif_du[, S_known := sum(HHBWT0[hh_cat != "U"]),
            keyby = .(HHUNKCELL)]
dat_sdif_du[, S_elig1 := sum(HHBWT0[hh_cat %in% c("L", "R", "NR")]),
            keyby = .(HHUNKCELL)]

dat_sdif_du[hh_cat == "I", F_1 := S_total / S_known]
dat_sdif_du[hh_cat == "U", F_1 := S_elig1 / S_known]
dat_sdif_du[hh_cat %in% c("L", "R", "NR"), F_1 := 1]

dat_sdif_du[, summary(F_1)]
dat_sdif_du[, table(hh_cat, is.na(F_1))]

dat_sdif_du[, HHUEWT0 := round(HHBWT0 * F_1, 6)]

dat_sdif_du[, map(.SD, sum), .SDcols = c("HHBWT0", "HHUEWT0")]
dat_sdif_du[, map(.SD, sum), .SDcols = c("HHBWT0", "HHUEWT0"),
            keyby = .(hh_cat)][, diff := HHUEWT0 - HHBWT0][]

dat_sdif_du[hh_cat != "I", weight_descr_stat(HHUEWT0)]

if (
  !isTRUE(dat_sdif_du[, all.equal(HHUEWT0, HHUEWT0_test, check.attributes = F)])
) {
  stop("HHUEWT0 does not match")
}

# dat_sdif_du[, as.character(all.equal(HHUEWT0, HHUEWT0_test, check.attributes = F)),
#             keyby = .(hh_cat)]
# dat_sdif_du[, as.character(all.equal(HHUEWT0, HHUEWT0_test, check.attributes = F)),
#             keyby = .(hh_cat, HHUNKCELL)][V1 != "TRUE"]
# dat_sdif_du[, round(sum(HHUEWT0) - sum(HHUEWT0_test)),
#             keyby = .(hh_cat)]
# dat_sdif_du[, .(CASEID, hh_cat, HHUNKCELL,
#                 HHBWT0, HHUEWT0, HHUEWT0_test,
#                 diff = HHUEWT0 - HHUEWT0_test)][abs(diff) > 1][order(abs(diff))]



# Nonresponse adjustment

# U is treated as NR after the unknown eligibility adjustment
dat_sdif_du[, S_elig2 := sum(HHUEWT0[hh_cat %in% c("R", "NR", "U")]),
            keyby = .(HHNRCELL)]
dat_sdif_du[, S_resp := sum(HHUEWT0[hh_cat == "R"]),
            keyby = .(HHNRCELL)]

dat_sdif_du[hh_cat %in% c("L", "I"), F_2 := 1]
dat_sdif_du[hh_cat == "R", F_2 := S_elig2 / S_resp]
dat_sdif_du[hh_cat %in% c("NR", "U"), F_2 := 0]

dat_sdif_du[, summary(F_2)]
dat_sdif_du[, table(hh_cat, is.na(F_2))]

dat_sdif_du[, HHNRWT0 := round(HHBWT0 * F_1 * F_2, 6)]

dat_sdif_du[, summary(HHNRWT0)]
dat_sdif_du[, table(hh_cat, is.na(HHNRWT0))]

if (
  !isTRUE(dat_sdif_du[, all.equal(HHNRWT0, HHNRWT0_test, check.attributes = F)])
) {
  stop("HHNRWT0 does not match")
}

dat_sdif_du[DISP_SCR %in% 1:2, weight_descr_stat(HHNRWT0)]

dat_sdif_du[, map(.SD, sum), .SDcols = c("HHBWT0", "HHUEWT0", "HHNRWT0")]
dat_sdif_du[
  , map(.SD, sum), .SDcols = c("HHBWT0", "HHUEWT0", "HHNRWT0"),
  keyby = .(hh_cat)
][
  , diff_UE := HHUEWT0 - HHBWT0
][
  , diff_NR := HHNRWT0 - HHUEWT0
][]



grep("^SPFWT", names(dat_wif), value = T, invert = T)

# dat_sdif_du[hh_cat != "I", weight_descr_stat(HHBWT0)]
# dat_sdif_du[hh_cat != "I", weight_descr_stat(HHUEWT0)]
# dat_sdif_du[hh_cat == "R", weight_descr_stat(HHNRWT0)]

tab_HH_WT0 <- rbindlist(
  l = list(HHBWT0 = dat_sdif_du[hh_cat != "I",
                                weight_descr_stat(HHBWT0_test),
                                keyby = .(REGION)],
           HHUEWT0 = dat_sdif_du[hh_cat != "I",
                                 weight_descr_stat(HHUEWT0_test),
                                 keyby = .(REGION)],
           HHNRWT0 = dat_sdif_du[hh_cat == "R",
                                 weight_descr_stat(HHNRWT0_test),
                                 keyby = .(REGION)]),
  idcol = "WT0"
)

tab_HH_WT0[, val := as.numeric(Value)]
tab_HH_WT0[, WT0 := factor(x = WT0, levels = c("HHBWT0", "HHUEWT0", "HHNRWT0"))]

pl_w4 <- ggplot(data = tab_HH_WT0) +
  geom_col(mapping = aes(x = REGION, y = val, fill = WT0), position = "dodge") +
  facet_wrap(facets = vars(`Descriptive statistics`), scales = "free_y") +
  theme_bw() +
  theme(legend.position = "bottom")

cairo_pdf(
  filename = "WeightingQCForms/WeightingQCForm-4_HH_LVA.pdf",
  width = 16,
  height = 9
)
print(pl_w4)
dev.off()


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

if (!isTRUE(all.equal(tab_w4_2[, round(value, 2)], tab_HH_WT0[, val]))) {
  stop("Check table 2 at W4")
}


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
), keyby = .(hh_cat)]


# W4 Table 5.3

dat_sdif_du[, .(
  N = as.numeric(.N),
  WGT_SUM = sprintf("%.2f", sum(HHUEWT0))
), keyby = .(hh_cat)]

names(dat_sdif_du)

tab_w4_5_3 <- dat_sdif_du[, .(
  N = as.numeric(.N),
  WGT_SUM = sum(HHUEWT0)
), keyby = .(HHUNKCELL, hh_cat, ADJ_FACTOR = F_1)] |> melt.data.table(
  id.vars = c("HHUNKCELL", "hh_cat")
)

tab_w4_5_3[, variable := factor(x = variable, levels = c("N", "ADJ_FACTOR", "WGT_SUM"))]
tab_w4_5_3[, levels(variable)]
setorder(tab_w4_5_3, HHUNKCELL, hh_cat, variable)

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
      rep(dat_sdif_du[, levels(hh_cat)], each = 3),
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

tab_w4_5_3_test[, c("hh_cat", "variable") := tstrsplit(
  x = variable, split = "|", fixed = T
)]

tab_w4_5_3_test[, hh_cat := factor(
  x = hh_cat, levels = dat_sdif_du[, levels(hh_cat)]
)]
tab_w4_5_3_test[, variable := factor(
  x = variable, levels = c("N", "ADJ_FACTOR", "WGT_SUM")
)]

setorder(tab_w4_5_3_test, ADJ_CELL, hh_cat, variable)

tab_w4_5_3_test
tab_w4_5_3

tab_w4_5_3_tmp <- merge(
  x = tab_w4_5_3,
  y = tab_w4_5_3_test,
  by = c("ADJ_CELL", "hh_cat", "variable"),
  all = TRUE
)

if (!isTRUE(tab_w4_5_3_tmp[, all.equal(value.x, value.y)])) {
  stop("Check table 5.3 at W4")
}

tab_w4_5_3_tmp[is.na(value.x)]


# W4 Table 5.5

dat_sdif_du[, .(
  N = as.numeric(.N),
  WGT_SUM = sprintf("%.2f", sum(HHNRWT0))
), keyby = .(hh_cat)]

names(dat_sdif_du)

tab_w4_5_5 <- dat_sdif_du[, .(
  N = as.numeric(.N),
  WGT_SUM = sum(HHNRWT0)
), keyby = .(HHNRCELL, hh_cat, ADJ_FACTOR = F_2)] |> melt.data.table(
  id.vars = c("HHNRCELL", "hh_cat")
)

tab_w4_5_5[variable == "WGT_SUM" & value == 0]
tab_w4_5_5 <- tab_w4_5_5[variable != "WGT_SUM" | value > 0]
tab_w4_5_5[variable == "WGT_SUM" & value == 0]


tab_w4_5_5[, variable := factor(x = variable, levels = c("N", "ADJ_FACTOR", "WGT_SUM"))]
tab_w4_5_5[, levels(variable)]
setorder(tab_w4_5_5, HHNRCELL, hh_cat, variable)

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
      rep(dat_sdif_du[, levels(hh_cat)], each = 3),
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

tab_w4_5_5_test[, c("hh_cat", "variable") := tstrsplit(
  x = variable, split = "|", fixed = T
)]

tab_w4_5_5_test[, hh_cat := factor(
  x = hh_cat, levels = dat_sdif_du[, levels(hh_cat)]
)]
tab_w4_5_5_test[, variable := factor(
  x = variable, levels = c("N", "ADJ_FACTOR", "WGT_SUM")
)]

setorder(tab_w4_5_5_test, ADJ_CELL, hh_cat, variable)

tab_w4_5_5_test
tab_w4_5_5

tab_w4_5_5_tmp <- merge(
  x = tab_w4_5_5,
  y = tab_w4_5_5_test,
  by = c("ADJ_CELL", "hh_cat", "variable"),
  all = TRUE
)

if (!isTRUE(tab_w4_5_5_tmp[, all.equal(value.x, value.y)])) {
  stop("Check table 5.5 at W4")
}

tab_w4_5_5_tmp[is.na(value.x)]
tab_w4_5_5_tmp[is.na(value.y)]


# Remove all W4 tables and plots
rm(list = ls(pattern = "w4"))
rm(dat_sdif_du)
rm(tab_HH_WT0)


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

dat_pers[, .N]
dat_pers[, .N, keyby = .(PROB_PERS = is.na(PROB_PERS),
                         DISP_CIBQ = is.na(DISP_CIBQ))]

grep("^SP", names(dat_pers), value = T)

dat_pers[, .N, .(HHBWT0   = !is.na(HHBWT0),
                 HHUEWT0  = !is.na(HHUEWT0),
                 HHNRWT0  = !is.na(HHNRWT0),
                 SPBWT0   = !is.na(SPBWT0),
                 SPNRWT0  = !is.na(SPNRWT0),
                 SPLNRWT0 = !is.na(SPLNRWT0),
                 SPTWT0   = !is.na(SPTWT0),
                 SPFWT0   = !is.na(SPFWT0))]

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

dat_pers[, .N, keyby = .(hh_cat, pers_cat)]


# Base weight (SPBWT0)
if (dat_pers[, !isTRUE(all.equal(SPBWT0, round(HHNRWT0 / PROB_PERS, 6),
                                 check.attributes = FALSE))]) {
  stop("Check SPBWT0")
}
# TRUE

dat_pers[, sum(SPBWT0, na.rm = TRUE)]
dat_pers[, sum(SPBWT0, na.rm = TRUE), keyby = .(pers_cat)]

dat_pers[pers_cat != "I", weight_descr_stat(SPBWT0)]


# Nonliteracy-related Nonresponse adjustment (SPNRWT0)
names(dat_wif)
dat_pers[, .N, keyby = .(SPNRCELL)]

dat_pers[, .N, keyby = .(pers_cat = !is.na(pers_cat),
                         SPNRCELL = !is.na(SPNRCELL),
                         SPBWT0   = !is.na(SPBWT0))]

# dat_pers[!is.na(pers_cat) & is.na(SPNRCELL), .N]
# dat_pers[!is.na(pers_cat) & is.na(SPNRCELL),
#          .(CASEID, PERSID, DISP_SCR, DISP_CIBQ, DISP_MAIN, DISP_DS, QCFLAG)]

# dat_pers[, c("S_R", "S_NR", "S_D") := NULL]
dat_pers[!is.na(SPNRCELL),
         S_R  := sum(SPBWT0 * (pers_cat == "R")),
         by = .(SPNRCELL)]
dat_pers[!is.na(SPNRCELL),
         S_NR := sum(SPBWT0 * (pers_cat == "NR")),
         by = .(SPNRCELL)]
dat_pers[!is.na(SPNRCELL),
         S_D  := sum(SPBWT0 * (pers_cat == "D")),
         by = .(SPNRCELL)]
# dat_pers[, class(S_R)]

dat_pers[pers_cat %in% c("L1", "L2", "I"), F_3 := 1]
dat_pers[pers_cat %in% "R", F_3 := (S_R + S_NR + S_D) / S_R]
dat_pers[pers_cat %in% c("NR", "D"), F_3 := 0]

dat_pers[!is.na(pers_cat), as.list(summary(F_3)), keyby = .(pers_cat)]

if (!isTRUE(dat_pers[, all.equal(SPNRWT0, round(SPBWT0 * F_3, 6),
                                 check.attributes = FALSE)])) {
  stop("Check SPNRWT0")
}

dat_pers[, map(.SD, sum, na.rm = TRUE), .SDcols = c("SPBWT0", "SPNRWT0")]
dat_pers[, map(.SD, sum, na.rm = TRUE), .SDcols = c("SPBWT0", "SPNRWT0"),
         keyby = .(pers_cat)]

dat_pers[pers_cat == "R", weight_descr_stat(SPNRWT0)]


# Literacy-related nonresponse adjustment (SPLNRWT0)

names(dat_pers)

dat_pers[, .N, keyby = .(SPLNRCELL)]
dat_pers[, .N, keyby = .(hh_cat, pers_cat, SPLNRCELL)]

dat_pers[, map(.SD, sum, na.rm = TRUE),
         .SDcols = c("SPBWT0", "SPNRWT0", "SPLNRWT0")]
dat_pers[, map(.SD, sum, na.rm = TRUE),
         .SDcols = c("SPBWT0", "SPNRWT0", "SPLNRWT0"),
         keyby = .(pers_cat)][, F_4 := SPLNRWT0 / SPNRWT0][]

dat_pers[, Sp_L  := sum(HHBWT0 * (hh_cat == "L"))]
dat_pers[!is.na(SPLNRCELL),
         Sp_L1 := sum(HHBWT0 / pers_n * (pers_cat == "L1")),
         by = .(SPLNRCELL)]
dat_pers[!is.na(SPLNRCELL),
         Sp_L2 := sum(HHBWT0 / pers_n * (pers_cat == "L2")),
         by = .(SPLNRCELL)]

dat_pers[pers_cat %in% c("I", "R", "NR", "D"), F_4 := 1]
dat_pers[pers_cat %in% c("L1"), F_4 := (Sp_L + Sp_L1 + Sp_L2) / Sp_L1]
dat_pers[pers_cat %in% c("L2"), F_4 := 0]

dat_pers[!is.na(SPLNRCELL), .(SPLNRCELL, Sp_L, Sp_L1, Sp_L2, F_4)]

dat_pers[!is.na(pers_cat), as.list(summary(F_4)), keyby = .(pers_cat)]

if (!isTRUE(dat_pers[, all.equal(SPLNRWT0, round(SPNRWT0 * F_4, 6),
                                 check.attributes = FALSE)])) {
  stop("Check SPLNRWT0")
}

dat_pers[, map(.SD, sum, na.rm = TRUE),
         .SDcols = c("SPBWT0", "SPNRWT0", "SPLNRWT0")]
dat_pers[, map(.SD, sum, na.rm = TRUE),
         .SDcols = c("SPBWT0", "SPNRWT0", "SPLNRWT0"),
         keyby = .(pers_cat)]

dat_pers[pers_cat == "L1", weight_descr_stat(SPLNRWT0)]


# Calibration

names(dat_wif)

dat_pers[, map(.SD, sum, na.rm = TRUE),
         .SDcols = c("SPBWT0", "SPNRWT0", "SPLNRWT0")]
dat_pers[, map(.SD, sum, na.rm = TRUE),
         .SDcols = c("SPBWT0", "SPNRWT0", "SPLNRWT0"),
         keyby = .(pers_cat)]

grep("RAKEDIM", names(dat_pers), value = T)

dat_pers[pers_cat %in% c("R", "L1"), .N, keyby = .(RAKEDIM1)]
dat_pers[pers_cat %in% c("R", "L1"), .N, keyby = .(RAKEDIM2)]
dat_pers[pers_cat %in% c("R", "L1"), .N, keyby = .(RAKEDIM3)]

gen_cal_vars <- function(x, name) {
  values <- sort(unique(x))
  dat <- map(
    .x = values,
    .f = \(val) as.integer(x == val)
  ) |> as.data.table()
  setnames(dat, paste("cal", name, values, sep = "_"))
  return(dat)
}

dat_pers <- list_cbind(x = list(
  dat_pers,
  dat_pers[, gen_cal_vars(x = RAKEDIM1, name = "RAKEDIM1")],
  dat_pers[, gen_cal_vars(x = RAKEDIM2, name = "RAKEDIM2")],
  dat_pers[, gen_cal_vars(x = RAKEDIM3, name = "RAKEDIM3")]
))

names(dat_pers)

# totals
cal_tot <- map(
  .x = 1:3,
  .f = \(x) openxlsx2::read_xlsx(
    file = "result/CY2_Final_MS_Benchmark_WIF_LVA.xlsx",
    sheet = x
  )
) |> rbindlist(use.names = FALSE)


# calib

grep("^cal_", names(dat_pers), value = T)

length(grep("^cal_", names(dat_pers), value = T))
cal_tot[, .N]

dat_pers[
  pers_cat %in% c("R", "L1"),
  F_6 := sampling::calib(
    Xs = .SD,
    d = SPLNRWT0,
    total = cal_tot$TOTAL,
    method = "raking"
  ),
  .SDcols = patterns("^cal_")
]

dat_pers[pers_cat %in% c("R", "L1"), summary(F_6)]

dat_pers[, SPRWT0 := SPLNRWT0 * F_6]

dat_pers[pers_cat %in% c("R", "L1"), weight_descr_stat(SPRWT0)]

dat_pers[
  pers_cat %in% c("R", "L1"),
  .(median(SPRWT0), sqrt(1 + (sd(SPRWT0) / mean(SPRWT0)) ^ 2))
]

dat_pers[
  pers_cat %in% c("R", "L1"),
  median(SPRWT0) * 3.5 * sqrt(1 + (sd(SPRWT0) / mean(SPRWT0)) ^ 2)
]
