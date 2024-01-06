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
dat_sdif <- haven::read_sas(
  data_file = "data-weights/SAS/SAS7BDAT/psdlvams.sas7bdat"
) |> setDT()


# WIF
dat_wif <- haven::read_sas(
  data_file = "data-weights/WIF_QCChecks_LVA.sas7bdat"
) |> setDT()

# SPRWT0
dat_sprwt <- haven::read_sas(
  data_file = "data-weights/sprwt.sas7bdat"
) |> setDT()

dat_wif <- merge(
  x = dat_wif,
  y = dat_sprwt,
  by = "PERSID",
  all.x = TRUE
)
rm(dat_sprwt)

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

dat_sdif_du[, .(PROB_PSU)]
dat_sdif_du[, class(PROB_PSU)]
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


tab_w4_5_5[, variable := factor(
  x = variable, levels = c("N", "ADJ_FACTOR", "WGT_SUM")
)]
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


# Weighting QC Form W-5 (Screener) LVA ####
# https://piaac.ets.org/portal/weighting-qc-form-w-5-screener-lva/
# WeightingQCForm-5_Person (Screener)_LVA.xlsx

# stop()

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
                 SPRWT0   = !is.na(SPRWT0),
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


# Screener

# W_k
dat_pers[, W_k := 1 / PROB_PSU / PROB_HH]
dat_pers[, all.equal(HHBWT0, W_k, check.attributes = FALSE)]

dat_pers[, .(n_hh = sum(1 / pers_n), n_pers = .N), keyby = .(hh_cat)]


# F_1
grep("CELL$", names(dat_pers), value = TRUE)

dat_pers[, S1_L  := sum(W_k * (hh_cat == "L")),  by = .(HHUNKCELL)]
dat_pers[, S1_R  := sum(W_k / pers_n * (hh_cat == "R")), by = .(HHUNKCELL)]
dat_pers[, S1_NR := sum(W_k * (hh_cat == "NR")), by = .(HHUNKCELL)]
dat_pers[, S1_I  := sum(W_k * (hh_cat == "I")),  by = .(HHUNKCELL)]
dat_pers[, S1_U  := sum(W_k * (hh_cat == "U")),  by = .(HHUNKCELL)]

dat_pers[
  hh_cat == "I",
  F_1 := (S1_L + S1_R + S1_NR + S1_I + S1_U) / (S1_L + S1_R + S1_NR + S1_I)
]
dat_pers[
  hh_cat == "U",
  F_1 := (S1_L + S1_R + S1_NR) / (S1_L + S1_R + S1_NR + S1_I)
]
dat_pers[hh_cat %in% c("L", "R", "NR"), F_1 := 1]

dat_pers[, all.equal(HHUEWT0, W_k * F_1, check.attributes = FALSE)]


# F_2
grep("CELL$", names(dat_pers), value = TRUE)

dat_pers[, S2_R  := sum(W_k * F_1 / pers_n * (hh_cat == "R")), by = .(HHNRCELL)]
dat_pers[, S2_NR := sum(W_k * F_1 * (hh_cat == "NR")), by = .(HHNRCELL)]
dat_pers[, S2_U  := sum(W_k * F_1 * (hh_cat == "U")),  by = .(HHNRCELL)]

dat_pers[hh_cat %in% c("L", "I"), F_2 := 1]
dat_pers[hh_cat %in% c("R"), F_2 := (S2_R + S2_NR + S2_U) / S2_R]
dat_pers[hh_cat %in% c("NR", "U"), F_2 := 0]

dat_pers[, all.equal(HHNRWT0, W_k * F_1 * F_2, check.attributes = FALSE)]



# Base weight (SPBWT0)
dat_pers[, W_l := W_k * F_1 * F_2 / PROB_PERS]
if (dat_pers[, !isTRUE(all.equal(SPBWT0, W_l, check.attributes = FALSE))]) {
  stop("Check SPBWT0")
}
# TRUE

dat_pers[, sprintf("%.2f", sum(SPBWT0, na.rm = TRUE))]
dat_pers[, sprintf("%.2f", sum(SPBWT0, na.rm = TRUE)), keyby = .(pers_cat)]

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
dat_pers[!is.na(SPNRCELL), S3_R  := sum(W_l * (pers_cat == "R")),  by = .(SPNRCELL)]
dat_pers[!is.na(SPNRCELL), S3_NR := sum(W_l * (pers_cat == "NR")), by = .(SPNRCELL)]
dat_pers[!is.na(SPNRCELL), S3_D  := sum(W_l * (pers_cat == "D")),  by = .(SPNRCELL)]
# dat_pers[, class(S_R)]

dat_pers[pers_cat %in% c("L1", "L2", "I"), F_3 := 1]
dat_pers[pers_cat %in% c("R"), F_3 := (S3_R + S3_NR + S3_D) / S3_R]
dat_pers[pers_cat %in% c("NR", "D"), F_3 := 0]

dat_pers[!is.na(pers_cat), as.list(summary(F_3)), keyby = .(pers_cat)]

if (!isTRUE(dat_pers[, all.equal(SPNRWT0, W_l * F_3, check.attributes = FALSE)])) {
  stop("Check SPNRWT0")
}

dat_pers[, map(.SD, sum, na.rm = TRUE), .SDcols = c("SPBWT0", "SPNRWT0")]
dat_pers[, map(.SD, sum, na.rm = TRUE), .SDcols = c("SPBWT0", "SPNRWT0"),
         keyby = .(pers_cat)]

dat_pers[pers_cat == "R", weight_descr_stat(SPNRWT0)]


# Literacy-related nonresponse adjustment (SPLNRWT0)

# (LL: The factor F_4 is actually calculated using screener base weights
# (note the S' instead of S in the formula,
# where S' is the sum of screener base weights
# as explained in the footnote under the table).
# F_4 is calculated at the household level including L, L1 and L2 cases.
# If a household has multiple L1/L2 persons,
# it will be counted as one household in this calculation.
# Then the factor F_4 is applied to the person-level weights.
# Please see sections 2.3.2.2 and 2.3.3.3 for some explanation.)

names(dat_pers)

dat_pers[, .N, keyby = .(SPLNRCELL)]
dat_pers[, .N, keyby = .(hh_cat, pers_cat, SPLNRCELL)]

dat_pers[, map(.SD, sum, na.rm = TRUE),
         .SDcols = c("SPBWT0", "SPNRWT0", "SPLNRWT0")]
dat_pers[, map(.SD, sum, na.rm = TRUE),
         .SDcols = c("SPBWT0", "SPNRWT0", "SPLNRWT0"),
         keyby = .(pers_cat)][, F_4 := SPLNRWT0 / SPNRWT0][]

dat_pers[
  pers_n > 1,
  .(pers_cat = paste(sort(pers_cat), collapse = "-")),
  by = .(CASEID)
][, .N, keyby = .(pers_cat)]
# There is only 1 HH with 2 sampled persons and both with category L2

dat_pers[, hh_L1 := hh_cat == "R" & any(pers_cat == "L1"), by = .(CASEID)]
dat_pers[, hh_L2 := hh_cat == "R" & any(pers_cat == "L2"), by = .(CASEID)]

dat_F_4 <- dat_pers[
  hh_cat == "L" | hh_L1 | hh_L2,
  .(CASEID, HHBWT0, pers_n, SPLNRCELL, hh_cat, hh_L1, hh_L2)
] |> unique()

dat_F_4[, S4_L  := sum(HHBWT0 * (hh_cat == "L"))]
dat_F_4[, S4_L1 := sum(HHBWT0 * (hh_L1))]
dat_F_4[, S4_L2 := sum(HHBWT0 * (hh_L2))]
dat_F_4[, hh_F_4 := (S4_L + S4_L1 + S4_L2) / S4_L1]

dat_F_4[, .(CASEID, S4_L, S4_L1, S4_L2, hh_F_4)]

dat_pers <- merge(
  x = dat_pers,
  y = dat_F_4[, .(CASEID, S4_L, S4_L1, S4_L2, hh_F_4)],
  by = "CASEID",
  all.x = TRUE
)
dat_pers[hh_cat == "L" | hh_L1 | hh_L2, .(CASEID, S4_L, S4_L1, S4_L2, hh_F_4)]

dat_pers[pers_cat %in% c("I", "R"),        F_4 := 1]
dat_pers[pers_cat %in% c("L1"),            F_4 := hh_F_4]
dat_pers[pers_cat %in% c("L2", "NR", "D"), F_4 := 0]

dat_pers[!is.na(SPLNRCELL), .(SPLNRCELL, S4_L, S4_L1, S4_L2, hh_F_4, F_4)]

dat_pers[!is.na(pers_cat), as.list(summary(F_4)), keyby = .(pers_cat)]

if (!isTRUE(dat_pers[, all.equal(SPLNRWT0, W_l * F_3 * F_4,
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

dat_pers <- cbind(
  dat_pers,
  dat_pers[, gen_cal_vars(x = RAKEDIM1, name = "RAKEDIM1")],
  dat_pers[, gen_cal_vars(x = RAKEDIM2, name = "RAKEDIM2")],
  dat_pers[, gen_cal_vars(x = RAKEDIM3, name = "RAKEDIM3")]
)

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
  F_5 := sampling::calib(
    Xs = .SD,
    d = SPLNRWT0,
    total = cal_tot$TOTAL,
    method = "raking"
  ),
  .SDcols = patterns("^cal_")
]

dat_pers[pers_cat %in% c("R", "L1"), summary(F_5)]

dat_pers[, all.equal(SPRWT0, W_l * F_3 * F_4 * F_5, check.attributes = FALSE)]

dat_pers[, SPRWT0_test := W_l * F_3 * F_4 * F_5]
dat_pers[, all.equal(SPRWT0, SPRWT0_test, check.attributes = F)]

dat_pers[, SPRWT0_abs_diff := abs(SPRWT0_test - SPRWT0)]
dat_pers[pers_cat %in% c("R", "L1")][SPRWT0_abs_diff > .01][
  order(-SPRWT0_abs_diff),
  .(CASEID, PERSID, SPRWT0, SPRWT0_test, SPRWT0_abs_diff)
]

dat_pers[pers_cat %in% c("R", "L1"), weight_descr_stat(SPRWT0)]
dat_pers[pers_cat %in% c("R", "L1"), weight_descr_stat(SPRWT0_test)]



# Sample person trimmed weight (SPTWT0)

# stop()

# After the first cycle of raking, trimming was conducted.
# There is one trimming group.
# The trimming cutoff is 867.936 (= 6.94 *median of the initial raked weight).
# Any weights greater than the cutoff were trimmed to that value.

# We decided not to use 3.5*SQRT(1+CV^2) to define the trimming cutoff,
# since it trimmed too many cases (>200).
# Instead, we increased the cutoff to the point where
# sum(SPTWT0)/sum(SPRWT0) = 0.98.
# I added this explanation to table 8.1

dat_pers[, as.character(max(SPTWT0, na.rm = T))]
867.936143169207 / 6.94
dat_pers[pers_cat %in% c("R", "L1"), median(SPRWT0)]

dat_pers[
  pers_cat %in% c("R", "L1"),
  .(a = 3.5, b = sqrt(1 + (sd(SPRWT0) / mean(SPRWT0)) ^ 2), c = median(SPRWT0))
][, thr := a * b * c][]

a <- 3.5
b <- dat_pers[pers_cat %in% c("R", "L1"),
              sqrt(1 + (sd(SPRWT0) / mean(SPRWT0)) ^ 2)]
6.94 / a
6.94 / b

dat_pers[
  pers_cat %in% c("R", "L1"),
  .(a = 3.5,
    b = sqrt(1 + (sd(SPRWT0) / mean(SPRWT0)) ^ 2),
    c = median(SPRWT0))
][, cutoff := a * b * c][]

dat_pers[
  pers_cat %in% c("R", "L1"),
  median(SPRWT0) * 3.5 * sqrt(1 + (sd(SPRWT0) / mean(SPRWT0)) ^ 2)
]

cutoff <- 867.936143169207
dat_pers[SPRWT0 > cutoff, .N]

dat_pers[SPRWT0 <= cutoff, F_6 := 1]
dat_pers[SPRWT0 >  cutoff, F_6 := cutoff / SPRWT0]

dat_pers[, SPTWT0_test := W_l * F_3 * F_4 * F_5 * F_6]
dat_pers[, all.equal(SPTWT0, SPTWT0_test, check.attributes = F)]
dat_pers[, SPTWT0_abs_diff := abs(SPTWT0_test - SPTWT0)]
dat_pers[pers_cat %in% c("R", "L1")][SPTWT0_abs_diff > .01][
  order(-SPTWT0_abs_diff),
  .(CASEID, PERSID, SPTWT0, SPTWT0_test, SPTWT0_abs_diff)
]

dat_pers[pers_cat %in% c("R", "L1"), weight_descr_stat(SPTWT0)]
dat_pers[pers_cat %in% c("R", "L1"), weight_descr_stat(SPTWT0_test)]

dat_pers[pers_cat %in% c("R", "L1"), sum(SPTWT0) / sum(SPRWT0)]


# Final calibrated sample person weight (SPFWT0)
dat_pers[
  pers_cat %in% c("R", "L1"),
  F_7 := sampling::calib(
    Xs = .SD,
    d = SPTWT0,
    total = cal_tot$TOTAL,
    method = "raking"
  ),
  .SDcols = patterns("^cal_")
]

dat_pers[pers_cat %in% c("R", "L1"), summary(F_7)]

dat_pers[, SPFWT0_test := W_l * F_3 * F_4 * F_5 * F_6 * F_7]
dat_pers[, all.equal(SPFWT0, SPFWT0_test, check.attributes = F)]
dat_pers[, SPFWT0_abs_diff := abs(SPFWT0_test - SPFWT0)]
dat_pers[pers_cat %in% c("R", "L1")][SPFWT0_abs_diff > .01][
  order(-SPFWT0_abs_diff),
  .(CASEID, PERSID, SPFWT0, SPFWT0_test, SPFWT0_abs_diff)
]

dat_pers[pers_cat %in% c("R", "L1"), weight_descr_stat(SPFWT0)]
dat_pers[pers_cat %in% c("R", "L1"), weight_descr_stat(SPFWT0_test)]


# Table 1

tab_SPBWT0   <- dat_pers[pers_cat != "I", weight_descr_stat(SPBWT0)]
tab_SPNRWT0  <- dat_pers[pers_cat == "R", weight_descr_stat(SPNRWT0)]
tab_SPLNRWT0 <- dat_pers[pers_cat == "L1", weight_descr_stat(SPLNRWT0)]
tab_SPRWT0   <- dat_pers[pers_cat %in% c("R", "L1"), weight_descr_stat(SPRWT0)]
tab_SPTWT0   <- dat_pers[pers_cat %in% c("R", "L1"), weight_descr_stat(SPTWT0)]
tab_SPFWT0   <- dat_pers[pers_cat %in% c("R", "L1"), weight_descr_stat(SPFWT0)]

setnames(tab_SPBWT0,   "Value", "SPBWT0")
setnames(tab_SPNRWT0,  "Value", "SPNRWT0")
setnames(tab_SPLNRWT0, "Value", "SPLNRWT0")
setnames(tab_SPRWT0,   "Value", "SPRWT0")
setnames(tab_SPTWT0,   "Value", "SPTWT0")
setnames(tab_SPFWT0,   "Value", "SPFWT0")

setkey(tab_SPBWT0,   `Descriptive statistics`)
setkey(tab_SPNRWT0,  `Descriptive statistics`)
setkey(tab_SPLNRWT0, `Descriptive statistics`)
setkey(tab_SPRWT0,   `Descriptive statistics`)
setkey(tab_SPTWT0,   `Descriptive statistics`)
setkey(tab_SPFWT0,   `Descriptive statistics`)

tab1 <- Reduce(f = merge, x = list(
  tab_SPBWT0,
  tab_SPNRWT0,
  tab_SPLNRWT0,
  tab_SPRWT0,
  tab_SPTWT0,
  tab_SPFWT0
))

tab1


# Table 2

tab_SPBWT0   <- dat_pers[pers_cat != "I", weight_descr_stat(SPBWT0),
                         keyby = .(REGION)]
tab_SPNRWT0  <- dat_pers[pers_cat == "R", weight_descr_stat(SPNRWT0),
                         keyby = .(REGION)]
tab_SPLNRWT0 <- dat_pers[pers_cat == "L1", weight_descr_stat(SPLNRWT0),
                         keyby = .(REGION)]
tab_SPRWT0   <- dat_pers[pers_cat %in% c("R", "L1"), weight_descr_stat(SPRWT0),
                         keyby = .(REGION)]
tab_SPTWT0   <- dat_pers[pers_cat %in% c("R", "L1"), weight_descr_stat(SPTWT0),
                         keyby = .(REGION)]
tab_SPFWT0   <- dat_pers[pers_cat %in% c("R", "L1"), weight_descr_stat(SPFWT0),
                         keyby = .(REGION)]

setnames(tab_SPBWT0,   "Value", "SPBWT0")
setnames(tab_SPNRWT0,  "Value", "SPNRWT0")
setnames(tab_SPLNRWT0, "Value", "SPLNRWT0")
setnames(tab_SPRWT0,   "Value", "SPRWT0")
setnames(tab_SPTWT0,   "Value", "SPTWT0")
setnames(tab_SPFWT0,   "Value", "SPFWT0")

setkey(tab_SPBWT0,   REGION, `Descriptive statistics`)
setkey(tab_SPNRWT0,  REGION, `Descriptive statistics`)
setkey(tab_SPLNRWT0, REGION, `Descriptive statistics`)
setkey(tab_SPRWT0,   REGION, `Descriptive statistics`)
setkey(tab_SPTWT0,   REGION, `Descriptive statistics`)
setkey(tab_SPFWT0,   REGION, `Descriptive statistics`)

tab2 <- Reduce(f = \(x, y) merge(x, y, all = TRUE), x = list(
  tab_SPBWT0,
  tab_SPNRWT0,
  tab_SPLNRWT0,
  tab_SPRWT0,
  tab_SPTWT0,
  tab_SPFWT0
))

tab2


# Table 3

grep("GENDER", names(dat_pers), value = T)
dat_pers[!is.na(DISP_CIBQ), .N,
         keyby = c(grep("GENDER", names(dat_pers), value = T))]

tab_SPBWT0   <- dat_pers[pers_cat != "I", weight_descr_stat(SPBWT0),
                         keyby = .(CI_GENDER)]
tab_SPNRWT0  <- dat_pers[pers_cat == "R", weight_descr_stat(SPNRWT0),
                         keyby = .(CI_GENDER)]
tab_SPLNRWT0 <- dat_pers[pers_cat == "L1", weight_descr_stat(SPLNRWT0),
                         keyby = .(CI_GENDER)]
tab_SPRWT0   <- dat_pers[pers_cat %in% c("R", "L1"), weight_descr_stat(SPRWT0),
                         keyby = .(CI_GENDER)]
tab_SPTWT0   <- dat_pers[pers_cat %in% c("R", "L1"), weight_descr_stat(SPTWT0),
                         keyby = .(CI_GENDER)]
tab_SPFWT0   <- dat_pers[pers_cat %in% c("R", "L1"), weight_descr_stat(SPFWT0),
                         keyby = .(CI_GENDER)]

setnames(tab_SPBWT0,   "Value", "SPBWT0")
setnames(tab_SPNRWT0,  "Value", "SPNRWT0")
setnames(tab_SPLNRWT0, "Value", "SPLNRWT0")
setnames(tab_SPRWT0,   "Value", "SPRWT0")
setnames(tab_SPTWT0,   "Value", "SPTWT0")
setnames(tab_SPFWT0,   "Value", "SPFWT0")

setkey(tab_SPBWT0,   CI_GENDER, `Descriptive statistics`)
setkey(tab_SPNRWT0,  CI_GENDER, `Descriptive statistics`)
setkey(tab_SPLNRWT0, CI_GENDER, `Descriptive statistics`)
setkey(tab_SPRWT0,   CI_GENDER, `Descriptive statistics`)
setkey(tab_SPTWT0,   CI_GENDER, `Descriptive statistics`)
setkey(tab_SPFWT0,   CI_GENDER, `Descriptive statistics`)

tab3 <- Reduce(f = \(x, y) merge(x, y, all = TRUE), x = list(
  tab_SPBWT0,
  tab_SPNRWT0,
  tab_SPLNRWT0,
  tab_SPRWT0,
  tab_SPTWT0,
  tab_SPFWT0
))

tab3


# Table 4

grep("AGE", names(dat_pers), value = T)

dat_pers[!is.na(DISP_CIBQ), .N,
         keyby = c(grep("AGE", names(dat_pers), value = T))]
dat_pers[!is.na(DISP_CIBQ), .N, keyby = "CI_AGE"]

dat_pers[, age_group := factor(
  x = trunc((CI_AGE - 6) / 10),
  levels = 1:5,
  labels = c("16-25", "26-35", "36-45", "46-55", "56-65")
)]

dat_pers[!is.na(DISP_CIBQ), .(.N, Min = min(CI_AGE), Max = max(CI_AGE)),
         keyby = .(age_group)]

tab_SPBWT0   <- dat_pers[pers_cat != "I", weight_descr_stat(SPBWT0),
                         keyby = .(age_group)]
tab_SPNRWT0  <- dat_pers[pers_cat == "R", weight_descr_stat(SPNRWT0),
                         keyby = .(age_group)]
tab_SPLNRWT0 <- dat_pers[pers_cat == "L1", weight_descr_stat(SPLNRWT0),
                         keyby = .(age_group)]
tab_SPRWT0   <- dat_pers[pers_cat %in% c("R", "L1"), weight_descr_stat(SPRWT0),
                         keyby = .(age_group)]
tab_SPTWT0   <- dat_pers[pers_cat %in% c("R", "L1"), weight_descr_stat(SPTWT0),
                         keyby = .(age_group)]
tab_SPFWT0   <- dat_pers[pers_cat %in% c("R", "L1"), weight_descr_stat(SPFWT0),
                         keyby = .(age_group)]

setnames(tab_SPBWT0,   "Value", "SPBWT0")
setnames(tab_SPNRWT0,  "Value", "SPNRWT0")
setnames(tab_SPLNRWT0, "Value", "SPLNRWT0")
setnames(tab_SPRWT0,   "Value", "SPRWT0")
setnames(tab_SPTWT0,   "Value", "SPTWT0")
setnames(tab_SPFWT0,   "Value", "SPFWT0")

setkey(tab_SPBWT0,   age_group, `Descriptive statistics`)
setkey(tab_SPNRWT0,  age_group, `Descriptive statistics`)
setkey(tab_SPLNRWT0, age_group, `Descriptive statistics`)
setkey(tab_SPRWT0,   age_group, `Descriptive statistics`)
setkey(tab_SPTWT0,   age_group, `Descriptive statistics`)
setkey(tab_SPFWT0,   age_group, `Descriptive statistics`)

tab4 <- Reduce(f = \(x, y) merge(x, y, all = TRUE), x = list(
  tab_SPBWT0,
  tab_SPNRWT0,
  tab_SPLNRWT0,
  tab_SPRWT0,
  tab_SPTWT0,
  tab_SPFWT0
))

tab4


# Table 5
tab5 <- melt.data.table(
  data = dat_pers[pers_cat %in% c("R", "L1")],
  id.vars = c("CASEID", "PERSID"),
  measure.vars = grep("^SPFWT[0-9]{1,2}$", names(dat_pers), value = T)
)

tab5[, sum(value), keyby = .(variable)][, .N, keyby = .(round(V1))]


# Table 6

tab5[grep("^SPFWT(0|1|5|10|15)$", variable),
     .(n_missing = sum(is.na(value)),
       mean = mean(value),
       min = min(value),
       max = max(value)),
     keyby = .(variable)]



# Table 7.1
tab7.1 <- dat_pers[
  !is.na(DISP_CIBQ),
  .(N = .N, WGT_SUM = round(sum(SPBWT0), 2)),
  keyby = .(pers_cat)
]
tab7.1[, .(N = sum(N), WGT_SUM = sum(WGT_SUM))]


# Table 7.2
grep("CELL", names(dat_pers), value = T)

tab7.2 <- dat_pers[
  !is.na(DISP_CIBQ),
  .(N = as.double(.N), ADJ.FACTOR = first(F_3), WGT.SUM = round(sum(SPBWT0), 2)),
  keyby = .(pers_cat, SPNRCELL)
] |> melt.data.table(
  id.vars = c("pers_cat", "SPNRCELL"),
  na.rm = T
)
tab7.2[variable == "WGT.SUM"]
tab7.2[variable == "WGT.SUM" & value == 0]

tab7.2_test <- openxlsx2::read_xlsx(
  file = "WeightingQCForms/WeightingQCForm-5_Person (Screener)_LVA.xlsx",
  rows = 308:373,
  skip_empty_cols = TRUE,
  na.strings = "."
) |> setDT(check.names = TRUE) |> melt.data.table(
  id.vars = "NA.",
  na.rm = TRUE
)
tab7.2_test <- tab7.2_test[grep("6$", variable, invert = T)]

tab7.2[, .N, keyby = .(pers_cat, variable)]
tab7.2_test[, .N, keyby = .(variable)]

tab7.2_test[value == 0]

tab7.2
