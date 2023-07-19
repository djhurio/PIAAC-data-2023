# WIF

# Reset
rm(list = ls())
gc()
source(".Rprofile")

dat_sdif_fieldw <- readRDS(file = "data/dat_sdif_fieldw.rds")
dat_bq <- readRDS(file = "data/dat_bq.rds")

dat_sdif_raking <- readRDS(file = "data/dat_sdif_raking.rds")
dat_bq_raking <- readRDS(file = "data/dat_bq_raking.rds")

dat_bq[, bq := TRUE]
dat_bq_raking[, bq := TRUE]

dat_raking <- merge(
  x = dat_sdif_raking[as.logical(weightflg)],
  y = dat_bq_raking,
  by = "persid",
  all.x = TRUE,
  sort = FALSE
)

dat_raking[, .N, keyby = .(bq)]
dat_raking[is.na(bq), .(caseid, persid)]
dat_raking[is.na(bq), caseid]

dat_bq[, caseid := substr(persid, 1, 8)]
dat_bq[, .(caseid, persid)]

x <- dat_raking[is.na(bq), as.character(caseid)]

dat_bq[caseid %chin% x, .(caseid, persid)]
# dat_raking[is.na(bq),
#            .(caseid, persid, disp_scr, disp_cibq, disp_ds, weightflg, bq)]



dat_raking[, .N, keyby = .(calcage)]

dat_raking[, .N, keyby = .(weightflg)]
dat_raking[, .N, keyby = .(weightflg, bq, age66 = calcage == 66)]

# dat_raking[
#   calcage > 65,
#   .(disp_scr, disp_cibq, disp_ds, weightflg,
#     dobyy, dobmm, curyear, curmonth, ci_age, calcage)
# ][order(dobyy, dobmm)]

dat_raking[, .N, keyby = .(gender)]
dat_raking[, .N, keyby = .(gender, a2_n02)]

dat_raking[, .N, keyby = .(calcage)]
dat_raking[, age_group := trunc((calcage - 11) / 5)]
dat_raking[calcage > 65, age_group := 10L]
dat_raking[, .(.N, min(calcage), max(calcage)), keyby = .(age_group)]

dat_raking[, rakedim1 := (gender - 1) * 10 + age_group]
dat_raking[, .N, keyby = .(rakedim1, gender, age_group)]


# Ethnicity

dat_raking[, .N, keyby = .(a2_n02lvx)]

dat_raking[a2_n02lvx %in% 1:10, rakedim2_init := a2_n02lvx]
dat_raking[is.na(a2_n02lvx) | a2_n02lvx > 10, rakedim2_init := 10L]
dat_raking[is.na(a2_n02lvx) | a2_n02lvx > 10, iflg_rakedim2 := 1L]

dat_raking[, rakedim2_init_label := factor(
  x = rakedim2_init,
  levels = 1:10,
  labels = c("Latvian", "Russian", "Ukrainian", "Belorussian", "Estonian",
             "Lithuanian", "Polish", "Jewish", "Roma", "Other"))]

dat_raking[, .N, keyby = .(rakedim2_init, rakedim2_init_label)]

dat_raking[, rakedim2 := rakedim2_init]
dat_raking[, rakedim2_n := .N, by = .(rakedim2)]
dat_raking[rakedim2_n < 30, rakedim2 := 10L]
dat_raking[, .N, keyby = .(rakedim2_init, rakedim2_init_label, rakedim2)]

dat_raking[, rakedim2_n := NULL]

# Country of birth

dat_raking[, .N, keyby = .(a2_q03a, a2_q03blv)]

dat_raking[a2_q03a == 1L, rakedim3_init := 1L]
dat_raking[a2_q03a == 2L & a2_q03blv %in% 1:9, rakedim3_init := a2_q03blv + 1L]
dat_raking[is.na(a2_q03a) | a2_q03a > 2L | (a2_q03a == 2L & a2_q03blv > 9),
           rakedim3_init := 10L]
dat_raking[, .N, keyby = .(rakedim3_init, a2_q03a, a2_q03blv)]

dat_raking[, rakedim3_init_label := factor(
  x = rakedim3_init,
  levels = 1:10,
  labels = c("Latvia", "Russia", "Belarus", "Ukraine", "Poland",
             "Estonia", "Lithuania", "Kazahkstan", "Germany", "Other country"))]

dat_raking[, rakedim3 := rakedim3_init]
dat_raking[, rakedim3_n := .N, by = .(rakedim3)]
dat_raking[rakedim3_n < 30, rakedim3 := 10L]
dat_raking[, .N, keyby = .(rakedim3_init, rakedim3_init_label, rakedim3)]

dat_raking[, rakedim3_n := NULL]


# Save

dat_raking_sel <- dat_raking[
  persid != "13611880019",
  .(persid, rakedim1, rakedim2, rakedim3)
]

saveRDS(dat_raking_sel, "data/dat_raking_sel.rds")


