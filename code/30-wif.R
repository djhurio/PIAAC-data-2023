# WIF

# Reset
rm(list = ls())
gc()
source(".Rprofile")

dat_sdif_fieldw <- readRDS(file = "data/dat_sdif_fieldw.rds")
dat_sdif_raking <- readRDS(file = "data/dat_sdif_raking.rds")

dat_bq <- readRDS(file = "data/dat_bq.rds")
dat_bq_raking <- readRDS(file = "data/dat_bq_raking.rds")

dat_sdif_fieldw[, class(caseid)]
dat_sdif_fieldw[, class(persid)]
dat_sdif_raking[, class(caseid)]
dat_sdif_raking[, class(persid)]
dat_bq[, class(persid)]
dat_bq_raking[, class(persid)]


dat_bq[, bq := TRUE]
dat_bq_raking[, bq := TRUE]

dat_raking <- merge(
  x = dat_sdif_raking,
  y = dat_bq_raking,
  by = "persid",
  all.x = TRUE,
  sort = FALSE
)

dat_raking[, .N, keyby = .(bq)]
dat_raking[is.na(bq), .(caseid, persid)]
dat_raking[is.na(bq), caseid]

dat_bq[, caseid := as.numeric(substr(persid, 1, 8))]
dat_bq[, .(caseid, persid)]

x <- dat_raking[is.na(bq), caseid]

dat_bq[caseid %in% x, .(caseid, persid)]
# dat_raking[is.na(bq),
#            .(caseid, persid, disp_scr, disp_cibq, disp_ds, weightflg, bq)]
rm(x)

dat_raking[, .N, keyby = .(ci_age)]
dat_raking[, .N, keyby = .(ci_gender)]

dat_raking[, .N, keyby = .(scqage)]
dat_raking[, .N, keyby = .(scqage, scqagerange)]

# scqagerange labojums
dat_raking[scqage %in% 16:65 & !is.na(scqagerange), .(scqage, scqagerange)]
dat_raking[scqage %in% 16:65 & !is.na(scqagerange), scqagerange := NA]
dat_raking[scqage %in% 16:65 & !is.na(scqagerange), .(scqage, scqagerange)]

dat_raking[, .N, keyby = .(ci_age)]
dat_raking[, all(scqage == ci_age)]

dat_raking[, .N, keyby = .(scqagerange)]
dat_raking[ci_age > 65, .N, keyby = .(scqagerange)]

# dat_raking[, .N, keyby = .(ci_age)]
# dat_raking[, .N, keyby = .(diagerange)]
dat_raking[, .N, keyby = .(a2_d01b)]

dat_raking[
  ci_age > 65 | ci_gender > 2,
  .(persid, scqage, scqagerange, ci_age, ci_gender, a2_n02, a2_d01b)
][order(ci_age, ci_gender)]

# str(dat_raking$scqagerange)
table(dat_raking$scqagerange)

dat_raking[, .N, keyby = .(scqagerange, factor(x = scqagerange, levels = 1:7,
                                               labels = c("<16",
                                                          "16-25",
                                                          "26-35",
                                                          "36-45",
                                                          "46-55",
                                                          "56-65",
                                                          "66+")), a2_d01b)]

# Age and gender imputation
# stop()

dat_raking[, .N, keyby = .(ci_age)]
dat_raking[, .N, keyby = .(ci_gender)]

dat_raking[, impflgag := as.integer(ci_age > 65)]
dat_raking[, impflgge := as.integer(ci_gender > 2)]

dat_raking[, .N, keyby = .(impflgag, impflgge)]

# No BQ nevar imputēt

x <- dat_raking[ci_age > 65 | ci_gender > 2, persid]
# dat_bq[, .(a2_q01a, a2_q01b)]
dat_bq[persid %in% x, .(persid, a2_n02, a2_d01b, a2_q01a, a2_q01b)]
dat_sdif_fieldw[persid %in% x, .SD, .SDcols = patterns("age|dob|gender")]
rm(x)

# dat_raking[, .N, keyby = .(weightflg)]

any(grepl("agegrp", names(dat_raking)))

dat_raking[ci_age %in% 16:65, agegrp := (ci_age + 4) %/% 10]
dat_raking[ci_age > 65, agegrp := scqagerange]
dat_raking[, as.list(summary(ci_age))[c("Min.", "Max.")],
           keyby = .(ci_age > 65, agegrp)]

ggplot(data = dat_raking[ci_age %in% 16:65], mapping = aes(x = factor(ci_age))) +
  geom_bar() +
  scale_x_discrete() +
  facet_wrap(facets = vars(agegrp), scales = "free_x") +
  theme_bw()


dat_imp <- dat_raking[
  order(caseid, persid),
  .(caseid, persid, strat_psu, id_psu, ci_age, ci_gender, agegrp)
]

dat_imp[ci_age > 65, ci_age := NA]
dat_imp[ci_gender > 2, ci_gender := NA]

# set.seed(134718)
# dat_imp1 <- VIM::hotdeck(
#   data = dat_imp,
#   variable = c("ci_age", "ci_gender"),
#   domain_var = c("strat_psu", "agegrp")
# )
# 
# set.seed(134718)
# dat_imp2 <- VIM::hotdeck(
#   data = dat_imp,
#   variable = c("ci_age", "ci_gender"),
#   domain_var = c("strat_psu", "agegrp")
# )
# 
# all.equal(dat_imp1, dat_imp2)
# 
# dat_imp1
# dat_imp2

set.seed(134718)
dat_imp <- VIM::hotdeck(
  data = dat_imp,
  variable = c("ci_age", "ci_gender"),
  domain_var = c("strat_psu", "agegrp")
)

dat_imp[(ci_age_imp) | (ci_gender_imp)]

ggplot(data = dat_imp, mapping = aes(x = factor(ci_age), fill = ci_age_imp)) +
  geom_bar() +
  scale_x_discrete() +
  facet_wrap(facets = vars(agegrp), scales = "free_x") +
  theme_bw()

# Add imputed variables back
dat_imp[, .(caseid, persid, ci_age, ci_gender)]
dat_raking[, c("ci_age", "ci_gender", "agegrp") := NULL]
if (all(!c("ci_age", "ci_gender", "agegrp") %in% names(dat_raking))) {
  dat_raking <- merge(
    x = dat_raking,
    y = dat_imp[, .(caseid, persid, ci_age, ci_gender)],
    by = c("caseid", "persid"),
    all.x = TRUE
  )
}

dat_raking[impflgag == 1L]
dat_raking[impflgge == 1L]

dat_raking[, .N, keyby = .(ci_gender)]
dat_raking[, .N, keyby = .(ci_age)]

dat_raking[, .N, keyby = .(a2_n02)]
dat_raking[, .N, keyby = .(a2_d01b)]
dat_raking[a2_d01b == 66]

dat_raking[, .N, keyby = .(ci_gender, a2_n02)]
dat_raking[, .N, keyby = .(ci_age, a2_d01b)]

# The final RAKEDIM1 should be based on AGE_R and GENDER_R which were derived by IEA using BQ/Doorstep interview age/ gender, and CI_AGE/CI_GENDER if missing BQ/DS age/gender.
dat_raking[, age_r    := ifelse(a2_d01b %in% 16:65, a2_d01b, ci_age)]
dat_raking[, gender_r := ifelse(a2_n02  %in%   1:2, a2_n02,  ci_gender)]

dat_raking[, age_group := trunc((age_r - 11) / 5)]
dat_raking[, .(.N, min(age_r), max(age_r)), keyby = .(age_group)]

dat_raking[, rakedim1 := (gender_r - 1) * 10 + age_group]

dat_raking[, .N, keyby = .(impflgag, !a2_d01b %in% 16:65,
                           impflgge, !a2_n02 %in% 1:2)]

dat_raking[, iflg_rakedim1 := as.integer(
  (impflgag & !a2_d01b %in% 16:65) | (impflgge & !a2_n02 %in% 1:2)
)]
dat_raking[, .N, keyby = .(iflg_rakedim1)]
dat_raking[, .N, keyby = .(impflgag, impflgge, iflg_rakedim1)]

dat_raking[, .N, keyby = .(rakedim1, gender_r, age_group)]


# Ethnicity
dat_raking[, .N, keyby = .(a2_n02lvx)]

dat_raking[a2_n02lvx %in% 1:10, rakedim2_init := a2_n02lvx]
dat_raking[, iflg_rakedim2 := as.integer(is.na(a2_n02lvx) | a2_n02lvx > 10)]
dat_raking[as.logical(iflg_rakedim2), rakedim2_init := 10L]

dat_raking[, rakedim2_init_label := factor(
  x = rakedim2_init,
  levels = 1:10,
  labels = c("Latvian", "Russian", "Ukrainian", "Belorussian", "Estonian",
             "Lithuanian", "Polish", "Jewish", "Roma", "Other"))]

dat_raking[, .N, keyby = .(rakedim2_init, rakedim2_init_label, iflg_rakedim2)]

dat_raking[, rakedim2 := rakedim2_init]
dat_raking[, rakedim2_n := .N, by = .(rakedim2)]
dat_raking[rakedim2_n < 30, rakedim2 := 10L]
dat_raking[, .N, keyby = .(rakedim2_init, rakedim2_init_label,
                           rakedim2, iflg_rakedim2)] |> print()

dat_raking[, rakedim2_n := NULL]


# Country of birth
dat_raking[, .N, keyby = .(a2_q03a, a2_q03blv)]

dat_raking[a2_q03a == 1L, rakedim3_init := 1L]
dat_raking[a2_q03a == 2L & a2_q03blv %in% 1:9, rakedim3_init := 1L + a2_q03blv]
dat_raking[, iflg_rakedim3 := as.integer(
  is.na(a2_q03a) | a2_q03a > 2L | (a2_q03a == 2L & a2_q03blv > 9)
)]
dat_raking[as.logical(iflg_rakedim3), rakedim3_init := 10L]
dat_raking[, .N, keyby = .(iflg_rakedim3, rakedim3_init, a2_q03a, a2_q03blv)]

dat_raking[, rakedim3_init_label := factor(
  x = rakedim3_init,
  levels = 1:10,
  labels = c("Latvia", "Russia", "Belarus", "Ukraine", "Poland",
             "Estonia", "Lithuania", "Kazahkstan", "Germany", "Other country"))]

dat_raking[, rakedim3 := rakedim3_init]
dat_raking[, rakedim3_n := .N, by = .(rakedim3)]
dat_raking[rakedim3_n < 30, rakedim3 := 10L]
dat_raking[, .N, keyby = .(iflg_rakedim3,
                           rakedim3_init, rakedim3_init_label, rakedim3)] |>
  print()

dat_raking[, rakedim3_n := NULL]


dat_raking[, .N, keyby = .(rakedim1)]
dat_raking[, .N, keyby = .(rakedim2)]
dat_raking[, .N, keyby = .(rakedim3)]

dat_raking[, .N, keyby = .(iflg_rakedim1, iflg_rakedim2, iflg_rakedim3)]


# Test with feedback
dat_RAKEDIM1_update <- read.xlsx(
  xlsxFile = "feedback/Feedback_on_MS_Final_SDIF_LVA_2023-09-29_v2.xlsx",
  sheet = "RAKEDIM1_update"
) |> setDT() |> setnames(tolower)

dat_RAKEDIM1_update <- merge(
  x = dat_RAKEDIM1_update,
  y = dat_raking[, .(persid, age_r, gender_r, rakedim1)],
  by = "persid",
  all.x = TRUE,
  sort = FALSE
)

if (dat_RAKEDIM1_update[rakedim1 != updated.rakedim1, .N]) stop("Feedback")


# Save
dat_raking_sel <- dat_raking[,
  .(persid, scqage, scqagerange,
    ci_age, ci_gender, impflgag, impflgge,
    age_r, gender_r,
    rakedim1, rakedim2, rakedim3,
    iflg_rakedim1, iflg_rakedim2, iflg_rakedim3)
]

saveRDS(dat_raking_sel, "data/dat_raking_sel.rds")
