# Cases with screener data

# Reset
rm(list = ls())
gc()
source(".Rprofile")

# Load data
fname <- "data/Screener/Cases_Export_20JUL2023.xlsx"
# getSheetNames(file = fname)
dat_scr <- read.xlsx(
  xlsxFile = fname, sheet = "Cases_Export", detectDates = FALSE
) |> setDT() |> setnames(tolower)
rm(fname)

# BQ load
dat_bq <- readRDS(file = "data/dat_bq.rds")

# Raking for age_r
dat_raking <- readRDS(file = "data/dat_raking_sel.rds")
dat_raking[, .(persid, age_r)]

# SDIF for strata & PSU
dat_sdif_sample <- fread(
  file = "data2-sample/sample_piaac_sdif.csvy",
  yaml = TRUE
) |> setnames(tolower)

dat_sdif_sample[, class(caseid)]
dat_sdif_sample[, caseid := as.numeric(caseid)]
dat_sdif_sample[, class(caseid)]

dat_sdif_sample[, .(caseid, strat_psu, id_psu)]

# Final disposition code for household for screener
names(dat_scr)
grep("disp_scr", names(dat_scr), value = T)

unique(dat_scr[, .(caseid, disp_scr)])[, .N, keyby = .(disp_scr)]
unique(dat_scr[, .(caseid, disp_scr)])[disp_scr %in% 1:2, .N, keyby = .(disp_scr)]

dat_scr[, class(caseid)]
dat_scr[, class(persid)]

dat_scr[, .N, keyby = .(caseid)]
dat_scr[, .N, keyby = .(caseid)][, .N, keyby = .(N)]

dat_scr[, .N, keyby = .(caseid, persid)]
dat_scr[!is.na(persid), .N, keyby = .(caseid)][, .N, keyby = .(N)]
x <- dat_scr[!is.na(persid), .N, keyby = .(caseid)][N == max(N), caseid]
dat_scr[caseid %in% x, .N, keyby = .(caseid, persid)]


# PERSVAR1: Education level collected from Screener
# hihgEduCode
dat_scr[, .N, keyby = .(hihgeducode)]
dat_scr[disp_scr %in% 1:2, .N, keyby = .(hihgeducode)]
#   1: Pamatizglītība vai zemāka
#   2: Vidējā izglītība vai profesionālā/arodizglītība
#   3: Augstākā izglītība
# 980: Nezina
# 990: Atteicās atbildēt
dat_scr[, .N, keyby = .(caseid, hihgeducode)]
# dat_scr[hihgeducode %in% 1:3,
#         .(hihgeducode = paste(unique(sort(hihgeducode)), collapse = "-"), .N),
#         keyby = .(caseid)][, .N, keyby = .(hihgeducode)]

dat_scr[hihgeducode %in% 1:3, persvar1 := as.integer(hihgeducode)]
dat_scr[!is.na(persid), iflg_persvar1 := as.integer(is.na(persvar1))]
dat_scr[, .N, keyby = .(!is.na(persid), persvar1, iflg_persvar1)]


# Atlasa
dat_scr_pers <- dat_scr[!is.na(persid),
                        .(caseid, persid, persvar1, iflg_persvar1)]

# Pievieno stratu un PSU
dat_scr_pers <- merge(
  x = dat_scr_pers,
  y = dat_sdif_sample[, .(caseid, strat_psu, id_psu)],
  by = "caseid"
)

# Pievieno vecumu un atlasa tikai aptaujai atlasītās personas
dat_scr_pers <- merge(
  x = dat_scr_pers,
  y = dat_raking[, .(persid, age_r)],
  by = "persid"
)
dat_scr_pers

# Missing for 5.8% of sampled persons. For some of the missing, can you impute them with the education level from BQ? Then for the remaining missing ones, can you impute them using another approach? Since education is highly related with proficiency levels, it would be beneficial to use the education variable for weighting adjustment.

# Pievieno izglītības līmeni no BQ
dat_bq[b2_q01lv %in% 0:16, .N, keyby = .(b2_q01lv)]

if (!"b2_q01lv" %in% names(dat_scr_pers)) {
  dat_scr_pers <- merge(
    x = dat_scr_pers,
    y = dat_bq[b2_q01lv %in% 0:16, .(persid, b2_q01lv)],
    by = "persid",
    all.x = TRUE,
    sort = FALSE
  )
}

dat_scr_pers[, .N, keyby = .(persvar1, iflg_persvar1, b2_q01lv)]
dat_scr_pers[!is.na(b2_q01lv), table(b2_q01lv, persvar1)]

# Impute from BQ
# 1: 0-3
# 2: 4-9
# 3: 10-16
dat_scr_pers[b2_q01lv %in% 0:3,   b2_q01lv_grp := 1L]
dat_scr_pers[b2_q01lv %in% 4:9,   b2_q01lv_grp := 2L]
dat_scr_pers[b2_q01lv %in% 10:16, b2_q01lv_grp := 3L]
dat_scr_pers[is.na(persvar1), persvar1 := b2_q01lv_grp]
dat_scr_pers[, .N, keyby = .(iflg_persvar1, persvar1)]
dat_scr_pers[, b2_q01lv_grp := NULL]

dat_scr_pers[, .N, keyby = .(iflg_persvar1)][, P := prop.table(N)][]
dat_scr_pers[, .N, keyby = .(is.na(persvar1))][, P := prop.table(N)][]

dat_scr_pers[!is.na(persvar1), table(age_r, persvar1)]

dat_scr_pers[, .N, keyby = .(strat_psu, id_psu)]

# dat_scr_pers1 <- VIM::hotdeck(
#   data = dat_scr_pers,
#   variable = "persvar1",
#   ord_var = "age_r",
#   domain_var = "strat_psu"
# )
# 
# dat_scr_pers2 <- VIM::hotdeck(
#   data = dat_scr_pers,
#   variable = "persvar1",
#   ord_var = "age_r",
#   domain_var = "strat_psu"
# )
# 
# all.equal(dat_scr_pers1, dat_scr_pers2)

set.seed(174835)
dat_scr_pers <- VIM::hotdeck(
  data = dat_scr_pers,
  variable = "persvar1",
  ord_var = "age_r",
  domain_var = "strat_psu"
)

ggplot(data = dat_scr_pers, mapping = aes(x = age_r, fill = factor(persvar1))) +
  geom_bar() +
  facet_grid(rows = vars(persvar1_imp), scales = "free_y") +
  theme_bw()

dat_scr_pers[, .N, keyby = .(persvar1)]
dat_scr_pers[, .N, keyby = .(iflg_persvar1)][, P := prop.table(N)][]
dat_scr_pers[, .N, keyby = .(iflg_persvar1, persvar1)]

saveRDS(object = dat_scr_pers[, .(persid, persvar1, iflg_persvar1)],
        file = "data/dat_scr_pers.rds")



# DUVAR_SCRRESP1: Household size
# SCREENER_HHMEMBERS
dat_scrresp1 <- dat_scr[disp_scr %in% 1:2,
                        .(n = .N),
                        keyby = .(caseid, disp_scr, screener_hhmembers)]
dat_scrresp1
dat_scrresp1[, .N, keyby = .(screener_hhmembers)]
dat_scrresp1[, .N, keyby = .(as.numeric(screener_hhmembers))]
dat_scrresp1[, .N, keyby = .(n)]
dat_scrresp1[, .N, keyby = .(n, screener_hhmembers)]
dat_scrresp1[, cor(n, as.numeric(screener_hhmembers))]
# low cor because of 990

dat_scrresp1[!grepl("^[0-9]*$", screener_hhmembers), .N,
        keyby = .(screener_hhmembers)]
dat_scrresp1[, iflg_duvar_scrresp1 := as.integer(
  grepl("\\.", screener_hhmembers) | screener_hhmembers == "990"
)]
dat_scrresp1[, .N, keyby = .(iflg_duvar_scrresp1)]
dat_scrresp1[iflg_duvar_scrresp1 == 0, cor(n, as.numeric(screener_hhmembers))]
# 0.96 OK

dat_scrresp1[, .N, keyby = .(
  as.integer(stringr::str_extract(screener_hhmembers, "[0-9]+")),
  screener_hhmembers,
  iflg_duvar_scrresp1
)]

dat_scrresp1[, screener_hhmembers_orig := screener_hhmembers]
dat_scrresp1[, screener_hhmembers := as.integer(
  stringr::str_extract(screener_hhmembers, "[0-9]+")
)]
dat_scrresp1[, .N, keyby = .(screener_hhmembers, screener_hhmembers_orig,
                             iflg_duvar_scrresp1)]

dat_scrresp1[screener_hhmembers < 99L, duvar_scrresp1 := screener_hhmembers]
dat_scrresp1[screener_hhmembers >= 99L, duvar_scrresp1 := n]

dat_scrresp1[, .N, keyby = .(duvar_scrresp1)]
dat_scrresp1[, .N, keyby = .(duvar_scrresp1, iflg_duvar_scrresp1)]
dat_scrresp1[, .N, keyby = .(duvar_scrresp1, iflg_duvar_scrresp1,
                             screener_hhmembers_orig)]
dat_scrresp1[, .N, keyby = .(caseid, duvar_scrresp1, iflg_duvar_scrresp1)]


# DUVAR_SCRRESP2: Indicator if a child in a household
# age
dat_scr[disp_scr %in% 1:2, .N, keyby = .(age_reg)]
dat_scr[disp_scr %in% 1:2, .N, keyby = .(age)]
dat_scr[disp_scr %in% 1:2, .N, keyby = .(agerange)]

dat_scr[, class(age)]
dat_scr[, .N, keyby = .(nchar(age))]
dat_scr[nchar(age) > 3, .N, keyby = .(age)]
dat_scr[nchar(age) > 3, .(age, dobyy, dobmm,
                          as.Date(as.integer(age), origin = "1899-12-30"))]

dat_scr[!grepl("^[0-9]*$", age), .N, keyby = .(age)]

dat_scr[, iflg_duvar_scrresp2 := as.integer(
  is.na(age) | grepl("\\.", age) | (!is.na(age) & nchar(age) > 3)
)]
dat_scr[disp_scr %in% 1:2, .N, keyby = .(iflg_duvar_scrresp2)]

dat_scr[disp_scr %in% 1:2, .N, keyby = .(
  age,
  as.integer(
    map_chr(
      .x = stringr::str_extract_all(age, "[0-9]+"),
      .f = stringr::str_c,
      collapse = ""
    )
  ),
  iflg_duvar_scrresp2
)]

dat_scr[, age_orig := age]
dat_scr[nchar(age) > 3, .(age)]
dat_scr[nchar(age) > 3, age := "0"]

dat_scr[, age := as.integer(
  map_chr(
    .x = stringr::str_extract_all(age, "[0-9]+"),
    .f = stringr::str_c,
    collapse = ""
  )
)]

dat_scr[, .N, keyby = .(age, iflg_duvar_scrresp2)]
dat_scr[iflg_duvar_scrresp2 == 1, .N,
        keyby = .(age, age_orig, iflg_duvar_scrresp2)]

dat_scr[, .N, keyby = .(!is.na(persid), !is.na(age))]

dat_scrresp2 <- dat_scr[
  disp_scr %in% 1:2,
  .(duvar_scrresp2 = as.integer(any(age < 16, na.rm = TRUE)),
    iflg_duvar_scrresp2 = max(iflg_duvar_scrresp2)),
  by = .(caseid, disp_scr)
]
dat_scrresp2

dat_scrresp2[, .N, keyby = .(duvar_scrresp2)]
dat_scrresp2[, .N, keyby = .(duvar_scrresp2, iflg_duvar_scrresp2)]

dat_scrresp2[is.na(duvar_scrresp2), caseid]

dat_scr[caseid %in% dat_scrresp2[is.na(duvar_scrresp2), caseid],
        .(caseid, persid, age, dobyy, dobmm)]


# Merge
dat_scr_case <- merge(
  x = dat_scrresp1[, .(caseid, duvar_scrresp1, iflg_duvar_scrresp1)],
  y = dat_scrresp2[, .(caseid, duvar_scrresp2, iflg_duvar_scrresp2)],
  by = "caseid",
  all = TRUE
)
dat_scr_case

dat_scr_case[, .N, keyby = .(duvar_scrresp1)]
dat_scr_case[, .N, keyby = .(iflg_duvar_scrresp1)]
dat_scr_case[, .N, keyby = .(duvar_scrresp1, iflg_duvar_scrresp1)]

dat_scr_case[, .N, keyby = .(duvar_scrresp2)]
dat_scr_case[, .N, keyby = .(iflg_duvar_scrresp2)]
dat_scr_case[, .N, keyby = .(duvar_scrresp2, iflg_duvar_scrresp2)]

dat_scr_case[, .N, keyby = .(iflg_duvar_scrresp1, iflg_duvar_scrresp2)]
dat_scr_case[is.na(duvar_scrresp2)]

saveRDS(object = dat_scr_case, file = "data/dat_scr_case.rds")
